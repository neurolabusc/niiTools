unit mindcore; 

{$mode objfpc}{$H+}
{$include isgui.inc} //is this compiled for command line or GUI


interface

uses
  Classes, SysUtils, nii_core, define_types;
const
  kReportMode = 0;
  kNeighborMode = 1;
  kDefMode = kNeighborMode;
  kVers = 'Missingness in Neuroimaging Data (MIND) ALPHA NOT EXTENSIVELY TESTED 9/9/2011';
type
  TImputeOptions = record
    SaveMask,SaveImputed,ReportOnly: boolean;
    SearchRadiusMM: single;
    ReplacementNeighbors,MinObservations: integer;
  end;

function ImputeDefaults: TImputeOptions;

function GroupImpute(lImages: Tstringlist; lOpts: TImputeOptions): boolean; overload;
implementation

function ImputeDefaults: TImputeOptions;
begin
    with result do begin
        SaveMask := true;
        SaveImputed := true;
        ReportOnly:= false;
        SearchRadiusMM:= 18;
        ReplacementNeighbors := 5;
        MinObservations := 1;
    end;
end;


function Sum4DnotNAN(var lNII4D,lNIIsum: TNIFTIImg): boolean;
//given 4D nifti image generates lNIIsum where each voxel reports the number of volumes in the 4D image that are suitable numbers
// in other words, excludes values that are INF+, INF-, NaN, etc.
var
  lnVox,lVox,lnVol,lVol,lOffset: integer;
begin
     result := false;
     lnVox := lNII4D.hdr.dim[1]*lNII4D.hdr.dim[2]*lNII4D.hdr.dim[3];
     lnVol := lNII4D.hdr.dim[4];
     if (lnVox < 1) or (lnVol < 1)  then exit;
    CreateZeroedFloat3DNII(lNII4D,lNIIsum,ChangeFilePostfix(lNII4D.hdrname,'_sum'));
    for lVol := 1 to lnVol do begin
        lOffset := (lVol-1)*lnVox;
        for lVox := 1 to lnVox do begin
            if not specialsingle(lNII4D.f32^[lVox+lOffset]) then
               lNIIsum.f32^[lVox] := lNIIsum.f32^[lVox] + 1;
        end;//for each vox
    end;//for each vol
    result := true;
end;


function ReportGroupImpute(var lNII4D: TNIFTIImg; lSaveMask: boolean): boolean;
//reports number of voxels with missing data.
//returns true if there is some data available to impute....
label
     666;
var
 lNIImask: TNIFTIImg;
 lnVox,lVox,lnVol,lVol,lnPartial,lnTotal: integer;
 lCount: array of integer;
begin
     result := false;
     riteln('Group Impute Report');
     lnVox := lNII4D.hdr.dim[1]*lNII4D.hdr.dim[2]*lNII4D.hdr.dim[3];
     lnVol := lNII4D.hdr.dim[4];
     if (lnVox < 2) or  (lnVol < 2) then begin
       riteln('GroupImputeReport requires at least 2 volumes each with multiple voxels');
       exit;
     end;
     CreateNII(lNIImask);
     if not Sum4DnotNAN( lNII4D,lNIImask) then goto 666;
     if lSaveMask then begin  //save sum map to disk
       riteln('Saving coverage map showing number of images contributing to each voxel '+ lNIImask.HdrName);
       WriteNII(lNIImask.HdrName,lNIImask);
     end;
     Setlength(lCount,lnVol+1);
     for lVox := 1 to lnVox do begin
          lVol := round(lNIImask.f32^[lVox]);
          if lVol <= lnVol then
             inc(lCount[lVol]);
     end;
     for lVol := 0 to lnVol do
         riteln('Voxels with data from '+inttostr(lVol)+' images:'+kTab+inttostr(lCount[lVol]) );
     lnPartial := 0;
     for lVol := 1 to lnVol-1 do
            lnPartial := lnPartial + lCount[lVol];
     lnTotal := lnPartial+ lCount[0]+lCount[lnVol];
     if lnTotal > 0 then
        riteln('Voxels with partial coverage (which could be imputed):'+kTab+realtostr(lnPartial/lnTotal*100,2)+'%' );
     if lnPartial <> lnTotal then
              result := true;
     lCount := nil;
     FreeNII(lNIIMask);
666:
end;

function Impute(var lNII4D, lNIImask: TNIFTIImg; lSearchRadiusMM: single; lReplacementNeighbors,MinObservations: integer): boolean;
//imputes voxels that are NaN in lNII4D but not NaN in lNIImask...
type
  TPt =  RECORD //peristimulus plot
    X,Y,Z,O: integer; //x,y,z relative and offset to origin
    DxOK: boolean;//Dx: single; //distance from origin
  end;
label
     666;
var
 nImpute,lVoxMask,lSearchVox,lX,lY,lZ,lnVox,lVox,lnVol,lVol,Xdim,Ydim,Zdim: integer;
 lDx: single;
 lBox,lSphere: array of TPt;
 lValidNeighbors: array of single;
function ImputeSearch: single;
var
 lS: integer;
 lOK: integer;
 lSum: double;
begin
     //look in search sphere for voxels with data...
     result := kNaNs;
     lOK := 0;
     for lS := 0 to (lSearchVox-1) do begin

          if ((lX+lSphere[lS].X)>0) and ((lY+lSphere[lS].Y)>0) and ((lZ+lSphere[lS].Z)>0)
          and ((lX+lSphere[lS].X)<= lNII4D.hdr.dim[1]) and ((lY+lSphere[lS].Y)<= lNII4D.hdr.dim[2]) and ((lZ+lSphere[lS].Z)<= lNII4D.hdr.dim[3])
          and (not specialsingle(lNII4D.f32^[lVox+lSphere[lS].O]) ) then begin
            lValidNeighbors[lOK] :=lNII4D.f32^[lVox+lSphere[lS].O];
            inc(lOK);
          end;
     end;
     if lOK < 1 then
        exit;
     lSum := 0;
     for lS := 1 to  lReplacementNeighbors do
         lSum := lSum + lValidNeighbors[random(lOK)];
     result := lSum/lReplacementNeighbors;
     inc(nImpute); //success!
end;
begin
     result := false;
     if (lReplacementNeighbors < 1) or (lSearchRadiusMM <= 0) then begin
       riteln('Impute error: invalid values for search radius and/or replacement neighbors.');
       exit;
     end;
     lnVox := lNII4D.hdr.dim[1]*lNII4D.hdr.dim[2]*lNII4D.hdr.dim[3];
     lnVol := lNII4D.hdr.dim[4];
     if (lNII4D.hdr.pixdim[1] = 0) or (lNII4D.hdr.pixdim[2] = 0) or (lNII4D.hdr.pixdim[3]= 0) then begin
       riteln('Impute error: header specifies dimension with 0mm spacing.');
       exit;
     end;
     if (lnVox < 2) or  (lnVol < 2) then begin
       riteln('Impute requires at least 2 volumes each with multiple voxels');
       exit;
     end;
     Xdim := trunc(lSearchRadiusMM/lNII4D.hdr.pixdim[1]);
     Ydim := trunc(lSearchRadiusMM/lNII4D.hdr.pixdim[2]);
     Zdim := trunc(lSearchRadiusMM/lNII4D.hdr.pixdim[3]);
     if (Xdim = 0) and (Ydim = 0) and (Zdim = 0) then begin
       riteln('Impute SearchRadiusMM too small for voxel resolution');
       exit;
     end;
     //next: make search sphere
     setlength(lBox,((Xdim *2)+1)*((Ydim *2)+1)*((Zdim *2)+1));
     lVox := 0;
     lSearchVox := 0;
     for lZ := -Zdim to Zdim do
         for lY := -Ydim to YDim do
             for lX := -Xdim to XDim do begin
                 lBox[lVox].X := lX;
                 lBox[lVox].Y := lY;
                 lBox[lVox].Z := lZ;
                 lBox[lVox].O := lX+ (lY*lNII4D.hdr.dim[1])+(lZ*lNII4D.hdr.dim[1]*lNII4D.hdr.dim[2] );
                 //next: distance by pythagorean theorem...
                 lDx := sqrt( sqr(lX*lNII4D.hdr.pixdim[1])+ sqr(lY*lNII4D.hdr.pixdim[2])+ sqr(lZ*lNII4D.hdr.pixdim[3])  );
                 //if lVox < 10 then
                 //   riteln(inttostr(lBox[lVox].X)+kTab+ inttostr(lBox[lVox].Y)+kTab+inttostr(lBox[lVox].Z)+kTab+inttostr(lBox[lVox].O) );
                 if (lDx <=lSearchRadiusMM) and (lDx > 0) then begin
                    lBox[lVox].DxOK := true;
                    inc(lSearchVox);
                 end else
                     lBox[lVox].DxOK := false;
                 inc(lVox);
             end;
     if lSearchVox < lReplacementNeighbors then begin
       riteln('Impute SearchRadiusMM too small for voxel resolution');
       lBox := nil;
       exit;
     end;
     riteln('Imputation will select '+inttostr(lReplacementNeighbors)+' neighbors from the surrounding '+inttostr(lSearchVox)+' neighboring voxels');
     setlength(lSphere,lSearchVox);
     setlength(lValidNeighbors,lSearchVox);
     lSearchVox := 0;
     for lVox := 0 to ( ((Xdim *2)+1)*((Ydim *2)+1)*((Zdim *2)+1)-1) do
         if (lBox[lVox].DxOK) then begin
            lSphere[lSearchVox] := lBox[lVox];
            inc(lSearchVox);
         end;
     //next - for each NaN voxel in all volumes, impute neighboring values....
     nImpute := 0;
     lVox := 0;
     for lVol := 1 to lnVol do begin
         lVoxMask := 0;
         for lZ := 1 to lNII4D.hdr.dim[3] do begin
             for lY := 1 to lNII4D.hdr.dim[2] do begin
                 for lX := 1 to lNII4D.hdr.dim[1] do begin
                     inc(lVox);
                     inc(lVoxMask);
                     if (lNIImask.f32^[lVoxMask]>= MinObservations) and (specialsingle(lNII4D.f32^[lVox]))  then begin
                       lNII4D.f32^[lVox] := ImputeSearch;
                     end;
                 end;//for lX
             end; //for lY
         end; //for lZ

     end; //for each volume
     if nImpute > 0 then begin
       result := true;
       riteln('Imputed '+inttostr(nImpute)+' voxels');
     end else
         riteln('No voxels found to impute');
     lSphere := nil;
     lBox := nil;
     lValidNeighbors := nil;
666:
end;
function GroupImpute(lImages: Tstringlist; var lNII4D: TNIFTIImg; lOpts: TImputeOptions): boolean; overload;
label
     666;
var
 lNIImask: TNIFTIImg;
begin
     result := false;
     riteln(kVers);
     if not ReportGroupImpute(lNII4D, lOpts.SaveMask) then begin  //group impute checks that there are is some missing data and that the data dimensions are approrpiate
        riteln('Group impute error: perhaps no voxels are missing in only a portion of the group?');
        exit;
     end;
     if lOpts.ReportOnly then
        exit;
     CreateNII(lNIImask);
     if not Sum4DnotNAN( lNII4D,lNIImask) then goto 666;
     if Impute(lNII4D,lNIImask,lOpts.SearchRadiusMM,lOpts.ReplacementNeighbors,lOpts.MinObservations) then begin
        ReportGroupImpute(lNII4D,false);
        if lOpts.SaveImputed then begin
           if (lImages.Count = lNII4D.hdr.dim[4]) then
              Save4DTo3D(lImages,lNII4D,'i')
           else
            WriteNII(ChangeFilePrefix(lNII4D.hdrname,'i'),lNII4D);
        end; //if saveimpute
     end; //if impute succeeded
     FreeNII(lNIIMask);
     result := true;
666:
end;


function GroupImpute(lImages: Tstringlist; lOpts: TImputeOptions): boolean; overload;
var
 {$IFDEF GUI}s: string; i: integer; {$ENDIF}
 lNII4D: TNIFTIImg;
begin
     result := false;
     if lImages.Count < 2 then begin
        riteln('Impute error: at least 2 images required');
        exit;
     end;
     {$IFDEF GUI}
     if lOpts.ReportOnly then
        i := kReportMode
     else
        i := kNeighborMode;
     s := ' mind -m'+inttostr(i) +' -n '+inttostr(lOpts. ReplacementNeighbors)+' -o '+inttostr(lOpts.MinObservations)+ ' -s '+realtostr(lOpts.SearchRadiusMM,3);
     for i := 0 to (lImages.Count -1) do
         s := s + ' "'+lImages[i]+'"';
     riteln('Command line instruction (e.g. can be called from matlab script):');
     riteln(s);
     {$ENDIF}
     CreateNII(lNII4D);
     if Read4DF32(lImages, lNII4D) then
        result := GroupImpute(lImages,lNII4D,lOpts)
     else
         riteln('GroupImpute error loading 4D images');
     FreeNII(lNII4D);
end;

end.

