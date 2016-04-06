unit nii_smooth;

{$IFDEF FPC}{$mode objfpc}{$H+} {$ENDIF}
{$include isgui.inc}
interface

uses
   {$IFDEF GUI} Forms, {$ENDIF}//required for application.processmessages
  classes, define_types,nii_core, dialogsx, sysutils;

procedure nii_smooth_cubic (var lNII4D:TNIfTIimg); overload;
procedure nii_smooth_cubic (lImages: Tstrings); overload;

function nii_slicetimecorrect_cubic (var lNII4D:TNIfTIimg; lSliceOrder: integer): boolean; overload;
function nii_slicetimecorrect_cubic (lImages: Tstrings; lSliceOrder: integer):boolean; overload;

function nii_smooth_gauss (var lNII4D:TNIfTIimg; lFWHMmm: single):boolean; overload;
function nii_smooth_gauss (lImages: Tstrings; lFHWMmm: single):boolean; overload;
function nii_smooth_gauss (lImage: string; lFHWMmm: single):boolean; overload;

function nii_smooth_gauss_sec (var lNII4D:TNIfTIimg; lFWHMsec: single):boolean; overload;
function nii_smooth_gauss_sec (lImages: Tstrings; lFHWMsec: single):boolean; overload;
function nii_smooth_slice_order (lSlice,lnSlices,lSliceOrder: integer): integer;

implementation

function nii_smooth_gauss_sec (var lNII4D:TNIfTIimg; lFWHMsec: single):boolean; overload;
//temporal smooth
var
lra: singlep0;
lMin,lMax,lcutoffvox,lVox,lnVox,lnVol,lVol,
lV,lPos: integer;
  lImg: singlep;
  lWt,lSum: double;
procedure CreatKernel (lsec: single);
var
lI: integer;
lsigma,lexpd,lcumgaussr: double;
begin
   lsigma  := (lFWHMsec/lsec)/sqrt(8*ln(2));  //      % FWHM -> sigma
   lcutoffvox  := round(6*lsigma);       //    % highest / lowest voxel to go out to
   getmem(lra,(lcutoffvox+1)*sizeof(single));
   lexpd := 2*lsigma*lsigma;
   lCumGaussR := 0;
   for lI := 0 to lcutoffvox do begin
          lra^[lI] := exp(-1*(lI*lI)/lexpd) ;
          lCumGaussR := lCumGaussR + lra^[lI];
   end;
   lCumGaussR := 2*lCumGaussR - lra^[0];
   if lCumGaussR = 0 then
   	exit;
   for lI := 0 to lcutoffvox do
   	 lra^[lI] := lra^[lI]/lCumGaussR;
end;

begin
      result := false;
      lnVox := lNII4D.hdr.dim[1]*lNII4D.hdr.dim[2]*lNII4D.hdr.dim[3];
      lnVol := lNII4D.hdr.dim[4];
      if lnVol < 1 then lnVol := 1;
      if (lnVox < 1) or (lNII4D.hdr.datatype <> kDT_FLOAT) or (lNII4D.hdr.pixdim[4]<= 0)  then begin
         riteln('nii_smooth_gauss_sec error: smoothing only available for 4D 32-bit single precision data with defined temporal duration.');
         exit;
      end;
      for lV := 1 to (lnVol*lnVox) do
      	 if specialsingle(lNII4D.f32^[lV]) then begin
          	riteln('nii_smooth_gauss_sec error: this image has special values (e.g. infinity, not-a-number).');
             exit;

      	 end;
      getmem(lImg,lnVox * sizeof(single));
      riteln('Gaussian temporal smoothing '+inttostr(lnVol)+' volumes with with a TR of '+realtostr(lNII4D.hdr.pixdim[4],3)+'sec used a '+realtostr(lFWHMsec,3)+'sec FWHM [this may take a while...].');
      CreatKernel (lNII4D.hdr.pixdim[4]);
      getmem(lImg,lnVol*sizeof(single));
      for lVox := 1 to lnVox do begin
      	  for lVol := 1 to lnVol do
            lImg^[lVol] := lNII4D.f32^[lVox+((lVol-1)*lnVox)] ;//copy raw data <- these values should all get overwritten, only if Wt<= 0
          for lVol := 1 to lnVol do begin
              lMin := lVol - lCutoffVox;
              lMax := lVol + lCutoffVox;
              lMin := Bound(lMin,1,lnVol);
              lMax := Bound(lMax,1,lnVol);
              lWt := 0;
              lSum := 0;
              for lPos := lMin to lMax do begin
                 lWt := lWt+lra^[abs(lVol-lPos)] ;
                 lSum := lSum + lra^[abs(lVol-lPos)]*lNII4D.f32^[lVox+((lPos-1)*lnVox)] ;
             end;
             if lWt > 0 then
              lImg^[lVol] :=  lSum/lWt;
          end;
      	  for lVol := 1 to lnVol do
            lNII4D.f32^[lVox+((lVol-1)*lnVox)] := lImg^[lVol];//paste smoothed data
 	  end; //for each vox
     freemem(lra);
     freemem(lImg);
     result := true;
end; //nii_smooth_gauss_sec

function nii_smooth_gauss_sec (lImages: Tstrings; lFHWMsec: single):boolean; overload;
var
  lNImg,lI: integer;
  lNIIImg: TNIfTIimg;
begin
     result := false;
     lNImg := lImages.Count;
     if lNImg < 1 then begin
        riteln('nii_smooth_gauss error: no files');
      exit;
     end;
     CreateNII(lNIIImg);
     for lI := 1 to lNImg do begin
         if not Read4DF32(lImages[lI-1], lNIIImg) then begin
            showmsg('Error loading '+lImages[lI-1]);
         end else begin
             lNIIImg.HdrName := changefileprefix(lImages[lI-1],'s');//smoothed
             result := nii_smooth_gauss_sec(lNIIImg,lFHWMsec);
             if result then begin

             	WriteNII(lNIIImg.HdrName,lNIIImg);
          	 	riteln('temporally smoothed '+lNIIImg.HdrName);
             end;

         end;
     end;
     FreeNII(lNIIImg);
end;


function nii_smooth_gauss (var lNII4D:TNIfTIimg; lFWHMmm: single):boolean; overload;
var
  lMin,lMax,lcutoffvox,lVol,lnVol,lVolo,
  lV,lnVox,lPos,lZo,lYo,lXY,
    lX,lY,lZ: integer;
    lImg: singlep;
    lWt,lSum: double;
  lra: singlep0;
procedure CreatKernel (lmm: single);
var
  lI: integer;
  lsigma,lexpd,lcumgaussr: double;
begin
	 lsigma  := (lFWHMmm/lmm)/sqrt(8*ln(2));  //      % FWHM -> sigma
     lcutoffvox  := round(6*lsigma);       //    % highest / lowest voxel to go out to
     getmem(lra,(lcutoffvox+1)*sizeof(single));
     lexpd := 2*lsigma*lsigma;
     lCumGaussR := 0;
     for lI := 0 to lcutoffvox do begin
            lra^[lI] := exp(-1*(lI*lI)/lexpd) ;
            lCumGaussR := lCumGaussR + lra^[lI];
     end;
     lCumGaussR := 2*lCumGaussR - lra^[0];
     if lCumGaussR = 0 then
     	exit;
     for lI := 0 to lcutoffvox do
     	 lra^[lI] := lra^[lI]/lCumGaussR;
end;

begin
     result := false;
     lXY := lNII4D.hdr.dim[1]*lNII4D.hdr.dim[2];
     lnVox := lXY*lNII4D.hdr.dim[3];
     lnVol := lNII4D.hdr.dim[4];
     if lnVol < 1 then lnVol := 1;
     if (lnVox < 1) or (lNII4D.hdr.datatype <> kDT_FLOAT) or (lNII4D.hdr.dim[1]<3) or (lNII4D.hdr.dim[2]<3) or  (lNII4D.hdr.dim[3]<3)  then begin
        riteln('nii_smooth_gauss error: smoothing only available for 3D 32-bit single precision data.');
        exit;
     end;
     for lV := 1 to (lnVol*lnVox) do
     	 if specialsingle(lNII4D.f32^[lV]) then begin
         	riteln('nii_smooth_gauss error: this image has special values (e.g. infinity, not-a-number). Perhaps try cubic smooth');
            exit;

     	 end;
     getmem(lImg,lnVox * sizeof(single));
     riteln('Gaussian spatial smoothing '+inttostr(lnVol)+' volumes with '+realtostr(lFWHMmm,2)+'mm FWHM [this may take a while...].');
for lVol := 1 to lnVol do begin
    DebugFractionCompleted := lVol/lnVol;
     {$IFDEF GUI} application.processmessages;{$ENDIF}
    lVolo := (lVol-1)*lnVox;//volume offset
    for lV := 1 to lnVox do
     	 lImg^[lV] := lNII4D.f32^[lV+lVolo];
     //blur in X direction
     CreatKernel (lNII4D.hdr.pixdim[1]);
     for lZ := 1 to (lNII4D.hdr.dim[3]) do begin
         lZo := (lZ-1)*lXY;

         for lY := 1 to (lNII4D.hdr.dim[2]) do  begin
             lYo := (lY-1)*lNII4D.hdr.dim[1] ;
             for lX := 1 to (lNII4D.hdr.dim[1]) do begin
                 lMin := lX - lCutoffVox;
                 lMax := lX + lCutoffVox;
                 lMin := Bound(lMin,1,lNII4D.hdr.dim[1]);
                 lMax := Bound(lMax,1,lNII4D.hdr.dim[1]);
                 lWt := 0;
                 lSum := 0;
                 for lPos := lMin to lMax do begin
                    lWt := lWt+lra^[abs(lX-lPos)] ;
                    lSum := lSum + lra^[abs(lX-lPos)]*lNII4D.f32^[lZo+lYo+lPos+lVolo] ;
                end;
                if lWt > 0 then
                 lImg^[lZo+lYo+lX] :=  lSum/lWt;
             end;
         end;
     end;
     freemem(lra);
     for lV := 1 to lnVox do
     	 lNII4D.f32^[lV+lVolo] := lImg^[lV];
     //blur in Y direction
     CreatKernel (lNII4D.hdr.pixdim[2]);
     for lZ := 1 to (lNII4D.hdr.dim[3]) do begin
         lZo := (lZ-1)*lXY;

         for lX := 1 to (lNII4D.hdr.dim[1]) do  begin
             for lY := 1 to (lNII4D.hdr.dim[2]) do begin
                 lYo := (lY-1)*lNII4D.hdr.dim[1] ;
             	 lMin := lY - lCutoffVox;
                 lMax := lY + lCutoffVox;
                 lMin := Bound(lMin,1,lNII4D.hdr.dim[2]);
                 lMax := Bound(lMax,1,lNII4D.hdr.dim[2]);
                 lWt := 0;
                 lSum := 0;
                 for lPos := lMin to lMax do begin
                    lWt := lWt+lra^[abs(lY-lPos)] ;
                    lSum := lSum + lra^[abs(lY-lPos)]*lNII4D.f32^[lZo+lX+((lPos-1)*lNII4D.hdr.dim[1])+lVolo] ;
                end;
                if lWt > 0 then
                 lImg^[lZo+lYo+lX] :=  lSum/lWt;
             end;
         end;
     end;
     freemem(lra);
     for lV := 1 to lnVox do
     	 lNII4D.f32^[lV+lVolo] := lImg^[lV];
     //blur in Z direction
     CreatKernel (lNII4D.hdr.pixdim[3]);
     for lY := 1 to (lNII4D.hdr.dim[2]) do begin
         lYo := (lY-1)*lNII4D.hdr.dim[1] ;
         for lX := 1 to (lNII4D.hdr.dim[1]) do  begin
             for lZ:= 1 to (lNII4D.hdr.dim[3]) do begin
                  lZo := (lZ-1)*lXY;
             	 lMin := lZ - lCutoffVox;
                 lMax := lZ + lCutoffVox;
                 lMin := Bound(lMin,1,lNII4D.hdr.dim[3]);
                 lMax := Bound(lMax,1,lNII4D.hdr.dim[3]);
                 lWt := 0;
                 lSum := 0;
                 for lPos := lMin to lMax do begin
                    lWt := lWt+lra^[abs(lZ-lPos)] ;
                    lSum := lSum + lra^[abs(lZ-lPos)]*lNII4D.f32^[lYo+lX+((lPos-1)*lXY)+lVolo] ;
                end;
                if lWt > 0 then
                 lImg^[lZo+lYo+lX] :=  lSum/lWt;
             end;
         end;
     end;
     freemem(lra);
     for lV := 1 to lnVox do
     	 lNII4D.f32^[lV+lVolo] := lImg^[lV];
end;//for each vol
     freemem(lImg);
     result := true;
end;

function nii_smooth_gauss (lImage: string; lFHWMmm: single):boolean; overload;
var
lNIIImg: TNIfTIimg;
begin
     result := false;
     CreateNII(lNIIImg);
     if not Read4DF32(lImage, lNIIImg) then
        exit;
     result := nii_smooth_gauss(lNIIImg,lFHWMmm);
     if result then
        WriteNII(changefileprefix(lImage,'s'),lNIIImg);
     FreeNII(lNIIImg);
end;

function nii_smooth_gauss (lImages: Tstrings; lFHWMmm: single):boolean; overload;
var
  lNImg,lI: integer;
  lNIIImg: TNIfTIimg;
begin
     result := false;
     lNImg := lImages.Count;
     if lNImg < 1 then begin
        riteln('nii_smooth_gauss error: no files');
      exit;
     end;
     CreateNII(lNIIImg);
     for lI := 1 to lNImg do begin
         if not Read4DF32(lImages[lI-1], lNIIImg) then begin
            showmsg('Error loading '+lImages[lI-1]);
         end else begin
             lNIIImg.HdrName := changefileprefix(lImages[lI-1],'s');//smoothed
             result := nii_smooth_gauss(lNIIImg,lFHWMmm);
             if result then begin

             	WriteNII(lNIIImg.HdrName,lNIIImg);
          	 	riteln('smoothed '+lNIIImg.HdrName);
             end;

         end;
     end;
     FreeNII(lNIIImg);
end;

procedure nii_smooth_cubic (var lNII4D:TNIfTIimg); overload;
const
     kNaN : single = 1/0;
var
  lVol,lnVol,lVolo,
  lV,lnVox,lPos,lZo,lYo,lXY,
    lX,lY,lZ: integer;
    lImg: singlep;
    lWt,lSum: double;
procedure AddWeight(lAddPos: integer; lAddWeight: double);
begin
	 if lNII4D.f32^[lAddPos] <> kNaN then begin
     	lSum := lSum + lNII4D.f32^[lAddPos+lVolo];
        lWt := lWt + lAddWeight;
	 end;

end;//nested proc AddWeight
begin
     lXY := lNII4D.hdr.dim[1]*lNII4D.hdr.dim[2];
     lnVox := lXY*lNII4D.hdr.dim[3];
     lnVol := lNII4D.hdr.dim[4];
     if lnVol < 1 then lnVol := 1;
     if (lnVox < 1) or (lNII4D.hdr.datatype <> kDT_FLOAT) or (lNII4D.hdr.dim[1]<3) or (lNII4D.hdr.dim[2]<3) or  (lNII4D.hdr.dim[3]<3)  then begin
        riteln('nii_smooth error: smoothing only available for 32-bit single precision data.');
        exit;
     end;
     getmem(lImg,lnVox * sizeof(single));
     riteln('Smoothing '+inttostr(lnVol)+' volumes with Cubic B-Spline [this may take a while...].');
for lVol := 1 to lnVol do begin
    lVolo := (lVol-1)*lnVox;//volume offset
    for lV := 1 to lnVox do
     	 lImg^[lV] := lNII4D.f32^[lV+lVolo];
     //blur in X direction
     for lZ := 2 to (lNII4D.hdr.dim[3]-1) do begin
         lZo := (lZ-1)*lXY;
         for lY := 2 to (lNII4D.hdr.dim[2]-1) do  begin
             lYo := (lY-1)*lNII4D.hdr.dim[1] ;
             for lX := 2 to (lNII4D.hdr.dim[1]-1) do begin
                 lWt := 0;
                 lSum := 0;
                 lPos := lZo+lYo+lX;
                 AddWeight(lPos-1,1/6);
                 AddWeight(lPos,4/6);
                 AddWeight(lPos+1,1/6);
                 if lWt > 0.5 then
                 	lImg^[lV] := lSum/lWt;
             end;
         end;
     end;
     for lV := 1 to lnVox do
     	 lNII4D.f32^[lV+lVolo] := lImg^[lV];
     //blur in Y direction
     for lZ := 2 to (lNII4D.hdr.dim[3]-1) do begin
         lZo := (lZ-1)*lXY;
         for lY := 2 to (lNII4D.hdr.dim[2]-1) do  begin
             lYo := (lY-1)*lNII4D.hdr.dim[1] ;
             for lX := 2 to (lNII4D.hdr.dim[1]-1) do begin
                 lWt := 0;
                 lSum := 0;
                 lPos := lZo+lYo+lX;
                 AddWeight(lPos-lNII4D.hdr.dim[1],1/6);
                 AddWeight(lPos,4/6);
                 AddWeight(lPos+lNII4D.hdr.dim[1],1/6);
                 if lWt > 0 then
                 	lImg^[lV] := lSum/lWt;
             end;
         end;
     end;
     for lV := 1 to lnVox do
     	 lNII4D.f32^[lV+lVolo] := lImg^[lV];
     //blur in Z direction
     for lZ := 2 to (lNII4D.hdr.dim[3]-1) do begin
         lZo := (lZ-1)*lXY;
         for lY := 2 to (lNII4D.hdr.dim[2]-1) do  begin
             lYo := (lY-1)*lNII4D.hdr.dim[1] ;
             for lX := 2 to (lNII4D.hdr.dim[1]-1) do begin
                 lWt := 0;
                 lSum := 0;
                 lPos := lZo+lYo+lX;
                 AddWeight(lPos-lXY,1/6);
                 AddWeight(lPos,4/6);
                 AddWeight(lPos+lXY,1/6);
                 if lWt > 0 then
                 	lImg^[lV] := lSum/lWt;
             end;
         end;
     end;
     for lV := 1 to lnVox do
     	 lNII4D.f32^[lV+lVolo] := lImg^[lV];
end;//for each vol

     freemem(lImg);
end;

procedure nii_smooth_cubic (lImages: Tstrings); overload;
var
  lNImg,lI: integer;
  lNIIImg: TNIfTIimg;
begin
     lNImg := lImages.Count;
     if lNImg < 1 then
      exit;
     CreateNII(lNIIImg);


     for lI := 1 to lNImg do begin
         if not Read4DF32(lImages[lI-1], lNIIImg) then begin
            showmsg('Error loading '+lImages[lI-1]);
         end else begin
         	 nii_smooth_cubic(lNIIImg);
          	 lNIIImg.HdrName := changefileprefix(lImages[lI-1],'s');//smoothed
             WriteNII(lNIIImg.HdrName,lNIIImg);
          	 riteln('smoothed '+lNIIImg.HdrName);

         end;
     end;
     FreeNII(lNIIImg);
end;

function KeysHt(x: single; B,C: single): single;
//This function creates Keys Filters.
// You can make the code a tiny bit faster for cubic B-Spline and Catmull-Rom by removing all the factors multiplied by C or B respectively
//  However, the speed up is small and this provides a universal source
// 'B' (as used for 'B-spline' curves) and 'C' ('Cardinal' curves, referred to as 'A' in descriptions of 'Keys' filter a value)
//http://http.developer.nvidia.com/GPUGems/gpugems_ch24.html
   // B = 1,   C = 0   - cubic B-spline
   // B = 1/3, C = 1/3 - recommended  Mitchell Netravali  Filter
   // B = 0,   C = 1/2 - Catmull-Rom spline
//http://www.imagemagick.org/Usage/img_diagrams/cubic_survey.gif
//http://www.imagemagick.org/Usage/resize/#filter_cubics
//http://www.imagemagick.org/Usage/resize/#mitchell
//http://www.imagemagick.org/Usage/resize/#catrom
var
   ax: single;
begin
  ax := abs(x);
  if (ax < 1) then
     result := ((12 - 9 * B - 6 * C) * ax * ax * ax +
            (-18 + 12 * B + 6 * C) * ax * ax + (6 - 2 * B)) / 6
  else if  (ax < 2) then
      result :=  ((-B - 6 * C) * ax * ax * ax +
              (6 * B + 30 * C) * ax * ax + (-12 * B - 48 * C) *
              ax + (8 * B + 24 * C)) / 6
  else
      result := 0;
end;

(*function CatRomHt(x: single): single;
begin
     result := KeysHt(x,0,1/2);
end;

function MitchellHt(x: single): single;
begin
     result := KeysHt(x,1/3,1/3);
end;  *)

function CubicHt(x: single): single;
begin
     result := KeysHt(x,1,0);
end;


(*function FilterHt (X: single; lFilt: integer): single;
begin
     case lFilt of
          kLanczosFilter: result := LanczosHt(X);
          kCatRomFilter: result := CatRomHt(X);
          kMitchellFilter: result := MitchellHt(X);
          else result := CubicHt(X);
     end;
end;*)

function SliceOrder (lSlice,lnSlices,lSliceOrder: integer): integer;
var
   lSliceO : integer;
begin
  //simulataneous: 1,2,3,4 = 1,1,1,1
  //ascending: 1,2,3,4 = 1,2,3,4
  //descending: 1,2,3,4 = 4,3,2,1
  //AscendingInterleavedPhilGE : 1,2,3,4 = 1,3,2,4
  //DescendingInterleavedPhilGE: 1,2,3,4 = 4,2,3,1
  //For interleaved, Siemens is a bit weird in that interleaved acquisitions start on the first slice if you have an odd number of total slices, but on the second slice if you have an even number." (http://www.nitrc.org/forum/forum.php?thread_id=3785&forum_id=1456)
  if (lSliceOrder = kDescending) or (lSliceOrder = kDescendingInterleavedPhilGE) or (lSliceOrder = kDescendingInterleavedSiemens) then
    lSliceO := (lnSlices+1)-lSlice
  else
    lSliceO := lSlice;
  if (lSliceOrder = kAscendingInterleavedPhilGE) or (lSliceOrder = kDescendingInterleavedPhilGE)
     or (odd(lnSlices) and (lSliceOrder = kAscendingInterleavedSiemens)) or (odd(lnSlices) and (lSliceOrder = kDescendingInterleavedSiemens)) then begin //adjust for interleaved
     if odd(lSliceO) then
       lSliceO := (lSliceO+1) div 2 //slice 1,3,5 = 1,2,3
     else
       lSliceO := ((lnSlices+1) div 2)+ (lSliceO div 2); // if 14 slices, 2,4,6 = 8,9,10
  end else if (lSliceOrder = kAscendingInterleavedSiemens) or (lSliceOrder = kDescendingInterleavedSiemens) then begin  //Siemens Interleaved with EVEN slices
     if odd(lSliceO) then
        lSliceO := ((lnSlices+1) div 2)+ ((lSliceO+1) div 2) // if 14 slices, 1,2,3= 8,9,10
     else
        lSliceO := (lSliceO) div 2; //slice 2,4,6 = 1,2,3
  end;

  if (lSliceOrder =kSimultaneous) then lSliceO := 1;
  result := lSliceO;
  (*

     if (lSliceOrder = kDescending) or (lSliceOrder =kDescendingInterleaved) then
       result := (lnSlices+1)-lSlice
     else
       result := lSlice;
     if (lSliceOrder = kAscendingInterleaved) or (lSliceOrder =kDescendingInterleaved) then begin //adjust for interleaved
        if odd(result) then
          result := (result+1) div 2 //slice 1,3,5 = 1,2,3
        else
          result := ((lnSlices+1) div 2)+ ((result) div 2); // if 14 slices, 2,4,6 = 8,9,10
     end;   *)
end;

function nii_smooth_slice_order (lSlice,lnSlices,lSliceOrder: integer): integer;
begin
     result := SliceOrder (lSlice,lnSlices,lSliceOrder);
end;

function nii_slicetimecorrect_cubic (var lNII4D:TNIfTIimg; lSliceOrder: integer): boolean; overload;
var
   lD,lHtm2,lHtm1,lHt,lHtp1: double;
   lVolo,lVol,lnVol,lVox,lnVox,lnXY,lXY,lZ,lnZ: integer;
   lImg,lZFrac: singlep;
begin
     result := false;
     if (lSliceOrder < kAscending)  or ( lSliceOrder > kDescendingInterleavedSiemens) then begin
        showmsg('Slice timing correction skipped: slice order unknown or simultaneous '+inttostr(lSliceOrder));
        exit;
	 end;
 lnXY := lNII4D.hdr.dim[1]*lNII4D.hdr.dim[2]; //slice size
 lnZ := lNII4D.hdr.dim[3];
 lnVox := lnXY*lnZ;
 lnVol := lNII4D.hdr.dim[4];
 if (lnVox < 1) or (lNII4D.hdr.datatype <> kDT_FLOAT) or (lNII4D.hdr.dim[1]<3) or (lNII4D.hdr.dim[2]<3) or  (lnZ<2)  then begin
    riteln('nii_slicetimecorrect_cubic error: available for 32-bit single precision 4D data.');
    exit;
 end;
 if lnVol < 5  then begin
    riteln('nii_slicetimecorrect_cubic error: requires at least 5 volumes.');
    exit;

 end;
 getmem(lImg,lnVox *lnVol* sizeof(single));
 getmem(lZFrac, lNII4D.hdr.dim[3]* sizeof(single));
 riteln('Slice-time correction '+inttostr(lnVol)+' volumes with Cubic B-Spline [this may take a while...].');
 for lVox := 1 to (lnVox*lnVol) do
 	 lImg^[lVox] := lNII4D.f32^[lVox];
 for lZ := 1 to lnZ do
  	lZFrac^[lZ] := (SliceOrder (lZ,lnZ,lSliceOrder)/(lnZ+1)){-0.5};

(* for lZ := -20 to 20 do begin
     lS := lZ/10;
     riteln('Ht '+floattostr(lS)+' time shifted '+floattostr(CubicHt(lS)));
 end; *)


 for lZ := 1 to lnZ do
 	 riteln('slice '+inttostr(lZ)+' time shifted '+floattostr(lZFrac^[lZ])+' TR');

 for lVol := 3 to (lnVol-1) do begin
     lVolo := (lVol-1)*lnVox;


     for lZ := 1 to lnZ do begin
         //lVOffset :=
         lVolo := ((lVol-1)*lnVox)+((lZ-1)*lnXY); //slice offset
         lHtm2 := CubicHt(lZFrac^[lZ]-2);
         lHtm1 := CubicHt(lZFrac^[lZ]-1);
         lHt := CubicHt(lZFrac^[lZ]);
         lHtp1 := CubicHt(lZFrac^[lZ]+1);

         for lXY := 1 to lnXY do begin
             lD := lHtm2 *lImg^[lVolo+lXY-lnVox-lnVox];//volume prior to previous volume
             lD := lD+ lHtm1 *lImg^[lVolo+lXY-lnVox];//previous volume
             lD := lD+ lHt *lImg^[lVolo+lXY];//target volume
             lNII4D.f32^[lVolo+lXY] := lD+ lHtp1 *lImg^[lXY+lnVox];//next volume
         end;//XY each voxel in slice
 	 //lImg^[lV] := lNII4D.f32^[lV];
     end; //for each slice
     DebugFractionCompleted := lVol/lnVol;
     {$IFDEF GUI}application.processmessages; {$ENDIF}
 end; //for each volume....
 freemem(lImg);
 freemem(lZFrac);
 result := true;
end;





function nii_slicetimecorrect_cubic (lImages: Tstrings; lSliceOrder: integer): boolean; overload;
var
  lNImg,lI: integer;
  lNIIImg: TNIfTIimg;
begin
 result := false;
 if (lSliceOrder < kAscending)  or ( lSliceOrder > kDescendingInterleavedSiemens) then begin
        showmsg('Slice timing correction skipped: assuming simultaneous 3D acquisition - slice order '+inttostr(lSliceOrder));
        exit;
 end;

     lNImg := lImages.Count;
     if lNImg < 1 then
      exit;
     CreateNII(lNIIImg);


     for lI := 1 to lNImg do begin
         if not Read4DF32(lImages[lI-1], lNIIImg) then begin
            showmsg('Error loading '+lImages[lI-1]);
         end else begin
          	 lNIIImg.HdrName := changefileprefix(lImages[lI-1],'stc');//slice time correct
         	 result := nii_slicetimecorrect_cubic(lNIIImg,lSliceOrder);
             if result then begin
             	WriteNII(lNIIImg.HdrName,lNIIImg);
          	 	riteln('slice-time corrected '+lNIIImg.HdrName+' with '+inttostr(lNIIImg.hdr.dim[4])+' volumes');
             end;
             //ReportMeanSliceTime(lNIIImg);

         end;
     end;
     FreeNII(lNIIImg);

end;



end.
