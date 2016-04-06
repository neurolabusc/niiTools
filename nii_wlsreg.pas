unit nii_wlsreg;
{$mode objfpc}{$H+}
interface

uses
  Forms, //required for application.processmessages
  //FileUtil, Controls, Graphics,
  //Dialogs, ComCtrls,
  //StdCtrls, ExtCtrls,
  classes, define_types,nii_core, dialogsx, math, sysutils,nii_smooth,uregmult, nii_ttest,utypes, valformat;

function maskedwls(lVALFilename,lMask,lOutname: String; lnPermute: integer; lROI,lSmooth: boolean): boolean;

implementation

function GetValReg (lVALFilename: string; var lnSubj,lnFactors: integer; var X : PMatrix; var lImageNames:  TStrings; var lPredictorList: TStringList): boolean;
var
   lTemplateName: string;
   lnRow,lnColWObs,lnCritPct,lInc,lRow,lCol: integer;
   lDesignUnspecified : boolean;
   lFileList:TStringList;
   lInRA: DoubleP0;
   lInP: Pointer;
begin
     result := false;
     lnSubj := 0;
     if not fileexists(lVALFilename) then
     	exit;
     riteln( 'VAL filename: '+lVALFilename);

     lFileList := TStringList.Create;
     if not OpenValFile (lVALFilename,lTemplateName, lnRow,lnFactors,lnColWObs,lnCritPct,lDesignUnspecified,lPredictorList,lFileList, lInP) then
        exit;
     if lnRow > 1 then begin
        lnSubj := lnRow -1; //top row is predictor
        {$IFDEF FPC}
        lInRA := align(lInP,16);
        {$ELSE}
        lInRA := DoubleP0($fffffff0 and (integer(lInP)+15));
        //lInRA := DoubleP0((integer(lInP) and $FFFFFFF0)+16);
        {$ENDIF}
        DimMatrix(X, lnFactors, lnSubj);
        for lCol := 1 to lnFactors do
            for lRow := 1 to lnSubj do
             X^[lCol]^[lRow] := lInRA^[(lRow*lnColWObs)-lnColWObs-1+lCol];
        for lInc := 1 to lnSubj do
            lImageNames.add(ExtractFileDirWithPathDelim(lVALFilename)+lFileList.Strings[lInc-1]);
        result := true;
     end;
     lFileList.free;
     Freemem(lInP);
end;

(*function tMultipleRegression (lnObservations,lnFactors: integer; var X:  PMatrix;
         var lImgIntensity: DoubleP0; var lOutT: DoubleP0): boolean;
var
   o,f: integer;
   ls: string;
BEGIN
     if (lnObservations < 1) or (lnFactors < 1) then
       exit;

     for o := 1 to lnObservations do begin
         ls := floattostr(lImgIntensity^[o-1])+kTab;
         for f := 1 to lnfactors do begin
             ls := ls + floattostr(X^[F]^[o])+kTab;
         end;
         riteln(ls);
     end;

end; *)

function isvariable(lData: DoubleP0; lnObs: integer): boolean;
var
   lO: integer;
begin
     result := false;
     if lnObs < 2 then
     	exit;
     result := true;
	 for lO := 1 to (lnObs-1) do
     	 if lData^[0] <> lData^[lO] then
         	exit;
     result := false;
end;

function maskedwls(lVALFilename,lMask,lOutname: String; lnPermute: integer; lROI,lSmooth: boolean): boolean;
//Mask: name of masking image, typically template used for normalization  MNI152_T1_2mm_brain.nii
//lImages: one per participant. For each image ('filename.nii') there should also be a lesion mask of same name ('filename.voi')
//for repeated measures t-test: lImages are DIFFERENCE MAPS. Max DF = lImages.Count-1. nGroup1=0
//for unrelated t-test, lImages are continuous images, Max DF=lImages-1.
//  Images 1..nGroup1 are from group 1, nGroup1+1..lImages.count are from group 2.

label
  666;

var
   lImages:  TStrings;
   lPredictorList: TStringList;
   X,Xdata : PMatrix;
   lNII4D,lNIIx,lNIIMask,lNIILesion: TNIfTIimg;
  lOffset,lS,lV,lnV,lnS,lnObs,lnFactors,lF,lDF: integer;
  //lSum: double;
  lOutT,lData: DoubleP0;
  lLabel, lLesionName,lOutnameX: string;
begin
	 lImages:= TStringList.Create; //not sure why TStrings.Create does not work???
  	 lPredictorList := TStringList.Create;
     CreateNII(lNII4d);
     CreateNII(lNIIx);
     CreateNII(lNIIMask);
     CreateNII(lNIILesion);

  	 if not GetValReg (lVALFilename, lnS,lnFactors,X, lImages, lPredictorList) then
        goto 666;
     lnS := lImages.Count;
     if (lnS <> lImages.count) or (lnS < 1) then
      goto 666;

     result := false;
     if fileexists(lMask) then begin
        riteln('MASK: '+lMask);
        if not Read3DBinary (lMask, lNIIMask) then
           goto 666;
     end else begin
     	 riteln('Warning: no brain mask: all voxels will be analyzed.');
         if not Read3DBinary (lImages[0], lNIIMask) then
         	goto 666;
         lnV := lNIIMask.hdr.dim[1]*lNIIMask.hdr.dim[2]*lNIIMask.hdr.dim[3];
          for lV := 1 to lnV do
              lNIIMask.i8^[lV] := 1;//all voxels included
     end;
     CreateZeroedFloat4DNII(lNIIMask,lNII4D,'TEMP',lnS);
     lnV := lNIIMask.hdr.dim[1]*lNIIMask.hdr.dim[2]*lNIIMask.hdr.dim[3];
     for lS := 1 to lnV*lnS do
         lNII4D.f32^[lS] := kNaNs;


         lLabel := 'R';
         riteln('WLS REGRESSION: DF= nObservations-nFactors-1');
     	 riteln('Filename'+kTab+'VoxelsTested(Possible='+inttostr(lnV)+')');

     for lS := 1 to lnS do begin
         if not Read3DF32(lImages[lS-1], lNIIx) then begin
            showmsg('Error loading '+lImages[lS-1]);
            goto 666;
         end;
         if not SameXYZDims(lNIIx,lNIIMask) then begin
            showmsg('Dimensions do not match '+lMask+' <> '+lNIIx.HdrName);
            goto 666;
         end;
         lLesionName := ChangeFileExtX(lIMages[lS-1],'.voi');
         if fileexists(lLesionName) then begin
         	if not Read3DBinary(lLesionName, lNIILesion) then begin
               showmsg('Error loading lesion'+lLesionName);
               goto 666;
         	end;
            if not SameXYZDims(lNIIx,lNIILesion) then begin
               showmsg('Dimensions do not match '+lNIILesion.HdrName+' <> '+lNIIx.HdrName);
               goto 666;
         	end;
         	lOffset := (lS-1)*lnV;
         	lnObs := 0;
         	for lV := 1 to lnV do begin
             if (lNIIMask.i8^[lV] <> 0) and (lNIILesion.i8^[lV] = 0) then begin
                lNII4D.f32^[lV+lOffset] := lNIIx.f32^[lV];
                inc(lnObs);
             end;
         	end;
         end else begin //if lesion else nolesion...
             riteln('Warning: no lesion mask. Unable to find '+lLesionName);
             lOffset := (lS-1)*lnV;
         	 lnObs := 0;
         	 for lV := 1 to lnV do begin
             	 if (lNIIMask.i8^[lV] <> 0.0) then begin
                 	lNII4D.f32^[lV+lOffset] := lNIIx.f32^[lV];
                	inc(lnObs);
             	 end;
         	 end;//for each voxel
         end;//if ..else lesion masking

         	 riteln(lImages[lS-1]+kTab+inttostr(lnObs) );
     	 application.processmessages;
     end;
     if lSmooth then
        nii_smooth_cubic(lNII4D);
     //For regions of interest, collapse to single voxel...
     if (lROI) then begin
     	if(ROIcollapse(lNII4D) < 1) then
     	  goto 666;
        lnV := 1;
        FreeNII(lNIIMask);
        lNIIMask.hdr.dim[1] := 1;
        lNIIMask.hdr.dim[2] := 1;
        lNIIMask.hdr.dim[3] := 1;

     end;

     //prepare output
     if lOutname = '' then
        lOutnameX := extractfilepath(lImages[0])+'ttest.nii.gz'

     else
     	 lOutnameX := lOutname;
     //next: t-test
     //FreeNII(lNIILesion);
     //CreateZeroedFloat3DNII(lNIIMask,lNIILesion,changefilepostfix(lOutnameX,'MnEffect'+lLabel));
     FreeNII(lNIIx);
     CreateZeroedFloat4DNII(lNIIMask,lNIIx,changefilepostfix(lOutnameX,'wlsZ'+lLabel),lnFactors+1);
     for lF := 1 to lnFactors do
         riteln('Volume '+inttostr(lF)+' = '+lPredictorList[lF-1]);
     riteln('Volume '+inttostr(lnFactors+1)+' = FullModel');
     lNIIx.Hdr.intent_code:=kNIFTI_INTENT_ZSCORE;
     Getmem(lData,lnS*sizeof(double));
     Getmem(lOutT,(lnFactors+1)*sizeof(double));
     //UNIQUE PORTION FOLLOWS
     DimMatrix(Xdata, lnFactors, lnS);
     for lV := 1 to lnV do begin
          lnObs := 0;
          //lSum := 0;
          for lS := 1 to lnS do begin
          	 lOffset := (lS-1)*lnV;
              if (lNII4D.f32^[lV+lOffset] <> kNaNs) then begin
                 inc(lnObs);
                 //lSum := lSum + lNII4D.f32^[lV+lOffset];
                 lData^[lnObs-1] := lNII4D.f32^[lV+lOffset];
                 for lF := 1 to lnFactors do
                 	 Xdata^[lF]^[lnObs] := X^[lF]^[lS] ;// ^[Factors]^[Subj]
              end;
          end;
          lDF := lnObs-lnFactors-1;
          //if lV = 1 then tMultipleRegression (lnObs,lnFactors, Xdata,lData, lOutT);
          if (lDF > 0) and (isvariable(lData,lnObs)) then begin //Must have at least one DF nObservations-nFactors-1
              MultipleRegression (lnObs,lnFactors, Xdata,lData, lOutT);
              for lF := 0 to lnFactors do begin//0..N instead of 1..N, as full model stored as final value
              	  lOutT^[lF] := TtoZ(lOutT^[lF],lDF);
          	  	  lOffset := (lF)*lnV;
                  lNIIx.f32^[lV+lOffset] :=  lOutT^[lF];
              end;
            // MultipleRegression (lnObservations,lnFactors: integer; var X:  PMatrix; var lImgIntensity: DoubleP0; var lOutT: DoubleP0): boolean;
          end;
     end;
     //PermutePairedTTest ( lNII4D, lnPermute);
     freemem(lData);
     freemem(lOutT);
     DelMatrix(Xdata, lnFactors, lnS);
     for lF := 1 to (lnFactors+1) do
     	 ReportMinMax(lNIIx,lF);
     if not lROI then begin
     	WriteNII(lNIIx.HdrName,lNIIx);
     	FreeNII(lNIIx);
     	CreateZeroedi16_3DNII(lNIIMask,lNIIx,changefilepostfix(lOutnameX,'Obs'+lLabel));
     	for lS := 1 to lnS do begin
         lOffset := (lS-1)*lnV;
         for lV := 1 to lnV do begin
             if (lNII4D.f32^[lV+lOffset] <> kNaNs) then begin
                lNIIx.i16^[lV] := lNIIx.i16^[lV]+1;
             end;
         end;
     	end;
     	WriteNII(lNIIx.HdrName,lNIIx);
     end;
666:
	DelMatrix(X, lnFactors, lnS);
     lImages.Free;
     lPredictorList.Free;
     FreeNII(lNII4D);
     FreeNII(lNIIx);
     FreeNII(lNIIMask);
     FreeNII(lNIILesion);
end;



end.

