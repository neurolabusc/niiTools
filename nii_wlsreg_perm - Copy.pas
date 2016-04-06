unit nii_wlsreg_perm;
{$mode objfpc}{$H+}
interface

uses
  Forms, //required for application.processmessages
  //FileUtil, Controls, Graphics,
  //Dialogs, ComCtrls,
  //StdCtrls, ExtCtrls,
  classes, define_types,nii_core, dialogsx, math, sysutils,nii_smooth,uregmult, nii_ttest,utypes, valformat;

function maskedwls_perm(lVALFilename,lMask,lOutname: String; lnPermute,lnThread: integer; lROI,lSmooth: boolean): boolean;

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

function isvariable(lData: PVector; lnObs: integer): boolean;
var
   lO: integer;
begin
     result := false;
     if lnObs < 2 then
     	exit;
     result := true;
	 for lO := 2 to (lnObs) do
     	 if lData^[1] <> lData^[lO] then
         	exit;
     result := false;
end;

procedure GenerateOrder (lnObservation: integer; var lOrder: PIntVector);
var
  lO,lR,lSwap: integer;

begin
  if lnObservation < 1 then
    exit;
  for lO := 1 to lnObservation do
    lOrder^[lO] := lO;
  if lnObservation < 2 then
    exit;
  for lO := lnObservation downto 2 do begin
    lR := random(lO)+1; //e.g. if l)=2 then 1..2
    lSwap := lOrder^[lO];
    lOrder^[lO] := lOrder^[lR];
    lOrder^[lR] := lSwap;
  end;
end;//proc GenerateOrder

procedure GeneratePermutationOrders ( var lPermOrder: PIntMatrix; lnPermute, lnObservation: integer);
//you will need to DelIntMatrix(lPermOrder,lnPermute,lnObservations);
var
  lP: integer;
begin
  DimIntMatrix(lPermOrder,lnPermute,lnObservation);
  randseed := 888;
  for lP := 1 to lnPermute do
    GenerateOrder(lnObservation,lPermOrder^[lP]);
end;

procedure GenerateMinMax ( var lPerm: PMatrix;  lnThread,lnPermute: integer);
//you will need to DelMatrix(lPerm,lnPermute,lnThread);
var
  lT,lP: integer;
begin
  DimMatrix(lPerm,lnThread,lnPermute);
  for lT := 1 to lnThread do
    for lP := 1 to lnPermute do

      lPerm^[lT]^[lP] := 0;
end;

procedure sort (lo, up: integer; var r:PVector);
//62ms Shell Sort http://www.dcc.uchile.cl/~rbaeza/handbook/algs/4/414.sort.p.html
label     999;
var  d, i, j : integer;
          tempr : single;
begin
     d := up-lo+1;
     while d>1 do begin
          if d<5 then
             d := 1
          else
              d := trunc( 0.45454*d );
          //Do linear insertion sort in steps size d
          for i:=up-d downto lo do begin
               tempr := r^[i];
               j := i+d;
               while j <= up do
                    if tempr > r^[j] then begin
                         r^[j-d] := r^[j];
                         j := j+d
                         end
                    else goto 999;  //break
               999:
               r^[j-d] := tempr
          end //for
     end //while
end; //proc Sort

function IndexPct(lnPermute: integer; lPct: single; lTop: boolean): integer;
begin
    result := round(lnPermute * lPct);
    if lTop then
       result := (lnPermute - result)+1;
    if (result < 1)  then
       result := 1;
    if (result > lnPermute) then
       result := lnPermute;
end;

function ReportThresh (lLabel: string; lnPermute: integer; var lRankedData: PVector;lTop:boolean): double;
begin
     result := lRankedData^[IndexPct(lnPermute,0.050,lTop)];
     riteln(lLabel+': permutationFWE '+
       ', 0.050='+realtostr({lRankedData^[IndexPct(lnPermute,0.050,lTop)]} result,8)+
       ', 0.025='+realtostr(lRankedData^[IndexPct(lnPermute,0.025,lTop)],8)+
       ', 0.01='+realtostr(lRankedData^[IndexPct(lnPermute,0.010,lTop)],8)+
       ' ');
end;

function  ReturnResiduals (lnObservations,lnFactors: integer; X: PMatrix; Y: PVector; var Yresidual: PVector; lOutSlope: DoubleP0): boolean;
var
  lEstimate: double;
  lF,lO: integer;
begin
    result := false;
    if (lnObservations < 1) or (lnfactors < 1) then
      exit;
    for lO := 1 to lnObservations do begin
      lEstimate :=  lOutSlope^[lnFactors];//constant
      for lF := 1 to (lnFactors) do
        lEstimate := lEstimate+ (X^[lF]^[lO] *lOutSlope^[lF-1]);
      //residual
      Yresidual^[lO] := Y^[lO]-lEstimate ;
    end;
    result := true;
end;

function PermutedZ (lnObservations,lnFactors,lFactorToTest,lnPermute: integer;var Xf: PMatrix; var Y: PVector; lPermOrder: PIntMatrix; var lMaxZ,lMinZ: PVector): double;
label
  666;
var
  lP,lF,lO,lDF : integer;
  X    : PMatrix;
  lOutSlope,lOutT: DoubleP0;
  lPermT: Double;
  lOK: boolean;
begin
  lDF := lnObservations-lnFactors-1;
  if lDF < 1 then
     exit;
  Getmem(lOutSlope,(lnFactors+1)*sizeof(double));
  Getmem(lOutT,(lnFactors+1)*sizeof(double));
  DimMatrix(X, lnFactors, lnObservations);
  MultipleRegressionVec (lnObservations,lnFactors,Xf,Y,lOutT,lOutSlope);
  result := TtoZ(lOutT^[lFactorToTest-1],lDF);
  for lP := 1 to lnPermute do begin
    for lF := 1 to (lnFactors) do begin
      for lO := 1 to lnObservations do
        X^[lF]^[lO] := Xf^[lF]^[lPermOrder^[lP]^[lO] ];
    end;
    MultipleRegressionVec (lnObservations,lnFactors,X,Y,lOutT,lOutSlope);
    lPermT := TtoZ(lOutT^[lFactorToTest-1],lDF);
    if lPermT < lMinZ^[lP] then
       lMinZ^[lP] := lPermT;
    if lPermT > lMaxZ^[lP] then
       lMaxZ^[lP] := lPermT;
  end;//for each Permutation
  DelMatrix(X, lnFactors,lnObservations);
  Freemem(lOutT);
  Freemem(lOutSlope);
end;

function MultipleRegressionPermutedZ (lnObservations,lnFactors,lFactorToTest,lnPermute: integer;  var Xf: PMatrix; var Y: PVector; lPermOrder: PIntMatrix; var lMaxZ,lMinZ: PVector): double;
label
  666;
var
  lO,lF,lDF,lFx : integer;
  Yresidual: PVector;
  X    : PMatrix;
  lOutSlope,lOutT: DoubleP0;
  lOK: boolean;
begin
  result := 0;
  lDF := lnObservations-lnFactors-1;
  if (lDF < 1) or (lFactorToTest < 1) or (lFactorToTest >lnFactors) then
    exit;
  if lnFactors < 2 then begin
     result := PermutedZ (lnObservations,lnFactors,lFactorToTest,lnPermute, Xf, Y,lPermOrder, lMaxZ,lMinZ);
      exit;
  end;
  //regress out all other factors...
  DimMatrix(X, lnFactors-1, lnObservations);
  DimVector(Yresidual, lnObservations);
  Getmem(lOutSlope,(lnFactors+1)*sizeof(double));
  Getmem(lOutT,(lnFactors+1)*sizeof(double));
  lFx := 0;
  for lF := 1 to (lnFactors) do begin
    if lF <> lFactorToTest then begin
      inc(lFx);
      for lO := 1 to lnObservations do
        X^[lFx]^[lO] := Xf^[lF]^[lO];
    end;
  end;

  if not MultipleRegressionVec (lnObservations,lnFactors-1,X,Y,lOutT,lOutSlope) then
    goto 666;
  if not ReturnResiduals (lnObservations,lnFactors-1, X, Y, Yresidual, lOutSlope) then
    goto 666;
  //permute here...
  result := PermutedZ (lnObservations,lnFactors,lFactorToTest,lnPermute, Xf, Yresidual,lPermOrder, lMaxZ,lMinZ);
  //PermutedP (lnObservations,lnFactors,lFactorToTest,lnPermute, Xf,Yresidual,l1Tailed,l2Tailed);

666:
  DelVector(Yresidual, lnObservations);
  DelMatrix(X, lnFactors-1,lnObservations);
  freemem(lOutSlope);
  freemem(lOutT);
end;

function maskedwls_perm(lVALFilename,lMask,lOutname: String; lnPermute,lnThread: integer; lROI,lSmooth: boolean): boolean;
//Mask: name of masking image, typically template used for normalization  MNI152_T1_2mm_brain.nii
//lImages: one per participant. For each image ('filename.nii') there should also be a lesion mask of same name ('filename.voi')
//for repeated measures t-test: lImages are DIFFERENCE MAPS. Max DF = lImages.Count-1. nGroup1=0
//for unrelated t-test, lImages are continuous images, Max DF=lImages-1.
//  Images 1..nGroup1 are from group 1, nGroup1+1..lImages.count are from group 2.

label
  666;

var
  lPermOrder: PIntMatrix;
  lImages:  TStrings;
   lPredictorList: TStringList;
   X,Xdata : PMatrix;
   lNII4D,lNIIx,lNIIMask,lNIILesion: TNIfTIimg;
  lOffset,lS,lV,lnV,lnS,lnObs,lnFactors,lF,lDF,lP,lT: integer;
  //lSum: double;
  lMaxZ,lMinZ: PMatrix;
  Y: PVector;
  lOutT,lOutSlope: DoubleP0;
  lLabel, lLesionName,lOutnameX,lStr: string;
begin
     //initialize
     lPredictorList := TStringList.Create;
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
     for lS := 1 to lnS do
       	 if not fileexists(lImages[lS-1]) then begin
           riteln('can not find '+lImages[lS-1]);
           goto 666;
       end;



     if (lnThread > 0) and (lnPermute > 0) then begin
        riteln('Computing '+inttostr(lnPermute)+' permutations with '+inttostr(lnThread)+' threads');
      	GeneratePermutationOrders(lPermOrder,lnPermute,lImages.Count);
     	GenerateMinMax(lMaxZ,lnThread,lnPermute);
     	GenerateMinMax(lMinZ,lnThread,lnPermute);
     end;
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

     riteln('PERMUTED WLS REGRESSION: DF= nObservations-nFactors-1');
     lLabel := lPredictorList[0];
     if lnFactors > 1 then
     	for lF := 2 to lnFactors do
          	lLabel := lLabel+kTab+ lPredictorList[lF-1];
     riteln('Imagename'+kTab+'VoxelsTested(Possible='+inttostr(lnV)+')'+kTab+lLabel);
     lLabel := 'R';
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

         lStr := '';
         for lF := 1 to lnFactors do
           	 lStr := lStr+kTab+ floattostr(X^[lF]^[lS]);
         riteln(lImages[lS-1]+kTab+inttostr(lnObs) +lStr);
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
     if (lnThread > 0) and (lnPermute > 0) then begin

  	 	CreateZeroedFloat4DNII(lNIIMask,lNIIx,changefilepostfix(lOutnameX,'wlsZ'+lLabel),1);
  	 	riteln('Saving statistics for 1st factor named '+lPredictorList[0]);

     end else begin
     	 CreateZeroedFloat4DNII(lNIIMask,lNIIx,changefilepostfix(lOutnameX,'wlsZ'+lLabel),lnFactors+1);
     	 for lF := 1 to lnFactors do
         	 riteln('Volume '+inttostr(lF)+' = '+lPredictorList[lF-1]);
     	 riteln('Volume '+inttostr(lnFactors+1)+' = FullModel');
     end;

     lNIIx.Hdr.intent_code:=kNIFTI_INTENT_ZSCORE;
     DimVector(Y, lnS);
     //Getmem(lData,lnS*sizeof(double));
     Getmem(lOutT,(lnFactors+1)*sizeof(double));
     Getmem(lOutSlope,(lnFactors+1)*sizeof(double));

     //UNIQUE PORTION FOLLOWS
     DimMatrix(Xdata, lnFactors, lnS);
     for lS := 1 to lnV*lnS do
         if specialsingle(lNII4D.f32^[lS]) then
         	lNII4D.f32^[lS] := kNaNs;
     for lV := 1 to lnV do begin
          lnObs := 0;
          //lSum := 0;
          for lS := 1 to lnS do begin
          	 lOffset := (lS-1)*lnV;
             if (lNII4D.f32^[lV+lOffset] <> kNaNs) then begin
                 inc(lnObs);
                 Y^[lnObs] := lNII4D.f32^[lV+lOffset];
                 for lF := 1 to lnFactors do
                 	 Xdata^[lF]^[lnObs] := X^[lF]^[lS] ;// ^[Factors]^[Subj]
              end;
          end;
          lDF := lnObs-lnFactors-1;
          //if lV = 1 then tMultipleRegression (lnObs,lnFactors, Xdata,lData, lOutT);
          if (lDF > 0) and (isvariable(Y,lnObs)) then begin //Must have at least one DF nObservations-nFactors-1
              if (lnThread > 0) and (lnPermute > 0) then begin
                if (lV mod 50) = 0 then begin
               	 DebugFractionCompleted := lV/lnV;
         		 application.processmessages;

                end;
               	lNIIx.f32^[lV] := MultipleRegressionPermutedZ (lnObs,lnFactors,1,lnPermute, Xdata, Y, lPermOrder, lMaxZ^[1],lMinZ^[1])
              end else begin
              	  MultipleRegressionVec (lnObs,lnFactors, Xdata,Y, lOutT,lOutSlope);
              	  for lF := 0 to lnFactors do begin//0..N instead of 1..N, as full model stored as final value
              	  	  lOutT^[lF] := TtoZ(lOutT^[lF],lDF);
          	  	  	  lOffset := (lF)*lnV;
                  	  lNIIx.f32^[lV+lOffset] :=  lOutT^[lF];
              	  end;
              end;
          end;
     end;

     //PermutePairedTTest ( lNII4D, lnPermute);
     freemem(lOutT);
     freemem(lOutSlope);
     DelMatrix(Xdata, lnFactors, lnS);
     if (lnThread > 0) and (lnPermute > 0) then
   	 	ReportMinMax(lNIIx,1)
     else
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

     //now show permuted results...
	 if (lnThread > 0) and (lnPermute > 0) then begin
       if lnThread > 1 then begin
        //collapse min/max across threads...
        for lP := 1 to lnPermute do
          for lT := 2 to lnThread do
            if lMaxZ^[lT]^[lP] > lMaxZ^[1]^[lP] then
              lMaxZ^[1]^[lP] := lMaxZ^[lT]^[lP];
        for lP := 1 to lnPermute do
          for lT := 2 to lnThread do
            if lMinZ^[lT]^[lP] < lMinZ^[1]^[lP] then
              lMinZ^[1]^[lP] := lMinZ^[lT]^[lP];
       end; //more than one thread
       Sort(1,lnPermute,lMaxZ^[1]);
       Sort(1,lnPermute,lMinZ^[1]);
       ReportThresh(lLabel+'+',lnPermute,lMaxZ^[1],true);
       ReportThresh(lLabel+'-',lnPermute,lMinZ^[1],false);
     end;

666:
	DelMatrix(X, lnFactors, lnS);
     lImages.Free;
     lPredictorList.Free;
     FreeNII(lNII4D);
     FreeNII(lNIIx);
     FreeNII(lNIIMask);
     FreeNII(lNIILesion);
	 if (lnThread > 0) and (lnPermute > 0) then begin
     	DelIntMatrix(lPermOrder,lnPermute,lnS);
      	DelMatrix(lMaxZ,lnThread,lnPermute);
   		DelMatrix(lMinZ,lnThread,lnPermute);
     end;
end;



end.

