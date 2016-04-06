unit nii_perfx;
    {$ifdef fpc}
{$mode delphi}{$H+} {$endif}
{$include isgui.inc}
interface

uses
   {$IFDEF GUI} Forms, {$ENDIF}//required for application.processmessages
  //math,
   Classes, SysUtils, define_types,nii_core, dialogsx, nii_math, gamma_powell,nii_smooth, fsl_calls;
type
  TPerfOpts =  RECORD //peristimulus plot
    SmoothFWHMmm,SmoothFWHMsec,
    //TEmsec, AIFx0,
    TRSec, //time per volume (seconds)
    MaskThreshSD,//only include voxels where peak is more the ThreshSD*StDev(mask-baseline) more or less than that voxels baseline intensity
    MinR2:single;
    //AIFVox,
    MaskVox,SliceTimeCorrect,DeleteVols,BaselineVols,FinalVol: integer;
    //AIFpeak, ConvertToConcentrationTime,  PreMask,
    MotionCorrect,BrainExtract,ComputeRaw,ComputeFitted,Normalize: boolean;
  end;
  TPerfEst =  RECORD //peristimulus plot
    TTP, //Mode:Time-To-Peak (seconds)
    FM, //FirstMoment:Mean
    R2a //Adjusted R^2
    //AIFScore//Highest for strong, early and compact signals

    //only include voxels where peak is more the ThreshSD*StDev(mask-baseline) more or less than that voxels baseline intensity

    : double;
  end;

function perfusionanalyze1 (lFilename4D: string;  lOptsx: TPerfOpts): boolean; overload;
procedure perfusionanalyze(lImages: Tstrings;  lOpts: TPerfOpts); overload;
procedure reportMinMax(var lNII: TNIFTIimg);
function defaultperf: TPerfOpts;

implementation


function defaultperf: TPerfOpts;
begin
     (*     result.TEmsec := 30;
     result.AIFx0 := 0;
     result.AIFvox := 100;
     result.ConvertToConcentrationTime := true;
     result.AIFpeak := false;
          result.premask := true;
     *)
     result.SliceTimeCorrect := kAutoDetect;// kAscendingInterleavedSiemens;
     result.SmoothFWHMmm := 2.35;
     result.SmoothFWHMsec := 3.53;
     result.TRsec := 0;
     result.Maskvox := 1000;
     result.MaskThreshSD := 2.8;
     result.DeleteVols := 1;
     result.BaselineVols := 5;
     result.FinalVol := 42;
     result.MinR2 := 0.5;
     result.MotionCorrect := true;
     result.BrainExtract:= true;
     result.ComputeRaw := false;
     result.ComputeFitted := true;
     result.Normalize := false;
end;

function StDev(lN,lSum,lSumSqr: double): double;
begin
   result := 0;
   if lN < 2 then
   	  exit;
   result := lSumSqr - (Sqr(lSum)/lN);
  if  (result > 0) then
        result :=  Sqrt ( result/(lN-1));
end;

procedure makemean (var lNII4D, lNIImask: TNIfTIimg);
var
  lSum: double;
  lVox,lVol,lnVox,lnVol: integer;
begin
	 lnVox := lNII4D.hdr.dim[1]*lNII4D.hdr.dim[2]*lNII4D.hdr.dim[3];
	 lnVol :=  lNII4d.NonspatialDimensionsLoaded;
     if (lnVox < 1) or (lnVol < 1) then exit;
     for lVox := 1 to lnVox do begin
         lSum := 0;
         for lVol := 1 to lnVol do
         	 lSum := lSum + lNII4D.f32^[lVox+((lVol-1)*lnVox)];
         lNIImask.f32^[lVox]  := lSum/lnVol; //mean
     end;
end;

function makepremask (var lNII4D, lNIImask: TNIfTIimg): boolean;
label 666;
  //remove any voxels that show no variability... do this prior to smoothing. Removes areas outside brain....
var
  lnVol,lnVox,lVol,lVox: integer;
begin
     result := false;
     //determine 1st and last volume for baseline time and for the portion where signal will be analyzed...
     lnVol := lNII4d.NonspatialDimensionsLoaded;
     lnVox := lNII4D.hdr.dim[1]*lNII4D.hdr.dim[2]*lNII4D.hdr.dim[3];
     if (lnVol<2 ) or (lnVox < 1) then begin
        riteln('MAKEPREMASK error: at least 2 volumes required');
        exit;
     end;
     //compute baseline mean/stdev during baseline for each voxel
     for lVox := 1 to lnVox do begin
         lNIImask.f32^[lVox] := 1;//assume the is variability
         for lVol := 1 to lnVol do
             if lNII4D.f32^[lVox] <> lNII4D.f32^[lVox+((lVol-1)*lnVox)] then
             	goto 666;
         lNIImask.f32^[lVox] := kNaNs;//you only get here if there is no variability...
         666:
    end;
    result := true;
end;

function positiveSingle (v: single): boolean;
begin
    if specialSingle(v) or (v <= 0) then
       result := false  //+Inf, NaN, <= 0
    else
           result := true;
end;

function makemask (var lNII4D, lNIImask: TNIfTIimg; var lOpts: TPerfOpts): boolean;
  //examine brightest maskfrac voxels, e.g. 0.1 for top 10%
  //accept voxels where peak is +/-kThreshSD beyond baseline mean
var
  lnUnMasked,lStartBase,lEndBase,lStartSig,lEndSig,i,lnBaseVol,lnSigVol,lnVox,lVol,lVox: integer;
  lSum,lSumSqr,lSD: double;
  lThresh,lV: single;
  lMask : boolean;
begin
     result := false;
     //determine 1st and last volume for baseline time and for the portion where signal will be analyzed...
     lStartBase := lOpts.DeleteVols;
     if lStartBase < 1 then
     	lStartBase := 1;
     lEndBase := lOpts.BaselineVols+lStartBase;
     if lEndBase < 1 then
     	lEndBase := 1;
     if lEndBase >= lNII4d.NonspatialDimensionsLoaded then
        exit;//there are no signal volumes....
     lStartSig := lEndBase+1;
     if lOpts.FinalVol < lNII4d.NonspatialDimensionsLoaded then
        lEndSig := lOpts.FinalVol
     else
     	 lEndSig := lNII4d.NonspatialDimensionsLoaded;
	 lnVox := lNII4D.hdr.dim[1]*lNII4D.hdr.dim[2]*lNII4D.hdr.dim[3];
     lnBaseVol := lEndBase-lStartBase+1;
     lnSigVol := lEndSig-lStartSig+1;
     if (lnSigVol<3 ) or (lnBaseVol<3 ) or (lnVox < 1) then begin
        riteln('MAKEMASK error: at least 3 baseline and signal volumes required');//need to compute stdev
        exit;
     end;
     (*if not lOpts.Premask then begin
     	for lVox := 1 to lnVox do
            lNIImask.f32^[lVox] := 1;
         riteln(' makeMask ignores premask (will examine all voxels)' );
     end;  *)

     //compute baseline mean/stdev during baseline for each voxel
     lnUnMasked := 0;
     for lVox := 1 to lnVox do begin
       if positiveSingle(lNIImask.f32^[lVox]) then begin //do not include voxels that were masked in the premask step
         lSum := 0;
         lSumSqr := 0;
         for lVol := lStartBase to lEndBase do begin
             i := lVox+((lVol-1)*lnVox);
             lSum := lSum +lNII4D.f32^[i];
             lSumSqr := lSumSqr +sqr(lNII4D.f32^[i]);
         end;
         lNIImask.f32^[lVox]  := lSum/lnBaseVol; //baseline mean
         lSD := abs(StDev( lnBaseVol,lSum,lSumSqr));
         lThresh := lNIImask.f32^[lVox]- abs(lSD*lOpts.MaskThreshSD);  //threshold: at least n standard deviations below baseline mean
         lMask := true;
         lSum := 0;
         for lVol := lStartSig to lEndSig do begin
           lV := lNII4D.f32^[lVox+((lVol-1)*lnVox)];
           if lV < lThresh then
             	lMask := false;
           lSum := lSum + lV;
         end; //for each volume
         if (not lMask) and ((lSum/lnSigVol)>  lNIImask.f32^[lVox]) then
         	lMask := true;
         if (not lMask) and (lOpts.MaskThreshSD > 3) then begin
            //this next step attempts to remove noisy voxels that have bright and dark peaks.... e.g. regions with head movement effects
            lThresh := lNIImask.f32^[lVox]+ abs(lSD*lOpts.MaskThreshSD);
         	for lVol := lStartSig to lEndSig do
         		if lNII4D.f32^[lVox+((lVol-1)*lnVox)] > lThresh then
             	   lMask := true;
         end;
         if  lMask then
            lNIImask.f32^[lVox]  := kNaNs
         else begin
         	inc(lnUnMasked);
                lNIImask.f32^[lVox]  := 1;
         end;
     end;//if not premasked
    end; //for each voxel
    riteln(inttostr(lnUnMasked)+'/'+inttostr(lVox)+' voxels survive threshold (peak >'+realtostr( lOpts.MaskThreshSD,1)+' StDev from baseline mean)');
    RemoveMaskNoise(lNIImask);
    result := true;
end;

procedure ReportMeanSliceTime (lNII: TNIFTIimg);
//if a voxel is surrounded by NaNs, set that voxel to a NaN
// The idea is to remove spckles from the mask
var
  lSum : double;
  lXY,lZ,lT,lnXY,lnXYZ,i, ln: integer;
begin
     if lNII.hdr.datatype <> kDT_FLOAT then begin
         riteln('NII_perf mean slice time error: only supports float datatype');
         exit;
     end;
     if (lNII.hdr.dim[1]<3) or (lNII.hdr.dim[2]<3) or (lNII.hdr.dim[3]<3) then begin
       riteln('NII_perf mean slice time error: requires 3D data');
       exit;
     end;
     riteln('Mean slice time report for file '+lNII.HdrName);
     riteln(' If odd slices have consistently higher or lower values than even slices you should check slice time correction (switch between interleaved and sequential)');
     riteln(' Likewise, consistent trends from top to bottom slices might suggest slice timing errors (switch between ascending and descending)');

     lnXY := lNII.hdr.dim[1] * lNII.hdr.dim[2];
     lnXYZ := lnXY * lNII.hdr.dim[3];
	 for lZ := 1 to ( lNII.hdr.dim[3]) do begin
           lSum := 0;
           ln := 0;
           for lT := 1 to lNII.NonspatialDimensionsLoaded do begin
           	   i := ((lT-1)*lnXYZ)+((lZ-1)*lnXY);
           	   for lXY := 1 to lnXY do begin
               	   if (not specialsingle(lNII.f32^[i+lXY])) and (lNII.f32^[i+lXY] <> 0) then begin
           		 	inc(ln);
                    lSum := lSum + lNII.f32^[i+lXY];
               	   end;
           	   end;//XY for each voxel in slice
               if ln > 0 then
                 	  riteln('Slice '+inttostr(lZ)+' mean of non-zero voxels'+kTab+floattostr(lSum/ln)+kTab+'n='+kTab+inttostr(ln) )
               else
                 	   riteln('Slice '+inttostr(lZ)+' no voxels > 0');

		   end;//T for each volume
     end;//Zfor each slice
end;

function TruncTail(Y: doubleP;lnVol: integer; Thresh: single): integer;
//lnVolOK will be 2..lnVol, and reflects position after peak where height is <Thresh of peak
//e.g. TruncTail(Y,lnVol,0.2); will return length of array where all items after peak are >20% peak threshold
var
  i,Peak: integer;
  ThreshHt: single;
begin
     result := lnVol;
     Peak := 1;
     for i := 1 to lnVol do
     	 if Y[i] > Y[Peak] then
            Peak := i;
     ThreshHt := Thresh*Y[Peak];
     if (Peak+3) >= lnVol then exit;
     for i := Peak+2 to lnVol-1 do begin
         if (Y[i]<ThreshHt) and (Y[i+1]<ThreshHt) then begin
          	result := i;
            exit;
         end;
     end;
end;

function RawBaselinecorrect (var lNII4D, lNIImask: TNIfTIimg; lStartvol,lEndVol: integer): boolean;
var
  lVol,lVox,lnVox,lVolo: integer;
begin
     result := true;
	 lnVox := lNII4D.hdr.dim[1]*lNII4D.hdr.dim[2]*lNII4D.hdr.dim[3];
     for lVol := lStartVol to lEndVol do begin
         lVolo := (lVol-1)*lnVox;
         for lvox := 1 to lnvox do
             if (not specialsingle(lNIImask.f32^[lVox]))  then
             lNII4D.f32^[lvox+lVolo] := lNIImask.f32^[lVox]-lNII4D.f32^[lvox+lVolo];
     end;
end;

function RawToConcentrationTime (var lNII4D, lNIImask: TNIfTIimg; lStartvol,lEndVol: integer; lTEmsec: double): boolean;
var
  lScale: double;
  lVol,lVox,lnVox,lVolo: integer;
begin
     result := false;
     if lTEmsec <= 0 then begin
     	riteln('RawToConcentrationTime Error: Echo Time not valid: will compute raw values');
        RawBaselinecorrect(lNII4D,lNIImask,lStartvol,lEndVol);
        exit;
     end;
     riteln('Converting raw signal S[t] to Concentration-time C[t], assuming TE= '+realtostr(lTEmsec,3)+'ms');
     riteln('  C[t]= -1/TE*ln(S[t]/S[0]');
     riteln(' Volume '+inttostr(lStartVol)+'..'+inttostr(lEndVol) );
     lnVox := lNII4D.hdr.dim[1]*lNII4D.hdr.dim[2]*lNII4D.hdr.dim[3];
     lScale :=  -1/lTEmsec;
     for lvox := 1 to lnvox do begin
         if (not specialsingle(lNIImask.f32^[lVox])) and (lNIImask.f32^[lVox] <> 0) then begin
     	 	for lVol := lStartVol to lEndVol do begin
         		lVolo := (lVol-1)*lnVox;
             	lNII4D.f32^[lvox+lVolo] := lScale* ln(lNII4D.f32^[lvox+lVolo]/lNIImask.f32^[lVox])
            end;//each volume

         end else //if not unusual voxel in mask....
                 lNIImask.f32^[lVox] := kNANs;
     end;
     result := true;
end;

 //http://www.amath.unc.edu/sysadmin/DOC4.0/pascal/lang_ref/ref_data.doc.html


(*  function computeRaw (var lNII4D, lNIImask,lNIIttp,lNIImsr: TNIfTIimg;lOpts: TPerfOpts): boolean;
 var
   iPeak,lStartVol,lEndVol,lnVol,lnVox,lVol,lVox: integer;
 begin
      result := false;
 	 lStartVol := lOpts.BaselineVols+lOpts.DeleteVols+1;
      if lStartVol < 1 then
      	lStartvol := 1;
      if lOpts.FinalVol < lNII4d.NonspatialDimensionsLoaded then
         lEndVol := lOpts.FinalVol
      else
      	 lEndVol := lNII4d.NonspatialDimensionsLoaded;
      riteln('RAW perfusion values: Time-To-Peak is in discrete steps (how many TRs)');
      riteln('Number of volumes '+inttostr(lNII4d.NonspatialDimensionsLoaded));
      riteln('Time per volume (TR, sec):'+realtostr(lNII4d.Hdr.pixdim[4],3));
      riteln('Fitting data for volumes '+inttostr(lStartVol)+'...'+inttostr(lEndVol));
      if lOpts.AIFx0 = 0 then
   	 	riteln('Warning: Raw TTP not adjusted for bolus arrival time')
      else
      	 riteln('Raw TTP adjusted for bolus arrival time '+floattostr(lOpts.AIFx0)+'sec after baseline scan');
 	 lnVox := lNII4D.hdr.dim[1]*lNII4D.hdr.dim[2]*lNII4D.hdr.dim[3];
      lnVol := lEndVol-lStartVol+1;
 	 if (lnVol<3 ) or (lNII4d.NonspatialDimensionsLoaded < lEndVol) or (lnVox < 1) then begin
         riteln('computeRaw error: at least 3 baseline scans required');//need to compute stdev
         exit;
      end;
      for lVox := 1 to lnVox do begin
         lNIIttp.f32^[lvox] := kNaNs;
         lNIImsr.f32^[lvox] := kNaNs;
      end;
      for lVox := 1  to (lnVox) do begin
         if lNIImask.f32^[lVox] <> kNaNs then begin
            iPeak := 1; //assume peak is first volume
            for lVol := 1 to lnVol do begin
            	   if lNII4d.f32^[lVox+((lVol-1)*lnVox)] < lNII4d.f32^[lVox+((iPeak-1)*lnVox)] then
                	  iPeak := lVol;
            end;
           lNIIttp.f32^[lvox] := ((iPeak-1) * lOpts.TRSec)-lOpts.AIFx0;
           if lNIImask.f32^[lVox] <> 0 then
           	 lNIImsr.f32^[lvox] :=  (lNIImask.f32^[lVox]-lNII4d.f32^[lVox+((iPeak-1)*lnVox)])/ lNIImask.f32^[lVox];
          end;//if unmasked
          if (lVox mod 1000) = 0 then begin
              DebugFractionCompleted := lVox/lnVox;
           {$IFDEF GUI}  application.processmessages;  {$ENDIF}
          end;
      end;//for each voxel
    result := true;
 end;       *)

 (*procedure forceSpecial(var lNII: TNIFTIimg);
 var
   lnVox,lVox: integer;
 begin
    lnVox := lNII.hdr.dim[1]*lNII.hdr.dim[2]*lNII.hdr.dim[3];
    for lVox := 1  to (lnVox) do begin
        if  positiveSingle(lNII.f32^[lVox]) then
              lNII.f32^[lVox] := 1
        else
             lNII.f32^[lVox] := kNaNs;
    end;
 end;

function computeGamma (var lNII4D, lNIImask,lNIIttp,lNIIr2,lNIImtt,lNIIfm ,lNIIaif: TNIfTIimg;lOpts: TPerfOpts): boolean;
label 666;
var
  SliceVox,n,lStartVol,lEndVol,lnVol,lnVox,lVol,lVox,I,lnVoxEstimated: integer;
  x,y: array of double;
  G: TGamma;
  Time0: singlep;
  slicefrac,AIFScore,Sum: double;
begin
     result := false;
	 lStartVol := lOpts.BaselineVols+lOpts.DeleteVols+1;
     if lStartVol < 1 then
     	lStartvol := 1;
     if lOpts.FinalVol < lNII4d.NonspatialDimensionsLoaded then
        lEndVol := lOpts.FinalVol
     else
     	 lEndVol := lNII4d.NonspatialDimensionsLoaded;
     riteln('Number of volumes '+inttostr(lNII4d.NonspatialDimensionsLoaded));
     riteln('Time per volume (TR, sec):'+realtostr(lOpts.TRSec,3 )); //lNII4d.Hdr.pixdim[4],3));
     riteln('Fitting data for volumes '+inttostr(lStartVol)+'...'+inttostr(lEndVol));
	 lnVox := lNII4D.hdr.dim[1]*lNII4D.hdr.dim[2]*lNII4D.hdr.dim[3];
     lnVol := lEndVol-lStartVol+1;
	 if (lnVol<3 ) or (lNII4d.NonspatialDimensionsLoaded < lEndVol) or (lnVox < 1) then begin
        riteln('computeGamma error: at least 3 baseline scans required');//need to compute stdev
        exit;
     end;
     if lOpts.ConvertToConcentrationTime then begin
     	if not RawToConcentrationTime(lNII4D, lNIImask, lStartvol,lEndVol,lOpts.TEmsec) then exit;
     end else
     	 RawBaselinecorrect ( lNII4D, lNIImask, lStartvol,lEndVol);
     //compute gamma
     setlength(X,lnVol);
     setlength(Y,lnVol);
     getmem(Time0,lnvox * sizeof(single) );
     lnVoxEstimated := 0;
      reportMinMax(lNIImask);

     for lVox := 1 to lnVox do begin
        lNIIttp.f32^[lvox] := kNaNs;
        lNIImtt.f32^[lvox] := kNaNs;
        lNIIfm.f32^[lvox] := kNaNs;
        lNIIr2.f32^[lvox] := kNaNs;
        lNIIaif.f32^[lvox] :=kNaNs;
        if not specialSingle(lNIImask.f32^[lVox]) then
           lnVoxEstimated := lnVoxEstimated + 1;
     end;
     riteln('Will compute parameters for '+inttostr(lnVoxEstimated)+' of '+inttostr(lnVox)+' voxels');
     for lVol := 0 to (lnVol-1) do
         X[lVol] :=lVol * lOpts.TRSec; //X is the onset time for each volume....
     SliceVox := lNII4D.hdr.dim[1]*lNII4D.hdr.dim[2];
     lnVoxEstimated := 0;
     showmsg(inttostr(lnVol-1)+'xsz');
     for lVox := 1  to (lnVox) do begin
        if not specialSingle(lNIImask.f32^[lVox]) then  begin

           for lVol := 0 to (lnVol-1) do begin
           	   i := lVox+((lVol+lStartVol-1)*lnVox);
               Y[lVol] := lNII4D.f32^[i];
           end;
          if  FitGamma (x,y, 0,lnVol-1,g, lOpts.ComputeFitted) then begin
            lnVoxEstimated := lnVoxEstimated + 1;
            if (g.RSquare > 0) then
              	 lNIIr2.f32^[lvox] := g.RSquare;
              if (g.RSquare <= 0) or (g.RSquare < lOpts.MinR2) then
              	 lNIImask.f32^[lVox] := 0//kNaNs
              else begin
              	   lnVoxEstimated := lnVoxEstimated + 1;
                   lNIIttp.f32^[lvox] :=  g.xMax;
                   lNIIfm.f32^[lvox] := g.Mean;
                   lNIImtt.f32^[lvox] := g.MTT;
                   //lNIImsr.f32^[lvox] :=  g.yMax;
                   //see http://www.ncbi.nlm.nih.gov/pubmed/20060614
                   //where AIF = (Hpeak*alpha^2)/(SSDfit*(T0*TMax)^4)
                   // since with our goodness of fit R-squared, larger values mean better fit, we use
                   //we use AIF = (Rsqr*Hpeak*alpha^2)/(T0*TMax)^4)
                   if g.x0 < 0.01 then //avoid divide by zero errors...
                   	 g.x0 := 0.01;
                   if g.xMax < 0.02 then
                      g.xMax := 0.02;
                   if g.RSquareRaw <= 0 then
                      g.RSquareRaw := 0.0001;
                   //slight bias for ventral voxels...
                   slicefrac := 2-(((lVox-1)/SliceVox))/lNII4D.hdr.dim[3]; //e.g. if 30 slices, 1st has value ~2 last has value ~1

                   lNIIaif.f32^[lvox] := (slicefrac*g.RSquareRaw*g.RSquare*g.YMax*sqr(g.alpha))/ (Power((g.xMax-g.x0)*g.xMax,3));
                   if lOpts.AIFpeak then
                	   Time0^[lvox] :=g.ixMax
                   else
                   	   Time0^[lvox] :=g.ix0;
              end;
           end else
           	   lNIIr2.f32^[lvox] := 0;//not able to compute fit
        end;//if unmasked
        if (lVox mod 1000) = 0 then begin
             DebugFractionCompleted := lVox/lnVox;
          {$IFDEF GUI} 	 application.processmessages;  {$ENDIF}
        end; //if lVox mod 1000 = time to provide progress update
	 end;//for each voxel
   //adjust arterial input function...
   riteln('Computed parameters for '+inttostr(lnVoxEstimated)+' of '+inttostr(lnVox)+' voxels');
    reportMinMax( lNIIttp);
    result := true;
    exit;//6666
   n := 0;
   Sum := 0;
   AIFscore := NIImax (lNIIaif,lOpts.AIFVox);
   if AIFscore > 0 then begin
      for lVol := 0 to (lnVol-1) do
      	  Y[lVol] := 0;
      for lVox := 1 to lnVox do begin
          //if (lNIImask.f32^[lVox]  <> kNANs) then begin
          if not specialSingle(lNIImask.f32^[lVox]) then begin
               if (lNIIaif.f32^[lVox]  >= AIFscore) then begin
                lNIImask.f32^[lVox] := 2;
                inc(n);
                Sum :=  Sum + Time0^[lvox];
                for lVol := 0 to (lnVol-1) do
               	    Y[lVol] :=Y[lVol]+ lNII4D.f32^[lVox+((lVol+lStartVol-1)*lnVox)];
             end else //survives threshold
           	   lNIImask.f32^[lVox] := 1;
          end;
          //else //in mask lNIImask.f32^[lVox] := 0;
   end;
   if (n > 0) then begin
      riteln ('Canonical AIF (X= time after baseline, sec)');
      riteln('X'+kTab+'Signal');
      for lVol := 0 to (lnVol-1) do
               	  riteln(realtostr(X[lVol],3)+kTab+realtostr(Y[lVol]/n,3) );

      lOpts.AIFx0 := Sum/n; //mean bolus arrival time for voxels selected as arterial input
      if lOpts.AIFpeak then begin
         riteln('Note that TTP will be adjusted relative to arterial peak (robust) not traditional arterial onset (noisy, esp. sensitive to temporal smoothing)');
         riteln(' Therefore TTP is actually PTP (time between arterial peak and regional peak');
         riteln('Arterial input estimated from '+inttostr(n)+' voxels with a mean bolus peak time of '+RealToStr(lOpts.AIFx0,3)+'sec after baseline scans');
      end else
      	  riteln('Arterial input estimated from '+inttostr(n)+' voxels with a mean bolus arrival time of '+RealToStr(lOpts.AIFx0,3)+'sec after baseline scans');
      for lVox := 1 to lnVox do
          //if lNIImask.f32^[lVox] <> kNaNs then
          if not specialSingle(lNIImask.f32^[lVox]) then
      	     lNIIttp.f32^[lvox] := lNIIttp.f32^[lvox]-lOpts.AIFx0; //adjust voxels to compensate for mean arrival time in the arterial input
      lOpts.AIFx0 := lOpts.AIFx0;
   end else
   	   riteln('Serious error: unable to accurately estimate arterial input');
   end;
   reportMinMax(lNIIttp);

   result := true;
666:
freemem(Time0);
X := nil;
Y := nil;
end;    *)

function volMinMax(var lNII: TNIFTIimg; lStartVol, lEndVol: integer; var lMin,lMax: single): single;
 var
   lEndVox,lVox, lnVol, lStartVox: integer;
   lSum : double;
 begin
       result := 0;
       lSum := 0;
       lMin := kInfs;
     lMax := kNegInfs;
     if lStartVol < 1 then lStartVol := 1;
     if lEndVol > lNII.NonspatialDimensionsLoaded  then lEndVol := lNII.NonspatialDimensionsLoaded;
     lnVol :=   lEndVol - lStartVol + 1;
     if lnVol < 1 then exit; //e.g. start=3, end= 2
     lStartVox :=   (lNII.hdr.dim[1]*lNII.hdr.dim[2]*lNII.hdr.dim[3]*(lStartVol-1))+1;
     lEndVox := lNII.hdr.dim[1]*lNII.hdr.dim[2]*lNII.hdr.dim[3]*lEndVol;
     for lVox := lStartVox  to lEndVox do begin
         if not specialSingle(lNII.f32^[lVox]) then begin
           if lNII.f32^[lVox] > lMax then
               lMax :=  lNII.f32^[lVox];
            if lNII.f32^[lVox] < lMin then
               lMin :=  lNII.f32^[lVox];
            lSum := lSum + lNII.f32^[lVox];
         end;
     end; //for each voxel
     result := lSum / (lEndVox-lStartVox+1); //mean
  end;

 procedure reportMinMax(var lNII: TNIFTIimg);
 var
   lMin,lMax, lMean: single;
 begin
    lmean := volMinMax(lNII,1,maxint,lMin,lMax);
    riteln(lNII.HdrName + ' mean '+floattostr(lMean)+' range '+floattostr(lMin)+'...'+floattostr(lMax) );
 end;

 procedure invertIntensity4d(var lNII: TNIFTIimg);
 var
   lnVox,lVox: integer;
 begin
    lnVox := lNII.hdr.dim[1]*lNII.hdr.dim[2]*lNII.hdr.dim[3]*lNII.NonspatialDimensionsLoaded;
    for lVox := 1  to (lnVox) do
       if not specialSingle(lNII.f32^[lVox]) then
          lNII.f32^[lVox] := -lNII.f32^[lVox];
 end;

 procedure makeBolusPositive(var lNII: TNIFTIimg;lOptsx: TPerfOpts);
 //with CT perfusion the contrast makes the image bright, in MRI it makes the image dark.
 //  if this function detects a MRI it flips the contrast so the bolus is bright (we detect the peak, not trough)
 var
   lOpts: TPerfOpts;
   lBaselineMean,lSignalMean,lMin,lMax: single;
 begin
    lOpts := lOptsx;
    if  lOpts.DeleteVols < 0 then lOpts.DeleteVols := 0;
    if  lOpts.BaselineVols < (lOpts.DeleteVols+1) then lOpts.BaselineVols := lOpts.DeleteVols+1;
    if  lOpts.FinalVol < (lOpts.BaselineVols+1) then lOpts.FinalVol := lOpts.BaselineVols+1;
    lBaselineMean := volMinMax(lNII,lOpts.DeleteVols+1,lOpts.BaselineVols,lMin,lMax);
    lSignalMean := volMinMax(lNII,lOpts.BaselineVols+1,lOpts.FinalVol,lMin,lMax);
    if lBaselineMean >  lSignalMean then begin
       riteln(lNII.HdrName + ' will have image brightness inverted so bolus appears as a peak not a trough: baseline mean '+floattostr(lBaselineMean)+' post-baseline mean '+floattostr(lSignalMean) );
       invertIntensity4d(lNII);
    end;
 end;

 function computeGammaX (var lNII4D, lNIIttp,lNIIr2,lNIImtt : TNIfTIimg;lOpts: TPerfOpts): boolean;
label 666;
var
  lStartVol,lEndVol,lnVol,lnVox,lVol,lVox,I: integer;
  x,y: array of double;
  G: TGamma;
  baselineSignal: double;
begin
     result := false;
     lStartVol := lOpts.BaselineVols+lOpts.DeleteVols+1;
     if lStartVol < 1 then
     	lStartvol := 1;
     if lOpts.FinalVol < lNII4d.NonspatialDimensionsLoaded then
        lEndVol := lOpts.FinalVol
     else
     	 lEndVol := lNII4d.NonspatialDimensionsLoaded;
     riteln('Number of volumes '+inttostr(lNII4d.NonspatialDimensionsLoaded));
     riteln('Time per volume (TR, sec):'+realtostr(lOpts.TRSec,3 )); //lNII4d.Hdr.pixdim[4],3));
     riteln('Fitting data for volumes '+inttostr(lStartVol)+'...'+inttostr(lEndVol));
     lnVox := lNII4D.hdr.dim[1]*lNII4D.hdr.dim[2]*lNII4D.hdr.dim[3];
     lnVol := lEndVol-lStartVol+1;
     if (lnVol<3 ) or (lNII4d.NonspatialDimensionsLoaded < lEndVol) or (lnVox < 1) then begin
        riteln('computeGamma error: at least 3 baseline scans required');//need to compute stdev
        exit;
     end;
     {if lOpts.ConvertToConcentrationTime then begin
     	if not RawToConcentrationTime(lNII4D, lNIImask, lStartvol,lEndVol,lOpts.TEmsec) then exit;
     end else   666
     	 RawBaselinecorrect ( lNII4D, lNIImask, lStartvol,lEndVol);    *}
     //compute gamma
     setlength(X,lnVol);
     setlength(Y,lnVol);
     for lVox := 1 to lnVox do begin
        lNIIttp.f32^[lvox] := kNaNs;//kNaNs;
        lNIImtt.f32^[lvox] := kNaNs;
        lNIIr2.f32^[lvox] := kNaNs;
     end;
     for lVol := 0 to (lnVol-1) do
         X[lVol] :=lVol * lOpts.TRSec; //X is the onset time for each volume....
     //SliceVox := lNII4D.hdr.dim[1]*lNII4D.hdr.dim[2];
     for lVox := 1  to (lnVox) do begin
     //for lVox := lVoxX  to (lVoxX) do begin
        if not specialSingle(lNII4D.f32^[lVox]) then  begin
           //lnVoxEstimated := lnVoxEstimated + 1;
           //determine mean signal during baseline scans
           baselineSignal := 0;
           for lVol := 1 to lStartVol do begin
               i := lVox+((lVol-1)*lnVox);
               baselineSignal := baselineSignal + lNII4D.f32^[i];
           end;
           baselineSignal := baselineSignal/ lStartVol; //mean signal during baseline
           for lVol := 0 to (lnVol-1) do begin
               i := lVox+((lVol+lStartVol-1)*lnVox);
               Y[lVol] := lNII4D.f32^[i]- baselineSignal;
               //if lVox = lVoxX then showmsg(floattostr(Y[lVol]));
           end;
          if  FitGamma (x,y, 0,lnVol-1,g, lOpts.ComputeFitted) then begin
              //if lVox = lVoxX then showmsg('R2: '+floattostr(g.RSquare)+' TTP '+floattostr(g.xMax) +' mean '+floattostr(g.Mean) +' mtt '+floattostr(g.MTT));
              lNIIr2.f32^[lvox] := g.RSquare;
              if (g.RSquare >= lOpts.MinR2) then begin
                 lNIIttp.f32^[lvox] :=  g.xMax;
                 lNIImtt.f32^[lvox] := g.MTT;
                 (*if (g.MTT > 2000) then begin
                    for lVol := 0 to (lnVol-1) do begin
                        showmsg(floattostr(Y[lVol]));

                    end;
                    showmsg('R2: '+floattostr(g.RSquare)+' TTP '+floattostr(g.xMax) +' mean '+floattostr(g.Mean) +' mtt '+floattostr(g.MTT));
                    exit;
                 end;*)
              end; //if R^2 exceeds minR^2 threshold
           end;  //if fit gamma
        end;//if unmasked
  end;//for each voxel
   //adjust arterial input function...
   result := true;
666:
    X := nil;
    Y := nil;
end;

procedure fillGaps(var lNII: TNIFTIimg);
var
   lColVox,lnFilled, lnVox,lVox,lSliceVox,lStartVox,lEndVox: integer;
begin
     lColVox := lNII.hdr.dim[1]; //number of voxels in a column
     lSliceVox :=  lNII.hdr.dim[1]*lNII.hdr.dim[2];
     lnVox := lSliceVox*lNII.hdr.dim[3]*lNII.NonspatialDimensionsLoaded;
     if (lnVox < 1) or (lnVox < (3*lSliceVox)) then exit;
     lStartVox :=  (lNII.hdr.dim[1]*lNII.hdr.dim[2]) + 1; //start with second slice
     lEndVox :=  lnVox- (lNII.hdr.dim[1]*lNII.hdr.dim[2]) - 1; //end with n-1 slice
     lnFilled := 0;
     for lVox := lStartVox  to lEndVox do begin
       if  specialSingle(lNII.f32^[lVox]) then begin
          if  (not specialSingle(lNII.f32^[lVox+1])) and  (not specialSingle(lNII.f32^[lVox-1])) and  //previous/next row
          (not specialSingle(lNII.f32^[lVox+lColVox])) and   (not specialSingle(lNII.f32^[lVox-lColVox])) and //previous/next column
          (not specialSingle(lNII.f32^[lVox+lSliceVox])) and   (not specialSingle(lNII.f32^[lVox-lSliceVox]))  //previous/next slice
          then begin
              lNII.f32^[lVox] := (lNII.f32^[lVox+1]+lNII.f32^[lVox-1]+  //previous/next row
                              lNII.f32^[lVox+lColVox]+lNII.f32^[lVox-lColVox]+ //previous/next column
                              lNII.f32^[lVox+lSliceVox]+lNII.f32^[lVox-lSliceVox]) /6;
              lnFilled := lnFilled + 1;
          end; //all neighbors have values
       end; //voxel is not a number
     end; //for every voxel
     riteln(lNII.HdrName + ' filled '+inttostr(lnFilled) +' gaps');
 end;

function perfusionanalyzeInner (var lNII4D: TNIfTIimg;  lOptsx: TPerfOpts): boolean; overload;
label
  666;
var
lNIIttp,lNIIr2,lNIImtt: TNIfTIimg;
lOpts: TPerfOpts;
lImgNames: TStrings;
begin
     result := false;
     lOpts := lOptsx;
     if lOpts.FinalVol = 0 then
     	lOpts.FinalVol := lNII4d.NonspatialDimensionsLoaded;
     CreateZeroedFloat3DNII(lNII4D,lNIIttp,changefilepostfix(lNII4d.HdrName,'_ttp'));
     CreateZeroedFloat3DNII(lNII4D,lNIImtt,changefilepostfix(lNII4d.HdrName,'_mtt'));
     CreateZeroedFloat3DNII(lNII4D,lNIIr2,changefilepostfix(lNII4d.HdrName,'_r2'));
     makeBolusPositive(lNII4D,lOpts);
     if not computeGammaX(lNII4D,lNIIttp,lNIIr2,lNIImtt,lOpts) then
        goto 666;
     reportMinMax( lNIIttp);
     fillGaps( lNIIttp);
     WriteNII(lNIIttp.HdrName,lNIIttp);
     reportMinMax( lNIImtt);
     fillGaps( lNIImtt);
     WriteNII(lNIImtt.HdrName,lNIImtt);
     WriteNII(lNIIr2.HdrName,lNIIr2);
     if lOpts.Normalize then begin
        //fsl post processing....
        lImgNames := TStringlist.Create;
        lImgNames.Add(lNII4d.HdrName);
        lImgNames.Add(lNIIttp.HdrName);
        lImgNames.Add(lNIImtt.HdrName);
        FSLflirt(lImgNames);
        lImgNames.Free;
     end;
     result := true;
     666:  //release data
     FreeNII(lNIIttp);
     FreeNII(lNIIr2);
     FreeNII(lNIImtt);
 end;

 procedure mask4DwithImg(var lNII4D, lNIImask: TNIFTIimg);
 var
   lnVox,lVox,lnVol,lVol,lnMask: integer;
 begin
      lnVox := lNII4D.hdr.dim[1]*lNII4D.hdr.dim[2]*lNII4D.hdr.dim[3];
      if lnVox <> (lNIImask.hdr.dim[1]*lNIImask.hdr.dim[2]*lNIImask.hdr.dim[3]) then begin
         showmsg('Serious error: mask and 4D data have different sized volumes '+lNII4D.HdrName+' '+lNIImask.HdrName);
         exit;
      end;
      lnVol :=  lNII4d.NonspatialDimensionsLoaded;
      if (lnVol < 1) or (lNIImask.NonspatialDimensionsLoaded < 1) then begin
         showmsg('Serious error: mask and/or 4D data not loaded ' +lNII4D.HdrName+' '+lNIImask.HdrName);
         exit;
      end;
      lnMask := 0;
      for lVox := 1  to (lnVox) do begin
        if  not positiveSingle(lNIImask.f32^[lVox]) then begin
           lnMask := lnMask + 1;
           for lVol := 0 to (lnVol-1) do
              lNII4D.f32^[lVox+ (lVol*lnVox)] := kNaNs;
        end;
      end;  //for each voxel
      riteln('Masked '+inttostr(lnMask)+ ' of '+inttostr(lnVox)+' voxels in '+lNII4D.HdrName+' that were excluded by the masking image');
 end;

function perfusionanalyze1 (lFilename4D: string;  lOptsx: TPerfOpts): boolean; overload;
var
   lOpts: TPerfOpts ;
   lNIImask,lNII4D: TNIfTIimg;
   lName : string;
begin
     result := false;
     lOpts := lOptsx;
     lName := lFilename4D;
     CreateNII(lNII4D);
     if not ReadNIIHdr(lName,lNII4D) then exit;
     (*if not lOpts.premask then begin
        lOpts.premask := true;
        showmsg('Note: Premask always generated');
     end;*)
     if lOpts.TRsec = 0 then
        lOpts.TRsec := lNII4D.hdr.pixdim[4]
     else
         lNII4D.hdr.pixdim[4] := lOpts.TRsec;//TR required by temporal smooth
     if lOpts.TRsec < 0.05 then begin
        riteln('WARNING: unable to detect TR, setting this to 1sec - useful for arbitrary units (e.g. Z-scores) only');
        lOpts.TRsec := 1;
     end;
     if lOpts.FinalVol = 0 then
        lOpts.FinalVol := lNII4d.NonspatialDimensionsLoaded;
     if lOpts.SliceTimeCorrect = kAutoDetect then begin
        lOpts.SliceTimeCorrect := lNII4D.hdr.slice_code;
        if (lOpts.SliceTimeCorrect < kAscending) or (lOpts.SliceTimeCorrect > kDescendingInterleavedSiemens) then begin
           lOpts.SliceTimeCorrect := kSimultaneous;
           riteln('****WARNING the header does not report slice order : slice timing correction will be skipped for '+lFilename4D);
        end
     end;
     //next FSL pre processing (optional)
     (*if lOpts.SliceTimeCorrect <> kSimultaneous then
        lName := FSLslicetimer (lName, lOpts.SliceTimeCorrect, lOpts.TRsec)
     else
         showmsg('FSL slice timer skipped');*)
     if lOpts.MotionCorrect then
	lName := FSLmcflirt (lName)
     else
         riteln('FSL motion correction skipped');
     if lOpts.BrainExtract then
        lName := FSLbet(lName,0.4)
     else
         riteln('FSL brain extraction skipped');
     //next load data
     if not ReadNIIHdr(lName,lNII4D) then exit;
     if not Read4DF32(lNII4D.HdrName, lNII4D) then exit;
     if lOpts.SliceTimeCorrect <> kSimultaneous then //if motion correction was done, we have already completed slice time correction
        nii_slicetimecorrect_cubic( lNII4D,lOpts.SliceTimeCorrect)
     else
         riteln('Internal slice timer skipped');
     //create mask - do this before smoothing!
     if true then begin//make a mean image
        CreateZeroedFloat3DNII(lNII4D,lNIImask,changefilepostfix(lNII4d.HdrName,'_mean'));
        makemean(lNII4d,lNIImask); //make a mean prior to blurring with a smooth!
        WriteNII(lNIImask.HdrName,lNIImask);
        FreeNII(lNIImask);
     end;
     CreateZeroedFloat3DNII(lNII4D,lNIImask,changefilepostfix(lNII4d.HdrName,'_mask'));
     makepremask(lNII4D,lNIImask); //mask4D(lNII4D, lNIImask);
     //WriteNII(lNIImask.HdrName,lNIImask); exit;
     if lOpts.SmoothFWHMmm > 0 then
     	nii_smooth_gauss( lNII4D,lOpts.SmoothFWHMmm)
     else
         riteln('skipping spatial smoothing');
     if lOpts.SmoothFWHMsec > 0 then
     	nii_smooth_gauss_sec( lNII4D,lOpts.SmoothFWHMsec)
     else
         riteln('skipping temporal smoothing');
     if not makemask(lNII4D,lNIImask,lOpts) then begin
        riteln('A serious error has occurred with makemask');
        exit;
     end;
     //WriteNII(lNIImask.HdrName,lNIImask); //exit;
     mask4DwithImg (lNII4D, lNIImask);
     FreeNII(lNIImask);
     perfusionanalyzeInner (lNII4D, lOpts);
     FreeNII(lNII4D);
     result := true;
end;

(*function perfusionanalyze1 (lFilename4D: string;  lOptsx: TPerfOpts): boolean; overload;
label
  666;
var
lNIIttp,lNIIr2,lNIImask,lNIImtt,lNIIaif, lNII4D: TNIfTIimg;
lOpts: TPerfOpts;
lName: string;
begin
     result := true;
   lOpts := lOptsx;
   if lOpts.FinalVol = 0 then
     	lOpts.FinalVol := lNII4d.NonspatialDimensionsLoaded;
   lOpts.ConvertToConcentrationTime := false;
   CreateNII(lNII4D);
   if not ReadNIIHdr('my4d.nii',lNII4D) then exit;
   if not Read4DF32(lNII4D.HdrName, lNII4D) then exit;
   CreateNII(lNIImask);
   if not ReadNIIHdr('mymask.nii',lNIImask) then exit;
   if not Read4DF32(lNIImask.HdrName, lNIImask) then exit;
   forceSpecial (lNIImask);
   CreateZeroedFloat3DNII(lNII4D,lNIIttp,changefilepostfix(lNII4d.HdrName,'_ttp'));
   //CreateZeroedFloat3DNII(lNII4D,lNIIfm,changefilepostfix(lNII4d.HdrName,'_fm'));
   CreateZeroedFloat3DNII(lNII4D,lNIImtt,changefilepostfix(lNII4d.HdrName,'_mtt'));
   CreateZeroedFloat3DNII(lNII4D,lNIIr2,changefilepostfix(lNII4d.HdrName,'_r2'));
   CreateZeroedFloat3DNII(lNII4D,lNIIaif,changefilepostfix(lNII4d.HdrName,'_aif'));
   reportMinMax( lNII4D);
   makeBolusPositive(lNII4D,lOpts);
   reportMinMax( lNII4D);
   exit;
   if not computeGammaX(lNII4D,lNIIttp,lNIIr2,lNIImtt,lNIIaif,lOpts) then
        	goto 666;
   reportMinMax( lNIIttp);
   WriteNII(lNIIttp.HdrName,lNIIttp);
   //reportMinMax( lNIIfm);
   //WriteNII(lNIIfm.HdrName,lNIIfm);
   reportMinMax( lNIImtt);

   WriteNII(lNIImtt.HdrName,lNIImtt);
   WriteNII(lNIIr2.HdrName,lNIIr2);
   //WriteNII(lNIIaif.HdrName,lNIIaif);
   666:  //release data
   FreeNII(lNIIaif);
   FreeNII(lNIIttp);
   FreeNII(lNIIr2);
   FreeNII(lNIImtt);
   //FreeNII(lNIIFM);
   FreeNII(lNIImask);

 end;*)

(*function perfusionanalyze1666 (lFilename4D: string;  lOptsx: TPerfOpts): boolean; overload;
label
  666,555;
var
  lName,lMeanName: string;
  lNII4D: TNIfTIimg;
  lnVox,lnVol: integer;
  lNIIttp,lNIIr2,lNIImask,lNIImtt,lNIIfm,lNIIaif: TNIfTIimg;
  lOpts: TPerfOpts;
  lImgNames: TStrings;
begin
     //initialize settings
     result := false;
     lName := lFilename4D;
     CreateNII(lNII4D);
     if not ReadNIIHdr(lName,lNII4D) then exit;
     lnVox := lNII4D.hdr.dim[1]*lNII4D.hdr.dim[2]*lNII4D.hdr.dim[3];
     lnVol := lNII4D.NonspatialDimensionsAvailable;
     if (lnVox < 1) or (lnVol < 5) then begin
     	showmsg('nii_perf error with input (requires 4D data with at least 5 timepoints)');
        goto 555;
     end;
     lOpts := loptsx;
     lOpts.AIFx0 := 0;
     if lOpts.TRsec = 0 then
     	lOpts.TRsec := lNII4D.hdr.pixdim[4]
     else
         lNII4D.hdr.pixdim[4] := lOpts.TRsec;//TR required by temporal smooth
     if lOpts.FinalVol = 0 then
     	lOpts.FinalVol := lNII4d.NonspatialDimensionsLoaded;
     if lOpts.TRsec <= 0 then begin
	 showmsg('nii_perf error: invalid TR');
        goto 555;
     end;
     if lOpts.TEmsec <= 0 then begin  //do this after nVox<1 exclusion to avoid divide by zero
        lOpts.TEmsec := lOpts.TRsec/lNII4D.hdr.dim[3];
        riteln('Error: Echo Time not provided, assuming '+realtostr(lOpts.TEmsec,3)+'msec');
     end;
     //next FSL pre processing (optional)
     if lOpts.MotionCorrect then begin
        if lOpts.SliceTimeCorrect <> kSimultaneous then
     	   lName := FSLslicetimer (lName, lOpts.SliceTimeCorrect, lOpts.TRsec);
	lName := FSLmcflirt (lName);
     end else
         showmsg('FSL motion correction skipped');
     if lOpts.BrainExtract then begin
	 	//lMeanName := FSLmean(lName); //motion corrected mean with scalp
                lName := FSLbet(lName,0.3);
     end else
         showmsg('FSL brain extraction skipped');
     if not Read4DF32(lName, lNII4D) then begin
        showmsg('Error loading '+lName);
        goto 555;
     end;
	 //next main processing
     CreateZeroedFloat3DNII(lNII4D,lNIIfm,changefilepostfix(lNII4d.HdrName,'_fm'));
     CreateZeroedFloat3DNII(lNII4D,lNIImtt,changefilepostfix(lNII4d.HdrName,'_mtt'));
     CreateZeroedFloat3DNII(lNII4D,lNIIttp,changefilepostfix(lNII4d.HdrName,'_ttp'));
     CreateZeroedFloat3DNII(lNII4D,lNIIr2,changefilepostfix(lNII4d.HdrName,'_r2'));
     CreateZeroedFloat3DNII(lNII4D,lNIImask,changefilepostfix(lNII4d.HdrName,'_mask'));
     CreateZeroedFloat3DNII(lNII4D,lNIIaif,changefilepostfix(lNII4d.HdrName,'_aif'));

     //slice time correct first - data typically interleaved!
     if not lOpts.MotionCorrect then //if motion correction was done, we have already completed slice time correction
        nii_slicetimecorrect_cubic( lNII4D,lOpts.SliceTimeCorrect)
     else
         showmsg('Internal slice timer skipped');
     showmsg('making mean');
     lMeanName := changefilepostfix(lNII4d.HdrName,'_mean');
     makemean(lNII4d,lNIImask); //make a mean prior to blurring with a smooth!
     WriteNII(lMeanName,lNIImask);
     if lOpts.premask then
     	makepremask(lNII4D,lNIImask)
     else
         riteln('Premask skipped');

     if lOpts.SmoothFWHMmm > 0 then
     	nii_smooth_gauss( lNII4D,lOpts.SmoothFWHMmm)
     else
         showmsg('skipping spatial smoothing');
     if lOpts.SmoothFWHMsec > 0 then
     	nii_smooth_gauss_sec( lNII4D,lOpts.SmoothFWHMsec)
     else
         showmsg('skipping temporal smoothing');
      reportMinMax( lNIImask);

     if not makemask(lNII4D,lNIImask,lOpts) then
        	goto 666;
       reportMinMax( lNIImask);
     WriteNII(lNIImask.HdrName,lNIImask);
       WriteNII('my4d.nii',lNII4D);
       exit;//666666

     //fit gamma
     showmsg('fitting data - this may take a while');
     if lOpts.ComputeFitted then
        riteln('Computing gamma fits: initial Madsen approximation, final adjustments with Powell''s method')
     else
     	 riteln('Computing gamma fits: Madsen approximation');
     if not computeGamma(lNII4D,lNIImask,lNIIttp,lNIIr2,lNIImtt,lNIIfm,lNIIaif,lOpts) then
        	goto 666;
       WriteNII(lNIIttp.HdrName,lNIIttp);
     reportMinMax( lNIIttp);
exit; //666666
     WriteNII(lNIIr2.HdrName,lNIIr2);
     WriteNII(lNIIaif.HdrName,lNIIaif);
     WriteNII(lNIImask.HdrName,lNIImask);
     WriteNII(lNIImtt.HdrName,lNIImtt);
     WriteNII(lNIIfm.HdrName,lNIIfm);
     exit;//66666
     ReportMeanSliceTime (lNIIttp);
     {riteln('Created mirrored ttp and msr images (prefix ''m'')');
     nii_mirror(lNIImtt);
     nii_mirror(lNIIttp);
     WriteNII(changefileprefix(lNIIttp.HdrName,'m'),lNIIttp);
     WriteNII(changefileprefix(lNIImtt.HdrName,'m'),lNIImtt); }
     if lOpts.ComputeRaw then begin;
          if  computeRaw(lNII4D,lNIImask,lNIIttp,lNIImtt,lOpts) then begin
          	  WriteNII(changefilepostfix(lNII4d.HdrName,'_rawttp'),lNIIttp);
          	  WriteNII(changefilepostfix(lNII4d.HdrName,'_rawmsr'),lNIImtt);
          end;
     end else
         riteln('Skipping raw MSR and TTP estimates');
     result := true;
     if lOpts.Normalize then begin
        //fsl post processing....
        lImgNames := TStringlist.Create;
        lImgNames.Add(lMeanName);
        lImgNames.Add(lNIIttp.HdrName);
        lImgNames.Add(lNIImtt.HdrName);
        lImgNames.Add(lNIIfm.HdrName);
        FSLflirt(lImgNames);
        lImgNames.Free;
     end;
666:
     FreeNII(lNIIaif);
     FreeNII(lNIIttp);
     FreeNII(lNIIr2);
     FreeNII(lNIImtt);
     FreeNII(lNIIFM);
     FreeNII(lNIImask);
555:
	 FreeNII(lNII4D);
end;       *)

procedure perfusionanalyze(lImages: Tstrings;  lOpts: TPerfOpts); overload;
var
  lI,lnI: integer;
begin
     lnI := lImages.Count;
     if lnI < 1 then
      exit;
     for lI := 1 to lnI do
         perfusionanalyze1(lImages[lI-1],lOpts);
end;

end.

