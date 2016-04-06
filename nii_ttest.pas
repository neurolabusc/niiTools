unit nii_ttest;
interface
uses
  Forms, //required for application.processmessages
  //FileUtil, Controls, Graphics,
  //Dialogs, ComCtrls,
  //StdCtrls, ExtCtrls,
  classes, define_types,nii_core, dialogsx, math, sysutils,nii_smooth;


function maskedttest(lMask,lOutname: String; lImages: Tstringlist; lnGroup1, lnPermute: integer; lROI,lSmooth: boolean): boolean;
function ROIcollapse (var lNII4D:TNIfTIimg): integer;
procedure ReportMinMax (var lNII4D:TNIfTIimg; lVol: integer);
function TtoZ(t,df: extended): extended;

implementation



function TtoZ(t,df: extended): extended;
// Converts a t value to an approximate z value w.r.t the given df
// s.t. std.norm.(z) = t(z, df) at the two-tail probability level.
//from http://www.anu.edu.au/nceph/surfstat/surfstat-home/tables/t.php
	var
		A9,B9,T9,Z8, P7, B7: extended;
	begin
	A9 := df - 0.5;
	B9 := 48*A9*A9;
	T9 := t*t/df;
	if T9 >= 0.04 then
		Z8 :=A9*ln(1+T9)
	else
		Z8 := A9*(((1 - T9*0.75)*T9/3 - 0.5)*T9 + 1)*T9;
	P7 := ((0.4*Z8 + 3.3)*Z8 + 24)*Z8 + 85.5;
	B7 := 0.8*power(Z8, 2) + 100 + B9;
	result :=  (1 + (-P7/B7 + Z8 + 3)/B9)*sqrt(Z8);
	if t < 0 then
		result := -result;
end;

function TTest2Z(lnObs, lnGroup1: integer; var lIn: Singlep; var fxsize: single): single;
type
  TData = record
    n: integer;
    Sum,SumSqr,Vari: double;
  end;
const
	 tiny = 1.0e-5;
var
   lData: array[1..2] of TData;
	i,g: integer;
    lS: single;
    lD,lDfx: double;

begin
     fxsize := 0.0;
     result := 0.0;
     if  (lnObs < 3) then   //need at least 1 subj in each group,3 total
     	exit;
     for g := 1 to 2 do begin
            lData[g].n := 0;
            lData[g].Sum := 0;
            lData[g].SumSqr := 0;
            lData[g].Vari := 0;
     end;
	 for i := 1 to lnObs do begin //for each subject
            lS := lIn^[i];
            if not specialsingle(lS) then begin
               if (i > lnGroup1) then
                  g := 2
               else
                   g := 1;
               lData[g].n := lData[g].n+1;
               lData[g].Sum := lData[g].Sum + lS;
               lData[g].SumSqr := lData[g].SumSqr + sqr(lS);
            end;
     end;
     if (lData[1].n < 1) or (lData[2].n < 1) or ((lData[1].n+lData[2].n) < 3) then   //need at least 1 subj in each group
     	exit;
     for g := 1 to 2 do begin
            if lData[g].n > 1 then begin
	       lData[g].Vari := (lData[g].n*lData[g].SumSqr) - Sqr(lData[g].Sum);
               lData[g].Vari := lData[g].Vari / (lData[g].n*(lData[g].n-1))
            end;
        end;
        lD := (  ((lData[1].n - 1) * lData[1].Vari + (lData[2].n - 1) * lData[2].Vari) / (lnObs - 2){ldf}) ;
        if abs(lD) < tiny then
           exit;
        if lD < 0 then
           ShowMsg('Error: t-test variance should not be zero.');
	lD := sqrt( lD) ;
        lD := lD * sqrt(1 / lData[1].n + 1 / lData[2].n); //note - to get here both lnx and lny > 0
	if lD <> 0 then begin
       lDfx :=(lData[1].Sum/lData[1].n)-(lData[2].Sum/lData[2].n);
       fxsize := lDfx;
       result := lDfx /lD; //t-value: effect-size/std-var
       result := TtoZ(result,lnObs - 2);
    end;
end;

function PairedT2Z (lnObs: integer; var lIn: SingleP): single;
//lIn has data 1..lnObs observations of the differences...
var
	i: integer;
	lSqrSumDif,lSumDif,lSumDifSqr,lDF,lDif,lmeanDif,lVar: double;
begin
	 result := -0;
     if (lnObs < 2) then
           exit; //df
	lSumDif := 0;
	lSumDifSqr := 0;
	for i := 1 to (lnObs) do begin //for each subject
        lSumDif := lSumDif + lIn^[i];
        lSumDifSqr := lSumDifSqr + sqr(lIn^[i]);
    end;
    lDF := lnObs - 1;
    if (lSumDifSqr <> 0)and (lSumDif <> 0) then begin
       lmeanDif := lSumDif / lnObs;
       lSqrSumDif := sqr(lSumDif);
       lVar := lSumDifSqr - (lSqrSumDif / lnObs);
       lVar := lVar / (lnObs * lDF);
       lVar := sqrt(lVar);
       if lVar <> 0 then begin
              result := lmeanDif / lVar;
              result := TtoZ(result,lDF);
       end;
    end;

end;


//start permute
function TTest4Z(lnObs: integer; var lGroup: bytep; var lIn: Singlep): single;
type
  TData = record
    n: integer;
    Sum,SumSqr,Vari: double;
  end;
const
	 tiny = 1.0e-5;
var
   lData: array[1..2] of TData;
	i,g: integer;
    lS: single;
    lD,lDfx: double;

begin
     result := -0.0;
     if  (lnObs < 3) then   //need at least 1 subj in each group,3 total
     	exit;
     for g := 1 to 2 do begin
            lData[g].n := 0;
            lData[g].Sum := 0;
            lData[g].SumSqr := 0;
            lData[g].Vari := 0;
     end;
	 for i := 1 to lnObs do begin //for each subject
            lS := lIn^[i];
            if not specialsingle(lS) then begin
               if lGroup^[i]=1 then
                  g := 1
               else
                   g := 2;
               lData[g].n := lData[g].n+1;
               lData[g].Sum := lData[g].Sum + lS;
               lData[g].SumSqr := lData[g].SumSqr + sqr(lS);
            end;
     end;
     if (lData[1].n < 1) or (lData[2].n < 1) or ((lData[1].n+lData[2].n) < 3) then   //need at least 1 subj in each group
     	exit;
     for g := 1 to 2 do begin
            if lData[g].n > 1 then begin
	       lData[g].Vari := (lData[g].n*lData[g].SumSqr) - Sqr(lData[g].Sum);
               lData[g].Vari := lData[g].Vari / (lData[g].n*(lData[g].n-1))
            end;
        end;
        lD := (  ((lData[1].n - 1) * lData[1].Vari + (lData[2].n - 1) * lData[2].Vari) / (lnObs - 2){ldf}) ;
        if abs(lD) < tiny then
           exit;
        if lD < 0 then
           ShowMsg('Error: t-test variance should not be zero.');
	lD := sqrt( lD) ;
        lD := lD * sqrt(1 / lData[1].n + 1 / lData[2].n); //note - to get here both lnx and lny > 0
	if lD <> 0 then begin
       lDfx :=(lData[1].Sum/lData[1].n)-(lData[2].Sum/lData[2].n);
       	  result := lDfx /lD; //t-value: effect-size/std-var
           result := TtoZ(result,lnObs - 2);
        end;
end;

procedure sort (lo, up: integer; var r:SingleP);
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

function ReportThresh (lLabel: string; lnPermute: integer; var lRankedData: singleP;lTop:boolean): double;
begin
     result := lRankedData^[IndexPct(lnPermute,0.050,lTop)];
     riteln(lLabel+': permutationFWE '+
       //'0.500='+realtostr(lRankedData[IndexPct(lnPermute,0.500,lTop)],3)+
       ', 0.050='+realtostr({lRankedData^[IndexPct(lnPermute,0.050,lTop)]} result,8)+
       ', 0.025='+realtostr(lRankedData^[IndexPct(lnPermute,0.025,lTop)],8)+
       ', 0.01='+realtostr(lRankedData^[IndexPct(lnPermute,0.010,lTop)],8)+
       ' ');
end;

function PermuteTTest (var lNII4D:TNIfTIimg; lnGroup1,lnPermute: integer): boolean;
var
    lRand,lSwap,lP,lS,lV,lnVox,lnSubj,lnObs,lOffset: integer;
    lData,lMinZ,lMaxZ: Singlep;
    lZ: single;
    lGroup,lRanOrder: bytep;
begin
     result := false;
     DebugFractionCompleted := 0.0;
     lnVox := lNII4D.hdr.dim[1]*lNII4D.hdr.dim[2]*lNII4D.hdr.dim[3];
     lnSubj := lNII4D.hdr.dim[4];
     if (lnGroup1 < 1) or (lnPermute < 1) or (lnVox < 1) or (lnSubj < 3) or (lnGroup1 >= lnSubj) then
     	exit;
     riteln('Thresholding based on '+inttostr(lnPermute)+' permutations [this may take a while...].');
     randomize;
     GetMem(lRanOrder,lnSubj * 1);
     GetMem(lGroup,lnSubj * 1);
     Getmem(lData,lnSubj*sizeof(single));
     Getmem(lMinZ,lnPermute*sizeof(single));
     Getmem(lMaxZ,lnPermute*sizeof(single));
     //initialize group labels
     for lS := 1 to lnGroup1 do
     	 lRanOrder^[lS] := 1;
     for lS := (lnGroup1+1) to (lnSubj) do
         	 lRanOrder^[lS] := 2;
     for lP := 1 to lnPermute do begin
         //for every permutation
          DebugFractionCompleted := lP/lnPermute;
         application.processmessages;
         lMinZ^[lP] := 0;
         lMaxZ^[lP] := 0;
     	 //scramble group labels
         for lS := lnSubj downto 2 do begin
             lRand := Random(lS)+1;
             lSwap := lRanOrder^[lRand];
         	 lRanOrder^[lRand] := lRanOrder^[lS];
         	 lRanOrder^[lS] := lSwap;
     	 end;
         //compute min/max T for this permutated order of group labels...
		 for lV := 1 to lnVox do begin
         	 lnObs := 0;
         	 for lS := 1 to lnSubj do begin
         	 	 lOffset := (lS-1)*lnVox;
             	 if (lNII4D.f32^[lV+lOffset] <> kNaNs) then begin
                 	inc(lnObs);
                    lGroup^[lnObs] := lRanOrder^[lS];
                	lData^[lnObs] := lNII4D.f32^[lV+lOffset];
             	 end;
         	 end;
         	 if (lnObs > 2) then begin
         	 	lZ := TTest4Z(lnObs, lGroup, lData);
                if lZ < lMinZ^[lP] then
				   lMinZ^[lP] := lZ;
                if lZ > lMaxZ^[lP] then
				   lMaxZ^[lP] := lZ;
             end; //for voxels where there are enought samples to test
     	 end; //for each voxel
     end; //for each volume
     sort (1, lnPermute,lMaxZ);
     ReportThresh('Permuted+Z',lnPermute,lMaxZ,true);
     sort (1, lnPermute,lMinZ);
     ReportThresh('Permuted-Z',lnPermute,lMinZ,false);
     Freemem(lMinZ);
     Freemem(lMaxZ);
     Freemem(lData);
     Freemem(lGroup);
     Freemem(lRanOrder);
     result := true;
end;

function PermutePairedTTest (var lNII4D:TNIfTIimg; lnPermute: integer): boolean;
var
    lP,lS,lV,lnVox,lnSubj,lnObs,lOffset: integer;
    lSwap: ByteP;
    lData,lMinZ,lMaxZ: Singlep;
    lZ: single;
begin
     result := false;
     DebugFractionCompleted := 0.0;
     lnVox := lNII4D.hdr.dim[1]*lNII4D.hdr.dim[2]*lNII4D.hdr.dim[3];
     lnSubj := lNII4D.hdr.dim[4];
     if (lnPermute < 1) or (lnVox < 1) or (lnSubj < 3)  then
     	exit;
     riteln('Thresholding based on '+inttostr(lnPermute)+' permutations [this may take a while...].');
     randomize;
     //Setlength(lSwap,lS+1);//+1, as dynamic arrays indexed from 0
     Getmem(lSwap,lnSubj*sizeof(byte));
     Getmem(lData,lnSubj*sizeof(single));
     Getmem(lMinZ,lnPermute*sizeof(single));
     Getmem(lMaxZ,lnPermute*sizeof(single));
     for lP := 1 to lnPermute do begin
         //for every permutation
          DebugFractionCompleted := lP/lnPermute;
         application.processmessages;
         lMinZ^[lP] := 0;
         lMaxZ^[lP] := 0;
     	 //scramble group labels
         for lS := 1 to lnSubj do
             lSwap^[lS] := random(2);
         //compute min/max T for this permutated order of group labels...
		 for lV := 1 to lnVox do begin
         	 lnObs := 0;
         	 for lS := 1 to lnSubj do begin
         	 	 lOffset := (lS-1)*lnVox;
             	 if (lNII4D.f32^[lV+lOffset] <> kNaNs) then begin
                 	inc(lnObs);
                    if odd(lSwap^[lS]) then
                       lData^[lnObs] := -lNII4D.f32^[lV+lOffset]
                    else
                		lData^[lnObs] := lNII4D.f32^[lV+lOffset];
             	 end;
         	 end;
         	 if (lnObs > 2) then begin
         	 	lZ := PairedT2Z (lnObs, lData);
                if lZ < lMinZ^[lP] then
				   lMinZ^[lP] := lZ;
                if lZ > lMaxZ^[lP] then
				   lMaxZ^[lP] := lZ;
             end; //for voxels where there are enought samples to test
     	 end; //for each voxel
     end; //for each volume *)
     sort (1, lnPermute,lMaxZ);
     ReportThresh('Permuted+Z',lnPermute,lMaxZ,true);
     sort (1, lnPermute,lMinZ);
     ReportThresh('Permuted-Z',lnPermute,lMinZ,false);
     Freemem(lMinZ);
     Freemem(lMaxZ);
     Freemem(lData);
     Freemem(lSwap);
     //lSwap := nil;
     result := true;
end;

//end permute

procedure ReportMinMax (var lNII4D:TNIfTIimg; lVol: integer);
var
    lOffset,lV,lnVox: integer;
    lS, lMin,lMax: single;
    lStr: string;
begin
     lnVox := lNII4D.hdr.dim[1]*lNII4D.hdr.dim[2]*lNII4D.hdr.dim[3];
     lOffset := 0;
     lStr := '';
     if lVol > 0 then
        lStr := 'Vol '+inttostr(lVol)+' ';
     if lVol > 1 then
        lOffset := lnVox * (lVol-1);
     if lnVox < 1 then
     	exit;
	 lMin := lNII4D.f32^[lOffset+1];
     lMax := lMin;
     for lV := 1 to lnVox do begin
     	 lS := lNII4D.f32^[lOffset+lV];
         if lS < lMin then lMin := lS;
         if lS > lMax then lMax := lS;
     end;
     if lnVox = 1 then
    	 riteln(lStr+'Observed Z: '+realtostr(lMin,8))
     else
     	 riteln(lStr+'Observed Min/MaxZ: '+realtostr(lMin,8)+kTab+realtostr(lMax,8));
end;


function ROIcollapse (var lNII4D:TNIfTIimg): integer;
//returns number of people with at least 1 voxel intact in volume of interest
var
    lV,lnVox,lS,lnSubj, lROIvox,lOffset: integer;
    lROISum: double;
    lMean: singlep;
begin
     result := 0;
     lnVox := lNII4D.hdr.dim[1]*lNII4D.hdr.dim[2]*lNII4D.hdr.dim[3];
     lnSubj := lNII4D.hdr.dim[4];
     if (lnVox <= 1) or (lnSubj < 1) then
     	exit;
     getmem(lMean, lnSubj * sizeof(single));
     for lS := 1 to lnSubj do begin
            lROIvox := 0;
            lROIsum := 0;
    	 	lOffset := (lS-1)*lnVox;
            for lV := 1 to lnVox do begin
         		if (lNII4D.f32^[lV+lOffset] <> kNaNs) then begin
            	   inc(lROIvox);
               	   lROIsum := lROIsum+lNII4D.f32^[lV+lOffset];
                end; //voxel in VOI
            end; //for each voxel
            if lROIvox > 0 then begin
               inc(result);//valid participant
               lMean^[result] := lROIsum/lROIvox
            end;

     end;
	 //count how many people have at least one valid voxel...
     if result > 0 then begin
        FreeNII(lNII4D);
        lNII4D.hdr.dim[1] := 1;
        lNII4D.hdr.dim[2] := 1;
        lNII4D.hdr.dim[3] := 1;
        CreateZeroedFloat4DNII(lNII4D,lNII4D,lNII4D.HdrName ,result);
        for lS := 1 to result do
        	lNII4D.f32^[lS] := lMean^[lS];
     end;
	 freemem(lMean);
end;

function maskedttest(lMask,lOutname: String; lImages: Tstringlist; lnGroup1,lnPermute: integer; lROI,lSmooth: boolean): boolean;
//Mask: name of masking image, typically template used for normalization  MNI152_T1_2mm_brain.nii
//lImages: one per participant. For each image ('filename.nii') there should also be a lesion mask of same name ('filename.voi')
//for repeated measures t-test: lImages are DIFFERENCE MAPS. Max DF = lImages.Count-1. nGroup1=0
//for unrelated t-test, lImages are continuous images, Max DF=lImages-1.
//  Images 1..nGroup1 are from group 1, nGroup1+1..lImages.count are from group 2.

label
  666;

var
   lNII4D,lNIIx,lNIIMask,lNIILesion: TNIfTIimg;
  lOffset,lI,lJ,lVx,lN,lnObs,lnG1: integer;
  lSum: double;
  lData: singlep;
  lLabel, lLesionName,lOutnameX: string;
begin
     lN := lImages.Count;
     if lN < 1 then
      exit;
     CreateNII(lNII4d);
     CreateNII(lNIIx);
     CreateNII(lNIIMask);
     CreateNII(lNIILesion);

     result := false;
     if fileexists(lMask) then begin
        riteln('MASK: '+lMask);
        if not Read3DBinary (lMask, lNIIMask) then
           goto 666;
     end else begin
     	 riteln('Warning: no brain mask: all voxels will be analyzed.');
         if not Read3DBinary (lImages[0], lNIIMask) then
         	goto 666;
         lVx := lNIIMask.hdr.dim[1]*lNIIMask.hdr.dim[2]*lNIIMask.hdr.dim[3];
          for lJ := 1 to lVx do
              lNIIMask.i8^[lJ] := 1;//all voxels included
     end;
     CreateZeroedFloat4DNII(lNIIMask,lNII4D,'TEMP',lN);
     lVx := lNIIMask.hdr.dim[1]*lNIIMask.hdr.dim[2]*lNIIMask.hdr.dim[3];
     for lI := 1 to lVx*lN do
         lNII4D.f32^[lI] := kNaNs;

     if lnGroup1 > 0 then begin
         lLabel := 'U';
    	 riteln('UNRELATED T-TEST: DF=Obs-2');
    	 riteln('Filename'+kTab+'VoxelsTested(Possible='+inttostr(lVx)+')'+kTab+'Group');
     end else begin
         lLabel := 'P';
         riteln('RELATED (PAIRED) T-TEST: DF=Obs-1');
     	 riteln('DifferenceMapFilename'+kTab+'VoxelsTested(Possible='+inttostr(lVx)+')');
     end;
     for lI := 1 to lN do begin
         if not Read3DF32(lImages[lI-1], lNIIx) then begin
            showmsg('Error loading '+lImages[lI-1]);
            goto 666;
         end;
         if not SameXYZDims(lNIIx,lNIIMask) then begin
            showmsg('Dimensions do not match '+lMask+' <> '+lNIIx.HdrName);
            goto 666;
         end;
         lLesionName := ChangeFileExtX(lIMages[lI-1],'.voi');
         if fileexists(lLesionName) then begin
         	if not Read3DBinary(lLesionName, lNIILesion) then begin
               showmsg('Error loading lesion'+lLesionName);
               goto 666;
         	end;
            if not SameXYZDims(lNIIx,lNIILesion) then begin
               showmsg('Dimensions do not match '+lNIILesion.HdrName+' <> '+lNIIx.HdrName);
               goto 666;
         	end;
         	lOffset := (lI-1)*lVx;
         	lnObs := 0;
         	for lJ := 1 to lVx do begin
             if (lNIIMask.i8^[lJ] <> 0) and (lNIILesion.i8^[lJ] = 0) then begin
                lNII4D.f32^[lJ+lOffset] := lNIIx.f32^[lJ];
                inc(lnObs);
             end;
         	end;
         end else begin //if lesion else nolesion...
             riteln('Warning: no lesion mask. Unable to find '+lLesionName);
             lOffset := (lI-1)*lVx;
         	 lnObs := 0;
         	 for lJ := 1 to lVx do begin
             	 if (lNIIMask.i8^[lJ] <> 0.0) then begin
                 	lNII4D.f32^[lJ+lOffset] := lNIIx.f32^[lJ];
                	inc(lnObs);
             	 end;
         	 end;//for each voxel
         end;//if ..else lesion masking
         if lnGroup1 > 0 then begin
            if lI > lnGroup1 then
               	  lnG1 := 2
            else
            	lnG1 := 1;
            riteln(lImages[lI-1]+kTab+inttostr(lnObs)+kTab+inttostr(lnG1) );
         end else
         	 riteln(lImages[lI-1]+kTab+inttostr(lnObs) );
     	 application.processmessages;
     end;
     if lSmooth then
        nii_smooth_cubic(lNII4D);
     //For regions of interest, collapse to single voxel...
     if (lROI) then begin
     	if(ROIcollapse(lNII4D) < 1) then
     	  goto 666;
        lVx := 1;
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
     FreeNII(lNIILesion);
     CreateZeroedFloat3DNII(lNIIMask,lNIILesion,changefilepostfix(lOutnameX,'MnEffect'+lLabel));
     FreeNII(lNIIx);
     CreateZeroedFloat3DNII(lNIIMask,lNIIx,changefilepostfix(lOutnameX,'Z'+lLabel));
     lNIIx.Hdr.intent_code:=kNIFTI_INTENT_ZSCORE;
     Getmem(lData,lN*sizeof(single));
     if lnGroup1 > 0 then begin //conduct paired test
     	for lJ := 1 to lVx do begin
         lnObs := 0;
         lnG1 := 0;
         lSum := 0;
         for lI := 1 to lN do begin
         	 lOffset := (lI-1)*lVx;
             if (lNII4D.f32^[lJ+lOffset] <> kNaNs) then begin
                inc(lnObs);
                if (lI <= lnGroup1) then
                   inc(lnG1);
                lSum := lSum + lNII4D.f32^[lJ+lOffset];
                lData^[lnObs] := lNII4D.f32^[lJ+lOffset];
             end;
         end;
         if lnObs > 0 then
         	lNIIx.f32^[lJ] := TTest2Z(lnObs,lnG1,lData,lNIILesion.f32^[lJ]);
     	end;
        PermuteTTest (lNII4D, lnGroup1,lnPermute);
     end else begin //if unrelated else paired t-test
      	for lJ := 1 to lVx do begin
          lnObs := 0;
          lSum := 0;
          for lI := 1 to lN do begin
          	 lOffset := (lI-1)*lVx;
              if (lNII4D.f32^[lJ+lOffset] <> kNaNs) then begin
                 inc(lnObs);
                 lSum := lSum + lNII4D.f32^[lJ+lOffset];
                 lData^[lnObs] := lNII4D.f32^[lJ+lOffset];
              end;
          end;
          if lnObs > 0 then begin
          	lNIIx.f32^[lJ] := PairedT2Z(lnObs,lData);
             lNIILesion.f32^[lJ] := lSum/lnObs
          end;
      	end;
        PermutePairedTTest ( lNII4D, lnPermute);
     end; //if unrelated else paired t-test
     ReportMinMax(lNIIx,0);
     if not lROI then begin
     	WriteNII(lNIIx.HdrName,lNIIx);
     	WriteNII(lNIILesion.HdrName,lNIILesion);
     end else
     	 Riteln('Mean Effect Size'+kTab+realtostr(lNIILesion.f32^[1],8) );
     freemem(lData);

     //next: DF map : number of observations
     FreeNII(lNIIx);
     CreateZeroedi16_3DNII(lNIIMask,lNIIx,changefilepostfix(lOutnameX,'Obs'+lLabel));
     for lI := 1 to lN do begin
         lOffset := (lI-1)*lVx;
         for lJ := 1 to lVx do begin
             if (lNII4D.f32^[lJ+lOffset] <> kNaNs) then begin
                lNIIx.i16^[lJ] := lNIIx.i16^[lJ]+1;
             end;
         end;
     end;
     if not lROI then
     	WriteNII(lNIIx.HdrName,lNIIx);
666:
     FreeNII(lNII4D);
     FreeNII(lNIIx);
     FreeNII(lNIIMask);
     FreeNII(lNIILesion);
end;



end.
