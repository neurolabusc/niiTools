unit upart;
//Physiological Artifact Removal Tool
{$H+}
interface
uses
    define_types,SysUtils,nii_core, StrUtils, classes, dialogsx, ezdicom, nii_smooth;

function ApplyPart( lImgFilename, lOutname{optional},l1stDICOMname{optional}: string; lPhysionames: TStrings; lBins,lSliceOrder : integer; lTRsec,lDeleteVols: single; lImgOut,lTextOut:boolean): string;




implementation
type
    TPhysioT =  RECORD
      StartTime,StopTime: Int64;
      Triggers,InterpolatedTriggers, Samples: integer;
      TriggerMedian,TriggerQ1,TriggerQ3: Double;
      TriggerRA: singleP;
    END;
function SaveTriggersAs3ColumnFSL(lPhysioIn: TPhysioT; lOutName: string): boolean;
var
   lF: textfile;
   lPos: integer;
begin
   result := false;
   if (lPhysioIn.Triggers < 1) then
        exit;
   assignfile(lF,lOutName+'.txt');
   Filemode := 0;
   rewrite(lF);
   for lPos := 1 to lPhysioIn.Triggers do
       Writeln(lf,realtostr(lPhysioIn.TriggerRA^[lPos],3)+kTab+'1'+kTab+'1');
   closefile(lF);
   Filemode := 2;
   result := true;
end;

procedure qsort(lower, upper : integer; var Data:SingleP);
//40ms - fast but very recursive...
var
       left, right : integer;
       pivot,lswap: single;
begin
     pivot:=Data^[(lower+upper) div 2];
     left:=lower;
     right:=upper;
     while left<=right do begin
             while Data^[left]  < pivot do left:=left+1;  { Parting for left }
             while Data^[right] > pivot do right:=right-1;{ Parting for right}
             if left<=right then begin   { Validate the change }
                 lswap := Data^[left];
                 Data^[left] := Data^[right];
                 Data^[right] := lswap;
                 left:=left+1;
                 right:=right-1;
             end; //validate
     end;//while left <=right
     if right>lower then qsort(lower,right,Data); { Sort the LEFT  part }
     if upper>left  then qsort(left ,upper,data); { Sort the RIGHT part }
end;

procedure QuartileTriggerSpacing(var lPhysio: TPhysioT);
var
   lTriggerDelayRA: singleP;
   lPos: integer;
begin
          lPhysio.TriggerQ1 := 0;
          lPhysio.TriggerMedian := 0;
          lPhysio.TriggerQ3 := 0;
          if lPhysio.Triggers < 4 then
             exit;
          getmem(lTriggerDelayRA,(lPhysio.Triggers-1)*sizeof(single));
          for lPos := 1 to (lPhysio.Triggers-1) do
              lTriggerDelayRA^[lPos] := abs(lPhysio.TriggerRA^[lPos]-lPhysio.TriggerRA^[lPos+1]);
          qsort(1,lPhysio.Triggers-1,lTriggerDelayRA);//-1 : fence post vs wire 
          lPos := lPhysio.Triggers div 2;
          lPhysio.TriggerMedian := lTriggerDelayRA^[lPos];
          lPos := lPhysio.Triggers div 4;
          lPhysio.TriggerQ1 := lTriggerDelayRA^[lPos];
          lPos := round(0.75*lPhysio.Triggers );
          lPhysio.TriggerQ3 := lTriggerDelayRA^[lPos];
          freemem(lTriggerDelayRA);
end;


(*function PARToolText (var lPhysio: TPhysioT; lTRsec: single; lImgVol, lnBin : integer; lOutname: string): string;
const
   {$IFDEF UNIX} keoln = UNIXeoln; {$ELSE} keoln = chr (13)+chr (10);{$ENDIF}
var
   lBinTxt: array of string;
   lBinWidthStr,lBaseOutname: string;
   lnBinDiv2,lBin,lNextTrigger,i: integer;
   lBinWidth,lBinMin,lBinMax,lTimeSinceTrigger,lOnsetTime,lPrevTriggerTime: double;
  tFile : TextFile;
begin
  result := '';
  if (lPhysio.Triggers < 4) or (lImgVol < 4) then begin
     riteln('PART requires at least 4 triggers and at least 4 volumes each with at least 4 voxels');
     exit;
  end;
  if (lnBin< 4) then begin
     riteln('PART requires at least 4 data bins');
     exit;
  end;
  QuartileTriggerSpacing(lPhysio);
  //find number bin range - this is median-1.5IQR..median+1.5IQR
  lBinMin := -lPhysio.TriggerMedian/2-(abs(lPhysio.TriggerQ1-lPhysio.TriggerQ3)*0.75);
  lBinMax := +lPhysio.TriggerMedian/2+abs(lPhysio.TriggerQ1-lPhysio.TriggerQ3)*0.75;
  lBinWidth := abs((lBinMax-lBinMin)/(lnBin-1));//lnBin-1: fenceposts vs wire
  lBinWidthStr := realtostr(lBinWidth*lTRsec,3);
  lnBinDiv2 := (lnBin div 2)+1;
  setlength(lBinTxt,lnBin);
  for i := 0 to (lnBin-1) do
  	  lBinTxt[i] := '';
  lPrevTriggerTime := -MaxInt;
  lNextTrigger := 1;
  lOnsetTime := 0;
  while lOnsetTime < lImgVol do begin
  		if lOnsetTime > lPhysio.TriggerRA^[lNextTrigger] then begin
           while (lNextTrigger <= lPhysio.Triggers ) and (lOnsetTime > lPhysio.TriggerRA^[lNextTrigger]) do begin
                      lPrevTriggerTime := lPhysio.TriggerRA^[lNextTrigger];
                      inc(lNextTrigger);
           end; //while
        end;//if onset >
        lTimeSinceTrigger :=  lOnsetTime-lPrevTriggerTime;
        if lTimeSinceTrigger > abs(lPhysio.TriggerRA^[lNextTrigger]-lOnsetTime) then
           lTimeSinceTrigger :=  -abs(lPhysio.TriggerRA^[lNextTrigger]-lOnsetTime);//use abs in case we are past final trigger
        //now compute bin...
        if (lTimeSinceTrigger > lBinMin) and (lTimeSinceTrigger < lBinMax) then begin
           lBin := round( (lTimeSinceTrigger)/ lBinWidth)+lnBinDiv2;
           if (lBin < 1) or (lBin > lnBin) then
           	  riteln('PART error: impossible bin')
           else
           	   lBinTxt[lBin-1] := lBinTxt[lBin-1] + realtostr(lOnsetTime*lTRsec ,3)+kTab+ lBinWidthStr+kTab+'1'+ kEoln;
        end;
        lOnsetTime := lOnsetTime + lBinWidth;
  end; //for each bin time
  //output results....
  lBaseOutName := ChangeFileExtX(lOutname,'');
  lBin := 0;
  for i := 0 to (lnBin-1) do begin
      if lBinTxt[i] <> '' then begin
         inc(lBin);
         AssignFile(tFile, lBaseOutname+inttostr(lBin)+'.txt');
  		 ReWrite(tFile);
  		 Write(tFile,lBinTxt[i]);
  		 CloseFile(tFile);
      end; //this bin happened at least once...
      lBinTxt[i] := '';
  end;

end;      *)

function PARTool (var lPhysio: TPhysioT; lImgData: singleP; lTRsec: single; lnVolVox,lnSlices, lImgVol, lBinIn,PhysioOrder,lSliceOrder : integer): string;
const
     kMinSamplesPerBin = 4;
var
   lV,lSliceTime,lMeanSignal,lOnsetTime,lBinWidth,lBinMin,lBinMax,lTimeSinceTrigger,lPrevTriggerTime: double;
   lSlice,lSlicePos,lnSliceVox,lnSlicePos,lVoxel,lBin,lSample,lnBin,lnBinDiv2,lNextTrigger,lSamplesWithVariance,lCorrectedSamples,lVolOffset,lSliceO: integer;
   lBinCountRA,lVolBinRA: longintp;
   lVariance : boolean;
   lBinEstimateRA: doublep;
begin
     result := '';
     if (lPhysio.Triggers < 4) or (lnVolVox < 4) or (lImgVol < 4) then begin
        riteln('PART requires at least 4 triggers and at least 4 volumes each with at least 4 voxels');
        exit;
     end;
     if (lBinIn < 4) then begin
        riteln('PART requires at least 4 data bins');
        exit;
     end;
     lnSliceVox := lnVolVox div lnSlices;
     if (lnVolVox mod lnSlices) <> 0 then begin
         riteln('PART requires volvox to be evenly divisible by number of slices.');
         exit;
     end;
     lSamplesWithVariance := 0;
     lCorrectedSamples := 0;
     QuartileTriggerSpacing(lPhysio);
     //find number bin range - this is median-1.5IQR..median+1.5IQR
     lBinMin := -lPhysio.TriggerMedian/2-(abs(lPhysio.TriggerQ1-lPhysio.TriggerQ3)*0.75);
     lBinMax := +lPhysio.TriggerMedian/2+abs(lPhysio.TriggerQ1-lPhysio.TriggerQ3)*0.75;
     //next - create bins
     lnBin := lBinIn;
     //could adjust number of bins and return here wth a label
    lBinWidth := abs((lBinMax-lBinMin)/(lnBin-1));//lnBin-1: fenceposts vs wire
    lnBinDiv2 := (lnBin div 2)+1;
    getmem(lBinCountRA,lnBin*sizeof(integer));
    getmem(lBinEstimateRA,lnBin*sizeof(double));
    getmem(lVolBinRA,lImgVol*sizeof(integer));
    lVoxel := 0;
    if PhysioOrder = 1 then begin
       case lSliceOrder of
       	 kDescending: riteln('Slice order: descending sequential');
       	 kDescendingInterleavedPhilGE: riteln('Slice order: descending interleaved (Philips/GE)');
         kAscendingInterleavedPhilGE : riteln('Slice order: ascending interleaved (Philips/GE)');
         kDescendingInterleavedSiemens: riteln('Slice order: descending interleaved (Siemens)');
         kAscendingInterleavedSiemens : riteln('Slice order: ascending interleaved (Siemens)');
            else riteln('Slice order: ascending sequential');
       end;

    end;
    //For interleaved, Siemens is a bit weird in that interleaved acquisitions start on the first slice if you have an odd number of total slices, but on the second slice if you have an even number." (http://www.nitrc.org/forum/forum.php?thread_id=3785&forum_id=1456)
    for lSlice := 1 to lnSlices do begin

            //adjust slices so slice 1 occurs at 0, slice 2 at 1/nslices...
            (*if (lSliceOrder = kDescending) or (lSliceOrder = kDescendingInterleavedPhilGE) or (lSliceOrder = kDescendingInterleavedSiemens) then
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
            end;        *)
            lSliceO := nii_smooth_slice_order (lSlice,lnSlices,lSliceOrder);  //see nii_smooth

            if (lSliceOrder =kSimultaneous) then lSliceO := 1;

            lSliceTime := ((lSliceO-1)/lnSlices);
            if (lSlice = 1) and (PhysioOrder = 1) then begin  //WARN user about interleaved slice orders!
               if (lSliceOrder = kAscendingInterleavedPhilGE) or (lSliceOrder = kDescendingInterleavedPhilGE)
               or (lSliceOrder = kAscendingInterleavedSiemens) or (lSliceOrder = kDescendingInterleavedSiemens) then
                  riteln('WARNING: Check slice order- vendors (Siemens, GE, Philips) use different interleaving schemes.');
            end;
            //lSliceTime := lSliceTime-1; //-1 as "lSample" starts at one, not zero
            if PhysioOrder = 1 then
               riteln('Time for slice number='+inttostr(lSlice)+', order='+inttostr(lSliceO)+', time='+floattostr((lSliceTime)*lTRsec)+'sec');

        //do next step for each slice - different slices have different bin distributions due to different slicetime
        //next count number of samples in each bin
        for lBin := 1 to lnBin do
            lBinCountRA^[lBin] := 0;
        lPrevTriggerTime := -MaxInt;
        lNextTrigger := 1;
        for lSample := 1 to lImgVol do begin
                   //for each sample, find nearest trigger
                   lOnsetTime := (lSample-1)+lSliceTime;//-1 as "Sample" indexed from 1 not 0
                   if lOnsetTime > lPhysio.TriggerRA^[lNextTrigger] then begin
                      while (lNextTrigger <= lPhysio.Triggers ) and (lOnsetTime > lPhysio.TriggerRA^[lNextTrigger]) do begin
                            lPrevTriggerTime := lPhysio.TriggerRA^[lNextTrigger];
                            inc(lNextTrigger);
                      end; //while
                   end;//if onset >
                   lTimeSinceTrigger :=  lOnsetTime-lPrevTriggerTime;
                   if lTimeSinceTrigger > abs(lPhysio.TriggerRA^[lNextTrigger]-lOnsetTime) then
                      lTimeSinceTrigger :=  -abs(lPhysio.TriggerRA^[lNextTrigger]-lOnsetTime);//use abs in case we are past final trigger
                   //now compute bin...
                   //inc(lCorrectedSamples);
                   if (lTimeSinceTrigger > lBinMin) and (lTimeSinceTrigger < lBinMax) then begin
                      lBin := round( (lTimeSinceTrigger)/ lBinWidth)+lnBinDiv2;
                      lVolBinRA^[lSample] := lBin;
                      if (lBin < 1) or (lBin > lnBin) then
                         riteln('PART error: impossible bin')
                      else
                          inc(lBinCountRA^[lBin]);
                   end else
                       lVolBinRA^[lSample] := 0;
        end; //for each volume
        for lSlicePos := 1 to lnSliceVox do begin
            inc(lVoxel);
            //first - only correct voxels with variability - do not waste time outside brain
            lVolOffset := lVoxel;
            lVariance := false;
            lSample := 1;
            lV := lImgData^[lVolOffset];
            while (not lVariance) and (lSample <= lImgVol) do begin
                  if lV <> lImgData^[lVolOffset] then
                     lVariance := true;
                  inc(lSample);
                  lVolOffset := lVolOffset+lnVolVox;
            end; //while no variance
            if lVariance then begin //voxel intensity varies accross time - attempt to remove artifact
               lSamplesWithVariance := lSamplesWithVariance +lImgVol;
               //1st - sum effects
               for lBin := 1 to lnBin do
                   lBinEstimateRA^[lBin] := 0;
               lMeanSignal := 0;
               lVolOffset := lVoxel;
               for lSample := 1 to lImgVol do begin
                   lMeanSignal := lImgData^[lVolOffset] + lMeanSignal;
                   lBin := lVolBinRA^[lSample];
                   if (lBin > 0) and (lBinCountRA^[lBin] > kMinSamplesPerBin) then
                         lBinEstimateRA^[lBin] := lBinEstimateRA^[lBin]+ lImgData^[lVolOffset];
                   lVolOffset := lVolOffset+lnVolVox;
               end; //for each volume
               lMeanSignal := lMeanSignal /lImgVol;
               //next compute correction... average signal in bin - average voxel intensity irrelevant of bin
               for lBin := 1 to lnBin do
                   if lBinCountRA^[lBin] > kMinSamplesPerBin then
                      lBinEstimateRA^[lBin] := (lBinEstimateRA^[lBin]/lBinCountRA^[lBin])-lMeanSignal;
               //next  apply correction - inner loop complete for each voxel!
               lVolOffset := lVoxel;
               for lSample := 1 to lImgVol do begin
                   //for each sample, find nearest trigger
                   lBin := lVolBinRA^[lSample];
                   if (lBin > 0) and (lBinCountRA^[lBin] > kMinSamplesPerBin) then begin
                         lImgData^[lVolOffset] :=  (lImgData^[lVolOffset]-lBinEstimateRA^[lBin]);
                         inc(lCorrectedSamples)
                   end;
                   lVolOffset := lVolOffset+lnVolVox;
               end; //for each volume
            end; //if variance
        end;//for each voxel in slice
    end; //for slice
    //**INNER LOOP end -
    //next - report results
    result :=' Time per vol (TR) [sec] '+realtostr(lTRsec,4)+kCR;
    result :=result +' fMRI Volumes '+inttostr(lImgVol)+kCR;
    result :=result +' Triggers n/First...Last [vol] '+realtostr(lPhysio.Triggers,0)+'/'+realtostr(lPhysio.TriggerRA^[1],2)+'...'+realtostr(lPhysio.TriggerRA^[lPhysio.Triggers],2)+kCR;
    if abs(lImgVol-lPhysio.TriggerRA^[lPhysio.Triggers]) > 10 then begin
       result :=result +'******* WARNING: Duration of fMRI session and duration of triggers is very different *******';
       result :=result +'******* Please ensure specified TR is correct, files are correct and onset of fMRI was synchronized with physio data *******';
    end;
    result := result + '    Q1/Median/Q2 [sec] '+realtostr(lTRsec*lPhysio.TriggerQ1,2)+'/'+realtostr(lTRsec*lPhysio.TriggerMedian,2)+'/'+realtostr(lTRsec*lPhysio.TriggerQ3,2)+kCR;
    result  :=  result + ' Bin n/Range [sec] '+inttostr(lnBin)+'/'+realtostr(lTRsec*lBinMin,2)+ '...'+realtostr(lTRsec*lBinMax,2)+kCR;
    result := result+   '  voxels without variance (outside brain) %: '+realtostr(100*( (lnVolVox-(lSamplesWithVariance/lImgVol))/lnVolVox),2)+kCR;
    if lSamplesWithVariance > 0 then
       result := result+   '  voxels with variance which were corrected %: '+realtostr(100*(lCorrectedSamples/lSamplesWithVariance),2)+kCR;
    for lBin := 1 to lnBin do
        result := result+('  Bin '+inttostr(lBin)+ ' '+realtostr(lBin*lBinWidth+lBinMin ,2) +' '+inttostr(lBinCountRA^[lBin])  )+kCR;
    freemem(lBinCountRA);
    freemem(lBinEstimateRA);
    freemem(lVolBinRA);
end;  

function StrVal (var lNumStr: string): integer;
begin
     try
        result := strtoint(lNumStr);
     except
           on EConvertError do begin
              riteln('StrVal Error - Unable to convert the string '+lNumStr+' to a number');
              result := MaxInt;
           end;
     end;
end;

procedure AddSample(var lNumStr: string; var lnTotal,lnSample, lnTrigger: integer; var lPhysio: TPhysioT);
var
   lVal: integer;
begin
     lVal := StrVal(lNumStr);
     if lVal > 5000 then
        exit;
     lNumStr := '';
     inc(lnTotal);
     //https://cfn.upenn.edu/aguirre/wiki/public:pulse-oximetry_during_fmri_scanning
     // The first five values of the first line are the parameters of the acquisition
     if lnTotal < 6 then exit;
          if lVal > 4096 then begin
             if lVal <> 5000 then
                riteln('Potentially serious error: unknown trigger type : '+inttostr(lVal));
             inc(lnTrigger);
             if (lPhysio.Triggers <> 0) then
                lPhysio.TriggerRA^[lnTrigger] := lnSample;
          end else begin
             inc(lnSample);
          end;
end;

function AdjustStartPos (var lStr: string; var lStartPos: integer): boolean;
//Some Siemens physio files appear to have nonsense characters befor real data<bh:ef><bh:bb><bh:bf>1
var
   lLen: integer;
begin
   lLen := length(lStr);
   result := false;
   if (lLen-lStartPos)<2 then
      exit;
   result := true;
   repeat
         if  lStr[lStartPos] in [ '0'..'9'] then
             exit;
         inc(lStartPos);
   until (lStartPos = lLen);
    result := false;
end;

procedure CountValidItems(var lStr, lFilename: string; var lStartPos,lnSample, lnTrigger: integer; var lPhysio: TPhysioT);
label
     123;
var
   lPos,lnTotal: integer;
   lNumStr: string;
   lSkipValues: boolean;
begin
   lnTotal:= 0;
   lSkipValues := false;
   lnSample := 0;
   lnTrigger := 0;
   lNumStr := '';
   if (length(lStr)<2) or (not AdjustStartPos ( lStr, lStartPos)) then
   	  exit; //Oct 2009
   riteln(lFilename+' Length '+inttostr(length(lStr))+' StartPos '+ inttostr(lStartPos));
   for lPos := lStartPos to length(lStr) do begin
       if (lStr[lPos] = ' ') and (lNumStr <> '') then begin
       		//https://cfn.upenn.edu/aguirre/wiki/public:pulse-oximetry_during_fmri_scanning
          // The first five values of the first line are the parameters of the acquisition
          //riteln('e '+lNumStr );
          if (lNumStr = '6002') then
             lSkipValues := false
          else if (lNumStr = '5002') then
             lSkipValues := true
          else if (lNumStr = '5003') then  //Feb 2015
              goto 123 //end of recording
          else if lSkipValues = false then
              AddSample(lNumStr, lnTotal,lnSample, lnTrigger, lPhysio);
          lNumStr := '';
       end else begin
           if  lStr[lPos] in [ '0'..'9'] then
               lNumStr := lNumStr + lStr[lPos];
           //else if lStr[lPos] in [' '] then

           //else begin
                //Showmessage(lStr[lPos]);
                //goto 123;
           //end;
       end;
   end; //for length
123:
   if (lNumStr <> '') and (lSkipValues = False) then
      AddSample(lNumStr, lnTotal,lnSample, lnTrigger, lPhysio);
   lStartPos := lPos;
   while (lStartPos < length(lStr)) and ( lStr[lStartPos] <> ' ') do
         inc(lStartPos);
   if lnTrigger < 1 then begin
      riteln('Error reading '+lFilename+' Samples '+inttostr(lnSample)+' Triggers '+ inttostr(lnTrigger));
      halt(666);
   end;
   riteln(' File '+lFilename+' Samples '+inttostr(lnSample)+' Triggers '+ inttostr(lnTrigger));
end;

procedure CreatePhysio (var lPhysio: TPhysioT);
begin
     lPhysio.Triggers := 0;
     lPhysio.StartTime := -1;
     lPhysio.Samples := 0;
     lPhysio.StopTime := 0;
end;

procedure ClosePhysio (var lPhysio: TPhysioT);
begin
     with lPhysio do begin
          if Triggers > 0 then
             freemem(TriggerRA);
          Triggers := 0;
     end;
end;

procedure InitPhysio(lnTrigger: integer; var lPhysio: TPhysioT);
begin
     ClosePhysio (lPhysio);
     with lPhysio do begin
          Triggers := lnTrigger;
          InterpolatedTriggers := 0;
          if Triggers > 0 then
             getmem(TriggerRA,Triggers*sizeof(single));
     end;
end;

function load3ColTxtPhysio (lFilename: string; var lPhysio: TPhysioT): boolean;
var
   F: TextFile;
   lnTrigger: integer;
   lFloat,lFloat2,lFloat3: single;
begin
    result := false;
     if not fileexists(lFilename) then exit;
     ClosePhysio(lPhysio);
     AssignFile(F, lFilename);
     FileMode := 0;  //Set file access to read only
     //pass 1 - count number of triggers
     lnTrigger := 0;
     Reset(F);
     while not EOF(F) do begin
	  {$I-}
          read(F,lFloat,lFloat2,lFloat3); //read triplets instead of readln: this should load UNIX files
          {$I+}
          if (ioresult = 0) and (lFloat > 0) then
             inc(lnTrigger);
     end;
     //pass 2 - load array
     InitPhysio(lnTrigger, lPhysio);
     lnTrigger := 0;
     Reset(F);
     while not EOF(F) do begin
	  {$I-}
          read(F,lFloat,lFloat2,lFloat3); //read triplets instead of readln: this should load UNIX files
          {$I+}
          if (ioresult = 0) and (lFloat > 0) then begin
             inc(lnTrigger);
             lPhysio.TriggerRA^[lnTrigger] := lFloat;
          end;
     end;
     FileMode := 2;  //Set file access to read/write
     CloseFile(F);
    result := true;
end;

procedure ReadlnX (var F: TextFile; var lResult: string);
var
   lCh: char;
begin
     lResult := '';
     while not Eof(F) do begin
           Read(F, lCh);
           if (lCh in [#10,#13]) then begin
              if lResult <> '' then begin
                 //Showmessage(lResult);
                 exit;
              end;
           end else
               lResult := lResult + lCh;
     end;
end; //ReadlnX

procedure GetSVal (var S: string; var Int: int64);
label
     666;
var
     i: integer;
     T: String;
begin
   if length(S) < 1 then
     exit;
   T := '';
   for i := length(S) downto 1 do begin
       if S[i] in ['0'..'9'] then
       	  T := S[i] + T
       else if T <> '' then
       		goto 666;
   end;
 666:
   if T = '' then exit;
   Int := strtoint(T);
end;

procedure GetStartStopTime (var S: string; var Start,Stop: int64);
//http://cfn.upenn.edu/aguirre/wiki/public:pulse-oximetry_during_fmri_scanning
//Jaemin Shin suggests  LogStartMPCUTime and LogStopMPCUTime are perfectly in sync with TR
begin
  if length(S) < 16 then exit;
  if AnsiContainsStr(S, 'LogStartMDHTime') then//LogStartMPCUTime
     GetSVal(S,Start);
  if AnsiContainsStr(S, 'LogStopMDHTime') then//LogStopMPCUTime
     GetSVal(S,Stop);
end;

function loadSiemensPhysio (lFilename: string; var lPhysio: TPhysioT): boolean;
var
   F: TextFile;
   lStr,lS: string;
   lPos,lnSample,lnTrigger: integer;
begin
    result := false;
     if not fileexists(lFilename) then exit;
     ClosePhysio(lPhysio);
     AssignFile(F, lFilename);
     FileMode := 0;  //Set file access to read only
     Reset(F);
     ReadlnX(F,lStr);//ColNames
     if length(lStr) < 1 then begin
        CloseFile(F);
        exit;
     end;
     //first pass - count items
     lPos := 1;
     CountValidItems(lStr,lFilename,lPos,lnSample,lnTrigger,lPhysio);
     repeat
           ReadlnX(F,lS);
           GetStartStopTime(lS,lPhysio.StartTime,lPhysio.StopTime);
     until lS = '';
     lPhysio.Samples := lnSample;
     //second pass - load array
     if (lnSample < 1) and (lnTrigger < 1) then begin
        CloseFile(F);
        exit;
     end;
     //2nd pass...
     InitPhysio(lnTrigger, lPhysio);
     lPos := 1;
     CountValidItems(lStr,lFilename,lPos,lnSample,lnTrigger,lPhysio);
     FileMode := 2;  //Set file access to read/write
     CloseFile(F);
    result := true;
end;

function InterpolateGaps (var lPhysioIn: TPhysioT): boolean;
//attempts to fill missing trigger pulses
//you must call QuartileTriggerSpacing before this function!
//   it assumes q1/median/q3 are filled
var
   lGap,l2Min,l2Max,l3Min,l3Max: double;
   lnReplace,lTrigger,lTrigger2: integer;
   lTempPhysio: TPhysioT;
begin
     result := false;
     if (lPhysioIn.Triggers < 4) then begin
        riteln('InterpolateGaps requires at least 4 triggers.');
        exit;
     end;
     l2Min := 2*lPhysioIn.TriggerMedian-(abs(lPhysioIn.TriggerQ1-lPhysioIn.TriggerQ3)*1.5);
     l2Max := 2*lPhysioIn.TriggerMedian+(abs(lPhysioIn.TriggerQ1-lPhysioIn.TriggerQ3)*1.5);

     l3Min := 3*lPhysioIn.TriggerMedian-(abs(lPhysioIn.TriggerQ1-lPhysioIn.TriggerQ3)*1.5);
     l3Max := 3*lPhysioIn.TriggerMedian+(abs(lPhysioIn.TriggerQ1-lPhysioIn.TriggerQ3)*1.5);
     if l2Max > l3Min then begin
        riteln('Variability too high for interpoation.');
        exit; //variability too high to determine gaps
     end;
     lnReplace := 0;
     for lTrigger := 2 to lPhysioIn.Triggers do begin
        lGap := lPhysioIn.TriggerRA^[lTrigger] - lPhysioIn.TriggerRA^[lTrigger-1];
         if (lGap > l2Min) and (lGap < l2Max) then
           inc(lnReplace);
        if (lGap > l3Min) and (lGap < l3Max) then
           inc(lnReplace,2);
     end;
     if lnReplace = 0 then begin
         result := true;
         exit;
     end;
     //Riteln(' '+inttostr(lnReplace)+' gaps filled, '+inttostr(lPhysioIn.Triggers)+' actual triggers detected');
     //create temp backup
     CreatePhysio(lTempPhysio);
     InitPhysio(lPhysioIn.Triggers, lTempPhysio);
     for lTrigger := 1 to lPhysioIn.Triggers do
         lTempPhysio.TriggerRA^[lTrigger] := lPhysioIn.TriggerRA^[lTrigger];
     //create resized array
     InitPhysio(lTempPhysio.Triggers+lnReplace, lPhysioIn);
     //fill gaps
     lPhysioIn.TriggerRA^[1] := lTempPhysio.TriggerRA^[1];

     lTrigger2 := 1;
     for lTrigger := 2 to lTempPhysio.Triggers do begin
        inc(lTrigger2);
        lGap := lTempPhysio.TriggerRA^[lTrigger] - lTempPhysio.TriggerRA^[lTrigger-1];
        if ((lGap > l2Min) and (lGap < l2Max)) then begin //1 beat
           lPhysioIn.TriggerRA^[lTrigger2] := lTempPhysio.TriggerRA^[lTrigger-1]+(lgap / 2);
           Riteln(' Trigger number '+inttostr(lTrigger2)+' was interpolated.');
           inc(lTrigger2);

        end;
        if ((lGap > l3Min) and (lGap < l3Max)) then begin //2 beats
           lPhysioIn.TriggerRA^[lTrigger2] := lTempPhysio.TriggerRA^[lTrigger-1]+(lgap / 3);
           Riteln(' Trigger number '+inttostr(lTrigger2)+' was interpolated.');
           inc(lTrigger2);
           Riteln(' Trigger number '+inttostr(lTrigger2)+' was interpolated.');
           lPhysioIn.TriggerRA^[lTrigger2] := lTempPhysio.TriggerRA^[lTrigger-1]+(2*lgap / 3);
           inc(lTrigger2);
        end;
        lPhysioIn.TriggerRA^[lTrigger2] := lTempPhysio.TriggerRA^[lTrigger];
     end;
     ClosePhysio (lTempPhysio);
     lPhysioIn.InterpolatedTriggers := lnReplace;
     result := true;
end;

function ScalePhysioToTime(lPhysio: TPhysioT; lSamplesPerUnit: single): boolean;
const
   tiny=1.0e-20;
var
   lScale: single;
   lTrigger: integer;
begin
     result := false;
     if (lPhysio.Triggers < 4) then begin
        riteln('ScalePhysioToTime requires at least 4 triggers.');
        exit;
     end;
     if (lSamplesPerUnit < tiny) then begin //avoid divide by zero
        riteln('ScalePhysioToTime requires TR(sec) and samples/sec >0.');
        exit;
     end;
     lScale := 1/(lSamplesPerUnit); //use reciprocal: mults faster than divides
     for lTrigger := 1 to lPhysio.Triggers do
        lPhysio.TriggerRA^[lTrigger] := lPhysio.TriggerRA^[lTrigger] * lScale;
    result := true;
end;

procedure EnsureAscending(lPhysio: TPhysioT);
//check if order is correct - if not the sort...
//an alternative is to always sort, but this method is faster and less resource intensive for sorted data
var
   lPos,i: integer;
begin
   if lPhysio.Triggers < 2 then exit;
   for lPos := 2 to lPhysio.Triggers do begin
       if lPhysio.TriggerRA^[lPos] < lPhysio.TriggerRA^[lPos-1] then begin
          riteln('Warning: input times are not in ascending order - data will be sorted.');//1987
          for i := 1 to 6 do
             riteln(realtostr(lPhysio.TriggerRA^[i],4));
          riteln('  n='+inttostr(lPos)+'  '+realtostr(lPhysio.TriggerRA^[lPos-1],4)+'>='+realtostr(lPhysio.TriggerRA^[lPos],4));
          qsort(1,lPhysio.Triggers,lPhysio.TriggerRA); //ensure trigger timings are in order...
          exit;
       end;
   end;
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

procedure PhysioMeanStDev(var lPhysio: TPhysioT);
var
   lPos: integer;
   lTimeDelta,lSum,lSumSqr,lSD,lMean,lSDm,lSDp: double;
begin
   //xx
   if lPhysio.Triggers < 3 then exit; //3 trials = 2 intervals, SD based on N-1
   lSum := 0;
   lSumSqr := 0;
   for lPos := 2 to lPhysio.Triggers do begin
       lTimeDelta := lPhysio.TriggerRA^[lPos]-lPhysio.TriggerRA^[lPos-1];
   	   lSum := lSum + lTimeDelta;
       lSumSqr := lSumSqr + sqr(lTimeDelta);
   end;
   lMean := lSum/(lPhysio.Triggers-1);
   lMean := 1/lMean;//AverageDuration to Hz
   lSD := StDev(lPhysio.Triggers-1,lSum,lSumSqr);
   if
   lSD > 0 then
   //lSD := 1/lSD;
   riteln(' N/MeanInterval/IntervalStDev[Hz]'+kTab+inttostr(lPhysio.Triggers)+kTab+realtostr(lMean,3)+kTab+realtostr(lSD,3) );
end;

function ApplyPartCore( lFilename, lTextOutname: string;lImgData: singleP; lBins,lVolVox,lSlices, lImgVol,lMRIonsettime,PhysioOrder,lSliceOrder : integer; lTRsec,lDeleteVols: single; lImgOut,lTextOut:boolean): string;
var
   lPhysio: TPhysioT;
   lHz: single;
   lMRIonsettimeAdj, lPhysio2MRIDelayMS: integer;
begin
     result := '';
     if not fileexists (lFilename) then exit;
     CreatePhysio(lPhysio);
     if UpCaseExt(lFilename) = '.TXT' then begin
         if not load3ColTxtPhysio(lFilename,lPhysio) then exit;
     end else
         if not loadSiemensPhysio(lFilename,lPhysio) then exit;

     if (lMRIonsetTime >= 0) and (lPhysio.StartTime >= 0) then begin

        lMRIonsettimeAdj := round(lMRIonsettime +(lTRsec*1000*lDeleteVols));//e.g. if you have a TR=2s, and 3 delete volumes, the start time was 2*3*1000=6000ms later than reported
        lPhysio2MRIDelayMS :=  lMRIonsettimeAdj-lPhysio.StartTime; //e.g. if MRI starts at 100, and physio starts at 25, than physio2mri is 75ms
        if lDeleteVols <> 0 then
           riteln(' MRIstarttimems = '+inttostr(lMRIonsetTime)+' adjusted for '+realtostr(lDeleteVols,2)+' delete volumes');
        riteln(' MRIstartms = '+inttostr(lMRIonsettimeAdj)+' PHYSIOstartms = '+inttostr(lPhysio.StartTime)+' Physio begins '+inttostr(lPhysio2MRIDelayMS)+'ms before MRI' );
     end else begin
      riteln('Warning: unsure of relative start time of physio data and MRI scans');
      lPhysio2MRIDelayMS := 0;
     end;
     if lDeleteVols <> 0.0 then begin
      lPhysio2MRIDelayMS := lPhysio2MRIDelayMS + round(lTRsec*1000*lDeleteVols);
      riteln(' Adjusted for '+realtostr(lDeleteVols,1)+' delete volumes, physio starts '+inttostr(lPhysio2MRIDelayMS)+'ms before MRI');
     end;
     EnsureAscending(lPhysio);
     QuartileTriggerSpacing(lPhysio);
     if not InterpolateGaps (lPhysio) then
         exit;
     if UpCaseExt(lFilename) <> '.TXT' then begin//export Siemens file as 3-column text

         if (lPhysio.StopTime > lPhysio.StartTime) and (lPhysio.Samples > 0) then begin
           lHz := lPhysio.Samples/(lPhysio.StopTime-lPhysio.StartTime)*1000; //1000 to convert ms to s.
           riteln(inttostr(lPhysio.samples)+' samples detected in '+inttostr(lPhysio.StopTime- lPhysio.StartTime)+'ms = '+realtostr(lHz,3)+'Hz');
           ScalePhysioToTime(lPhysio,lHz); //50: siemens files use 50 Hz sampling -> convert to sec
         end else begin
           riteln('Warning: unable to find MPCU start and end times: assuming precisely 50Hz sampling rate.');
           ScalePhysioToTime(lPhysio,50); //50: siemens files use 50 Hz sampling -> convert to sec
        end;
        SaveTriggersAs3ColumnFSL(lPhysio,lFilename); //do this before TR conversion...
     end;
     //events are now in Secounds...
     PhysioMeanStDev(lPhysio);
     //Convert units from Seconds to TRs
     ScalePhysioToTime(lPhysio,lTRsec); //Convert sec to volumes
     //bug(lPhysio,lTRSec);
     if lImgOut then
     	result := PARTool (lPhysio,lImgData,lTRsec,lVolVox,lSlices, lImgVol, lBins,PhysioOrder,lSliceOrder)
     else
       riteln('Warning: image data is not being changed (we assume you will add text file regressor to analysis)');
     if lTextOut then
        riteln('***ERROR: requires recompile for text output (must add code for slice order)');
        //	PARToolText (lPhysio, lTRsec, lImgVol, lBins,lTextOutname);
     ClosePhysio(lPhysio);
end;

function VoxelwiseDifference(var v1,v2dest: TNIFTIimg): boolean;
//difference between v1-v2dest is  stored in v2dest
var
   Vox,nVox: integer;
begin
  nVox := v1.VoxelsLoaded;
  if (nVox < 1) or (nVox <> v2dest.VoxelsLoaded) then begin
     riteln('Unable to compute deltaSD map '+inttostr(nVox)+'  '+inttostr(v2dest.VoxelsLoaded) );
     exit;
  end;
   for Vox := 1 to nVox do
       	   v2dest.f32^[vox] :=  v1.f32^[vox]- v2dest.f32^[vox];
end;

function VoxelwiseSD(var vIn,vSD: TNIFTIimg): boolean;
var
   vol,nVol,vox,nVox: integer;
   lSum,lSumSqr,lMean: double;
begin
  result := false;
  nVol := vIn.NonspatialDimensionsLoaded;
  CreateZeroedFloat3DNII(vIn,vSD,'sd.nii');
  nVox := vSD.VoxelsLoaded;
  if (nVol < 2) or (nVox < 1) or ((nVol*nVox) <> vIn.VoxelsLoaded) then begin
     riteln('Unable to compute volume StDev vol='+inttostr(nVol)+' Vox='+inttostr(nVox)+' 4Dvox='+inttostr(vIn.VoxelsLoaded));
     exit;
  end;
  //compute StDev for each voxel in volume...
  for vox := 1 to nVox do begin
      lSum := 0;
      lSumSqr := 0;
      for vol := 1 to nVol do begin
          lSum := lSum + vIn.f32^[((vol-1)*nVox)+vox];
          lSumSqr := lSumSqr + sqr( vIn.f32^[((vol-1)*nVox)+vox]);

      end;//for each vol
      //lMean := lSum/nVol;
   	  //lSD := StDev(nVol,lSum,lSumSqr);
	  vSD.f32^[vox] := StDev(nVol,lSum,lSumSqr);  // <- StdDev
      //lMean := lSum/nVol;
      //if lMean <> 0 then  //<- problem air with values ~0 make this very noisy!
      //	 vSD.f32^[vox] := StDev(nVol,lSum,lSumSqr)/lMean;  // <- StDev / Mean
  end; //for each vox
  result := true;
end;
function ApplyPart( lImgFilename, lOutname{optional},l1stDICOMname{optional}: string; lPhysionames: TStrings; lBins,lSliceOrder : integer; lTRsec,lDeleteVols: single; lImgOut,lTextOut:boolean): string;
//typically we use two physio files: one records respiration, the other records pulse.
const
   tiny=1.0e-20;
var
     vIn,vPre,vPost: TNIFTIimg;
     lMRIonsettime,i,n: integer;
     lTRs: single;
     lOpts: TReadOptions;
begin
	 result := '';
     n := lPhysionames.count;
     if n < 1 then begin
        Riteln('ApplyPart error: No physio files specified.');
        exit;
     end;
     createNII(vPre);
     createNII(vPost);
     createNII(vIn);
  lOpts := DefaultReadOpts ( TFloatx, 0);
  if not ReadNII(lImgFilename,vIn,lOpts) then
     exit;
  Riteln('PART '+kVers+' phsyiological artifact correction tool by Chris Rorden');
  Riteln(' Image='+lImgFilename);
  Riteln(' Bins='+RealToStr(lBins,4));
  lTRs := lTRsec;
  if lTRs < tiny then begin
     lTRs := vIn.Hdr.pixdim[4];
     Riteln(' Using TR from NIfTI header='+RealToStr(lTRs,4));
  end else
  	  Riteln(' Using specified TR='+RealToStr(lTRs,4));

  Riteln(' Dimensions X*Y*Z*T = '
      + inttostr(vIn.Hdr.Dim[1])
      +'*'+ inttostr(vIn.Hdr.Dim[2])
      +'*'+ inttostr(vIn.Hdr.Dim[3])
      +'*'+ inttostr(vIn.Hdr.Dim[4]) );
  if (l1stDICOMname <> '') and Fileexists(l1stDICOMname) then
      lMRIonsettime :=  DICOMmsSinceMidNight(l1stDICOMname)
  else if (length(vIn.Hdr.aux_file) > 1) and (vIn.Hdr.aux_file[1] = '!') then
    lMRIonsettime :=  msSinceMidNight(vIn.Hdr.aux_file)
  else
    lMRIonsettime := -1;
  if lMRIonsettime < 0 then
    Riteln('*UNKOWN MRI ONSET TIME! Please supply name of first DICOM file, or a NIfTI image with "!HHMMSS" as header''s aux_file');
  Riteln(' Assuming 1st MRI scan occurred at '+inttostr(lMRIonsettime)+'ms after midnight.');
  VoxelwiseSD(vIn,vPre);
  for i := 1 to n do begin

      if fileexists(lPhysionames[i-1]) then begin
  	  	 Riteln(' Physio'+chr(64+i)+'='+lPhysionames[i-1]);
         result := result + ApplyPartCore(lPhysionames[i-1],ChangeFilePostfix(lOutname,chr(64+i) ),vIn.f32,lBins,vIn.hdr.dim[1]*vIn.hdr.dim[2]*vIn.hdr.dim[3],vIn.hdr.dim[3],vIn.hdr.dim[4],lMRIonsettime,i,lSliceOrder,lTRs,lDeleteVols, lImgOut,lTextOut);
  	  end else
      	  Riteln('Warning: unable to find '+lPhysionames[i-1]);
  end;//for each physio name
  VoxelwiseSD(vIn,vPost);
  VoxelwiseDifference(vPre,vPost);
  if (lImgOut) and (lOutname <> '') then begin
     WriteNII(ChangeFilePostfix(lOutname,'_dSD' ),vPost);
     WriteNII(lOutname,vIn);
  end;

  FreeNII(vIn);//not required if we only read the header....
  FreeNII(vPre);
  FreeNII(vPost);
end;

end.