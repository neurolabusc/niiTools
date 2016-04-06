unit nii_peak;

{$mode objfpc}{$H+}

interface

uses
  classes, define_types,nii_core, dialogsx, math, sysutils, nii_smooth;
function threshpeaks(lImages: Tstrings; DesiredPeaks: integer; lStepSize, lSmoothFWHM: single): boolean; overload;
//function threshpeaks(var lNII:TNIfTIimg; DesiredPeaks: integer):boolean; overload;

implementation

function PeakCount (var lImg: bytep; lX,lY,lZ: integer): integer;
//warning: wraps edges!
var
  lV,lTSlice,lDSlice,lXY,lXYZ,lQTail,lQHead,lQSz: integer;
  lQra: LongIntP;
const
     kFillValue = -2;
Procedure IncQra(var lVal, lQSz: integer);
begin
    inc(lVal);
    if lVal >= lQSz then
     lVal := 1;
end; //nested incQra
procedure Check(lPixel: integer);
 begin
    if (lImg^[lPixel]<>0) then begin//add item
        incQra(lQHead,lQSz);
        lImg^[lPixel] := 0;
        lQra^[lQHead] := lPixel;
   end;
end;//nested Check
PROCEDURE RetirePixel; //FIFO cleanup , 1410: added 18-voxel check
var
    lVal: integer;
BEGIN
   lVal := lQra^[lQTail];
   if (lVal < lTSlice) and (lVal > lDSlice) then begin
      Check(lVal-1); //left
      Check(lVal+1); //right
      Check(lVal-lX); //up
      Check(lVal+lX); //down
      Check(lVal-lXY); //+Z
      Check(lVal+lXY); //-Z

      Check(lVal-lX-1); //upL
      Check(lVal+lX-1); //downL
      Check(lVal-lX-1); //upR
      Check(lVal+lX-1); //downR
      //up a slice
      Check(lVal-1+lXY); //left
      Check(lVal+1+lXY); //right
      Check(lVal-lX+lXY); //up
      Check(lVal+lX+lXY); //down
      //down a slice
      Check(lVal-1-lXY); //left
      Check(lVal+1-lXY); //right
      Check(lVal-lX-lXY); //up
      Check(lVal+lX-lXY); //down
      //up a slice
      Check(lVal-lX-1-lXY); //upL
      Check(lVal+lX-1-lXY); //downL
      Check(lVal-lX-1-lXY); //upR
      Check(lVal+lX-1-lXY); //downR
      //down a slice
      Check(lVal-lX-1+lXY); //upL
      Check(lVal+lX-1+lXY); //downL
      Check(lVal-lX-1+lXY); //upR
      Check(lVal+lX-1+lXY); //downR    end;//not outside volume
   end;
   incQra(lQTail,lQSz); //done with this pixel
END;
procedure FillStart (lPt: integer); {FIFO algorithm: keep memory VERY low}
//var lI: integer;
begin
  lQHead := 0;
  lQTail := 1;
  Check(lPt);
  RetirePixel;
  while ((lQHead+1) <> lQTail) do begin//complete until all voxels in buffer have been tested
        RetirePixel;
        if (lQHead = lQSz) and (lQTail = 1) then
           exit; //break condition: avoids possible infinite loop where QTail is being incremented but QHead is stuck at maximum value
  end;
end;

begin
     result := 0;
     lXY := lX * lY;
     lXYZ := lXY * lZ;
     lDSlice := lXY+lX+1;
     lTSlice := lXYZ-lXY-lX-1;
   if lXYZ < 1 then
        exit;
     GetMem(lQra,lQsz * sizeof(longint) );
     for lV := 1 to lXYZ do begin
         if lImg^[lV] <> 0 then begin
            inc(result);
            FillStart(lV);
         end;
     end;
     FreeMem(lQra);
end;

function CountPeaks (var lNII:TNIfTIimg; lThresh: single): integer;
var
   lImg: bytep;
   lV,lnVox: integer;
begin
     result := 0;
     lnVox := lNII.hdr.dim[1]*lNII.hdr.dim[2]*lNII.hdr.dim[3];
     if (lNII.hdr.dim[4] > 1) or (lnVox < 1) or (lNII.hdr.datatype <> kDT_FLOAT)  then
        exit;
     getmem(lImg,lnVox * sizeof(byte));
     for lV := 1 to lnVox do
     	 lImg^[lV] := 0;
     for lV := 1 to lnVox do
         if lNII.f32^[lV]  >= lThresh then
            lImg^[lV] := 1;
     result := PeakCount ( lImg,lNII.hdr.dim[1],lNII.hdr.dim[2],lNII.hdr.dim[3]) ;
     freemem(lImg);
end;

function threshpeak(var lNII:TNIfTIimg; DesiredPeaks: integer; lStepSize: single):boolean; overload;
label
     666;
var
  lV,lnVox,lC,lCprev: integer;
  lMinG0,lMax,lThresh: single;

begin
  result := false;
  lnVox := lNII.hdr.dim[1]*lNII.hdr.dim[2]*lNII.hdr.dim[3];
  if (lNII.hdr.dim[4] > 1) or (lnVox < 1) or (lNII.hdr.datatype <> kDT_FLOAT)  then begin
        riteln('threshpeaks error: only available for 3D 32-bit single precision data.');
        exit;
  end;
  for lV:= 1 to lnVox do
      if specialsingle(lNII.f32^[lV]) then
      	lNII.f32^[lV] := 0;
  lMax := lNII.f32^[1];
  for lV:= 1 to lnVox do
      if lNII.f32^[lV] > lMax then
         lMax := lNII.f32^[lV];
  lMinG0 := lMax;  //minimum >0
  for lV:= 1 to lnVox do
      if (lNII.f32^[lV] < lMinG0) and (lNII.f32^[lV] > 0) then
         lMinG0 := lNII.f32^[lV];
  if (lMax <= 0) or (lMax=lMinG0) then begin
     riteln('threshpeaks error: no positive values or no range of values in image.');
     exit;
  end;
  riteln('range>0 : '+realtostr(lMinG0,3)+'...'+realtostr(lMax,3));

  lThresh := lMax;
  lCprev := -1;
  while lThresh >= lMinG0 do begin
        lC := CountPeaks ( lNII, lThresh);
        if lC <> lCprev then begin//only report when there is a change in the number of clusters...
           riteln('Thresh '+realtostr(lThresh,6)+' has '+inttostr(lC)+' clusters');
           if lCprev >= DesiredPeaks then begin
              lThresh := lThresh + 0.5*lStepSize;
              goto 666;
           end;
        end;
        lCprev := lC;
        lThresh := lThresh-lStepSize;
  end;
666:
  if lThresh <= lMinG0 then
     exit; //no solution found...
  riteln('Thresholding at '+realtostr(lThresh,6));
  for lV:= 1 to lnVox do
    if (lNII.f32^[lV] < lThresh) then
       lNII.f32^[lV] := 0;
  result := true;
end;//niiMirror

function threshpeaks(lImages: Tstrings; DesiredPeaks: integer; lStepSize,lSmoothFWHM: single): boolean; overload;
label
  666;
var
  lNImg,lI: integer;
  lNIIImg: TNIfTIimg;
begin
     result := false;
     lNImg := lImages.Count;
     if lNImg < 1 then
      exit;
     CreateNII(lNIIImg);
     for lI := 1 to lNImg do begin
         if not Read3DF32(lImages[lI-1], lNIIImg) then begin
            showmsg('Error loading '+lImages[lI-1]);
            goto 666;
         end;
         if lSmoothFWHM > 0 then
            nii_smooth_gauss (lNIIImg, lSmoothFWHM);

         result := threshpeak(lNIIImg,DesiredPeaks,lStepSize);
         if result then begin
            lNIIImg.HdrName := changefileprefix(lImages[lI-1],'p');//rescaled
            riteln('thresholded image '+lNIIImg.HdrName);
            WriteNII(lNIIImg.HdrName,lNIIImg);
         end else
          riteln('Unable to find peaks '+lNIIImg.HdrName);


     end;

666:
     FreeNII(lNIIImg);
end;

end.

