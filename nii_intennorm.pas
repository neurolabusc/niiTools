unit nii_intennorm;

interface

uses
  classes, define_types,nii_core, dialogsx, math, sysutils;

procedure intensitynormalize(lMask,lRegion1,lRegion2: String; lImages: Tstringlist);

implementation

procedure intensitynormalize(lMask,lRegion1,lRegion2: String; lImages: Tstringlist);
label
  666;
var
  lnVox,lNImg,lI,lV,lN: integer;
  lMean1,lMean2,lSum,lScale: double;
  lNIIImg,lNII1,lNII2,lNIIMask: TNIfTIimg;
begin
     lNImg := lImages.Count;
     if lNImg < 1 then
      exit;
     CreateNII(lNIIImg);
     CreateNII(lNII1);
     CreateNII(lNII2);
     CreateNII(lNIIMask);
     if not Read3DBinary (lRegion1, lNII1) then
        goto 666;
     if not Read3DBinary (lRegion2, lNII2) then
        goto 666;
     if (lMask <> '') and (fileexists(lMask)) then begin
      if not Read3DBinary (lMask, lNIIMask) then
          goto 666;
     end else
      lNIIMask.Hdr.dim[1] := 0;

     for lI := 1 to lNImg do begin
         if not Read3DF32(lImages[lI-1], lNIIImg) then begin
            showmsg('Error loading '+lImages[lI-1]);
            goto 666;
         end;
         lnVox := lNIIImg.hdr.dim[1]*lNIIImg.hdr.dim[2]*lNIIImg.hdr.dim[3];
         if (lNIImask.Hdr.dim[1] > 0) and (not SameXYZDims(lNIIImg,lNIIMask)) then begin
            showmsg('Dimensions do not match '+lMask+' <> '+lNIIImg.HdrName);
            goto 666;
         end;
         if (not SameXYZDims(lNIIImg,lNII1)) or  (not SameXYZDims(lNIIImg,lNII2))  then begin
            showmsg('Dimensions do not match '+lRegion1+ ' <> '+' <> '+lNIIImg.HdrName);
            goto 666;
         end;
         //mask
         if (lNIImask.Hdr.dim[1] > 0) then
            for lV := 1 to lnVox do
              if (lNIIMask.i8^[lV] = 0) then
                lNIIImg.f32^[lV] := 0;
         //mean1
         lN := 0;
         lSum := 0;
         for lV := 1 to lnVox do
             if (lNII1.i8^[lV] <> 0) then begin
                lSum := lSum + lNIIImg.f32^[lV];
                lN := lN+1;
             end;
         if lN > 0 then
          lMean1 := lSum/lN
         else
          lMean1 := 0;
         //mean2
         lN := 0;
         lSum := 0;
         for lV := 1 to lnVox do
             if (lNII2.i8^[lV] <> 0) then begin
                lSum := lSum + lNIIImg.f32^[lV];
                lN := lN+1;
             end;
         if lN > 0 then
          lMean2 := lSum/lN
         else
          lMean2 := 0;
         if (lMean2 <> lMean1) then begin
          lScale := 1/(lMean2-lMean1);
          for lV := 1 to lnVox do
                lNIIImg.f32^[lV] := ((lNIIImg.f32^[lV]-lMean1)*lScale)+1;
          //next - check....
          (*//mean1
          lN := 0;
          lSum := 0;
          for lV := 1 to lnVox do
              if (lNII1.i8^[lV] <> 0) then begin
                 lSum := lSum + lNIIImg.f32^[lV];
                 lN := lN+1;
              end;
          if lN > 0 then
           lMean1 := lSum/lN
          else
           lMean1 := 0;
          //mean2
          lN := 0;
          lSum := 0;
          for lV := 1 to lnVox do
              if (lNII2.i8^[lV] <> 0) then begin
                 lSum := lSum + lNIIImg.f32^[lV];
                 lN := lN+1;
              end;
          if lN > 0 then
           lMean2 := lSum/lN
          else
           lMean2 := 0;
          fx(lmean1,lmean2);*)

          lNIIImg.Hdr.scl_slope := 1;
          lNIIImg.Hdr.scl_inter := 0;
          lNIIImg.HdrName := changefileprefix(lImages[lI-1],'r');//rescaled
          riteln('scaled '+lNIIImg.HdrName+' output=(input-'+realtostr(lMean1,3)+')*'+realtostr(lScale,3)+' + 1');
          WriteNII(lNIIImg.HdrName,lNIIImg);
         end else
          riteln('Unable to scale '+lNIIImg.HdrName+' (regions have identical intensity)');


     end;

666:
     FreeNII(lNIIImg);
     FreeNII(lNII1);
     FreeNII(lNIIMask);
     FreeNII(lNII2);

end;

end.
 