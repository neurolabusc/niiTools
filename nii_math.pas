unit nii_math;
interface
uses Classes, define_types, nii_core, sysutils, dialogsx;

function AddVols3D( Filenames: TStrings; lOutname: string): boolean;
procedure InspectVols(Filenames: TStrings);
function NIImin (lNII: TNIFTIimg): double; overload;
function NIImax (lNII: TNIFTIimg): double; overload;
function NIImax (lNII: TNIFTIimg; lVox: integer): double; overload; //threshold to select frac portion of voxels
procedure RemoveMaskNoise (lNII: TNIFTIimg); //for mask zero any voxels surrounded by zeros on all sides....
procedure makeTPM (c1,c2,c3,c4,c5, brainmask: string; lFWHM: single);
//function nii_mirror (var lNII:TNIfTIimg):boolean; overload;
function nii_mirror (lImages: Tstrings):boolean; overload;
function nii_cropslices (lName: string; lCropSlices: integer):boolean;
procedure nii_pastehdr (lHdrName,lImgName: string);
procedure nii_pastehdr8 (lHdrName,lImgName: string);
procedure nii_copyhdr (lOld,lNew: string; slices: integer);
function nii_4Dunity (lName: string; val: single):boolean;
function nii_4Dunity (var lNII: TNIfTIimg; val: single):boolean;
procedure nii_scale (lHdrName,lImgName: string; lScale: single);
procedure maskTPM (lMask,lTPM: string; c1,c2,c3,c4,c5,c6, smoothFWHM: single);
function nii_cropslicesx (lName: string; lCropSlices: integer; lNX: TNIfTIimg):boolean;
procedure setedges (lImage: string; lSlices: integer; lVal: single);
procedure copybottom (lImage: string; lSlices: integer);

implementation
uses nii_smooth;

procedure copybottom (lImage: string; lSlices: integer);
var
  lV,lZ, lXY,lIn,lOut: integer;
  lT:TNIfTIimg;
 begin
  if (not fileexists(lImage)) then exit;
  CreateNII(lT);
  if not Read4DF32(lImage, lT) then exit;
  lV := 0;
  lXY := lT.hdr.dim[1]*lT.hdr.dim[2];
  lIn := lSlices * lXY;
  for lZ := 1 to lSlices do begin
    lOut := (lZ-1) *lXY;
    for lV :=  1 to lXY do begin
          lT.f32^[lOut+lV] := lT.f32^[lIn+lV]
    end;
  end;
  WriteNII(changefileprefix(limage,'x'), lT);
  FreeNII(lT);
end;


procedure setedges (lImage: string; lSlices: integer; lVal: single);
const
  kT = 20;
var
  lnV,lV,lX,lY,lZ: integer;
  lT:TNIfTIimg;
 begin
  if (not fileexists(lImage)) then exit;
  CreateNII(lT);
  if not Read4DF32(lImage, lT) then exit;
  lnV:=  lT.hdr.dim[1]*lT.hdr.dim[2]*lT.hdr.dim[3]*lT.NonspatialDimensionsLoaded;
  lV := 0;
  for lZ := 1 to lT.hdr.dim[3] do
    for lY := 1 to lT.hdr.dim[2] do
      for lX := 1 to lT.hdr.dim[1] do begin
          inc(lV);
      if (lX <= lSlices) or (lX > (lT.hdr.dim[1]-lSlices))
         or (lY = lSlices) or (lY > (lT.hdr.dim[2]-lSlices))
         {or (lZ = 1)} or (lZ > (lT.hdr.dim[3]-lSlices))
         then
             lT.f32^[lV] := lVal

      end;
  WriteNII(changefileprefix(limage,'x'), lT);
  FreeNII(lT);
end;



(*function nii_flipslices (var lNII:TNIfTIimg):boolean; overload;
var
  lnVox,lVol,lnVol,lVolo,
  lZo,lYo,lXY,
    lP,lZ,lZr: integer;
  lImg: bytep;
begin
  result := false;
  lXY := lNII.hdr.dim[1]*lNII.hdr.dim[2];
  lnVox := lXY*lNII.hdr.dim[3];
  lnVol := lNII.hdr.dim[4];
  if lnVol < 1 then lnVol := 1;
  if  (lnVox < 1) or (lNII.hdr.datatype <> kDT_UNSIGNED_CHAR) or (lNII.hdr.dim[1]<3) or (lNII.hdr.dim[2]<3) or  (lNII.hdr.dim[3]<3)  then begin
        riteln('nii_flipslices error: only available for 3D 8-bit single precision data.');
        exit;
  end;
  getmem(lImg,lnVox * sizeof(byte));
  for lVol := 1 to lnVol do begin
    lVolo := (lVol-1)*lnVox;//volume offset
    for lP := 1 to lnVox do
     	 lImg^[lP] := lNII.i8^[lP+lVolo];
    //mirror in Z direction
     for lZ := 1 to (lNII.hdr.dim[3]) do begin
         lZo := (lZ-1)*lXY;
         lZr := (lNII.hdr.dim[3]-lZ)*lXY; //Reversed slice order
         for lP := 1 to lXY do  begin
             lNII.i8^[lZr+lP+lVolo] := lImg^[lZo+lP];
         end;
     end;
end;//for each vol
     freemem(lImg);
     result := true;
end;//nii_flipslices*)

(*function nii_flip180 (var lNII:TNIfTIimg):boolean; overload;
var
  lnVox,lVol,lnVol,lVolo,
  lZo,lYo,lXY,
    lP,lZ: integer;
  lImg: bytep;
begin
  result := false;
  lXY := lNII.hdr.dim[1]*lNII.hdr.dim[2];
  lnVox := lXY*lNII.hdr.dim[3];
  lnVol := lNII.hdr.dim[4];
  if lnVol < 1 then lnVol := 1;
  if  (lnVox < 1) or (lNII.hdr.datatype <> kDT_UNSIGNED_CHAR) or (lNII.hdr.dim[1]<3) or (lNII.hdr.dim[2]<3) or  (lNII.hdr.dim[3]<3)  then begin
        riteln('nii_flip180 error: only available for 3D 8-bit single precision data.');
        exit;
  end;
  getmem(lImg,lnVox * sizeof(byte));
  for lVol := 1 to lnVol do begin
    lVolo := (lVol-1)*lnVox;//volume offset
    for lP := 1 to lnVox do
     	 lImg^[lP] := lNII.i8^[lP+lVolo];
    //mirror in Z direction
     for lZ := 1 to (lNII.hdr.dim[3]) do begin
         lZo := (lZ-1)*lXY;
         for lP := 1 to lXY do  begin
             lNII.i8^[(lXY-lP)+1+lVolo+lZo] := lImg^[lZo+lP];
         end;
     end;
end;//for each vol
     freemem(lImg);
     result := true;
end;//nii_flip180*)
(*function nii_flipAP (var lNII:TNIfTIimg):boolean; overload;
var
  lnVox,lVol,lnVol,lVolo,
  lZo,lXY,lX,lY,lYo,lYr,lZ: integer;
  lImg: bytep;
begin
  result := false;
  lXY := lNII.hdr.dim[1]*lNII.hdr.dim[2];
  lnVox := lXY*lNII.hdr.dim[3];
  lnVol := lNII.hdr.dim[4];
  if lnVol < 1 then lnVol := 1;
  if  (lnVox < 1) or (lNII.hdr.datatype <> kDT_UNSIGNED_CHAR) or (lNII.hdr.dim[1]<3) or (lNII.hdr.dim[2]<3) or  (lNII.hdr.dim[3]<3)  then begin
        riteln('nii_flipAP error: only available for 3D 8-bit single precision data.');
        exit;
  end;
  getmem(lImg,lnVox * sizeof(byte));
  for lVol := 1 to lnVol do begin
    lVolo := (lVol-1)*lnVox;//volume offset
    for lX := 1 to lnVox do
     	 lImg^[lX] := lNII.i8^[lX+lVolo];
    //mirror in Z direction
     for lZ := 1 to (lNII.hdr.dim[3]) do begin
         lZo := (lZ-1)*lXY;
         for lY := 1 to lNII.hdr.dim[2] do  begin
           lYo := (lY-1)* lNII.hdr.dim[1];
           lYr := (lNII.hdr.dim[2]-lY)* lNII.hdr.dim[1];
           for lX := 1 to lNII.hdr.dim[1] do  begin
                 lNII.i8^[lVolo+lZo+lYr+lX] := lImg^[lZo+lYo+lX];

             end;
         end;
     end;
end;//for each vol
     freemem(lImg);
     result := true;
end;//nii_flipAP

function nii_shiftarb (var lNII:TNIfTIimg; loffset: integer):boolean; overload;
var
  lnVox,lVol,lnVol,lVolo,lo,
  lP: integer;
  lImg: bytep;
begin
  result := false;
  lnVox := lNII.hdr.dim[1]*lNII.hdr.dim[2]*lNII.hdr.dim[3];
  lnVol := lNII.hdr.dim[4];
  if lnVol < 1 then lnVol := 1;
  if  (lnVox < 1) or (lNII.hdr.datatype <> kDT_UNSIGNED_CHAR) or (lNII.hdr.dim[1]<3) or (lNII.hdr.dim[2]<3) or  (lNII.hdr.dim[3]<3)  then begin
        riteln('nii_flipAP error: only available for 3D 8-bit single precision data.');
        exit;
  end;
  getmem(lImg,lnVox * sizeof(byte));
  for lVol := 1 to lnVol do begin
    lVolo := (lVol-1)*lnVox;//volume offset
    for lP := 1 to lnVox do
     	 lImg^[lP] := lNII.i8^[lP+lVolo];
    if loffset > 0 then begin
      for lP := (loffset+1) to lnVox do
       	 lNII.i8^[lP-lOffset+lVolo] := lImg^[lP];
    end else begin
      lo := abs(loffset);
      for lP := (lo+1) to lnVox do
       	 lNII.i8^[lP+lVolo] := lImg^[lP-lo];
    end;
end;//for each vol
     freemem(lImg);
     result := true;

end;  *)

procedure makeTPM (c1,c2,c3,c4,c5,brainmask: string; lFWHM: single);
const
  knv = 6;
var
  t,lnVox,lV,lVol: integer;
  s : array [1..knv] of single;
  frac,residual,air : single;
  c,o : array [1..knv] of TNIfTIimg;
  l4d, mask : tNIFTIImg;
begin
  if (not fileexists(c1)) or (not fileexists(c2)) or(not fileexists(c3)) or(not fileexists(c4)) or(not fileexists(c5)) then begin
     Showmsg('makeTPM could not find an image ');
     exit;
  end;

  for lVol := 1 to knv do
      CreateNII(c[lVol]);
  CreateNII(mask);
  if not Read3DF32(c1, c[1]) then exit;
  if not Read3DF32(c2, c[2]) then exit;
  if not Read3DF32(c3, c[3]) then exit;
  if not Read3DF32(c4, c[4]) then exit;
  if not Read3DF32(c5, c[5]) then exit;
  if not Read3DF32(c5, c[6]) then exit; //air does not exist...
  if not Read3DF32(brainmask, mask) then exit; //air does not exist...

  lnVox :=  c[1].hdr.dim[1]*c[1].hdr.dim[2]*c[1].hdr.dim[3];
  for lVol := 1 to knv-1 do
    CreateZeroedFloat3DNII(c[lVol],o[lVol],changefileprefix(c[lVol].HdrName,'x') );
  CreateZeroedFloat3DNII(c[knv],o[knv],changefileprefix(c[knv].HdrName,'z') );
  //Read3DF32(c3,csf );
  nii_smooth_gauss(mask,2);
  for lV := 1 to lnVox do begin
    for t := 1 to knv do
        s[t] := c[t].f32^[lV];
    //preserve MNI
    o[1].f32^[lV] :=  s[1];
    o[2].f32^[lV] :=  s[2];
    o[3].f32^[lV] :=  s[3];
    frac := (s[1]+s[2]+s[3]);
    if mask.f32^[lV] > frac then
       o[3].f32^[lV] := o[3].f32^[lV] + (mask.f32^[lV] - frac);
     //something clever here
     residual := 1 - mask.f32^[lV];
     //residual := 1 - (s[1]+s[2]+s[3]);


     if (residual > 0.0)   then begin
        o[6].f32^[lV] := (1-s[5])*residual;//air
        s[5] := s[5] * residual;
        if s[5] > 0 then begin
           o[4].f32^[lV] :=  s[4]*s[5]; //bone
           residual := s[5]-(s[4]*s[5]);

             if mask.f32^[lV] > 0.01 then
                  o[4].f32^[lV] := o[4].f32^[lV] + residual
             else
                 o[5].f32^[lV] := residual;
           //end;
        end;
     end;
  end;

  CreateZeroedFloat4DNII(c[1],l4d,changefileprefix(c[1].HdrName,'4d') ,knV);
  for lVol := 1 to knV do begin
    t := (lVol-1)*lnVox;
    for lV := 2 to lnVox do begin
          l4d.f32^[lV+t] := o[lVol].f32^[lV];
       end;
  end;
   if lFWHM > 0 then nii_smooth_gauss(l4d,lFWHM);
  nii_4Dunity(l4d,0.995);
  WriteNII(l4d.HdrName, l4d);
  for lVol := 1 to knv do begin
    //showmsg(o[lVol].HdrName);
    //WriteNII(o[lVol].HdrName, o[lvol]);
    FreeNII(c[lVol]);
    FreeNii(o[lVol]);
  end;
  FreeNii(mask);
  FreeNii(l4D);

end;

(*procedure maskTPM (lMask,lTPM: string; c1,c2,c3,c4,c5,c6, lFWHM: single);
var
  lnVox,lnVol,lV,lVol: integer;
  lSum: single;
  lM,lT:TNIfTIimg;
 function GetV(Amt: single; vl,vx: integer): single;
 begin
   result := 0;
   if Amt >= 0 then exit;
   result := (lT.f32^[vx+((vl-1)*lnVox)]*-Amt);
   lT.f32^[vx+((vl-1)*lnVox)] := lT.f32^[vx+((vl-1)*lnVox)]-result;
 end;
 procedure PutV(Amt: single; vl,vx: integer);
 begin
   if Amt < 0 then exit;
   lT.f32^[vx+((vl-1)*lnVox)] := lT.f32^[vx+((vl-1)*lnVox)]+Amt;
 end;
 begin

  if (not fileexists(lMask)) or (not fileexists (lTPM)) then exit;

  CreateNII(lM);
  if not Read3DF32(lMask, lM) then exit;
  if lFWHM > 0 then nii_smooth_gauss(lM,lFWHM);

  CreateNII(lT);
  if not Read4DF32(lTPM, lT) then exit;
  lnVox :=  lT.hdr.dim[1]*lT.hdr.dim[2]*lT.hdr.dim[3];
  lnVol := lT.hdr.dim[4];

  if (lnVol < 5) or (lnVox < 3) then begin
     showmsg('MaskTPM requires TPM with 5 volumes' + inttostr(lnVol));
     exit;
  end;
  if (lnVox <> (lM.hdr.dim[1]*lM.hdr.dim[2]*lM.hdr.dim[3]) ) then begin
     showmsg('Mask and TPM must have same dimensions');
     exit;
  end;
  for lV := 1 to lnVox do begin
      if lM.f32^[lV] > 0 then begin
      lSum := 0;
      //remove values...
      lSum := lSum + GetV(c1,1,lV);
      lSum := lSum + GetV(c2,2,lV);
      lSum := lSum + GetV(c3,3,lV);
      lSum := lSum + GetV(c4,4,lV);
      lSum := lSum + GetV(c5,5,lV);
      //lSum := lSum + GetV(c6,6,lV);
      if lSum > 0 then begin
          //PutV(lSum*c1,1,lV);
          PutV(lSum*c1,1,lV);

          PutV(lSum*c2,2,lV);
          PutV(lSum*c3,3,lV);
          PutV(lSum*c4,4,lV);
          PutV(lSum*c5,5,lV);
          //PutV(lSum*c6,6,lV);

      end;
     end;
  end;
  WriteNII(changefileprefix(lTPM,'x'), lT);
  //xxx
  FreeNII(lM);
  FreeNII(lT);
end;     *)

procedure NIIunity (var lNII: TNIFTIimg); //max value = 1
var
   lnV,lV: integer;
   lMax : single;
begin
     if lNII.hdr.datatype <> kDT_FLOAT then begin
         riteln('NIIunity error: only supports float datatype');
         exit;
     end;
     lnV := lNII.hdr.dim[1]*lNII.hdr.dim[2]*lNII.hdr.dim[3]* lNII.NonspatialDimensionsLoaded;

     if lnV < 1 then
     	exit;
     lMax := NiiMax(lNII);
     if lMax <= 0 then begin
        riteln('NIIunity error: no positive values!');
        exit;
     end;
     for lV := 1 to lnV do
         lNII.f32^[lV] := lNII.f32^[lV] / lMax;
end;
procedure maskTPM (lMask,lTPM: string; c1,c2,c3,c4,c5,c6, smoothFWHM: single);
var
  lnVox,lnVol,lV,lVol: integer;
  lFrac,lSum: single;
  lM,lT:TNIfTIimg;
(*function MoreCSFThanSoft (vx: integer): boolean;
begin
     result := lT.f32^[vx+((3-1)*lnVox)]  > lT.f32^[vx+((5-1)*lnVox)];
end;  *)

function GetV(Amt: single; vl,vx: integer): single;
 begin
   result := 0;
   if Amt >= 0 then exit;
   result := (lT.f32^[vx+((vl-1)*lnVox)]*-Amt);
   lT.f32^[vx+((vl-1)*lnVox)] := lT.f32^[vx+((vl-1)*lnVox)]-result;
 end;
 procedure PutV(Amt: single; vl,vx: integer);
 begin
   if Amt <= 0 then exit;
   lT.f32^[vx+((vl-1)*lnVox)] := lT.f32^[vx+((vl-1)*lnVox)]+Amt;
 end;
 begin

  if (not fileexists(lMask)) or (not fileexists (lTPM)) then begin
     Showmsg('maskTPM could not find an image '+lMask+' '+lTPM);
     exit;
  end;

  CreateNII(lM);
  if not Read3DF32(lMask, lM) then begin
    showmsg('mask tpm error');
    exit;

  end;
  NIIunity(lM);

  if smoothFWHM <> 0 then
     nii_smooth_gauss(lM,abs(smoothFWHM));

  CreateNII(lT);
  if not Read4DF32(lTPM, lT) then exit;
  lnVox :=  lT.hdr.dim[1]*lT.hdr.dim[2]*lT.hdr.dim[3];

  if (lnVol < 6) or (lnVox < 3) then begin
     showmsg('MaskTPM requires TPM with 6 volumes' + inttostr(lnVol));
     exit;
  end;
  if (lnVox <> (lM.hdr.dim[1]*lM.hdr.dim[2]*lM.hdr.dim[3]) ) then begin
     showmsg('Mask and TPM must have same dimensions');
     exit;
  end;
  for lV := 1 to lnVox do begin
    lFrac :=  lM.f32^[lV];
    if lFrac > 0 then begin
      //remove values...
      lSum := 0;
      lSum := lSum + GetV(c1*lFrac,1,lV);
      lSum := lSum + GetV(c2*lFrac,2,lV);
      lSum := lSum + GetV(c3*lFrac,3,lV);
      lSum := lSum + GetV(c4*lFrac,4,lV);
      lSum := lSum + GetV(c5*lFrac,5,lV);
      lSum := lSum + GetV(c6*lFrac,6,lV);
      //lSum := lSum + GetV(c6,6,lV);
      if lSum > 0 then begin

        (*if smoothFWHM < 0 then begin
           if MoreCSFThanSoft(lV) then
             PutV(lSum,3,lV)
          else
              PutV(lSum,5,lV);
        end else begin*)

            PutV(lSum*c1,1,lV);
          PutV(lSum*c2,2,lV);
          PutV(lSum*c3,3,lV);
          PutV(lSum*c4,4,lV);
          PutV(lSum*c5,5,lV);
          PutV(lSum*c6,6,lV);
        //end;
      end;
    end;
  end;
  WriteNII(changefileprefix(lTPM,'z'), lT);
  FreeNII(lM);
  FreeNII(lT);
end;


procedure nii_copyhdr (lOld,lNew: string; slices: integer);
var
  lNII:  TNIFTIimg;
  lF: file;
begin
 ReadNIIHdr(lOld, lNII);
 lNII.Hdr.dim[3] := slices;
 AssignFile(lF, lNew);
 Rewrite(lF, 1);
 BlockWrite(lF, lNII.Hdr, sizeof(TNIFTIhdr));
 CloseFile(lF);
end;

procedure nii_pastehdr8 (lHdrName,lImgName: string);
var
   lNIIh,lNIIi: TNIfTIimg;
  lV,lnV: integer;
begin
  if (not fileexists(lHdrName)) or (not fileexists (lImgName)) then exit;
  CreateNII(lNIIh);
  CreateNII(lNIIi);
  if not Read4Dbyte(lHdrName, lNIIh) then exit;
  if not Read4Dbyte(lImgName, lNIIi) then exit;
  lnV := lNIIh.hdr.dim[1]*lNIIh.hdr.dim[2]*lNIIh.hdr.dim[3]*lNIIh.hdr.dim[4];
  if lnV <> lNIIi.hdr.dim[1]*lNIIi.hdr.dim[2]*lNIIi.hdr.dim[3]*lNIIi.hdr.dim[4] then exit;
  for lV := 1 to lnV do
      lNIIh.i8^[lV] := lNIIi.i8^[lV];
  WriteNII(changefileprefix(lImgName,'c'), lNIIh);
  FreeNII(lNIIh);
  FreeNII(lNIIi);
end;

procedure nii_pastehdr (lHdrName,lImgName: string);
var
   lNIIh,lNIIi: TNIfTIimg;
  lV,lnV: integer;
begin
  if (not fileexists(lHdrName)) or (not fileexists (lImgName)) then exit;
  CreateNII(lNIIh);
  CreateNII(lNIIi);
  if not Read4DF32(lHdrName, lNIIh) then exit;
  if not Read4DF32(lImgName, lNIIi) then exit;
  lnV := lNIIh.hdr.dim[1]*lNIIh.hdr.dim[2]*lNIIh.hdr.dim[3]*lNIIh.hdr.dim[4];
  if lnV <> lNIIi.hdr.dim[1]*lNIIi.hdr.dim[2]*lNIIi.hdr.dim[3]*lNIIi.hdr.dim[4] then exit;
  for lV := 1 to lnV do
      lNIIh.f32^[lV] := lNIIi.f32^[lV];
  WriteNII(changefileprefix(lImgName,'c'), lNIIh);
  FreeNII(lNIIh);
  FreeNII(lNIIi);
end;

procedure nii_scale (lHdrName,lImgName: string; lScale: single);
var
   lNIIh,lNIIi: TNIfTIimg;
  lV,lnV: integer;
begin
  if (not fileexists(lHdrName)) or (not fileexists (lImgName)) then exit;
  CreateNII(lNIIh);
  CreateNII(lNIIi);
  if not Read4DF32(lHdrName, lNIIh) then exit;
  if not Read4DF32(lImgName, lNIIi) then exit;
  lnV := lNIIh.hdr.dim[1]*lNIIh.hdr.dim[2]*lNIIh.hdr.dim[3]*lNIIh.hdr.dim[4];
  if lnV <> lNIIi.hdr.dim[1]*lNIIi.hdr.dim[2]*lNIIi.hdr.dim[3]*lNIIi.hdr.dim[4] then exit;
  for lV := 1 to lnV do
      lNIIh.f32^[lV] := lNIIh.f32^[lV]+ (lScale*lNIIi.f32^[lV]);
  WriteNII(changefileprefix(lImgName,'c'), lNIIh);
  FreeNII(lNIIh);
  FreeNII(lNIIi);
end;
function nii_cropslicesx (lName: string; lCropSlices: integer; lNX: TNIfTIimg):boolean;
var
  lnVol,lnVox,lVox,lVol,lXY, lnVoxO,lVoli,lVolo,lO,lnVoxW: integer;
  lNIIin,lNIIout: TNIfTIimg;
begin
     result := false;
     if (not fileexists(lName)) or (lCropSlices = 0) then
      exit;
     CreateNII(lNIIin);
     CreateNII(lNIIout);
     if not Read4DF32(lName, lNIIin) then
        exit;
     for lXY := 0 to 7 do
        lNIIin.Hdr.pixdim[lXY] := lNX.Hdr.pixdim[lXY];

     lNIIin.Hdr.qform_code := lNX.Hdr.qform_code;
     lNIIin.Hdr.sform_code:= lNX.Hdr.sform_code;
    lNIIin.Hdr.quatern_b := lNX.Hdr.quatern_b ;
    lNIIin.Hdr.quatern_c := lNX.Hdr.quatern_c;
    lNIIin.Hdr.quatern_d := lNX.Hdr.quatern_d;
    lNIIin.Hdr.qoffset_x := lNX.Hdr.qoffset_x;
    lNIIin.Hdr.qoffset_y := lNX.Hdr.qoffset_y;
    lNIIin.Hdr.qoffset_z := lNX.Hdr.qoffset_z;
    for lXY := 0 to 3 do begin
       lNIIin.Hdr.srow_x[lXY] := lNX.Hdr.srow_x[lXY];
       lNIIin.Hdr.srow_y[lXY] := lNX.Hdr.srow_y[lXY];
       lNIIin.Hdr.srow_z[lXY] := lNX.Hdr.srow_z[lXY];
     end;

     lXY := lNIIin.hdr.dim[1]*lNIIin.hdr.dim[2];
     lnVox := lXY*lNIIin.hdr.dim[3];
     lnVol := lNIIin.hdr.dim[4];
     if (lnVol < 1) or (lnVox <1) or (lNIIin.hdr.dim[3] <= lCropSlices) then exit;
     lNIIout.Hdr := lNIIin.Hdr;

     lNIIout.Hdr.dim[3] := lNIIout.Hdr.dim[3]-lCropSlices;

     lNIIout.Hdr := lNX.Hdr;
     lNIIout.Hdr.bitpix:=8;
     lNIIout.Hdr.datatype:=kDT_UNSIGNED_CHAR;

       //fx( lNIIout.Hdr.srow_y[3],lNX.Hdr.srow_y[3]);
     lNIIout.HdrName := changefileprefix(lName,'c');//cropped
     lnVoxO := lNIIout.hdr.dim[1]*lNIIout.hdr.dim[2]*lNIIout.hdr.dim[3];
     if lCropSlices > 0 then begin
        lO := lCropSlices;
        lnVoxW := lnVoxO;
     end else begin
         lO := 0;
         lnVoxW := lnVox;
     end;
     //fx( lNIIout.Hdr.pixdim[3]);
     CreateZeroedFloat4DNII(lNIIout,lNIIout,lNIIout.HdrName,lnVol);
     //fx( lNIIout.Hdr.pixdim[3]);
     for lVol := 1 to lnVol do begin
         lVoli := (lnVox*(lVol-1))+ (lXY*lO);
         lVolo := (lnVoxO*(lVol-1));
         for lVox := 1 to lnVoxW do
           lNIIout.f32^[lVolo+lVox] := lNIIin.f32^[lVoli+lVox];
     end; //each vol
     WriteNII(lNIIout.HdrName,lNIIout);
     riteln('cropped '+lNIIout.HdrName);
     FreeNII(lNIIin);
     FreeNII(lNIIout);
     result := true;
end;

function nii_cropslices (lName: string; lCropSlices: integer):boolean;
var
  lnVol,lnVox,lVox,lVol,lXY, lnVoxO,lVoli,lVolo,lO,lnVoxW: integer;
  lNIIin,lNIIout: TNIfTIimg;
begin
     result := false;
     if (not fileexists(lName)) or (lCropSlices = 0) then
      exit;
     CreateNII(lNIIin);
     CreateNII(lNIIout);
     if not Read4DF32(lName, lNIIin) then
        exit;
     lXY := lNIIin.hdr.dim[1]*lNIIin.hdr.dim[2];
     lnVox := lXY*lNIIin.hdr.dim[3];
     lnVol := lNIIin.hdr.dim[4];
     if (lnVol < 1) or (lnVox <1) or (lNIIin.hdr.dim[3] <= lCropSlices) then exit;
     lNIIout.Hdr := lNIIin.Hdr;
     lNIIout.Hdr.dim[3] := lNIIout.Hdr.dim[3]-lCropSlices;


     lNIIout.HdrName := changefileprefix(lName,'c');//cropped
     lnVoxO := lNIIout.hdr.dim[1]*lNIIout.hdr.dim[2]*lNIIout.hdr.dim[3];
     if lCropSlices > 0 then begin
        lO := lCropSlices;
        lnVoxW := lnVoxO;
     end else begin
         lO := 0;
         lnVoxW := lnVox;
     end;
     CreateZeroedFloat4DNII(lNIIout,lNIIout,lNIIout.HdrName,lnVol);
     for lVol := 1 to lnVol do begin
         lVoli := (lnVox*(lVol-1))+ (lXY*lO);
         lVolo := (lnVoxO*(lVol-1));
         for lVox := 1 to lnVoxW do
           lNIIout.f32^[lVolo+lVox] := lNIIin.f32^[lVoli+lVox];
     end; //each vol
     WriteNII(lNIIout.HdrName,lNIIout);
     riteln('cropped '+lNIIout.HdrName);
     FreeNII(lNIIin);
     FreeNII(lNIIout);
     result := true;
end;

function nii_4Dunity (var lNII: TNIfTIimg; val: single):boolean;
var
   lnVol,lnVox,lVox,lVol: integer;
   lSum: double;
begin

  lnVox := lNII.hdr.dim[1]*lNII.hdr.dim[2]*lNII.hdr.dim[3];
  lnVol := lNII.hdr.dim[4];
  if (lnVol < 1) or (lnVox <1) or (lnVol < 2) then exit;
  for lVox := 1 to lnVox do begin
    lSum := 0;
    for lVol := 0 to (lnVol-1) do
          lSum := lSum + lNII.f32^[lVox+ (lVol*lnVox)];
    lSum := lSum/val;
    if (lSum <> 0) {and (lSum <> 1)} then
      for lVol := 0 to (lnVol-1) do
            lNII.f32^[lVox+ (lVol*lnVox)] := lNII.f32^[lVox+ (lVol*lnVox)]/lSum;

  end;

end;

function nii_4Dunity (lName: string; val: single):boolean;
var
  lNII: TNIfTIimg;
begin
     result := false;
     if (not fileexists(lName))  then exit;
     CreateNII(lNII);
     if not Read4DF32(lName, lNII) then
        exit;
     result := nii_4Dunity(lNII,val);
     lNII.HdrName := changefileprefix(lName,'u');//unity
     WriteNII(lNII.HdrName,lNII);
     riteln('unity '+lNII.HdrName);
     FreeNII(lNII);

end;


function nii_mirror (var lNII:TNIfTIimg):boolean; overload;
var
  lNan: boolean;
  lL,lR: single;
  lV,lnVox,lVol,lnVol,lVolo,
  lZo,lYo,lXY,
    lX,lY,lZ,lXdiv2: integer;
  lImg: singlep;
begin
  lNaN := false;
  result := false;
  lXdiv2 := lNII.hdr.dim[1] div 2;
  lXY := lNII.hdr.dim[1]*lNII.hdr.dim[2];
  lnVox := lXY*lNII.hdr.dim[3];
  lnVol := lNII.hdr.dim[4];
  if lnVol < 1 then lnVol := 1;
  if (lXdiv2 < 1) or (lnVox < 1) or (lNII.hdr.datatype <> kDT_FLOAT) or (lNII.hdr.dim[1]<3) or (lNII.hdr.dim[2]<3) or  (lNII.hdr.dim[3]<3)  then begin
        riteln('nii_mirror error: only available for 3D 32-bit single precision data.');
        exit;
  end;
  for lV:= 1 to (lnVol*lnVox) do
  	  if specialsingle(lNII.f32^[lV]) then
      	 lNaN := true;
  if lNaN then
  	 riteln('nii_mirror warning: some voxels special values (e.g. infinity, not-a-number). Voxels where either left or right source is special will appear special in output');
  getmem(lImg,lnVox * sizeof(single));
  for lVol := 1 to lnVol do begin
    lVolo := (lVol-1)*lnVox;//volume offset
    for lV := 1 to lnVox do
     	 lImg^[lV] := 0;
     //mirror in X direction
     for lZ := 1 to (lNII.hdr.dim[3]) do begin
         lZo := (lZ-1)*lXY;

         for lY := 1 to (lNII.hdr.dim[2]) do  begin
             lYo := (lY-1)*lNII.hdr.dim[1] ;
             for lX := 1 to lXdiv2 do begin
                 lL :=  lNII.f32^[lZo+lYo+lX+lVolo];
                 lR :=  lNII.f32^[lZo+lYo+lNII.hdr.dim[1]-lX+1+lVolo];
                 if specialsingle(lL) or specialsingle(lR) then
               	 	lImg^[lZo+lYo+lX] :=  kNanS
                 else
                 	lImg^[lZo+lYo+lX] :=  lL-lR;
             end;
         end;
     end;
     for lV := 1 to lnVox do
     	 lNII.f32^[lV+lVolo] := lImg^[lV];
end;//for each vol
     freemem(lImg);
     result := true;
end;//niiMirror

function nii_mirror (lImages: Tstrings):boolean; overload;
var
  lNImg,lI: integer;
  lNIIImg: TNIfTIimg;
begin
     result := false;
     lNImg := lImages.Count;
     if lNImg < 1 then begin
        riteln('nii_mirror error: no files');
      exit;
     end;
     CreateNII(lNIIImg);
     for lI := 1 to lNImg do begin
         if not Read4DF32(lImages[lI-1], lNIIImg) then begin
         //if not Read4Dbyte(lImages[lI-1], lNIIImg) then begin
             showmsg('Error loading '+lImages[lI-1]);
         end else begin
             lNIIImg.HdrName := changefileprefix(lImages[lI-1],'m');//mirrored
             result := nii_mirror(lNIIImg);
             //result := nii_flipAP(lNIIImg);
             //nii_shiftarb(lNIIImg, 2*lNIIImg.Hdr.dim[1]*lNIIImg.hdr.dim[2]);
             if result then begin
             	WriteNII(lNIIImg.HdrName,lNIIImg);
          	 	riteln('mirror '+lNIIImg.HdrName);
             end;

         end;
     end;
     FreeNII(lNIIImg);
end;


{procedure RemoveMaskNoise (lNII: TNIFTIimg);
//if a voxel is surrounded by NaNs, set that voxel to a NaN
// The idea is to remove spckles from the mask
var
  lX,lY,lZ,lT,lXi,lXYi,lXYZi,i: integer;
begin
     if lNII.hdr.datatype <> kDT_FLOAT then begin
         riteln('remove mask noise error: only supports float datatype');
         exit;
     end;
     if (lNII.hdr.dim[1]<3) or (lNII.hdr.dim[2]<3) or (lNII.hdr.dim[3]<3) then
       	exit;
     lXi := lNII.hdr.dim[1];
     lXYi := lXi * lNII.hdr.dim[2];
     lXYZi := lXYi * lNII.hdr.dim[3];

     for lT := 1 to lNII.NonspatialDimensionsLoaded do begin
	 	 for lZ := 2 to ( lNII.hdr.dim[3]-1) do
  	 	 	 for lY := 2 to ( lNII.hdr.dim[2]-1) do
	 	 	 	 for lX := 2 to ( lNII.hdr.dim[1]-1) do begin
                 	 i := ((lT-1)*lXYZi)+ ((lZ-1)*lXYi)+((lY-1)*lXi)+lX;
                     if (lNII.f32^[i-1]=kNaNs) and (lNII.f32^[i+1]=kNaNs) and
                     	(lNII.f32^[i-lXi]=kNaNs) and (lNII.f32^[i+lXi]=kNaNs) and
                        (lNII.f32^[i-lXYi]=kNaNs) and (lNII.f32^[i+lXYi]=kNaNs) then
                     			  lNII.f32^[i] :=kNaNs;
                 end;

     end;
end;}


procedure RemoveMaskNoise (lNII: TNIFTIimg);
//if a voxel is surrounded by NaNs, set that voxel to a NaN
// The idea is to remove spckles from the mask
var
  lX,lY,lZ,lT,lXi,lXYi,lXYZi,i: integer;
begin
     if lNII.hdr.datatype <> kDT_FLOAT then begin
         riteln('remove mask noise error: only supports float datatype');
         exit;
     end;
     if (lNII.hdr.dim[1]<3) or (lNII.hdr.dim[2]<3) or (lNII.hdr.dim[3]<3) then
       	exit;
     lXi := lNII.hdr.dim[1];
     lXYi := lXi * lNII.hdr.dim[2];
     lXYZi := lXYi * lNII.hdr.dim[3];

     for lT := 1 to lNII.NonspatialDimensionsLoaded do begin
	 	 for lZ := 2 to ( lNII.hdr.dim[3]-1) do
  	 	 	 for lY := 2 to ( lNII.hdr.dim[2]-1) do
	 	 	 	 for lX := 2 to ( lNII.hdr.dim[1]-1) do begin
                 	 i := ((lT-1)*lXYZi)+ ((lZ-1)*lXYi)+((lY-1)*lXi)+lX;
                     if specialsingle(lNII.f32^[i-1]) and specialsingle(lNII.f32^[i+1]) and
                     	specialsingle(lNII.f32^[i-lXi]) and specialsingle(lNII.f32^[i+lXi]) and
                        specialsingle(lNII.f32^[i-lXYi]) and specialsingle(lNII.f32^[i+lXYi]) then
                     			  lNII.f32^[i] :=kNaNs;
                 end;

     end;
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

function NIImin (lNII: TNIFTIimg): double; overload;
//lpct = 10 returns mean value of top 10% of voxels
//lpct = 0 returns absolute peak
var
  lnV,lV: integer;
begin
     result := 0;
     (*if lNII.hdr.datatype <> kDT_FLOAT then begin
         riteln('NIImin error: only supports float datatype');
         exit;
     end;*)
  	 lnV := lNII.hdr.dim[1]*lNII.hdr.dim[2]*lNII.hdr.dim[3]* lNII.NonspatialDimensionsLoaded;
     if lnV < 1 then
     	exit;
     if (lNII.hdr.datatype = kDT_FLOAT) and (lNII.hdr.bitpix = 32) then begin
        result := kInfs;
	for lV := 1 to lnV do
            if not specialsingle(lNII.f32^[lV]) and  (lNII.f32^[lV] < result) then
               result :=  lNII.f32^[lV];
     end else if (lNII.hdr.datatype = kDT_SIGNED_INT) and (lNII.hdr.bitpix = 32) then begin
        result := lNII.i32^[1];
	for lV := 1 to lnV do
	    if lNII.i32^[lV] < result then
               result :=  lNII.i32^[lV];
     end else if (lNII.hdr.bitpix = 16) then begin
        result := lNII.i16^[1];
	for lV := 1 to lnV do
	    if lNII.i16^[lV] < result then
               result :=  lNII.i16^[lV];
     end else if (lNII.hdr.bitpix = 8)  then begin
        result := lNII.i8^[1];
	for lV := 1 to lnV do
	    if lNII.i8^[lV] < result then
               result :=  lNII.i8^[lV];
     end else
         showmsg('Unsupported datatype.');

end;

function NIImax (lNII: TNIFTIimg): double; overload;
//returns absolute peak
var
  lnV,lV: integer;
begin
  	 result := 0;
     if lNII.hdr.datatype <> kDT_FLOAT then begin
         riteln('NIImax error: only supports float datatype');
         exit;
     end;
  	 lnV := lNII.hdr.dim[1]*lNII.hdr.dim[2]*lNII.hdr.dim[3]* lNII.NonspatialDimensionsLoaded;
     if lnV < 1 then
     	exit;
     result := -kInfd;
	 for lV := 1 to lnV do
	 	 if (not specialsingle(lNII.f32^[lV])) and (lNII.f32^[lV] > result) then
           	result :=  lNII.f32^[lV];
end;

function NIImax (lNII: TNIFTIimg; lVox: integer): double; overload;
//frac = 0.1 returns minimum value of top 10% of voxels  (threshold for top 10%)
//frac = 0 returns absolute peak
var
  lnV,lV,lVoxi: integer;
  lSort: singleP;
begin
     if lVox <= 1 then begin
       result := NIImax(lNII);
       exit;//single max value
     end;
     if lNII.hdr.datatype <> kDT_FLOAT then begin
         riteln('NIImax error: only supports float datatype');
         exit;
     end;
     lnV := lNII.hdr.dim[1]*lNII.hdr.dim[2]*lNII.hdr.dim[3]* lNII.NonspatialDimensionsLoaded;
     if lnV < 1 then
       	exit;
     if lVox > lnV then
       	lVoxi := lnV
     else
	 	 lVoxi := lVox;
     getmem(lSort,lnV*sizeof(single));
     for lV := 1 to lnV do
     	 lSort^[lV] := lNII.f32^[lV];
     qsort(1,lnV,lSort);
     //result := lSort^[lFraci]; //low threshold
     result := lSort^[lnV-lVoxi+1]; //high threshold
     freemem(lSort);
end;

procedure InspectVols(Filenames: TStrings);
var
  i: integer;
  vIn: TNIFTIimg;
  lOpts: TReadOptions;
begin
  if Filenames.Count < 1 then exit;
  createNII(vIn);
  lOpts := DefaultReadOpts ( TNativex, -1);
  for i := 0 to (Filenames.Count -1) do begin
    if not ReadNII(Filenames[i],vIn,lOpts) then
      exit;
    Riteln(Filenames[i]+ 'Dimensions X*Y*Z*T = '
      + inttostr(vIn.Hdr.Dim[1])
      +'*'+ inttostr(vIn.Hdr.Dim[2])
      +'*'+ inttostr(vIn.Hdr.Dim[3])
      +'*'+ inttostr(vIn.NonspatialDimensionsAvailable) );
    Riteln('BitPix '+inttostr(vIn.Hdr.bitpix) );
    if not IsNIFTIMagic(vIn.Hdr) then
         Riteln(' Format: Anlayze 7.5')
    else begin
         Riteln(' NIFTImatrix = [');
         Riteln('    '+realtostr(vIN.Hdr.srow_x[0],3)+' '+ realtostr(vIN.Hdr.srow_x[1],3)+' '+ realtostr(vIN.Hdr.srow_x[2],3)+' '+ realtostr(vIN.Hdr.srow_x[3],3)+';');
         Riteln('    '+realtostr(vIN.Hdr.srow_y[0],3)+' '+ realtostr(vIN.Hdr.srow_y[1],3)+' '+ realtostr(vIN.Hdr.srow_y[2],3)+' '+ realtostr(vIN.Hdr.srow_y[3],3)+';');
         Riteln('    '+realtostr(vIN.Hdr.srow_z[0],3)+' '+ realtostr(vIN.Hdr.srow_z[1],3)+' '+ realtostr(vIN.Hdr.srow_z[2],3)+' '+ realtostr(vIN.Hdr.srow_z[3],3)+';');
         Riteln('  ]');
    end;

  end;
  FreeNII(vIn);//not required if we only read the header....
end;

function AddVols3D( Filenames: TStrings; lOutname: string): boolean;
var
  i,v: integer;
  vIn,vOut: TNIFTIimg;
  lOpts: TReadOptions;
begin
  result := false;
  createNII(vIn);
  createNII(vOut);
  if Filenames.Count < 1 then begin
    Riteln('At least one image required for AddVols3D function');
    exit;
  end;
  lOpts := DefaultReadOpts ( TFloatx, 0);

  for i := 0 to (Filenames.Count -1) do begin
    if not ReadNII(Filenames[i],vIn,lOpts) then
      exit;
    if i = 0 then
      CreateZeroedFloat3DNII(vIn,vOut,'');
    if not SameXYZDims(vIn,vOut) then
      exit;
    if vIn.NonspatialDimensionsLoaded > 1 then
      Riteln('AddVols3D warning: only first volume of 4D input will be used.');
    for v := 1 to vOut.VoxelsLoaded do
      vOut.f32^[v] := vOut.f32^[v]+vIn.f32^[v];
  end;
  FreeNII(vIn);
  for v := 1 to vOut.VoxelsLoaded do
    vOut.f32^[v] := vOut.f32^[v]/Filenames.Count; //mean value
  WriteNII(lOutname,vOut);
  FreeNii(vOut);
  Riteln('Output '+lOutname);
  result := true;
end;
end.
 