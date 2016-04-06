unit nii_extract;
{$mode objfpc}{$H+}
interface

uses Classes, define_types, nii_core, sysutils, dialogsx, morphological, arrayu, nii_math, math{minDouble};

function nii_extract_object (lImages: TStringList; lModName, lOutname: string; lOtsuLevels,lMode: integer; lDilateVox,lClusterMM, lExplicitThreshold: single):boolean; overload;
function nii_extract_object (lImage: string; lOtsuLevels,lMode: integer; lDilateVox,lClusterMM: single):boolean; overload;

implementation

function nii_extract_object (lImage: string; lOtsuLevels,lMode: integer; lDilateVox,lClusterMM: single):boolean; overload;
var
   lImages: TStringlist;
begin
     lImages := TStringList.Create;
     lImages.add(lImage);
     result := nii_extract_object(lImages,'','',lOtsuLevels,lMode, lDilateVox,lClusterMM,0);
     lImages.Free;
end;

function nii_extract (var lNII:TNIfTIimg;  lOtsuLevels,lMode: integer; lDilateVox, lClusterMM: single):boolean; overload;
var
   lCubicMM: single;
   lClusterVox: integer;
begin
     lCubicMM := abs(lNII.hdr.pixdim[1]*lNII.hdr.pixdim[2]*lNII.hdr.pixdim[3]);
     if (lClusterMM > 0) and (lCubicMM > 0) then
        lClusterVox := round(lClusterMM/lCubicMM)
     else
         lClusterVox := 0;
     MaskBackground  (lNII.i8, lNII.hdr.Dim[1],lNII.hdr.Dim[2],lNII.hdr.Dim[3],lClusterVox,lOtsuLevels,lMode, lDilateVox);
     result := true;
end;//nii_extract

(*function Random_Normal: Extended;
//http://www.netlib.org/random/amrandom.pas
{ Adapted from the following Fortran 77 code
	 ALGORITHM 712, COLLECTED ALGORITHMS FROM ACM.
	 THIS WORK PUBLISHED IN TRANSACTIONS ON MATHEMATICAL SOFTWARE,
	 VOL. 18, NO. 4, DECEMBER, 1992, PP. 434-435.
  The function random_normal() returns a normally distributed pseudo-random
  number with zero mean and unit variance
  The algorithm uses the ratio of uniforms method of A.J. Kinderman
  and J.F. Monahan augmented with quadratic bounding curves. }
const
	VSmall = MinDouble;
const
	s = 0.449871;
	t = -0.386595;
	a = 0.19600;
	b = 0.25472;
	r1 = 0.27597;
	r2 = 0.27846;
var
	u, v, x, y, q: Extended;
	Done: Boolean;
begin
	Done := False;
	repeat
		u := Random;
		v := Random;
		v := 1.7156 * (v - 0.5);
		// Evaluate the quadratic form
		x := u - s;
		Y := abs (v) - t;
		q := Sqr (x) + y * (a * y - b * x);
		// Accept P if inside inner ellipse
		if (q < r1) then
			Done := True
		else if (q <= r2) and (Sqr (v) < -4.0 * Ln (u) * Sqr (u)) then
			Done := True;
	until Done;
	// Return ratio of P's coordinates as the normal deviate
	if u < VSmall then
		result := Random_Normal
        else
            Result := v / u;
end; *)

//IQR: compute inter-quartile range and then fill data with gaussian distribution that has the same IQR
//     this removes outliers
//50% of the distribution lies within 0.67448 standard deviations of the mean
//http://www.regentsprep.org/Regents/math/algtrig/ATS2/NormalLesson.htm
//round((Random_Normal * IQR * 0.67448)+Q2);

(*procedure IQRi16(lMask8: bytep; var lImg: smallintP; lnVox: integer );
var
   n,t: integer;
   Q1, Q2, Q3, IQR: single;
   ra: array of single;
begin
     if lnVox < 4 then exit;
     //count masked items...
     t := 0;
     for n := 1 to lnvox do
         if lMask8^[n] = 0 then
            inc(t);
     if t = 0 then
        exit;
     //compute quartiles...
     setlength(ra,t);
     t := 0;
     for n := 1 to lnvox do
         if lMask8^[n] = 0 then begin
             ra[t] := lImg^[n];
             inc(t);
         end;
     Quartiles32(ra, Q1, Q2, Q3);
     ra := nil;
     //add random noise....
     IQR := Q3-Q1;
     for n := 1 to lnvox do
         if (lMask8^[n] = 0) and (lImg^[n] > Q2) then begin
             lImg^[n] := round((abs(Random_Normal) * IQR * 0.67448)+Q2);
         end;
end;*)

(*procedure IQRi32(lMask8: bytep; var lImg: LongIntP; lnVox: integer);
var
   n,t: integer;
   Q1, Q2, Q3, IQR: single;
   ra: array of single;
begin
     if lnVox < 4 then exit;
     //count masked items...
     t := 0;
     for n := 1 to lnvox do
         if lMask8^[n] = 0 then
            inc(t);
     if t = 0 then
        exit;
     //compute quartiles...
     setlength(ra,t);
     t := 0;
     for n := 1 to lnvox do
         if lMask8^[n] = 0 then begin
             ra[t] := lImg^[n];
             inc(t);
         end;
     Quartiles32(ra, Q1, Q2, Q3);
     ra := nil;
     //add random noise....
     IQR := Q3-Q1;
     for n := 1 to lnvox do
         if lMask8^[n] = 0 then begin
             lImg^[n] := round((Random_Normal * IQR * 0.67448)+Q2);
         end;
end;

procedure IQRf32(lMask8: bytep; var lImg: SingleP; lnVox: integer );
var
   n,t: integer;
   Q1, Q2, Q3, IQR: single;
   ra: array of single;
begin
     if lnVox < 4 then exit;
     //count masked items...
     t := 0;
     for n := 1 to lnvox do
         if lMask8^[n] = 0 then
            inc(t);
     if t = 0 then
        exit;
     //compute quartiles...
     setlength(ra,t);
     t := 0;
     for n := 1 to lnvox do
         if lMask8^[n] = 0 then begin
             ra[t] := lImg^[n];
             inc(t);
         end;
     Quartiles32(ra, Q1, Q2, Q3);
     ra := nil;
     //add random noise....
     IQR := Q3-Q1;
     for n := 1 to lnvox do
         if lMask8^[n] = 0 then begin
             lImg^[n] := ((Random_Normal * IQR * 0.67448)+Q2);
         end;
end;*)

(*procedure NoiseBottom ( lNII :TNIfTIimg);
var
   n: integer;
begin
     for n := 1 to (lNII.hdr.Dim[1]*lNII.hdr.Dim[2]) do
         lNII.i16^[n] := random (8000);
end;
procedure Noise ( lNII :TNIfTIimg);
var
   n,x,y,z: integer;
begin
     n := 0;
     for z := 1 to (lNII.hdr.Dim[3]) do
         for y := 1 to (lNII.hdr.Dim[2]) do
             for x := 1 to (lNII.hdr.Dim[1]) do begin
                 inc(n);
                 if y = 1 then
                    lNII.i16^[n] := random (8000);
             end;

end;
*)

(*function Extracti16(lNII,lNII8:TNIfTIimg; lOtsuLevels,lMode: integer; lDilateVox: single; lNoise: boolean): boolean;
var
   min,nvox,n: integer;
begin
     result := false;
     nvox := lNII.hdr.Dim[1]*lNII.hdr.Dim[2]*lNII.hdr.Dim[3];
     if (nVox < 1) or (not nii_extract(lNII8,lOtsuLevels,lMode, lDilateVox) ) then exit;
    if lNoise  then
        IQRi16(lNII8.i8, lNII.i16, nVox)
     else begin
          min := lNII.i16^[1];
          for n := 1 to nvox do
              if lNII.i16^[n] < min then
                 min := lNII.i16^[n];
          for n := 1 to nvox do
              if lNII8.i8^[n] = 0 then
                 lNII.i16^[n] := min;
     end;
result := true;
end;

function Extracti32(lNII,lNII8:TNIfTIimg; lOtsuLevels,lMode: integer; lDilateVox: single; lNoise: boolean): boolean;
var
   min,nvox,n: integer;
begin
     result := false;
     nvox := lNII.hdr.Dim[1]*lNII.hdr.Dim[2]*lNII.hdr.Dim[3];
     if (nVox < 1) or (not nii_extract(lNII8,lOtsuLevels,lMode, lDilateVox) ) then exit;
     if lNoise  then
        IQRi32(lNII8.i8,  lNII.i32, nVox)
     else begin
          min := lNII.i32^[1];
          for n := 1 to nvox do
              if lNII.i32^[n] < min then
          min := lNII.i32^[n];
          for n := 1 to nvox do
              if lNII8.i8^[n] = 0 then
                 lNII.i32^[n] := min;
     end;
     result := true;
end;

function Extractf32(lNII,lNII8:TNIfTIimg; lOtsuLevels,lMode: integer; lDilateVox: single; lNoise: boolean): boolean;
var
   min: single;
   nvox,n: integer;
begin
     result := false;
     nvox := lNII.hdr.Dim[1]*lNII.hdr.Dim[2]*lNII.hdr.Dim[3];
     if (nVox < 1) or (not nii_extract(lNII8,lOtsuLevels, lMode,lDilateVox) ) then exit;

     if lNoise then
        IQRf32(lNII8.i8,  lNII.f32, nVox)
     else begin
          min := lNII.f32^[1];
          for n := 1 to nvox do
              if lNII.f32^[n] < min then
                 min := lNII.f32^[n];
          for n := 1 to nvox do
              if lNII8.i8^[n] = 0 then
                 lNII.f32^[n] := min;
     end;
     result := true;
end; *)

function AddVols3DByte( Filenames: TStrings; var vOut: TNIFTIimg; lExplicitThreshold: single): boolean;
label
  666;
var
  i,v: integer;
  str: string;
  lmin: single;
  vIn: TNIFTIimg;
begin
  result := false;
  if Filenames.Count < 1 then begin
       Riteln('At least one image required for AddVols3DByte function');
       exit;
  end;
  createNII(vIn);
  str := '';
  for i := 0 to (Filenames.Count -1) do begin
    if not Read3DF32(Filenames[i],vIn) then
      exit;
    str := str + Filenames[i] + ' ';
    if i = 0 then
      CreateZeroedFloat3DNII(vIn,vOut,'');
    if not SameXYZDims(vIn,vOut) then
      goto 666;
    if vIn.NonspatialDimensionsLoaded > 1 then
      Riteln('AddVols3D warning: only first volume of 4D input will be used.');
    for v := 1 to vOut.VoxelsLoaded do
      vOut.f32^[v] := vOut.f32^[v]+vIn.f32^[v];
  end;
  riteln('Extracting '+str);
  for v := 1 to vOut.VoxelsLoaded do
    vOut.f32^[v] := vOut.f32^[v]/Filenames.Count; //mean value
  if lExplicitThreshold > 0 then begin
     lmin := Infinity;
     Riteln(' masking with an explicit threshold of '+floattostr(lExplicitThreshold));
     for v := 1 to vOut.VoxelsLoaded do
         if not specialsingle(vOut.f32^[v]) then
            if vOut.f32^[v] < lmin then
               lmin := vOut.f32^[v]; //new min
     for v := 1 to vOut.VoxelsLoaded do
         if not specialsingle(vOut.f32^[v]) then
            if vOut.f32^[v] < lExplicitThreshold then
               vOut.f32^[v] := lmin;
  end;

  f32Toi8(vOut);
  result := true;
666:
  FreeNII(vIn);
end;

function MaskVol( lImage,lOutname: String; lMask8: TNIFTIimg): boolean;
label
  666;
var
  v, min: integer;
  smin: single;
  vIn: TNIFTIimg;
  lOpts: TReadOptions;
begin
  result := false;
  lOpts := DefaultReadOpts ( TNativex, 1);
  lOpts.NaN2Zero:=false;
  CreateNII(vIn);
  if (not ReadNII(lImage, vIn,lOpts)) then begin
        showmsg('Error loading '+lImage);
        goto 666;
  end;
  if (vIn.VoxelsLoaded <> lMask8.VoxelsLoaded) then begin
        showmsg('Error image and mask have different dimensions or volumes '+lImage);
        goto 666;
  end;
  smin := NIImin(vIn);
  min := 0;
  if (vIn.hdr.datatype = kDT_FLOAT) and (vIn.hdr.bitpix = 32) then begin
     for v := 1 to lMask8.VoxelsLoaded do
         if lMask8.i8^[v] = 0 then
            vIn.f32^[v] := smin;
  end else begin //integer
      min := round(smin); //only round if not f32: values larger than integer range will trigger exception
  if (vIn.hdr.bitpix = 16) then begin
     for v := 1 to lMask8.VoxelsLoaded do
         if lMask8.i8^[v] = 0 then
            vIn.i16^[v] := min;
  end else if (vIn.hdr.datatype = kDT_SIGNED_INT) and (vIn.hdr.bitpix = 32) then begin
     for v := 1 to lMask8.VoxelsLoaded do
         if lMask8.i8^[v] = 0 then
         vIn.i32^[v] := min;
  end else if (vIn.hdr.bitpix = 8)  then begin
    for v := 1 to lMask8.VoxelsLoaded do
        if lMask8.i8^[v] = 0 then
           vIn.i8^[v] := min;
  end else
      showmsg('Unsupported datatype.');

  end;
  if lOutname = '' then
     vIn.HdrName := changefileprefix(lImage,'e')
  else
      vIn.HdrName:= lOutname;
  WriteNII(vIn.HdrName,vIn);
  result := true;
666:
  FreeNII(vIn);
end;

(*function MaskVols( Filenames: TStrings; lMask8: TNIFTIimg): boolean;
var
  i: integer;
begin
  result := false;
  if Filenames.Count < 1 then
       exit;
  for i := 0 to (Filenames.Count-1) do
      result := MaskVol(Filenames[i],lMask8);
end;   *)


function nii_extract_object (lImages: TStringList; lModName,lOutname: string; lOtsuLevels,lMode: integer; lDilateVox,lClusterMM, lExplicitThreshold: single):boolean; overload;
label
  666;
var
   lMask: TNIfTIimg;
begin
     result := false;
     if lImages.count < 1 then exit;
     CreateNII(lMask);
     if not AddVols3DByte(lImages,lMask, lExplicitThreshold) then begin
        showmsg('Extract unable to load '+lImages[0]);
        goto 666;
     end;
     result := nii_extract(lMask,lOtsuLevels,lMode, lDilateVox,lClusterMM);
     if not result then goto 666;
     if (lModName <> '') and  (fileexists(lModName)) then
        MaskVol(lModName,lOutname,lMask)
     else
         MaskVol(lImages[0],lOutname,lMask);
666:
     FreeNII(lMask);
end;

end.

