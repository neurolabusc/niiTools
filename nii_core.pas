unit nii_core;

interface

{$H+}
//{ $D-,L-,O+,Q-,R-,Y-,S-}
{$IFDEF FPC}{$mode objfpc}{$H+} {$DEFINE GZIP} {$ENDIF}
{$include isgui.inc}
uses
  {$IFDEF GZIP}zstream, {$ENDIF}define_types, SysUtils, Math, Classes, dialogsx;
const //slice order
  kAutoDetect = -1;
  kSimultaneous = 0;
  kAscending = 1;
  kAscendingInterleavedPhilGE = 2;
  kDescending = 3;
  kDescendingInterleavedPhilGE = 4;
  kAscendingInterleavedSiemens = 5;
  kDescendingInterleavedSiemens = 6;
(* these are the possible slice_orders http://nifti.nimh.nih.gov/pub/dist/src/niftilib/nifti1.h
kNIFTI_SLICE_UNKNOWN =  0; %AUTO DETECT
kNIFTI_SLICE_SEQ_INC = 1; %1,2,3,4
kNIFTI_SLICE_SEQ_DEC = 2; %4,3,2,1
kNIFTI_SLICE_ALT_INC = 3; %1,3,2,4 Siemens: interleaved with odd number of slices, interleaved for other vendors
kNIFTI_SLICE_ALT_DEC = 4; %4,2,3,1 descending interleaved
kNIFTI_SLICE_ALT_INC2 = 5; %2,4,1,3 Siemens interleaved with even number of slices
kNIFTI_SLICE_ALT_DEC2 = 6; %3,1,4,2 Siemens interleaved descending with even number of slices    *)


type
  TNIFTIhdr = packed record //NIfTI header structure
    HdrSz: longint; //MUST BE 348
    Data_Type: array [1..10] of ansichar; //unused
    db_name: array [1..18] of ansichar; //unused
    extents: longint; //unused
    session_error: smallint; //unused                  `
    regular: ansichar; ////unused: in Analyze 7.5 this must be 114
    dim_info: byte; //MRI slice order
    dim: array[0..7] of smallint; //Data array dimensions
    intent_p1, intent_p2, intent_p3: single;
    intent_code: smallint;
    datatype: smallint;
    bitpix: smallint;
    slice_start: smallint;
    pixdim: array[0..7] of single;
    vox_offset: single;
    scl_slope: single;//scaling slope
    scl_inter: single;//scaling intercept
    slice_end: smallint;
    slice_code: byte; //e.g. ascending
    xyzt_units: byte; //e.g. mm and sec
    cal_max, cal_min: single; //unused
    slice_duration: single; //time for one slice
    toffset: single; //time axis to shift
    glmax, glmin: longint; //UNUSED
    descrip: array[1..80] of ansichar;
    aux_file: array[1..24] of ansichar;
    qform_code, sform_code: smallint;
    quatern_b, quatern_c, quatern_d,
    qoffset_x, qoffset_y, qoffset_z: single;
    srow_x: array[0..3] of single;
    srow_y: array[0..3] of single;
    srow_z: array[0..3] of single;
    intent_name: array[1..16] of ansichar;
    magic: longint;
  end; //TNIFTIhdr Header Structure

  TNIFTIimg = record
    HdrName: string;
    Hdr: TNIFTIhdr;
    VoxelsLoaded: integer;
    //umin,umax: double;//unscaled min..max not scaled with hdr.intercept or hdr.slope
    raw8, i8: bytep;
    f32: singlep;
    i32: longintP;
    i16: SmallIntP;
    NonspatialDimensionsAvailable, NonspatialDimensionsLoaded: integer;
    //number of volumes available, number loaded
  end;
  TFormat = (TNativex, TFloatx, TBytex, TBinaryBytex);

  TReadOptions = record
    ReadVol: integer; //-1 to only read the header, 0 to read all volumes, else read nth 3d volume only
    NaN2Zero: boolean;
    ForceDatatype: TFormat;
  end;



const
  kNIIImgOffset = 352;
  //DataTypes
  kDT_BINARY = 1;     // binary (1 bit/voxel)
  kDT_UNSIGNED_CHAR = 2;     // unsigned char (8 bits/voxel)
  kDT_SIGNED_SHORT = 4;     // signed short (16 bits/voxel)
  kDT_SIGNED_INT = 8;     // signed int (32 bits/voxel)
  kDT_FLOAT = 16;     // float (32 bits/voxel)
  kDT_COMPLEX = 32;     // complex (64 bits/voxel)
  kDT_DOUBLE = 64;     // double (64 bits/voxel)
  kDT_RGB = 128;     // RGB triple (24 bits/voxel)
  kDT_INT8 = 256;     // signed char (8 bits)
  kDT_UINT16 = 512;     // unsigned short (16 bits)
  kDT_UINT32 = 768;     // unsigned int (32 bits)
  kDT_INT64 = 1024;     // long long (64 bits)
  kDT_UINT64 = 1280;     // unsigned long long (64 bits)
  kDT_FLOAT128 = 1536;     // long double (128 bits)
  kDT_COMPLEX128 = 1792;     // double pair (128 bits)
  kDT_COMPLEX256 = 2048;     // long double pair (256 bits)
  //   slice_code values
  kNIFTI_SLICE_SEQ_UNKNOWN = 0;
  kNIFTI_SLICE_SEQ_INC = 1;
  kNIFTI_SLICE_SEQ_DEC = 2;
  kNIFTI_SLICE_ALT_INC = 3;
  kNIFTI_SLICE_ALT_DEC = 4;
  //xyzt_units values: note 3bit space and 3bit time packed into single byte
  kNIFTI_UNITS_UNKNOWN = 0;
  kNIFTI_UNITS_METER = 1;
  kNIFTI_UNITS_MM = 2;
  kNIFTI_UNITS_MICRON = 3;
  kNIFTI_UNITS_SEC = 8;
  kNIFTI_UNITS_MSEC = 16;
  kNIFTI_UNITS_USEC = 24;
  kNIFTI_UNITS_HZ = 32;
  kNIFTI_UNITS_PPM = 40;
  //qform_code, sform_code values
  kNIFTI_XFORM_UNKNOWN = 0;
  kNIFTI_XFORM_SCANNER_ANAT = 1;//Scanner-based anatomical coordinates
  kNIFTI_XFORM_ALIGNED_ANAT = 2;
  //Coordinates aligned to another file e.g. EPI coregistered to T1
  kNIFTI_XFORM_TALAIRACH = 3; //Talairach-Tournoux Atlas; (0,0,0)=AC, etc.
  kNIFTI_XFORM_MNI_152 = 4; //MNI 152 normalized coordinates
  //Magic values
  kNIFTI_MAGIC_SEPARATE_HDR = $0031696E;
  kNIFTI_MAGIC_EMBEDDED_HDR = $00312B6E;
  //kNIFTI_MAGIC_DCM = $0044434D;
  //byte-swapped magic values
  kswapNIFTI_MAGIC_SEPARATE_HDR = $6E693100;
  kswapNIFTI_MAGIC_EMBEDDED_HDR = $6E2B3100;
  //Statistics Intention
  kNIFTI_INTENT_NONE = 0;
  kNIFTI_INTENT_CORREL = 2;
  kNIFTI_INTENT_TTEST = 3;
  kNIFTI_INTENT_FTEST = 4;
  kNIFTI_INTENT_ZSCORE = 5;
  kNIFTI_INTENT_CHISQ = 6;
  kNIFTI_INTENT_BETA = 7;
  kNIFTI_INTENT_BINOM = 8;
  kNIFTI_INTENT_GAMMA = 9;
  kNIFTI_INTENT_POISSON = 10;
  kNIFTI_INTENT_NORMAL = 11;
  kNIFTI_INTENT_FTEST_NONC = 12;
  kNIFTI_INTENT_CHISQ_NONC = 13;
  kNIFTI_INTENT_LOGISTIC = 14;
  kNIFTI_INTENT_LAPLACE = 15;
  kNIFTI_INTENT_UNIFORM = 16;
  kNIFTI_INTENT_TTEST_NONC = 17;
  kNIFTI_INTENT_WEIBULL = 18;
  kNIFTI_INTENT_CHI = 19;
  kNIFTI_INTENT_INVGAUSS = 20;
  kNIFTI_INTENT_EXTVAL = 21;
  kNIFTI_INTENT_PVAL = 22;
  NIFTI_INTENT_LOGPVAL = 23;
  NIFTI_INTENT_LOG10PVAL = 24;
  kNIFTI_LAST_STATCODE = 24;//kNIFTI_INTENT_PVAL;
  kNIFTI_INTENT_ESTIMATE = 1001;
  kNIFTI_FIRST_NONSTATCODE = kNIFTI_INTENT_ESTIMATE;
  kNIFTI_INTENT_LABEL = 1002;
  kNIFTI_INTENT_NEURONAME = 1003;
  kNIFTI_INTENT_GENMATRIX = 1004;
  kNIFTI_INTENT_SYMMATRIX = 1005;
  kNIFTI_INTENT_DISPVECT = 1006;
  kNIFTI_INTENT_VECTOR = 1007;
  kNIFTI_INTENT_POINTSET = 1008;
  kNIFTI_INTENT_TRIANGLE = 1009;
  kNIFTI_INTENT_QUATERNION = 1010;
  function IsNIFTIMagic (var lNII: TNIFTIhdr): boolean;
function Read4DF32(lImages: Tstringlist; var lNII: TNIFTIImg): boolean; overload;
function Read4DF32(FileName: ansistring; var lNII: TNIFTIImg): boolean; overload;
function Read4Dbyte(FileName: ansistring; var lNII: TNIFTIImg): boolean; overload;

//load all volumes as single-precision float
function Read3DF32(FileName: ansistring; var lNII: TNIFTIImg): boolean;
//load first volume as f32 image, with min=0 and max=1
function Read3DBinary(FileName: ansistring; var lNII: TNIFTIImg): boolean;
function Read3DByte(FileName: ansistring; var lNII: TNIFTIImg): boolean;
//load first volume as binary image, with min=0 and max=1
function ReadNII(FileName: ansistring; var lNII: TNIFTIImg;
  lOpts: TReadOptions): boolean;
function WriteNII(FileName: ansistring; var lNII: TNIFTIImg): boolean; overload;
function WriteNII(FileNames: Tstringlist; var lNII: TNIFTIImg): boolean; overload; //allows you to specify unique names for each volume in a 4D dataset...
procedure CreateNII(var lNII: TNIfTIimg);
procedure FreeNII(var lNII: TNIfTIimg);
function SameXYZDims(lA, lB: TNIfTIimg): boolean;
//procedure CreateZeroedFloat3DNII(var lSrcHdr,lNewVol: TNIFTIimg);
procedure Force3DNII(var lNII: TNIfTIimg);
function CreateZeroedi16_3DNII(var lSrcHdr, lNewVol: TNIFTIimg;
  lOutname: ansistring): boolean;
function CreateZeroedFloat3DNII(var lSrcHdr, lNewVol: TNIFTIimg;
  lOutname: ansistring): boolean;
function CreateZeroedFloat4DNII(var lSrcHdr, lNewVol: TNIFTIimg;
  lOutname: ansistring; lnVol: integer): boolean;
procedure f32Toi8(var lNII: TNIFTIImg);
procedure Riteln(S: ansistring);
function ReadNIIHdr(FileName: ansistring; var lNII: TNIFTIimg): boolean; overload;
function Save4DTo3D(FileNames: Tstringlist; var lNII: TNIFTIImg; lPrefix: string): boolean;

function DefaultReadOpts: TReadOptions; overload;
function DefaultReadOpts (lF: TFormat): TReadOptions; overload;
function DefaultReadOpts (lF: TFormat; lVol: integer): TReadOptions; overload;

var
  DebugStrings: TStringList;
  DebugFractionCompleted: single;

implementation

function DefaultReadOpts: TReadOptions; overload;
begin
  result.NaN2Zero:=false;
  result.ReadVol:= 0; //only read header
  result.ForceDatatype:=TNativex;
end;

function DefaultReadOpts (lF: TFormat): TReadOptions; overload;
begin
  result.NaN2Zero:=true;
  result.ReadVol:= 0; //only read header
  result.ForceDatatype:=lF;
end;

function DefaultReadOpts (lF: TFormat; lVol: integer): TReadOptions; overload;
begin
  result.NaN2Zero:=true;
  result.ReadVol:= lVol;
  result.ForceDatatype:=lF;
end;




function SubBound(lVal, lMin: integer): integer;
begin
  Result := lVal;
  if Result < lMin then
    Result := lMin;
end;

function NonspatialDimensionsNII(lA: TNIFTIhdr): integer; overload;
  //returns sum of 4th, 5th, 6th and 7th dimension...
begin
  Result := SubBound(lA.dim[4], 1) * SubBound(lA.dim[5], 1) *
    SubBound(lA.dim[6], 1) * SubBound(lA.dim[7], 1);
end;

function NonspatialDimensionsNII(lA: TNIFTIimg): integer; overload;
  //returns sum of 4th, 5th, 6th and 7th dimension...
begin
  Result :=NonspatialDimensionsNII(lA.hdr);
  //Result := SubBound(lA.Hdr.dim[4], 1) * SubBound(lA.Hdr.dim[5], 1) * SubBound(lA.Hdr.dim[6], 1) * SubBound(lA.Hdr.dim[7], 1);
end;

function MinMaxNIIRaw(var lNII: TNIfTIimg; lVol: integer;
  var lMin, lMax: double): boolean;
  //returns min and max intensity in as Volume.
  //For 4D data, use lVol to specify the volume
  //  if lVol < 1 then all volumes
var
  i, lnVol, lVox, lVoxOffset: integer;
begin
  Result := False;
  if lNII.raw8 = nil then
  begin
    Riteln('MinMax Error: image not loaded.');
    exit;//image not loaded...
  end;
  lnVol := NonspatialDimensionsNII(lNiI);
  lVox := lNII.Hdr.dim[1] * lNII.Hdr.dim[2] * lNII.Hdr.dim[3];
  if (lnVol < 1) or (lVox < 1) then
    exit;
  lVoxOffset := 0;
  if (lVol < 1) or (lVol > lnVol) then
    lVox := lVox * lnVol
  else
    lVoxOffset := (lVol - 1) * lVox;

  case lNII.Hdr.datatype of
    kDT_UNSIGNED_CHAR:
    begin
      lMin := lNII.i8^[lVoxOffset + 1];
      lMax := lMin;
      for i := 1 to lVox do
        if lNII.i8^[lVoxOffset + i] > lMax then
          lMax := lNII.i8^[lVoxOffset + i];
      for i := 1 to lVox do
        if lNII.i8^[lVoxOffset + i] < lMin then
          lMin := lNII.i8^[lVoxOffset + i];
    end;//CHAR
    kDT_SIGNED_SHORT:
    begin
      lMin := lNII.i16^[lVoxOffset + 1];
      lMax := lMin;
      for i := 1 to lVox do
        if lNII.i16^[lVoxOffset + i] > lMax then
          lMax := lNII.i16^[lVoxOffset + i];
      for i := 1 to lVox do
        if lNII.i16^[lVoxOffset + i] < lMin then
          lMin := lNII.i16^[lVoxOffset + i];
    end;//kDT_SIGNED_SHORT
    kDT_SIGNED_INT:
    begin
      lMin := lNII.i32^[lVoxOffset + 1];
      lMax := lMin;
      for i := 1 to lVox do
        if lNII.i32^[lVoxOffset + i] > lMax then
          lMax := lNII.i32^[lVoxOffset + i];
      for i := 1 to lVox do
        if lNII.i32^[lVoxOffset + i] < lMin then
          lMin := lNII.i32^[lVoxOffset + i];
    end;//kDT_SIGNED_INT
    kDT_FLOAT:
    begin

      lMin := lNII.f32^[lVoxOffset + 1];
      lMax := lMin;

      for i := 1 to (lvox - 1000) do
        if lNII.f32^[i + lVoxOffset] > lMax then
          lMax := lNII.f32^[i + lVoxOffset];
      for i := 1 to lVox do
        if lNII.f32^[i + lVoxOffset] < lMin then
          lMin := lNII.f32^[i + lVoxOffset];
    end;//float
  end;// datatype
  Result := True;
end;

function SameXYZDims(lA, lB: TNIfTIimg): boolean;
begin
  Result := True;
  if (lA.Hdr.dim[1] = lB.Hdr.dim[1]) and (lA.Hdr.dim[2] = lB.Hdr.dim[2]) and
    (lA.Hdr.dim[3] = lB.Hdr.dim[3]) then
    exit;
  Result := False;
  Riteln('Image dimensions do not match ' + lA.HdrName + ' <> ' + lB.HdrName);
end;

procedure FreeNII(var lNII: TNIfTIimg);
begin
  //lNII.umin := 0;
  //lNII.umax := 0;
  lNII.VoxelsLoaded := 0;
  lNII.NonspatialDimensionsAvailable := 0;
  lNII.NonspatialDimensionsLoaded := 0;
  if lNII.raw8 <> nil then
    Freemem(lNII.raw8);
  lNII.raw8 := nil;
end;

procedure CreateNII(var lNII: TNIfTIimg);
var
  i: integer;
begin
  lNII.hdr.HdrSz := SizeOf(TNIFTIHdr);
  lNII.hdr.scl_slope := 1;
  lNII.hdr.scl_inter := 0;
  for i := 0 to 7 do
    lNII.Hdr.pixdim[i] := 1;
  for i := 0 to 3 do
  begin
    lNII.Hdr.srow_x[i] := 0;
    lNII.Hdr.srow_y[i] := 0;
    lNII.Hdr.srow_z[i] := 0;
  end;
  lNII.Hdr.srow_x[0] := 1;
  lNII.Hdr.srow_y[1] := 1;
  lNII.Hdr.srow_z[2] := 1;
  //lNII.hdr.vox_offset := 0;

  lNII.raw8 := nil;
  FreeNII(lNII);
end;

procedure Force3DNII(var lNII: TNIfTIimg);
var
  lVol: integer;
begin
  for lVol := 4 to 7 do
    lNII.Hdr.dim[lVol] := 1;
  lNII.Hdr.dim[0] := 3;//SPM wants to be explicitly told this is 3D
  lNII.NonspatialDimensionsAvailable := 1;
end;

procedure SetLengthB(var lPtr: Bytep; lBytes: integer);
begin
  if lPtr <> nil then
    freemem(lPtr);
  if lBytes < 1 then
  begin
    lPtr := nil;
    exit;
  end;
  getmem(lPtr, lBytes);
end;

procedure SetPtrs(var lNII: TNIfTIimg);
begin
  lNII.f32 := SingleP(@lNII.raw8^[1]);
  lNII.i32 := LongintP(@lNII.raw8^[1]);
  lNII.i16 := SmallIntP(@lNII.raw8^[1]);
  lNII.i8 := ByteP(@lNII.raw8^[1]);
end;


function CreateZeroedi16_3DNII(var lSrcHdr, lNewVol: TNIFTIimg;
  lOutname: ansistring): boolean;
var
  i: integer;
begin
  Result := False;
  lNewVol := lSrcHdr;
  CreateNII(lNewVol);
  Force3DNII(lNewVol);
  lNewVol.hdr := lSrcHdr.Hdr;
  lNewVol.hdr.datatype := kDT_SIGNED_SHORT;
  lNewVol.hdr.bitpix := 16;
  lNewVol.Hdr.Dim[4] := 1;
  lNewVol.NonspatialDimensionsLoaded := 1;
  lNewVol.VoxelsLoaded := lNewVol.Hdr.Dim[1] * lNewVol.Hdr.Dim[2] *
    lNewVol.Hdr.Dim[3] * lNewVol.Hdr.Dim[4];
  if lOutname <> '' then
    lNewVol.HdrName := lOutname;
  if lNewVol.VoxelsLoaded < 1 then
    exit;
  SetLengthB(lNewVol.raw8, lNewVol.VoxelsLoaded * 2);//*2 as i16
  SetPtrs(lNewVol);
  for i := 1 to lNewVol.VoxelsLoaded do
    lNewVol.i16^[i] := 0;
  Result := True;
end;

function CreateZeroedFloat4DNII(var lSrcHdr, lNewVol: TNIFTIimg;
  lOutname: ansistring; lnVol: integer): boolean;
var
  i: integer;
  lHdr: TNIFTIhdr;
begin
  Result := False;
  lHdr := lSrcHdr.Hdr;
  CreateNII(lNewVol);
  lNewVol.Hdr := lHdr;
  Force3DNII(lNewVol);
  lNewVol.hdr := lSrcHdr.Hdr;
  lNewVol.hdr.datatype := kDT_FLOAT;
  lNewVol.hdr.bitpix := 32;
  lNewVol.Hdr.Dim[4] := lnVol;
  lNewVol.NonspatialDimensionsLoaded := lnVol;
  lNewVol.VoxelsLoaded := lNewVol.Hdr.Dim[1] * lNewVol.Hdr.Dim[2] *
    lNewVol.Hdr.Dim[3] * lNewVol.Hdr.Dim[4];
  if lOutname <> '' then
    lNewVol.HdrName := lOutname;
  if lNewVol.VoxelsLoaded < 1 then
    exit;
  SetLengthB(lNewVol.raw8, lNewVol.VoxelsLoaded * 4);//*4 as single precision
  SetPtrs(lNewVol);
  for i := 1 to lNewVol.VoxelsLoaded do
    lNewVol.f32^[i] := 0;
  Result := True;
end;

function CreateZeroedFloat3DNII(var lSrcHdr, lNewVol: TNIFTIimg;
  lOutname: ansistring): boolean;
begin
  result := CreateZeroedFloat4DNII(lSrcHdr, lNewVol, lOutname, 1);
end;

(*function CreateZeroedFloat3DNII(var lSrcHdr,lNewVol: TNIFTIimg; lOutname: AnsiString): boolean;
var
  i: integer;
begin
    result := false;
    lNewVol := lSrcHdr;
    CreateNII(lNewVol);
    Force3DNII(lNewVol);
    lNewVol.hdr.datatype := kDT_FLOAT;
    lNewVol.hdr.bitpix := 32;
    lNewVol.VoxelsLoaded := lNewVol.Hdr.Dim[1]*lNewVol.Hdr.Dim[2]*lNewVol.Hdr.Dim[3];
    lNewVol.NonspatialDimensionsLoaded := 1;
    //lNewVol.umin := 0;
    //lNewVol.umax := 0;
    if lOutname <> '' then
      lNewVol.HdrName := lOutname;
    if lNewVol.VoxelsLoaded < 1 then
      exit;
    SetLengthB(lNewVol.raw8,lNewVol.VoxelsLoaded*4);//*4 as single precision
    SetPtrs(lNewVol);
    for i := 1 to lNewVol.VoxelsLoaded do
      lNewVol.f32^[i] := 0;
    result := true;
end;*)

procedure NIFTIhdr_SwapBytes(var lAHdr: TNIFTIhdr);
//Swap Byte order for the Analyze type
var
  lInc: integer;
begin
  with lAHdr do
  begin
    swap4(hdrsz);
    swap4(extents);
    session_error := swap(session_error);
    for lInc := 0 to 7 do
      dim[lInc] := swap(dim[lInc]);
    Xswap4r(intent_p1);
    Xswap4r(intent_p2);
    Xswap4r(intent_p3);
    intent_code := swap(intent_code);
    datatype := swap(datatype);
    bitpix := swap(bitpix);
    slice_start := swap(slice_start);
    for lInc := 0 to 7 do
      Xswap4r(pixdim[linc]);
    Xswap4r(vox_offset);
    Xswap4r(scl_slope);
    Xswap4r(scl_inter);
    slice_end := swap(slice_end);
    Xswap4r(cal_max);
    Xswap4r(cal_min);
    Xswap4r(slice_duration);
    Xswap4r(toffset);
    swap4(glmax);
    swap4(glmin);
    qform_code := swap(qform_code);
    sform_code := swap(sform_code);
    Xswap4r(quatern_b);
    Xswap4r(quatern_c);
    Xswap4r(quatern_d);
    Xswap4r(qoffset_x);
    Xswap4r(qoffset_y);
    Xswap4r(qoffset_z);
    for lInc := 0 to 3 do //alpha
      Xswap4r(srow_x[lInc]);
    for lInc := 0 to 3 do //alpha
      Xswap4r(srow_y[lInc]);
    for lInc := 0 to 3 do //alpha
      Xswap4r(srow_z[lInc]);
  end; //with NIFTIhdr
end; //proc NIFTIhdr_SwapBytes

function SwapImg(var lNII: TNIFTIImg): boolean;
var
  i, lVox: integer;
begin
  Result := True;
  lVox := lNII.NonspatialDimensionsLoaded * lNII.Hdr.Dim[1] * lNII.Hdr.Dim[2] *
    lNII.Hdr.Dim[3];
  if (lVox < 1) or (lNII.Hdr.datatype = kDT_UNSIGNED_CHAR) then
    exit; //no need to swap
  case lNII.Hdr.datatype of
    kDT_SIGNED_SHORT:
    begin
      for i := 1 to lVox do
        lNII.i16^[i] := Swap(lNII.i16^[i]);
    end;

    kDT_SIGNED_INT, kDT_FLOAT:
    begin//supported format
      for i := 1 to lVox do
        pswap4r(lNII.f32^[i]);
    end;
    else
    begin
      Result := False;
      riteln('niftiutil UnSwapImg error: datatype not supported.');
      exit;
    end;
  end; //case

end;

function LoadRaw(FileName: ansistring; var lNII: TNIFTIImg;
  lOpts: TReadOptions): boolean;
var
  {$IFDEF GZIP}
  Stream: TGZFileStream;
  //GZip stream reads both compressed and uncompressed data, which is very useful
  {$ELSE}
  Stream: TFileStream;
  {$ENDIF}
  lVolSkipBytes, lVolBytes: longint;
  lSwap: boolean;
begin
  Result := False;
  lSwap := False;
  FreeNII(lNII);
  if not fileexists(Filename) then
  begin
    Riteln('Unable to find file ' + Filename);
    exit;
  end;
  {$IFDEF GZIP}
  Stream := TGZFileStream.Create(FileName, gzopenread);
  {$ELSE}
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  {$ENDIF}
  try
    Stream.ReadBuffer(lNII.Hdr, SizeOf(TNIFTIHdr));
    //fx(swap4(lNII.Hdr.hdrsz), lNII.Hdr.magic,kswapNIFTI_MAGIC_SEPARATE_HDR, kswapNIFTI_MAGIC_EMBEDDED_HDR);
    //if (lNII.Hdr.magic = kswapNIFTI_MAGIC_SEPARATE_HDR) or (lNII.Hdr.magic = kswapNIFTI_MAGIC_EMBEDDED_HDR) then begin
    lVolBytes := lNII.Hdr.HdrSz;
    swap4(lVolBytes);
    if (lVolBytes = SizeOf(TNIFTIHdr)) then
    begin
      NIFTIhdr_SwapBytes(lNII.Hdr);
      lSwap := True;
    end;
    if lNII.Hdr.HdrSz <> SizeOf(TNIFTIHdr) then
    begin
      {$IFDEF GZIP}
      Riteln('Unable to read image ' + Filename + ' - problem reading header.');
      {$ELSE}
      Riteln('Unable to read image ' + Filename +
        ' - this software can only read UNCOMPRESSED NIfTI files with correct headers.');
      {$ENDIF}
      exit;
    end;
    if (lNII.Hdr.bitpix <> 8) and (lNII.Hdr.bitpix <> 16) and
      (lNII.Hdr.bitpix <> 32) then
    begin
      Riteln('Unable to load ' + Filename +
        ' - this software can only read 8,16,32-bit NIfTI files, not ' + IntToStr(
        lNII.Hdr.bitpix) + '-bit images.');
      exit;
    end;
    lNII.NonspatialDimensionsAvailable := NonspatialDimensionsNII(lNII);

    lVolBytes := lNII.Hdr.Dim[1] * lNII.Hdr.Dim[2] * lNII.Hdr.Dim[3] *
      (lNII.Hdr.bitpix div 8);
    if (lNII.NonspatialDimensionsAvailable < 1) or (lVolBytes < 1) then begin
      Riteln('Header must specify at least one voxels.');
      exit;
    end;
    if lOpts.ReadVol < 0 then begin //only read header
      Result := True;
      exit;
    end;
    //Next: load image data
    //determine volume[s] to load
    lNII.NonspatialDimensionsLoaded := lNII.NonspatialDimensionsAvailable;
    lVolSkipBytes := 0;
    if (lOpts.ReadVol > 0) then
    begin //only load a single volume
      if (lOpts.ReadVol > lNII.NonspatialDimensionsAvailable) then
      begin
        Riteln('Error: unable to read volume number ' + IntToStr(
          lOpts.ReadVol) + '  (image only has ' + IntToStr(lNII.NonspatialDimensionsAvailable) +
          ' volumes).');
        exit;
      end;
      lVolSkipBytes := (lOpts.ReadVol - 1) * lVolBytes;
      Force3DNII(lNII);
      lNII.NonspatialDimensionsLoaded := 1;
    end;
    //read the image data
    if extractfileext(Filename) = '.hdr' then begin
      Stream.Free;
      if not fileexists(changefileext(FileName, '.img')) then
      begin
        Riteln('Unable to find image data ' + changefileext(FileName, '.img'));
        exit;
      end;
      {$IFDEF GZIP}
      Stream := TGZFileStream.Create(changefileext(FileName, '.img'), gzopenread);
      {$ELSE}
      Stream := TFileStream.Create(changefileext(FileName, '.img'),
        fmOpenRead or fmShareDenyWrite);
      {$ENDIF}
    end;
    Stream.Seek(round(lNII.Hdr.vox_offset) + lVolSkipBytes, soFromBeginning);
    SetLengthB(lNII.raw8, lVolBytes * lNII.NonspatialDimensionsLoaded);
    Stream.ReadBuffer(lNII.raw8^[1], lVolBytes * lNII.NonspatialDimensionsLoaded);
    SetPtrs(lNII);
    Result := True;
  finally
    Stream.Free;
  end;
  if Result and lSwap then
    Result := SwapImg(lNII);
  if Result then
    lNII.VoxelsLoaded := lNII.NonspatialDimensionsLoaded * lNII.Hdr.Dim[1] *
      lNII.Hdr.Dim[2] * lNII.Hdr.Dim[3];
end;

procedure i8Binarize(var lNII: TNIFTIImg);
//convert 8-bit bytes to binary
var
  i, vx, min: integer;
begin
  vx := (lNII.Hdr.dim[1] * lNII.Hdr.dim[2] * lNII.Hdr.dim[3] *
    lNII.NonspatialDimensionsLoaded);
  if (lNII.Hdr.bitpix <> 8) or (vx < 1) then
    exit;
  min := lNII.i8^[1];
  for i := 1 to vx do
    if lNII.i8^[i] < min then
      min := lNII.i8^[i];
  for i := 1 to vx do
  begin
    if lNII.i8^[i] = min then
      lNII.i8^[i] := 0
    else
      lNII.i8^[i] := 1;
  end;
end;

procedure i16Toi8(var lNII: TNIFTIImg);
//convert 16-bit int to 8-bit int
var
  scale: double;
  i, vx, min, max: integer;
  Src: SmallIntP;
begin
  vx := (lNII.Hdr.dim[1] * lNII.Hdr.dim[2] * lNII.Hdr.dim[3] *
    lNII.NonspatialDimensionsLoaded);
  if (lNII.Hdr.bitpix <> 16) or (vx < 1) then
    exit;
  getmem(Src, vx * 2);
  min := lNII.i16^[1];
  max := min;
  for i := 1 to vx do begin
    Src^[i] := lNII.i16^[i];
    if Src^[i] < min then
      min := Src^[i];
    if Src^[i] > max then
      max := Src^[i];
  end;
  if max = min then
    scale := 1
  else
    scale := 255 / (max - min);
  setlengthB(lNII.raw8, vx);
  SetPtrs(lNII);
  for i := 1 to vx do
    lNII.i8^[i] := round((Src^[i] - min) * scale);
  freemem(Src);//free memory
end;

procedure i32Toi8(var lNII: TNIFTIImg);
//convert 32-bit int to 8-bit int
var
  scale: double;
  i, vx, min, max: integer;
  Src: longintP;
begin
  vx := (lNII.Hdr.dim[1] * lNII.Hdr.dim[2] * lNII.Hdr.dim[3] *
    lNII.NonspatialDimensionsLoaded);
  if (lNII.Hdr.bitpix <> 32) or (vx < 1) then
    exit;
  getmem(Src, vx * 4);
  min := lNII.i32^[1];
  max := min;
  for i := 1 to vx do
  begin
    Src^[i] := lNII.i32^[i];
    if Src^[i] < min then
      min := Src^[i];
    if Src^[i] > max then
      max := Src^[i];
  end;
  if max = min then
    scale := 1
  else
    scale := 255 / (max - min);
  setlengthB(lNII.raw8, vx);
  SetPtrs(lNII);
  for i := 1 to vx do
    lNII.i8^[i] := round((Src^[i] - min) * scale);
  freemem(Src);//free memory
end;

(*procedure f32Toi8(var lNII: TNIFTIImg);
//convert 32-bit float to 8-bit int
var
  min,max,scale: double;
  i,vx: integer;
  Src: singlep;
begin
    vx := (lNII.Hdr.dim[1]*lNII.Hdr.dim[2]*lNII.Hdr.dim[3]*lNII.NonspatialDimensionsLoaded);
    if (lNII.Hdr.bitpix <> 32) or (vx < 1) then
      exit;

    getmem(Src,vx* 4);
    min := lNII.f32^[1];
    max := min;
  fx(1111);
    for i := 1 to vx do begin
        Src^[i] := lNII.f32^[i];
        if Src^[i] < min then
          min := Src^[i];
        if Src^[i] > max then
          max := Src^[i];
    end;

fx(121);
if max = min then
      scale := 1
    else
      scale := 255/(max-min);
fx(222);
setlengthB(lNII.raw8,vx);
    SetPtrs(lNII);
    for i := 1 to vx do
        lNII.i8^[i] := round((Src^[i]-min)*scale);
    freemem(Src);//free memory
end; *)
procedure f32Toi8(var lNII: TNIFTIImg);
//convert 32-bit float to 8-bit int
var
  min, max, scale: double;
  i, vx: integer;
  Src: singlep;
begin
  vx := (lNII.Hdr.dim[1] * lNII.Hdr.dim[2] * lNII.Hdr.dim[3] *
    lNII.NonspatialDimensionsLoaded);
  if (lNII.Hdr.bitpix <> 32) or (vx < 1) then
    exit;

  getmem(Src, vx * 4);
  for i := 1 to vx do
    if specialsingle(lNII.f32^[i]) then
      Src^[i] := 0
    else
      Src^[i] := lNII.f32^[i];
  min := Src^[1];
  max := min;
  for i := 1 to vx do
  begin
    if Src^[i] < min then
      min := Src^[i];
    if Src^[i] > max then
      max := Src^[i];
  end;
  if max = min then
    scale := 1
  else
    scale := 255 / (max - min);
  setlengthB(lNII.raw8, vx);
  SetPtrs(lNII);
  for i := 1 to vx do
    lNII.i8^[i] := round((Src^[i] - min) * scale);
  freemem(Src);//free memory
end;



procedure i8ToSingle(var lNII: TNIFTIImg);
//convert 8-bit bytes to 32-bit single
var
  i, vx: integer;
  Src: bytep;
begin
  vx := (lNII.Hdr.dim[1] * lNII.Hdr.dim[2] * lNII.Hdr.dim[3] *
    lNII.NonspatialDimensionsLoaded);
  if (lNII.Hdr.bitpix <> 8) or (vx < 1) then
    exit;
  getmem(Src, vx);
  for i := 1 to vx do
    Src^[i] := lNII.i8^[i];
  setlengthB(lNII.raw8, vx * 4);
  SetPtrs(lNII);
  for i := 1 to vx do
    lNII.f32^[i] := Src^[i];
  freemem(Src);//free memory
end;

procedure i16ToSingle(var lNII: TNIFTIImg);
//convert 16-bit signed to 32-bit single
var
  i, vx: integer;
  Src: SmallIntP;
begin
  vx := (lNII.Hdr.dim[1] * lNII.Hdr.dim[2] * lNII.Hdr.dim[3] *
    lNII.NonspatialDimensionsLoaded);
  if (lNII.Hdr.bitpix <> 16) or (vx < 1) then
    exit;
  getmem(Src, vx * 2);
  for i := 1 to vx do
    Src^[i] := lNII.i16^[i];
  setlengthB(lNII.raw8, vx * 4);
  SetPtrs(lNII);
  for i := 1 to vx do
    lNII.f32^[i] := Src^[i];
  freemem(Src);//free memory

end;

procedure i32ToSingle(var lNII: TNIFTIImg);
//convert 8-bit bytes to 32-bit single
var
  i, vx: integer;
  Src: longintP;
begin
  vx := (lNII.Hdr.dim[1] * lNII.Hdr.dim[2] * lNII.Hdr.dim[3] *
    lNII.NonspatialDimensionsLoaded);
  if (lNII.Hdr.bitpix <> 16) or (vx < 1) then
    exit;
  getmem(Src, vx * 4);
  for i := 1 to vx do
    Src[i] := lNII.i32[i];
  setlengthB(lNII.raw8, vx * 4);
  SetPtrs(lNII);
  for i := 1 to vx do
    lNII.f32^[i] := Src^[i];
  freemem(Src);//free memory
end;

function IsNIFTIMagic (var lNII: TNIFTIhdr): boolean;
var
  v: longint;
begin
  v := lNII.magic;  //Magic values
  if (v= kNIFTI_MAGIC_SEPARATE_HDR) or (v = kNIFTI_MAGIC_EMBEDDED_HDR)
  or (v =kswapNIFTI_MAGIC_SEPARATE_HDR) or (v = kswapNIFTI_MAGIC_EMBEDDED_HDR) then
     result := true
  else
    result := false;
end;

function WriteNII(FileName: ansistring; var lNII: TNIFTIImg): boolean;
var
  {$IFDEF GZIP}
  Stream: TGZFileStream;
{$ENDIF}
  lExt, lFilename: string;
  lF: file;
  lLong: longint;
  lImgBytes: integer;
begin
  //result := false; //<- currently not used
  lLong := 0;
  if lNII.NonspatialDimensionsLoaded > 1 then
    lNII.hdr.dim[0] := 4//??SPM checks to see if there are multiple volumes
  else
    Force3DNII(lNII);
  lImgBytes := lNII.Hdr.Dim[1] * lNII.Hdr.Dim[2] * lNII.Hdr.Dim[3] *
    lNII.NonspatialDimensionsLoaded * (lNII.Hdr.bitpix div 8);
  lFilename := Filename;
  lExt := UpCaseExt(lFileName);
  if lExt = '.IMG' then
    lFilename := changefileext(lFilename, '.hdr');
  if lExt = '' then
    lFilename := lFilename + '.nii';
  if (not IsNIFTIMagic (lNII.Hdr)) then begin
     riteln('Warning: Analyze 7.5 format image (not NIFTI)');
  end else if (lExt = '.IMG') or (lExt = '.HDR') then begin
    lNII.Hdr.magic := kNIFTI_MAGIC_SEPARATE_HDR;
    lNII.Hdr.vox_offset := 0;
  end else begin
    lNII.Hdr.magic := kNIFTI_MAGIC_EMBEDDED_HDR;
    lNII.Hdr.vox_offset := kNIIImgOffset;//352 bytes
  end;
  {$IFDEF GZIP}
  if GzExt(lFilename) then
  begin
    Riteln('Compressing to ' + lFilename);
    Stream := TGZFileStream.Create(lFileName, gzopenwrite);
    Stream.Writebuffer(lNII.Hdr, sizeof(TNIFTIhdr));
    Stream.Writebuffer(lLong, 4);
    Stream.Writebuffer(lNII.raw8^[1], lImgBytes);
    FreeAndNil(Stream);
    exit;
  end;
  {$ENDIF}
  Filemode := 1; //1366
  AssignFile(lF, lFileName);
  if fileexists(lFilename) then
    Reset(lF, 1)
  else
    Rewrite(lF, 1);
  BlockWrite(lF, lNII.Hdr, sizeof(TNIFTIhdr));
  if ((lExt = '.IMG') or (lExt = '.HDR')) and (lImgBytes > 0) then
  begin
    CloseFile(lF);
    lFilename := changefileext(lFilename, '.img');
    AssignFile(lF, lFileName);
    if fileexists(lFilename) then
      Reset(lF, 1)
    else
      Rewrite(lF, 1);
    BlockWrite(lF, lNII.raw8^[1], lImgBytes);
  end
  else
  begin
    lLong := 0;
    BlockWrite(lF, lLong, 4);
    if lImgBytes > 0 then
      BlockWrite(lF, lNII.raw8^[1], lImgBytes);
  end;
  CloseFile(lF);
  Filemode := 2; //1366
  Result := True;
end;

function Read3DF32(FileName: ansistring; var lNII: TNIFTIImg): boolean;
  //load first volume as single-precision float image
var
  lOpts: TReadOptions;
begin
  lOpts := DefaultReadOpts ( TFloatx, 1);
  Result := ReadNII(FileName, lNII, lOpts);
end;

(*function Sum4DnotNAN(var lNII4D,lNIIsum: TNIFTIImg): boolean; overload;
//given 4D nifti image generates lNIIsum where each voxel reports the number of volumes in the 4D image that are suitable numbers
// in other wrods, excludes values that are INF+, INF-, NaN, etc.
var
  lnVox,lVox,lnVol,lVol,lOffset: integer;
begin
     lnVox := lNII4D.hdr.dim[1]*lNII4D.hdr.dim[2]*lNII4D.hdr.dim[3];
     lnVol := lNII4D.hdr.dim[4];
     if (lnVox < 1) or (lnVol < 1)  then exit;
    CreateZeroedFloat3DNII(lNII4D,lNIIsum,ChangeFilePostfix(lNII4D.hdrname,'_sum'));
    for lVol := 1 to lnVol do begin
        lOffset := (lVol-1)*lnVox;
        for lVox := 1 to lnVox do begin
            if not specialsingle(lNII4D.f32^[lVox+lOffset]) then
               lNIIsum.f32^[lVox] := lNIIsum.f32^[lVox] + 1;
        end;//for each vox
    end;//for each vol
end; *)

function Read4DF32(lImages: Tstringlist; var lNII: TNIFTIImg): boolean; overload;
label 666;
var
  //lOpts: TReadOptions;
  lI,lVox,lnVox,lOffset: integer;
  lNII1: TNIFTIImg;
begin
  result := false;
  if lImages.Count < 1 then
     exit;
  if lImages.Count = 1 then begin
       result := Read4DF32(lImages[0], lNII);
       exit;
  end;
   CreateNII(lNII1);
   Read4DF32(lImages[0], lNII1);
   lnVox := lNII1.hdr.dim[1]*lNII1.hdr.dim[2]*lNII1.hdr.dim[3];
   if lnVox < 1 then goto 666;
   lOffset := 0;
   CreateZeroedFloat4DNII(lNII1,lNII,ChangeFilePostFix(lImages[0],'_'+inttostr(lImages.Count)),lImages.Count);
   for lVox := 1 to lnVox do
       lNII.f32^[lVox] := lNII1.f32^[lVox];
   for lI := 2 to lImages.Count do begin
         if not Read3DF32(lImages[lI-1], lNII1) then begin
            showmsg('Error loading '+lImages[lI-1]);
            goto 666;
         end;
         if not SameXYZDims(lNII1,lNII) then begin
            showmsg('Dimensions do not match '+lImages[0]+' <> '+lNII1.HdrName);
            goto 666;
         end;
         lOffset := lOffset + lnVox;
         for lVox := 1 to lnVox do
             lNII.f32^[lVox+lOffset] := lNII1.f32^[lVox];
   end;
   result := true;
  666:
   FreeNII(lNII1);
end;

function Save4DTo3D(FileNames: Tstringlist; var lNII: TNIFTIImg; lPrefix: string): boolean;
var
  pFileNames: Tstringlist;
  i: integer;
begin
   result := false;
   if (Filenames.count < 1) or (Filenames.Count <> lNII.hdr.dim[4]) then begin
        showmsg('Save4DTo3D incorrectly speficied.');
        exit;
   end;
   pFileNames := TStringlist.Create;
   for i := 0 to (Filenames.Count-1) do
         pFileNames.Add(ChangeFilePrefix(FileNames[i],lPrefix));
   WriteNII(pFilenames,lNII);
   pFileNames.Free;
   result := true;
end;

function WriteNII(FileNames: Tstringlist; var lNII: TNIFTIImg): boolean; overload;
var
  lVox,lVol,lnVox,lnVol,lOffset: integer;
  lNII3D: TNIFTIImg;
begin
   result := false;
   if lNII.hdr.datatype <> kDT_FLOAT then begin
      showmsg('Error: WriteNII with stringlist names currently only supports FLOAT32 data.');
      exit;
   end;
   lnVol := lNII.hdr.dim[4];
   lnVox := lNII.hdr.dim[1]*lNII.hdr.dim[2]*lNII.hdr.dim[3];
   if (lnVox < 1) or (lnVol < 1) then exit;
   for lVol := 0 to (lnVol-1) do begin
       lOffset := (lVol)*lnVox;
       CreateZeroedFloat3DNII(lNII,lNII3D,Filenames[lVol]);
       for lVox := 1 to lnVox do
            lNII3D.f32^[lVox] := lNII.f32^[lVox+lOffset];
       result := WriteNII(Filenames[lVol],lNII3D);
       FreeNII(lNII3D);
    end;
end;

function Read4Dbyte(FileName: ansistring; var lNII: TNIFTIImg): boolean; overload;
  //load all volumes as single-precision float
var
  lOpts: TReadOptions;
begin
  lOpts := DefaultReadOpts ( TBytex, 0);

  Result := ReadNII(FileName, lNII, lOpts);
end;

function Read4DF32(FileName: ansistring; var lNII: TNIFTIImg): boolean; overload;
  //load all volumes as single-precision float
var
  lOpts: TReadOptions;
begin
  lOpts := DefaultReadOpts ( TFloatx, 0);
  Result := ReadNII(FileName, lNII, lOpts);
end;


function Read3DBinary(FileName: ansistring; var lNII: TNIFTIImg): boolean;
  //load first volume as binary image, with min=0 and max=1
var
  lOpts: TReadOptions;
begin
  lOpts := DefaultReadOpts ( TBinaryBytex, 1);
  Result := ReadNII(FileName, lNII, lOpts);
end;

function Read3DByte(FileName: ansistring; var lNII: TNIFTIImg): boolean;
  //load first volume as binary image, with min=0 and max=1
var
  lOpts: TReadOptions;
begin

  lOpts := DefaultReadOpts ( Tbytex, 1);
  Result := ReadNII(FileName, lNII, lOpts);
end;

procedure Scalef32(var lNII: TNIFTIImg);
//take intercept and slope into account
var
  i, vx: integer;
begin
  vx := (lNII.Hdr.dim[1] * lNII.Hdr.dim[2] * lNII.Hdr.dim[3] *
    lNII.NonspatialDimensionsLoaded);
  if (vx < 1) or (lNII.hdr.datatype <> kDT_FLOAT) or (lNII.Hdr.scl_slope = 0) or
    ((lNII.Hdr.scl_slope = 1) and (lNII.Hdr.scl_inter = 0)) then
    exit;
  for i := 1 to vx do
    lNII.f32^[i] := (lNII.Hdr.scl_slope * lNII.f32^[i]) + lNII.Hdr.scl_inter;
  lNII.Hdr.scl_slope := 1;
  lNII.Hdr.scl_inter := 0;
end;

procedure NaN2Zero(var lNII: TNIFTIImg);
//take intercept and slope into account
var
  i, vx: integer;
begin
  vx := (lNII.Hdr.dim[1] * lNII.Hdr.dim[2] * lNII.Hdr.dim[3] *lNII.NonspatialDimensionsLoaded);
  if (vx < 1) or (lNII.hdr.datatype <> kDT_FLOAT) then
    exit;
  for i := 1 to vx do
    if specialsingle(lNII.f32^[i]) then
       lNII.f32^[i] := 0;
end;

function ReadNIIHdr(FileName: ansistring; var lNII: TNIFTIimg): boolean; overload;
var
   lOpts: TReadOptions;
begin
  lOpts.ReadVol:= -1;//-1 to only read the header, 0 to read all volumes, else read nth 3d volume only
  result := ReadNII(FileName,lNII,lOpts);
end;

function ReadNIIHdr(FileName: ansistring; var lHdr: TNIFTIhdr): boolean; overload;
var
   vIn: TNIFTIimg;
begin
  createNII(vIn);
  result := ReadNIIHdr(FileName,vIn);
  lHdr := vIn.Hdr;
  FreeNII(vIn);//not required if we only read the header...
end;


function ReadNII(FileName: ansistring; var lNII: TNIFTIImg;
  lOpts: TReadOptions): boolean;
var
  F_Filename: ansistring;
begin
  Result := False;
  if Filename = '' then
    exit;
  if uppercase(extractfileext(Filename)) = '.IMG' then
  begin
    //NIfTI images can be a single .NII file [contains both header and image]
    //or a pair of files named .HDR and .IMG. If the latter, we want to read the header first
    F_Filename := changefileext(FileName, '.hdr');
  end
  else
    F_Filename := Filename;
  if not Fileexists(F_FileName) then
  begin
      {$IFDEF LINUX}//LINUX is case sensitive, OSX is not
    Riteln('Unable to find header (case sensitive!) ' + F_Filename);
      {$ELSE}
    Riteln('Unable to find header ' + F_Filename);
      {$ENDIF}
    exit;
  end;
  if not LoadRaw(F_FileName, lNII, lOpts) then
    exit;
  lNII.HdrName := F_Filename;
  if lOpts.ReadVol < 0 then begin //only read header
    Result := True;
    exit;
  end;
  //MinMaxNIIRaw(lNII,-1,lNII.umin,lNII.umax);
  if (lOpts.NaN2Zero) and (lNII.hdr.datatype = kDT_FLOAT) then
     NaN2Zero(lNII);
  if (lOpts.ForceDatatype = Tbytex) or (lOpts.ForceDatatype = TBinaryBytex) then
  begin

    if (lNII.hdr.bitpix = 16) then
      i16Toi8(lNII)
    else if (lNII.hdr.datatype = kDT_SIGNED_INT) and (lNII.hdr.bitpix = 32) then
      i32Toi8(lNII)
    else if (lNII.hdr.datatype = kDT_FLOAT) and (lNII.hdr.bitpix = 32) then
      f32Toi8(lNII);
    lNII.hdr.datatype := kDT_UNSIGNED_CHAR;
    lNII.hdr.bitpix := 8;
  end;
  if lOpts.ForceDatatype = TBinaryBytex then
    i8Binarize(lNII);
  if lOpts.ForceDatatype = TFloatx then
  begin
    if (lNII.hdr.bitpix = 8) then
      i8ToSingle(lNII)
    else if (lNII.hdr.bitpix = 16) then
      i16ToSingle(lNII)
    else if (lNII.hdr.datatype = kDT_SIGNED_INT) and (lNII.hdr.bitpix = 32) then
      i32ToSingle(lNII);

    lNII.hdr.datatype := kDT_FLOAT;
    lNII.hdr.bitpix := 32;
    Scalef32(lNII);
  end;

  Result := True;
end;

procedure Riteln(S: ansistring);
begin
  showmsg(s);//DebugStrings.Add(S);
end;

initialization
  DebugFractionCompleted := 0;
  DebugStrings := TStringList.Create

finalization
  DebugStrings.Free;
end.
