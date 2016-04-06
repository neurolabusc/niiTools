unit define_types;
{$D-,L-,O+,Q-,R-,Y-,S-}
interface
{$H+}
 {$include isgui.inc}
        uses
        {$IFDEF FPC}
               {$IFDEF GUI}LCLType,lclintf, {$ENDIF}
        {$ENDIF}
        {$IFNDEF Unix} Windows,{$ENDIF}
        SysUtils,classes,IniFiles,
        {$IFDEF GUI} forms,dialogs,controls;{$ELSE} dialogsx;{$ENDIF}
const
     kVers = '14 Feb 2015';

  PositiveInfinityBits32:  longword = $7F800000;
  NegativeInfinityBits32:  longword = $FF800000 ;
  SilentNanBits32:  longword = $7fffffff;
  PositiveInfinityBits:  Int64 = $7FF0000000000000;                  // $7FF0000000000000;

VAR
  kNaNs   :  single ABSOLUTE SilentNanBits32;
  kInfs   :  single ABSOLUTE PositiveInfinityBits32;
  kNegInfs   :  single ABSOLUTE NegativeInfinityBits32;

  //kNegInfs :  single ABSOLUTE  SilentNanBits32;
  //kInf   :  single = 1/0;
  //kNegInfs :  single = -1/0;
  kInfd   :  double ABSOLUTE PositiveInfinityBits;
 const
    //NaN : double = 1/0;
     kMagicDouble : double = -111666222;
     kTxtFilter = 'Text (*.txt)|*.txt;*.csv|Comma Separated (*.csv)|*.csv';
        kNIIFilter = 'NIfTI (*.nii)|*.nii';
         kAnyFilter =  'Anything (*)|*';
        kAnaHdrFilter = 'Analyze Header (*.hdr)|*.hdr';
	 kImgPlusVOIFilter = 'NIfTI/Analyze/VOI|*.hdr;*.nii;*.nii.gz;*.voi|NIfTI/Analyze Header (*.hdr;*.nii)|*.hdr;*.nii;*.nii.gz|Volume of interest (*.voi)|*.voi';

     kImgFilter = 'NIfTI/Analyze Header (*.hdr;*.nii)|*.hdr;*.nii;*.nii.gz|Volume of interest (*.voi)|*.voi';
     kHistoBins = 256;//numbers of bins for histogram/image balance
     PixelCountMax = 32768;
     kTab = chr(9);
     kEsc = chr(27);
     kCR = chr (13);
     kBS = #8 ; // Backspace
     kDel = #127 ; // Delete
     UNIXeoln = chr(10);
     kVOI8bit = 1;//May07 100;
{$IFDEF unix}
	   PathDelim = '/';
{$ELSE}
	   PathDelim = '\';
{$ENDIF}

type
  TPSPlot =  RECORD //peristimulus plot
    TRSec,BinWidthSec: single;
    nNegBins,nPosBins,SPMDefaultsStatsFmriT,SPMDefaultsStatsFmriT0: integer;
    TextOutput,GraphOutput,
    SliceTime,SavePSVol,BaselineCorrect,PctSignal,RemoveRegressorVariability,TemporalDeriv,PlotModel,Batch: boolean
  end;
{$IFDEF Unix}
TRGBquad = PACKED RECORD
           rgbBlue,rgbGreen,rgbRed,rgbreserved: byte;
END;
{$ENDIF}
	  TLUTb = array[0..255] of byte;
  glRGBaQUAD = packed record
    rgbRed: Byte;
    rgbGreen: Byte;
    rgbBlue: Byte;
    rgbReserved: Byte;
  end;
  TUnitRect = record
     L,T,R,B: single;
  end;
    TGLRGBQuad = glRGBaQUAD;// Windows RGBquad is BGRA, GL is RGBA
  GLRGBQuadRA0 = array [0..0] of TGLRGBQuad;
  GLRGBQuadp0 = ^GLRGBQuadRA0;
	  TLUT = array[0..255] of TGLRGBQuad;
	  kStr20 = string[20];
  TCutout =  RECORD
		Lo : array [1..3] of integer;
		Hi : array [1..3] of integer;
  end;
  pRGBQuadArray = ^TRGBQuad;
  TRGBQuadeArray = ARRAY[0..PixelCountMax-1] OF TRGBQuad;
  RGBQuadRA = array [1..1] of TGLRGBQuad;
  RGBQuadp = ^RGBQuadRA;
  RGBQuadRA0 = array [0..0] of TGLRGBQuad;
  RGBQuadp0 = ^RGBQuadRA0;
	int32  = LongInt;
	uint32 = Cardinal;
	int16  = SmallInt;
	uint16 = Word;
	int8   = ShortInt;
	uint8  = Byte;
	SingleRA0 = array [0..0] of Single;
	Singlep0 = ^SingleRA0;
	ByteRA0 = array [0..0] of byte;
	Bytep0 = ^ByteRA0;
	WordRA0 = array [0..0] of Word;
	Wordp0 = ^WordRA0;
	SmallIntRA0 = array [0..0] of SmallInt;
	SMallIntp0 = ^SmallIntRA0;
	LongIntRA0 = array [0..0] of LongInt;
	LongIntp0 = ^LongIntRA0;
	DWordRA = array [1..1] of DWord;
	DWordp = ^DWordRA;
	ByteRA = array [1..1] of byte;
	Bytep = ^ByteRA;
	WordRA = array [1..1] of Word;
	Wordp = ^WordRA;
	SmallIntRA = array [1..1] of SmallInt;
	SMallIntp = ^SmallIntRA;
	LongIntRA = array [1..1] of LongInt;
	LongIntp = ^LongIntRA;
	SingleRA = array [1..1] of Single;
	Singlep = ^SingleRA;
 	SingleRARA = array [1..1] of Singlep;
	SingleRAp = ^SingleRARA;
	DoubleRA = array [1..1] of Double;
	Doublep = ^DoubleRA;
	DoubleRA0 = array [0..0] of Double;
	Doublep0 = ^DoubleRA0;
	HistoRA = array [0..kHistoBins] of longint;
         HistoDoubleRA = array [0..kHistoBins] of double;
function specialsingle (var s:single): boolean; //check if 32-bit float is Not-A-Number, infinity, etc
function FSize (lFName: String): longint;
function FileExistsEX(Name: String): Boolean;
function ParseFileName (lFilewExt:String): string;
function ParseFileFinalDir (lFileName:String): string;
function ExtractFileDirWithPathDelim(lInFilename: string): string;
function PadStr (lValIn, lPadLenIn: integer): string;
function ChangeFileExtX( lFilename: string; lExt: string): string;
function swap4r4i (s:single): longint; //swap and convert: endian-swap and then typecast 32-bit float as 32-bit integer
function conv4r4i (s:single): longint; //convert: typecast 32-bit float as 32-bit integer
function swap8r(s : double):double; //endian-swap 64-bit float
procedure pswap4i(var s : LongInt); //procedure to endian-swap 32-bit integer
procedure pswap4r ( var s:single);  //procedure to endian-swap 32-bit integer
function swap64r(s : double):double;
function specialdouble (d:double): boolean;
function RealToStr(lR: double {was extended}; lDec: integer): string;
function UpCaseExt(lFileName: string): string;
function ExtGZ (lFilename: string): boolean;
procedure swap4(var s : LongInt);
procedure Xswap4r ( var s:single);
function Bool2Char (lBool: boolean): char;
function Char2Bool (lChar: char): boolean;
function Log(X, Base: single): single;
function ChangeFilePostfix(lInName,lPostfix: string): string;
{$IFNDEF FPC}
function DiskFreeEx (DriveStr: String): Int64;
{$ELSE}
function DiskFreeEx (DriveStr: String): Int64;
{$ENDIF}
procedure SortSingle(var lLo,lHi: single);
procedure SortInteger(var lLo,lHi: integer);
procedure CopyFileEX (lInName,lOutName: string);
procedure CopyFileEXoverwrite (lInName,lOutName: string);
procedure fx (a: double); overload; //fx used to help debugging - reports number values
procedure fx (a,b: double); overload;
procedure fx (a,b,c: double); overload;
procedure fx (a,b,c,d: double); overload;
function Swap2(s: smallint): smallint;
function ChangeFilePostfixExt (lInName,lPostfix,lExt: string): string;
procedure SortCutout (var lCutout : TCutout); //ensure Lo < Hi
function freeRam: Int64;

function OKMsg(lMsg: string): boolean; //shows dialog with OK/Cancel returns true if user presses OK
function DirExists (lFolderName: String): boolean;
function FilenameParts (lInName: string; var lPath,lName,lExt: string): boolean;
function AddIndexToFilename (lInName: string; lIndex: integer): string;

procedure createArray64 (var ptr: pointer; var ra :Doublep0; Sz: integer); overload;
procedure createArray64 (var ptr: pointer; var ra :Doublep; Sz: integer); overload;
function GzExt(lFileName: string): boolean;
function ChangeFilePrefixExt (lInName,lPrefix,lExt: string): string;
function ChangeFilePrefix(lInName,lPrefix: string): string;
function makesmallint (b0,b1: byte): smallint;
function makesingle( b0,b1,b2,b3: byte): single;
procedure SortInt (var lMin,lMax: integer);
function Bound (lDefault,lMin,lMax: integer): integer;
function IsNiftiExt(lStr: string): boolean;
function IsVOIExt(lStr: string): boolean;
function StrToUnitRect (lS: string; var lU: TUnitRect): boolean;
function UnitRectToStr (lU: TUnitRect) : string;
function CreateUnitRect (L,T,R,B: single) : TUnitRect;
procedure SensibleUnitRect (var U: TUnitRect);
function StrToRGBA(lS: string; var lU: glRGBaQUAD): boolean;
function RGBAToStr (lU: glRGBaQUAD) : string;
function RGBA(lR,lG,lB,lA: byte): TGLRGBQuad;
procedure EnsureDirExists (var lFilename: string);
function FloatMaxVal (lA,lB,lC: single): single;
function FloatMinVal (lA,lB,lC: single): single;
function ResetIniDefaults : boolean;

implementation

procedure ShowDebug (lS: AnsiString);
begin
{$IFDEF UNIX}
  writeln(lS);
{$ELSE}
  {$IFDEF GUI}
  showmessage(lS);
  {$ELSE}
  writeln(lS);

  {$ENDIF}
{$ENDIF}
end;

function ResetIniDefaults : boolean;
const
     {$IFDEF LINUX}
     kKey = 'Right button';
     {$ELSE}
     kKey = 'Shift key';
     {$ENDIF}
var
   lKey: boolean;
begin
     result := false;
{$IFDEF GUI}
     {$IFDEF LINUX}

     lKey := ((GetKeyState(VK_RBUTTON) And $80)<>0) or ((GetKeyState(VK_SHIFT) And $80)<>0);
     {$ELSE}
     lKey := (ssShift in KeyDataToShiftState(vk_Shift));
     {$ENDIF}
     if not lKey then
        exit;
     {$IFDEF GUI}
    	case MessageDlg(kKey+' down during launch: do you want to reset the default preferences?', mtConfirmation,
				[mbYes, mbNo], 0) of	{ produce the message dialog box }
				idYes: result := true;
        end; //case
     {$ENDIF}
{$ENDIF}
end;

const
  kStrSep = '|';
(*function ColorToStr (lU: TColor) : string;
//floatrect values 0..1 convert to byte 0..1
begin
  lU.
  result := Inttostr(lU.rgbRed)+ kStrSep+Inttostr(lU.rgbGreen)+ kStrSep+Inttostr(lU.rgbBlue)+ kStrSep+Inttostr(lU.rgbReserved);
end;*)
function FloatMaxVal (lA,lB,lC: single): single;
//returns largest of three values
begin
  if (lA > lB) and (lA > lC) then
    result := lA
  else if lB > lC then
    result := lB
  else
    result := lC;
end; //func FloatMaxVal

function FloatMinVal (lA,lB,lC: single): single;
//returns largest of three values
begin
  if (lA < lB) and (lA < lC) then
    result := lA
  else if lB < lC then
    result := lB
  else
    result := lC;
end; //func FloatMaxVal
procedure EnsureDirExists (var lFilename: string);
begin
  if direxists (extractfiledir(lFilename)) then
    exit;
end;



function RGBA(lR,lG,lB,lA: byte): TGLRGBQuad;
//set red,green,blue and alpha of a Quad
begin
  result.rgbRed := lR;
  result.rgbGreen := lG;
  result.rgbBlue := lB;
  result.rgbreserved := lA;
end;

function RGBAToStr (lU: glRGBaQUAD) : string;
//floatrect values 0..1 convert to byte 0..1
begin
  result := Inttostr(lU.rgbRed)+ kStrSep+Inttostr(lU.rgbGreen)+ kStrSep+Inttostr(lU.rgbBlue)+ kStrSep+Inttostr(lU.rgbReserved);
end;

function ByteBound (lV: integer): Byte;
begin
  if lV < 0 then
    result := 0
  else if lV > 255 then
    result := 255
  else
    result := lV;
end;
function StrToRGBA(lS: string; var lU: glRGBaQUAD): boolean;
var
  lV: string;
  lI: byte;
  lLen,lP,lN: integer;
begin
  result := false;
  lLen := length(lS);
  if lLen < 7 then  //shortest possible: 1|1|1|1 or 0|0|0|0
    exit;
  //read values
  lV := '';
  lP := 1;
  lN := 0;
  while (lP <= lLen) do begin
    if lS[lP] in ['0'..'9'] then
      lV := lV + lS[lP];
    if (lV <> '') and ((lP = lLen) or (not (lS[lP] in ['0'..'9']))) then begin
        inc(lN);
        lI := ByteBound(strtoint(lV));
        case lN of
          1: lU.rgbRed := lI;
          2: lU.rgbGreen := lI;
          3: lU.rgbBlue := lI;
          4: lU.rgbReserved := lI;
        end;
        lV := '';
    end;
    inc(lP);
  end;
  if lN >= 4 then
    result := true;
end;

function UnitBound (lS: single): single;
begin
  if lS < 0 then
    result := 0
  else if lS > 1 then
    result := 1
  else
    result := lS;
end;

function CreateUnitRect (L,T,R,B: single) : TUnitRect;
begin
  result.L := UnitBound(L);
  result.T := UnitBound(T);
  result.R := UnitBound(R);
  result.B := UnitBound(B);
end;

procedure SensibleUnitRect (var U: TUnitRect);
begin
  U.L := UnitBound(U.L);
  U.T := UnitBound(U.T);
  U.R := UnitBound(U.R);
  U.B := UnitBound(U.B);
  //left should be lower value than right
  SortSingle(U.L,U.R);
  if U.L = U.R then begin
    if U.R < 0.1 then
      U.R := 0.1
    else
      U.L := U.R -0.1;
  end;
  //bottom should lower value than top
  SortSingle(U.B,U.T);
  if U.B = U.T then begin
    if U.T < 0.1 then
      U.T := 0.1
    else
      U.B := U.T -0.1;
  end;
end;

function UnitToByteStr (lS: single): string;
begin
  result := inttostr(round(255 * UnitBound(lS)));
end;

function UnitRectToStr (lU: TUnitRect) : string;
//floatrect values 0..1 convert to byte 0..1
begin
  result := UnitToByteStr(lU.L)+ kStrSep+UnitToByteStr(lU.T)+ kStrSep+UnitToByteStr(lU.R)+ kStrSep+UnitToByteStr(lU.B);
end;

function StrToUnitRect (lS: string; var lU: TUnitRect): boolean;
var
  lQ : glRGBaQUAD;
begin
  result := false;
  if not StrToRGBA(lS,lQ) then
    exit;
  lU.L := lQ.rgbRed / 255;
  lU.T := lQ.rgbGreen / 255;
  lU.R := lQ.rgbBlue / 255;
  lU.B := lQ.rgbReserved / 255;
  result := true;
end;
(*function StrToUnitRect (lS: string; var lU: TUnitRect): boolean;
var
  lV: string;
  lF: single;
  lLen,lP,lN: integer;
begin
  result := false;
  lLen := length(lS);
  if lLen < 7 then  //shortest possible: 1|1|1|1 or 0|0|0|0
    exit;
  //read values
  lV := '';
  lP := 1;
  lN := 0;
  while (lP <= lLen) do begin
    if lS[lP] in ['0'..'9'] then
      lV := lV + lS[lP];
    if (lV <> '') and ((lP = lLen) or (not (lS[lP] in ['0'..'9']))) then begin
        inc(lN);
        lF := UnitBound(strtoint(lV)/255);
        case lN of
          1: lU.L := lF;
          2: lU.T := lF;
          3: lU.R := lF;
          4: lU.B := lF;
        end;
        lV := '';
    end;
    inc(lP);
  end;
  if lN >= 4 then
    result := true;
end;    *)

(*procedure axl(lstr: string);
var
  lOutname: string;
  f: TextFile;
begin
  //lStr := floattostr(a)+'x'+floattostr(b)+'x'+floattostr(c)+'x'+floattostr(d)+' : '+floattostr(e)+'x'+floattostr(fx);
  lOutname:='c:\dx.txt';
  if fileexists(lOutname) then
  begin                    { open a text file }
    AssignFile(f, lOutname);
    Append(f);
    Writeln(f, lStr);
    Flush(f);  { ensures that the text was actually written to file }
    { insert code here that would require a Flush before closing the file }
    CloseFile(f);
  end;
end;



procedure ax(a,b,c,d,e,fx: double);
var
  lOutname,lStr: string;
  f: TextFile;
begin
  lStr := floattostr(a)+'x'+floattostr(b)+'x'+floattostr(c)+'x'+floattostr(d)+' : '+floattostr(e)+'x'+floattostr(fx);
  lOutname:='c:\dx.txt';
  if fileexists(lOutname) then
  begin                    { open a text file }
    AssignFile(f, lOutname);
    Append(f);
    Writeln(f, lStr);
    Flush(f);  { ensures that the text was actually written to file }
    { insert code here that would require a Flush before closing the file }
    CloseFile(f);
  end;
end; *)


function AddIndexToFilename (lInName: string; lIndex: integer): string;
var lPath,lName,lExt: string;
begin
     result := '';
     if not FilenameParts (lInName, lPath,lName,lExt) then exit;
     result := lPath+lName+inttostr(lIndex)+lExt;
end;

function Bound (lDefault,lMin,lMax: integer): integer;
begin
    result := lDefault;
    if result < lMin then
       result := lMin;
    if result > lMax then
       result := lMax;
end;

function IsVOIExt(lStr: string): boolean;
var
   lExt: string;
begin
     result := false;
     lExt := UpCaseExt(lStr);
     if (lExt = '.VOI')  then
        result := true;
end;
function IsNiftiExt(lStr: string): boolean;
var
   lExt: string;
begin
     result := false;
     lExt := UpCaseExt(lStr);
     if (lExt = '.NII') or (lExt = '.NII.GZ') then
        result := true;
     if (lExt = '.HDR') and (FSize(ChangeFileExt(lStr,'.img'))> 0) then
        result := true;
     if (lExt = '.IMG') and (FSize(ChangeFileExt(lStr,'.hdr'))> 0) then
        result := true;
end;

procedure SortInt (var lMin,lMax: integer);
var
   lSwap: integer;
begin
     if lMin <= lMax then
        exit;
     lSwap := lMax;
     lMax := lMin;
     lMin := lSwap;
end;

function makesmallint (b0,b1: byte): smallint;
type
  swaptype = packed record
    case byte of
      0:(b0,b1 : byte); //word is 16 bit
      1:(s:smallint);
  end;
  swaptypep = ^swaptype;
var
  //inguy:swaptypep;
  outguy:swaptype;
begin
  //inguy := @s; //assign address of s to inguy
  outguy.b0 := b0;
  outguy.b1 := b1;
  result:=outguy.s;
end;//makesmallint


function makesingle( b0,b1,b2,b3: byte): single;
type
  swaptype = packed record
    case byte of
      0:(b0,b1,b2,b3 : byte); //word is 16 bit
      1:(long:longint);
  end;
  swaptypep = ^swaptype;
var
  outguy:swaptype;
begin
  //inguy := @s; //assign address of s to inguy
  outguy.b0 := b0;
  outguy.b1 := b1;
  outguy.b2 := b2;
  outguy.b3 := b3;
  result:=outguy.long;
end;//swap4r4i

function ChangeFilePrefix(lInName,lPrefix: string): string;
var
   lP,lN,lX: string;
begin

  FilenameParts(lInName,lP,lN,lX);
  result := lP+lPrefix+lN+lX;

end;

function ChangeFilePrefixExt (lInName,lPrefix,lExt: string): string;
var
   lP,lN,lX: string;
begin
    FilenameParts(lInName,lP,lN,lX);
  result := lP+lPrefix+lN+lExt;

end;

(*function ChangeFilePrefix(lInName,lPrefix: string): string;
var
	lC,lLen,lPos: integer;
	lStr: string;
begin

	//result := changefileext(lInName,lExt);
        result := lInName;
	lLen := length (result);
	if lLen < 1 then exit;
	lPos := lLen;
	while (lPos > 1) and (result[lPos] <> pathdelim) do
		dec(lPos);
	lStr := '';
	for lC := 1 to lPos do
		lStr := lStr+result[lC];
	lStr := lStr+lPrefix;
	if lPos < lLen then
		for lC := (lPos+1) to lLen do
			lStr := lStr+result[lC];
	result := lStr;
end;

function ChangeFilePrefixExt (lInName,lPrefix,lExt: string): string;
var
	lC,lLen,lPos: integer;
	lStr: string;
begin
	result := changefileext(lInName,lExt);
	lLen := length (result);
	if lLen < 1 then exit;
	lPos := lLen;
	while (lPos > 1) and (result[lPos] <> pathdelim) do
		dec(lPos);
	lStr := '';
	for lC := 1 to lPos do
		lStr := lStr+result[lC];
	lStr := lStr+lPrefix;
	if lPos < lLen then begin
            lC := lPos+1;
            while (lC <= lLen) and (result[lC] <> '.') do begin
                  lStr := lStr + result[lC];
                  inc(lC);
            end;
        end;
        lStr := lStr + lExt;
	result := lStr;
end; *)


function GzExt(lFileName: string): boolean;
var lExt: string;
begin
     lExt := UpCaseExt(lFilename);
     if (lExt = '.VOI') or (lExt = '.NII.GZ') or (lExt = '.GZ') then
        result := true
     else
         result := false;
end;

function FilenameParts (lInName: string; var lPath,lName,lExt: string): boolean;
var
   lLen,lPos,lExtPos,lPathPos: integer;
begin
    result := false;
    lPath := '';
    lName := '';
    lExt := '';
    lLen := length(lInName);
    if lLen < 1 then
       exit;
    if DirExists(lInName) then begin //we have been passed a folder, not a file
       if lInName[lLen] = PathDelim then
          lPath := lInName
       else
           lPath := lInName + pathdelim;
       exit;
    end;
    //next find final pathdelim
    lPathPos := lLen;
    while (lPathPos > 0) and (lInName[lPathPos] <> '\') and (lInName[lPathPos] <> '/') do
          dec(lPathPos);
    if (lInName[lPathPos] = '\') or (lInName[lPathPos] = '/') then begin
       for lPos := 1 to lPathPos do
           lPath := lPath + lInName[lPos];
    end;
    // else
    //    dec(lPathPos);
    inc(lPathPos);
    //next find first ext
    //lExtPos := 1;
    lExtPos := length(lPath);//July 2009 -- beware of '.' in foldername...
    while (lExtPos <= lLen) and (lInName[lExtPos] <> '.') do
          inc(lExtPos);
    if (lInName[lExtPos] = '.')  then begin
       for lPos := lExtPos to lLen do
           lExt := lExt + lInName[lPos];
    end;
    // else
    //    inc(lExtPos);
    dec(lExtPos);
    //next extract filename
    //fx(lPathPos,lExtPos);
    if (lPathPos <= lExtPos) then
       for lPos := lPathPos to lExtPos do
           lName := lName + lInName[lPos];
    result := true;

end;


procedure createArray64 (var ptr: pointer; var ra :Doublep0; Sz: integer); overload;
var i: integer;
begin
	 getmem(ptr,16+(sizeof(double)*Sz));
         {$IFDEF FPC}
         ra := align(ptr,16);
         {$ELSE}
	 ra := DoubleP0((integer(ptr) and $FFFFFFF0)+16);
         {$ENDIF}
        for i := (Sz-1) downto 0 do //initialise array
		 ra^[i] := 0;
end;

procedure createArray64 (var ptr: pointer; var ra :Doublep; Sz: integer); overload;
var i: integer;
begin
	 getmem(ptr,16+(sizeof(double)*Sz));
         {$IFDEF FPC}
         ra := align(ptr,16);
         {$ELSE}
	 ra := DoubleP((integer(ptr) and $FFFFFFF0)+16);
         {$ENDIF}
         for i := (Sz) downto 1 do //initialise array
		 ra^[i] := 0;
end;


function OKMsg(lMsg: string): boolean; //shows dialog with OK/Cancel returns true if user presses OK
begin
     result := false;
     {$IFDEF GUI}
	 case MessageDlg(lMsg, mtConfirmation,
		[mbYes, mbCancel], 0) of
		mrCancel: exit;
     end; //case
     {$ELSE}
     showmsg('Assuming you agree with this statement: '+lMsg);
     (*case MsgDlg(lMsg, mtConfirmation,
		[mbYes, mbCancel], 0) of
		mrCancel: exit;
     end; //case *)
     {$ENDIF}
     result := true;
end;

(*function DirExists (lDir: String): boolean;
var lSearchRec: TSearchRec;
begin
  FindFirst(lDir, faAnyFile, lSearchRec);
    if (faDirectory and lSearchRec.attr) = faDirectory then
       DirExists := true
    else
        DirExists := false;
  FindClose(lSearchRec);{}
end;*)
function DirExists (lFolderName: string): boolean;
{$IFDEF oldFPC} //bug now fixed
var
	lSearchRec: TSearchRec;
begin
     result := false;
  if fileexists(lFoldername) then //File not folder
     exit;
  Filemode := 0; //readonly
	 if FindFirst(lFolderName, faDirectory, lSearchRec) = 0 then begin
	    result := true;
            FindClose(lSearchRec);
	 end else
	     result := false; //some files found
	 Filemode := 2;
{$ELSE}
begin
         result := DirectoryExists(lFolderName);
{$ENDIF}
end;

function freeRam: Int64;
{$IFDEF UNIX}
begin
     result := maxint;
end;
{$ELSE}
var
  memory:TMemoryStatus;

begin
  memory.dwLength:=sizeof(memory);
  GlobalMemoryStatus(memory);
  result := memory.dwavailPhys;
  //result := 1024;
end;
{$ENDIF}

procedure SortCutout (var lCutout : TCutout); //ensure Lo < Hi
var lInc,lSwap: integer;
begin
	 for lInc := 1 to 3 do
		if lCutout.Lo[lInc] > lCutout.Hi[lInc] then begin
			lSwap := lCutout.Lo[lInc];
			lCutout.Lo[lInc] := lCutout.Hi[lInc];
			lCutout.Hi[lInc] := lSwap;
		end;
end;

function ChangeFilePostfix(lInName,lPostfix: string): string;
var
   lPath,lName,lExt: string;
begin
     FilenameParts (lInName, lPath,lName,lExt);
     result := lPath+lName+lPostFix+lExt;
     //showmessage(result);
end;

function ChangeFilePostfixExt (lInName,lPostfix,lExt: string): string;
var
   lPath,lName,lExtIn: string;
begin
     FilenameParts (lInName, lPath,lName,lExtIn);
     result := lPath+lName+lPostFix+lExt;
     //showmessage(result);
end;

(*var
	lC,lLen,lPos: integer;
	lStr: string;
begin
	result := changefileext(lInName,lExt);
	lLen := length (result);
	if lLen < 1 then exit;
	lPos := lLen;
	while (lPos > 1) and (result[lPos] <> pathdelim) and (result[lPos] <> '.') do
		dec(lPos);
        if result[lPos] = '.' then
           dec(lPos);
	lStr := '';
	for lC := 1 to lPos do
		lStr := lStr+result[lC];
	lStr := lStr+lPostfix;
	if lPos < lLen then
		for lC := (lPos+1) to lLen do
			lStr := lStr+result[lC];
	result := lStr;
end;     *)

(*procedure ApplySaveDlgFilter (lSaveDlg: TSaveDialog);
var
   lLen,lPos,lPipes,lPipesReq: integer;
   lExt: string;
begin
     lPipesReq := (lSaveDlg.FilterIndex * 2)-1;
     if lPipesReq < 1 then exit;
     lLen := length(lSaveDlg.Filter);
     lPos := 1;
     lPipes := 0;
     while (lPos < lLen) and (lPipes < lPipesReq) do begin
           if lSaveDlg.Filter[lPos] = '|' then
              inc(lPipes);
           inc(lPos);
     end;
     if (lPos >= lLen) or (lPipes < lPipesReq) then
        exit;
     lExt := '';
     while (lPos <= lLen) and (lSaveDlg.Filter[lPos] <> '|') do begin
           if lSaveDlg.Filter[lPos] <> '*' then
              lExt := lExt + lSaveDlg.Filter[lPos];
           inc(lPos);
     end;
     if lExt <> '' then
        lSaveDlg.Filename := ChangeFileExt(lSaveDlg.Filename,lExt);
end;  *)

(*function DefaultsDir (lSubFolder: string): string;
//for Linux: DefaultsDir is ~/appname/SubFolder/, e.g. /home/username/mricron/subfolder/
//for Windows: DefaultsDir is in the location of the executable, e.g. c:\program files\mricron\subfolder\
//Note: Final character is pathdelim
var
   lBaseDir: string;
begin
     {$IFDEF Unix}
     lBaseDir := GetEnvironmentVariable ('HOME')+pathdelim+'.' +ParseFileName(ExtractFilename(paramstr(0) ) );
     if not DirectoryExists(lBaseDir) then begin
        {$I-}
        MkDir(lBaseDir);
        if IOResult <> 0 then begin
               showmessage('Unble to create new folder '+lBaseDir);
        end;
        {$I+}
     end;
     lBaseDir := lBaseDir+pathdelim;
     {$ELSE}
     lBaseDir := extractfiledir(paramstr(0))+pathdelim;
     {$ENDIF}
     //if not DirectoryExists(extractfiledir(lBaseDir)) then
     //mkDir(extractfiledir(lBaseDir));
     if lSubFolder <> '' then begin
         lBaseDir := lBaseDir + lSubFolder;
         if not DirectoryExists(lBaseDir) then begin
            {$I-}
            MkDir(lBaseDir);
            if IOResult <> 0 then begin
               showmessage('Unable to create new folder '+lBaseDir);
            end;
            {$I+}
         end;
         result := lBaseDir + pathdelim;
     end else
         result := lBaseDir;
end; *)

function Swap2(s : SmallInt): smallint;
type
  swaptype = packed record
    case byte of
      0:(Word1 : word); //word is 16 bit
      1:(Small1: SmallInt);
  end;
  swaptypep = ^swaptype;
var
  inguy:swaptypep;
  outguy:swaptype;
begin
  inguy := @s; //assign address of s to inguy
  outguy.Word1 := swap(inguy^.Word1);
  result :=outguy.Small1;
end;


procedure fx (a: double); overload; //fx used to help debugging - reports number values
begin
    {$IFDEF GUI}
	showmessage(floattostr(a));
    {$ELSE}
	msg(floattostr(a));
    {$ENDIF}
end;

procedure fx (a,b: double); overload; //fx used to help debugging - reports number values
begin
    {$IFDEF GUI}
	showmessage(floattostr(a)+'x'+floattostr(b));
    {$ELSE}
	msg(floattostr(a)+'x'+floattostr(b));
    {$ENDIF}
end;

procedure fx (a,b,c: double); overload; //fx used to help debugging - reports number values
begin
    {$IFDEF GUI}
	showmessage(floattostr(a)+'x'+floattostr(b)+'x'+floattostr(c));
    {$ELSE}
	msg(floattostr(a)+'x'+floattostr(b)+'x'+floattostr(c));
    {$ENDIF}
end;

procedure fx (a,b,c,d: double); overload; //fx used to help debugging - reports number values
begin
    {$IFDEF GUI}
	showmessage(floattostr(a)+'x'+floattostr(b)+'x'+floattostr(c)+'x'+floattostr(d));
    {$ELSE}
	msg(floattostr(a)+'x'+floattostr(b)+'x'+floattostr(c)+'x'+floattostr(d));
    {$ENDIF}
end;

procedure CopyFileEXoverwrite (lInName,lOutName: string);
var lFSize: Integer;
   lBuff: bytep0;
   lFData: file;
begin
	 lFSize := FSize(lInName);
	 if (lFSize < 1)  then exit;
	 assignfile(lFdata,lInName);
	 filemode := 0;
	 reset(lFdata,lFSize{1});
	 GetMem( lBuff, lFSize);
	 BlockRead(lFdata, lBuff^, 1{lFSize});
	 closefile(lFdata);
	 assignfile(lFdata,lOutName);
	 filemode := 2;
	 Rewrite(lFdata,lFSize);
	 BlockWrite(lFdata,lBuff^, 1  {, NumWritten});
	 closefile(lFdata);
	 freemem(lBuff);
end;

procedure CopyFileEX (lInName,lOutName: string);
var lFSize: Integer;
begin
	 lFSize := FSize(lInName);
	 if (lFSize < 1) or (fileexistsEX(lOutName)) then exit;
	CopyFileEXoverwrite (lInName,lOutName);
end;

procedure SortInteger(var lLo,lHi: integer);
var lSwap: integer;
begin
	if lLo > lHi then begin
		lSwap := lLo;
		lLo := lHi;
		lHi := lSwap;
	end; //if Lo>Hi
end; //proc SortInteger

procedure SortSingle(var lLo,lHi: single);
var lSwap: single;
begin
	if lLo > lHi then begin
		lSwap := lLo;
		lLo := lHi;
		lHi := lSwap;
	end; //if Lo>Hi
end; //proc SortSingle


{$IFDEF FPC}
   {$IFDEF UNIX} //FPC and Unix
   function DiskFreeEx (DriveStr: String): Int64;
   var
      lOutDisk: Integer;
   begin

     //lOutDisk :=  AddDisk(DriveStr);
     //result := DiskFree(lOutDisk);
     result := maxint;
   end;
   {$ELSE} //FPC and Windows
   function DiskFreeEx (DriveStr: String): Int64;
   var
      lOutDisk: Integer;
   begin
     lOutDisk := ord(upcase(DriveStr[1]))+1-ord('A');
     if (lOutDisk >= 0) and (lOutDisk <= 26) then
        result := DiskFree(lOutDisk)
     else
         result := 0;
     //showmessage(DriveStr+'->*'+inttostr(lOutDisk)+'*  :'+inttostr(result));
     //showmessage(inttostr(DiskFree(0){current drive})+'  :'+inttostr(DiskFree(3) {C drive}));
   end;
   {$ENDIF}
{$ELSE} //Delphi Windows
   function DiskFreeEx (DriveStr: String): Int64;
   var
      lOutDisk: Integer;
   begin
     lOutDisk := ord(upcase(DriveStr[1]))+1-ord('A');
     if (lOutDisk >= 0) and (lOutDisk <= 26) then
        result := DiskFree(lOutDisk)
     else
         result := 0;
     //showmessage(DriveStr+'->*'+inttostr(lOutDisk)+'*  :'+inttostr(result));
     //showmessage(inttostr(DiskFree(0){current drive})+'  :'+inttostr(DiskFree(3) {C drive}));
   end;
  {$ENDIF}

function Log(X, Base: single): single;
begin
  if X = 0 then
	result := 0
  else
  	Log := Ln(X) / Ln(Base);
end;

function Bool2Char (lBool: boolean): char;
begin
	if lBool then
		result := '1'
	else
		result := '0';
end;

function Char2Bool (lChar: char): boolean;
begin
	if lChar = '1' then
		result := true
	else
		result := false;
end;

procedure Xswap4r ( var s:single);
type
  swaptype = packed record
	case byte of
	  0:(Word1,Word2 : word); //word is 16 bit
  end;
  swaptypep = ^swaptype;
var
  inguy:swaptypep;
  outguy:swaptype;
begin
  inguy := @s; //assign address of s to inguy
  outguy.Word1 := swap(inguy^.Word2);
  outguy.Word2 := swap(inguy^.Word1);
  inguy^.Word1 := outguy.Word1;
  inguy^.Word2 := outguy.Word2;
end;

procedure swap4(var s : LongInt);
type
  swaptype = packed record
    case byte of
      0:(Word1,Word2 : word); //word is 16 bit
      1:(Long:LongInt);
  end;
  swaptypep = ^swaptype;
var
  inguy:swaptypep;
  outguy:swaptype;
begin
  inguy := @s; //assign address of s to inguy
  outguy.Word1 := swap(inguy^.Word2);
  outguy.Word2 := swap(inguy^.Word1);
  s:=outguy.Long;
end;


function UpCaseExt(lFileName: string): string;
var lI: integer;
l2ndExt,lExt : string;
begin
	 lExt := ExtractFileExt(lFileName);
	 if length(lExt) > 0 then
		for lI := 1 to length(lExt) do
			lExt[lI] := upcase(lExt[lI]);
	 result := lExt;
	 if lExt <> '.GZ' then exit;
	 lI := length(lFileName) - 6;
	 if li < 1 then exit;
	 l2ndExt := upcase(lFileName[lI])+upcase(lFileName[lI+1])+upcase(lFileName[li+2])+upcase(lFileName[li+3]);
	 if l2ndExt = '.NII' then
		result := '.NII.GZ'
end;

function ExtGZ (lFilename: string): boolean;
var
   lI: integer;
         lExt : string;
begin
     lExt := ExtractFileExt(lFileName);
     if length(lExt) > 0 then
     for lI := 1 to length(lExt) do
         lExt[lI] := upcase(lExt[lI]);
     if lExt = '.GZ' then
        result := true
     else
         result := false;
end;

function RealToStr(lR: double {was extended}; lDec: integer): string;
begin
     RealTOStr := FloatToStrF(lR, ffFixed,7,lDec);
end;

FUNCTION specialdouble (d:double): boolean;
//returns true if s is Infinity, NAN or Indeterminate
//8byte IEEE: msb[63] = signbit, bits[52-62] exponent, bits[0..51] mantissa
//exponent of all 1s =   Infinity, NAN or Indeterminate
CONST kSpecialExponent = 2047 shl 20;
VAR Overlay: ARRAY[1..2] OF LongInt ABSOLUTE d;
BEGIN
  IF ((Overlay[2] AND kSpecialExponent) = kSpecialExponent) THEN
     RESULT := true
  ELSE
      RESULT := false;
END;

function swap8r(s : double):double;
type
  swaptype = packed record
    case byte of
      0:(Word1,Word2,Word3,Word4 : word); //word is 16 bit
      1:(float:double);
  end;
  swaptypep = ^swaptype;
var
  inguy:swaptypep;
  outguy:swaptype;
begin
  inguy := @s; //assign address of s to inguy
  outguy.Word1 := swap(inguy^.Word4);
  outguy.Word2 := swap(inguy^.Word3);
  outguy.Word3 := swap(inguy^.Word2);
  outguy.Word4 := swap(inguy^.Word1);
  try
    result:=outguy.float;
  except
        result := 0;
        exit;
  end;
end; //func swap8r

procedure pswap4i(var s : LongInt);
type
  swaptype = packed record
    case byte of
      0:(Word1,Word2 : word); //word is 16 bit
      1:(Long:LongInt);
  end;
  swaptypep = ^swaptype;
var
  inguy:swaptypep;
  outguy:swaptype;
begin
  inguy := @s; //assign address of s to inguy
  outguy.Word1 := swap(inguy^.Word2);
  outguy.Word2 := swap(inguy^.Word1);
  s:=outguy.Long;
end; //proc swap4

function swap64r(s : double):double;
type
  swaptype = packed record
    case byte of
      0:(Word1,Word2,Word3,Word4 : word); //word is 16 bit
      1:(float:double);
  end;
  swaptypep = ^swaptype;
var
  inguy:swaptypep;
  outguy:swaptype;
begin
  inguy := @s; //assign address of s to inguy
  outguy.Word1 := swap(inguy^.Word4);
  outguy.Word2 := swap(inguy^.Word3);
  outguy.Word3 := swap(inguy^.Word2);
  outguy.Word4 := swap(inguy^.Word1);
  try
    swap64r:=outguy.float;
  except
        swap64r := 0;
        exit;
  end;{}
end;

procedure pswap4r ( var s:single);
type
  swaptype = packed record
    case byte of
      0:(Word1,Word2 : word); //word is 16 bit
  end;
  swaptypep = ^swaptype;
var
  inguy:swaptypep;
  outguy:swaptype;
begin
  inguy := @s; //assign address of s to inguy
  outguy.Word1 := swap(inguy^.Word2);
  outguy.Word2 := swap(inguy^.Word1);
  inguy^.Word1 := outguy.Word1;
  inguy^.Word2 := outguy.Word2;
end; //proc Xswap4r

function conv4r4i (s:single): longint;
type
  swaptype = packed record
    case byte of
      1:(long:longint);
  end;
  swaptypep = ^swaptype;
var
  inguy:swaptypep;
begin
  inguy := @s; //assign address of s to inguy
  conv4r4i:=inguy^.long;
end;

function swap4r4i (s:single): longint;
type
  swaptype = packed record
    case byte of
      0:(Word1,Word2 : word); //word is 16 bit
      1:(long:longint);
  end;
  swaptypep = ^swaptype;
var
  inguy:swaptypep;
  outguy:swaptype;
begin
  inguy := @s; //assign address of s to inguy
  outguy.Word1 := swap(inguy^.Word2);
  outguy.Word2 := swap(inguy^.Word1);
  swap4r4i:=outguy.long;
end;//swap4r4i

(*function ChangeFileExtX( var lFilename: string; lExt: string): string;
begin
    result := ChangeFileExt(lFilename,lExt);
end;      *)
function ChangeFileExtX( lFilename: string; lExt: string): string;
//sees .nii.gz as single extension
var
   lPath,lName,lOrigExt: string;
begin
     if FilenameParts (lFilename, lPath,lName,lOrigExt) then begin
        result := lPath+lName+lExt;
     end else begin
         //showmessage('z');
         result := ChangeFileExt(lFilename,lExt);
     end;
end;

function PadStr (lValIn, lPadLenIn: integer): string;
var lOrigLen,lPad : integer;
begin
 lOrigLen := length(inttostr(lValIn));
 result := inttostr(lValIn);
 if lOrigLen < lPadLenIn then begin
    lOrigLen := lPadLenIn-lOrigLen;
    for lPad := 1 to lOrigLen do
        result := '0'+result;
 end;
end;

function ExtractFileDirWithPathDelim(lInFilename: string): string;
//F:\filename.ext -> 'F:\' and F:\dir\filename.ext -> 'F:\dir\'
//Despite documentation, Delphi3's ExtractFileDir does not always retain final pathdelim
var lFilePath: string;
begin
     result := '';
     lFilePath := ExtractFileDir(lInFilename);
     if length(lFilepath) < 1 then exit;
     if lFilePath[length(lFilepath)] <> pathdelim then
        lFilepath := lFilepath + pathdelim; //Delphi3 bug: sometimes forgets pathdelim
     result := lFilepath;
end;

function ParseFileFinalDir (lFileName:String): string;
var
   lLen,lInc,lPos: integer;
   lInName,lName: String;
begin
     lInName := extractfiledir(lFilename);
     lName := '';
     lLen := length(lInName);
     if  lLen < 1 then exit;
     lInc := lLen;
     repeat
		dec(lInc);
     until (lInName[lInc] = pathdelim) or (lInc = 1);
     if lInName[lInc] = pathdelim then inc(lInc); //if '\folder' then return 'folder'
	 for lPos := lInc to lLen do
		lName := lName + lInName[lPos];
	 ParseFileFinalDir := lName;
end;

function ParseFileName (lFilewExt:String): string;
var
   lLen,lInc: integer;
   lName: String;
begin
	lName := '';
     lLen := length(lFilewExt);
	lInc := lLen+1;
	 if  lLen > 0 then begin
	   repeat
			  dec(lInc);
		until (lFileWExt[lInc] = '.') or (lInc = 1);
		if (UpCaseExt(lFilewExt) = '.NII.GZ') and (lInc > 1) then
			repeat
			  dec(lInc);
			until (lFileWExt[lInc] = '.') or (lInc = 1);
	 end;
     if lInc > 1 then
        for lLen := 1 to (lInc - 1) do
            lName := lName + lFileWExt[lLen]
     else
         lName := lFilewExt; //no extension
        ParseFileName := lName;
end;

Function {TMainForm.}FileExistsEX(Name: String): Boolean;
var
   F: File;
begin
  result := false;
  if Name = '' then
     exit;
  result := FileExists(Name);
  if result then exit;
   //the next bit attempts to check for a file to avoid WinNT bug
   AssignFile(F, Name);
   {$I-}
   Reset(F);
   {$I+}
   Result:=IOresult = 0;
   if Result then
     CloseFile(F);
end;

function FSize (lFName: String): longint;
var SearchRec: TSearchRec;
begin
  result := 0;
  if not fileexistsex(lFName) then exit;
  FindFirst(lFName, faAnyFile, SearchRec);
  result := SearchRec.size;
  FindClose(SearchRec);
end;

procedure Xswap8r(var s : double);
type
  swaptype = packed record
    case byte of
      0:(Word1,Word2,Word3,Word4 : word); //word is 16 bit
      //1:(float:double);
  end;
  swaptypep = ^swaptype;
var
  inguy:swaptypep;
  outguy:swaptype;
begin
  inguy := @s; //assign address of s to inguy
  outguy.Word1 := swap(inguy^.Word4);
  outguy.Word2 := swap(inguy^.Word3);
  outguy.Word3 := swap(inguy^.Word2);
  outguy.Word4 := swap(inguy^.Word1);
  inguy^.Word1 := outguy.Word1;
  inguy^.Word2 := outguy.Word2;
  inguy^.Word3 := outguy.Word3;
  inguy^.Word4 := outguy.Word4;
end;

FUNCTION specialsingle (var s:single): boolean;
//returns true if s is Infinity, NAN or Indeterminate
//4byte IEEE: msb[31] = signbit, bits[23-30] exponent, bits[0..22] mantissa
//exponent of all 1s =   Infinity, NAN or Indeterminate
CONST kSpecialExponent = 255 shl 23;
VAR Overlay: LongInt ABSOLUTE s;
BEGIN
  IF ((Overlay AND kSpecialExponent) = kSpecialExponent) THEN
     RESULT := true
  ELSE
      RESULT := false;
END;

end.
