program template;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  SysUtils, CustApp,
  define_types,nii_core;

type
      TOpts =  RECORD //peristimulus plot
    Normalize: boolean;
  end;

type
   Ttemplate = class(TCustomApplication)



  protected
    procedure DoRun; override;
    procedure SetBool (const ParamName: string; var ParamVal: boolean);
    procedure SetInt (const ParamName: string; var ParamVal: integer);
    procedure SetFloat (const ParamName: string; var ParamVal: single);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp (Opts: TOpts); virtual;
  end;

procedure Ttemplate.SetBool (const ParamName: string; var ParamVal: boolean);
var
  S: string;
begin
  if not HasOption(ParamName) then exit;
  S :=GetOptionValue(ParamName);
  if length(S) > 0 then
     ParamVal := Char2Bool(S[1]);
  writeln(' '+ParamName+' set to  '+Bool2Char(ParamVal));
end;

procedure Ttemplate.SetInt (const ParamName: string; var ParamVal: integer);
var
  S: string;
begin
  if not HasOption(ParamName) then exit;
  S :=GetOptionValue(ParamName);
  if length(S) > 0 then
     ParamVal := StrToInt(S);
  writeln(' '+ParamName+' set to  '+IntToStr(ParamVal));
end;

procedure Ttemplate.SetFloat (const ParamName: string; var ParamVal: single);
var
  S: string;
begin
  if not HasOption(ParamName) then exit;
  S :=GetOptionValue(ParamName);
  if length(S) > 0 then
     ParamVal := StrToFloat(S);
  writeln(' '+ParamName+' set to  '+RealToStr(ParamVal,3));
end;

procedure DisplayMessages;
var
  i: integer;
begin
  if DebugStrings.Count > 0 then
     for i := 1 to DebugStrings.Count do
         writeln(DebugStrings[i-1]);
  //Memo1.Lines.AddStrings(DebugStrings);
  DebugStrings.Clear;
end;

function DefaultOpts: TOpts;
begin
  result.Normalize:=false;
end;

function processFile (lImageName: string;  lOptsx: TOpts): boolean;
var
  lTxtname,lOutname : string;
  f : file of byte;
  hdr,img,txt : array of byte;
  i,sz,tsz,tsz16: longint;
begin
  lOutname := ChangeFilePrefix(lImageName,'x');
  if not Fileexists(lImageName) then begin
     writeln('Unable to find '+lImageName);
     exit;
  end;
  lTxtname := ChangeFileExtX(lImageName,'.txt') ;
  if not Fileexists(lTxtname) then begin
     writeln('Unable to find '+lTxtname);
     exit;
  end;
  assignfile(f,lImageName);
  reset(f);
  sz := FileSize(f)-kNIIImgOffset ;
  if (sz < 0) then  begin
     writeln('File too small');
     exit;
  end;
  setlength(hdr,kNIIImgOffset);
  blockread(f,hdr[0],kNIIImgOffset);
  setlength(img,sz);
  blockread(f,img[0],sz);
  closefile(f);
  //read text
  assignfile(f,lTxtname);
  reset(f);
  tsz := FileSize(f);
  tsz16 := ((tsz+15) div 16) * 16;
  writeln('Text size is '+inttostr(tsz16)+' header offset should be '+inttostr(tsz16+ kNIIImgOffset) );
  setlength(txt,tsz16);
  for i := 0 to (tsz16-1) do
      txt[i] := ord(' ');
  blockread(f,txt[0],tsz);
  closefile(f);
  //write combined
  AssignFile(f,lOutname);
  ReWrite(f, 1);
  BlockWrite(f, hdr[0],kNIIImgOffset);
  BlockWrite(f, txt[0],tsz16);
  BlockWrite(f, img[0],sz);
  CloseFile(f);
end;

procedure Ttemplate.DoRun;
var
  ImgName: String;
  Opts: TOpts;
  //lImages: TStrings;
begin
  Opts := DefaultOpts;
  if (ParamCount = 0) or (HasOption('h','help')) then begin
    WriteHelp(Opts);
    Terminate;
    Exit;
  end;
  ImgName := Paramstr(ParamCount);
  if not FileExistsEX(ImgName) then begin
     Writeln('Unable to find NIfTI format image named '+ImgName);
     Terminate;
     Exit;
  end;
  SetBool('n',Opts.Normalize);
  DebugStrings.Clear;
  processFile(ImgName, Opts);
  DisplayMessages;
  // stop program loop
  Terminate;
end;

constructor Ttemplate.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor Ttemplate.Destroy;
begin
  inherited Destroy;
end;

procedure Ttemplate.WriteHelp (Opts: TOpts);
var
  E,I: string;
begin
  E := extractfilename(ParamStr(0));
  writeln('Usage: ',E,'[options] input.nii');
  writeln('Version: '+kVers+' by Chris Rorden');
  writeln('Options:');
  //writeln(' -a Arterial input voxels (default '+inttostr(Opts.AIFVox)+')');
  writeln(' -n Normalize (uses fsl, default '+Bool2Char(Opts.Normalize)+')');
{$IFDEF UNIX}
 I := ' ~/folder/p1.nii';
{$ELSE}
I := ' c:\folder\p1.nii';
  {$ENDIF}
  writeln(' '+E+I);
  writeln('  Process'+I+' with default parameters');
  writeln(' '+E+' -n 0 '+I);
  {$IFDEF UNIX}
   I := ' ''~/my folder/my img.nii''';
  {$ELSE}
  I := ' ''c:\my folder\my img.nii''';
    {$ENDIF}
    writeln(' '+E+I);
    writeln('  Process'+I+' with default parameters (spaces in folder or filename)');
end;


var
  Application: Ttemplate;

{$R *.res}

begin
  Application:=Ttemplate.Create(nil);
  Application.Run;
  Application.Free;
end.

