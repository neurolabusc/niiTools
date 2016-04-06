program perfx;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  SysUtils, CustApp,
  nii_perfx, define_types,nii_core;

type
   Tperf = class(TCustomApplication)
  protected
    procedure DoRun; override;
    procedure SetBool (const ParamName: string; var ParamVal: boolean);
    procedure SetInt (const ParamName: string; var ParamVal: integer);
    procedure SetFloat (const ParamName: string; var ParamVal: single);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp (Opts: TPerfOpts); virtual;
  end;

procedure Tperf.SetBool (const ParamName: string; var ParamVal: boolean);
var
  S: string;
begin
  if not HasOption(ParamName) then exit;
  S :=GetOptionValue(ParamName);
  if length(S) > 0 then
     ParamVal := Char2Bool(S[1]);
  writeln(' '+ParamName+' set to  '+Bool2Char(ParamVal));
end;

procedure Tperf.SetInt (const ParamName: string; var ParamVal: integer);
var
  S: string;
begin
  if not HasOption(ParamName) then exit;
  S :=GetOptionValue(ParamName);
  if length(S) > 0 then
     ParamVal := StrToInt(S);
  writeln(' '+ParamName+' set to  '+IntToStr(ParamVal));
end;

procedure Tperf.SetFloat (const ParamName: string; var ParamVal: single);
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

procedure Tperf.DoRun;
var
  ImgName: String;
  Opts: TPerfOpts;
  lImages: TStrings;
begin
  Opts := defaultperf;
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
  //SetInt('a',Opts.AIFVox);
  SetBool('b',Opts.BrainExtract);
  //SetFloat('e',Opts.TEmsec);
  SetInt('d',Opts.DeleteVols);
  SetInt('f',Opts.FinalVol);
  SetFloat('g',Opts.SmoothFWHMmm);
  SetInt('i',Opts.BaselineVols);
  SetFloat('m',Opts.MaskThreshSD);
  SetBool('n',Opts.Normalize);
  SetBool('r',Opts.MotionCorrect);
  SetInt('s',Opts.SliceTimeCorrect);
  SetFloat('t',Opts.TRSec);
  SetFloat('z',Opts.SmoothFWHMsec);
   DebugStrings.Clear;
   lImages := TstringList.Create;
   lImages.Add(ImgName);
   perfusionanalyze(lImages, Opts);
   DisplayMessages;
   lImages.Free;
  // stop program loop
  Terminate;
end;

constructor Tperf.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor Tperf.Destroy;
begin
  inherited Destroy;
end;

procedure Tperf.WriteHelp (Opts: TPerfOpts);
(*SmoothFWHMmm,SmoothFWHMsec,
TEmsec,TRSec, //time per volume (seconds)
AIFx0,
MaskThreshSD,//only include voxels where peak is more the ThreshSD*StDev(mask-baseline) more or less than that voxels baseline intensity
MinR2:single;
AIFVox,MaskVox,SliceTimeCorrect,DeleteVols,BaselineVols,FinalVol: integer;
AIFpeak,MotionCorrect,BrainExtract,PreMask,ComputeRaw,ComputeFitted,ConvertToConcentrationTime,Normalize: boolean;  *)
var
  E,I: string;
begin
  E := extractfilename(ParamStr(0));
  writeln('Usage: ',E,'[options] input.nii');
  writeln('Version: '+kVers+' by Chris Rorden');
  writeln('Options:');
  //writeln(' -a Arterial input voxels (default '+inttostr(Opts.AIFVox)+')');
  writeln(' -b Brain extraction (uses fsl, default '+Bool2Char(Opts.BrainExtract)+')');
  //writeln(' -e Echo time (ms, default '+realtostr(Opts.TEmsec,2)+')');
  writeln(' -d Delete volumes from start (default '+inttostr(Opts.DeleteVols)+')');
  writeln(' -f Final volume (default '+inttostr(Opts.FinalVol)+')');
  writeln(' -g Gaussian smooth (FWHM in mm, default '+realtostr(Opts.SmoothFWHMmm,2)+', 0 for none)');
  writeln(' -h Help (show these instructions)');
  writeln(' -i Initial baseline volumes (default '+inttostr(Opts.BaselineVols)+')');
  writeln(' -m Mask threshold for brain (StDev, default '+realtostr(Opts.MaskThreshSD,2)+')');
  writeln(' -n Normalize (uses fsl, default '+Bool2Char(Opts.Normalize)+')');
  writeln(' -r Realign to correct for motion (uses fsl, default '+Bool2Char(Opts.MotionCorrect)+')');
  writeln(' -s Slice order (default '+inttostr(Opts.SliceTimeCorrect)+', AutoDetect='+inttostr(kAutoDetect)+' Instant(Skip)='+inttostr(kSimultaneous)+' AscSeq='
  +inttostr(kAscending)+' AscInt='+inttostr(kAscendingInterleavedPhilGE)+' DescSeq='+inttostr(kDescending)+' DescInt='+inttostr(kDescendingInterleavedPhilGE)
  +' AscInt2,4,1,3='+inttostr(kAscendingInterleavedSiemens) +' DescInt3,1,4,2='+inttostr(kDescendingInterleavedSiemens)
  );
  writeln(' -t TR in seconds (default '+realtostr(Opts.TRSec,3)+', if zero uses TR from input.nii''s header)');
  writeln(' -z temporal filter (FWHM in sec, default '+realtostr(Opts.SmoothFWHMsec,3)+', zero to skip)');
  writeln('Examples:');
{$IFDEF UNIX}
 I := ' ~/folder/p1.nii';
{$ELSE}
I := ' c:\folder\p1.nii';
  {$ENDIF}
  writeln(' '+E+I);
  writeln('  Process'+I+' with default parameters');
  writeln(' '+E+' -b 0 -n 0 -r 0 '+I);
  writeln('  Process'+I+' without using FSL (e.g. SPM used to normalize and realign)');
  writeln(' '+E+' -e 28 -t 2.8 -s '+inttostr(kAscending)+I);
  writeln('  Process'+I+' with TE=28ms, TR=2800ms, ascending sequential slice order');
  {$IFDEF UNIX}
   I := ' ''~/my folder/my img.nii''';
  {$ELSE}
  I := ' ''c:\my folder\my img.nii''';
    {$ENDIF}
    writeln(' '+E+I);
    writeln('  Process'+I+' with default parameters (spaces in folder or filename)');
end;


var
  Application: Tperf;

{$R *.res}

begin
  Application:=Tperf.Create(nil);
  Application.Run;
  Application.Free;
end.

