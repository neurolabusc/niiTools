program mind;

{$mode objfpc}{$H+}
 {$include isgui.inc}
uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, dialogsx,mindcore, define_types, nii_core;


procedure WriteHelp;
var
  E: string;
  lOpts : TImputeOptions;
begin
  lOpts := ImputeDefaults;
  E := extractfilename(ParamStr(0));
  writeln('Usage: ',E,'[options] file1.nii file2.nii file3.nii ... fileN.nii');
  writeln('Version: '+kVers);
  writeln('Options: [default]');
  writeln(' -m Mode ('+inttostr(kReportMode)+'=Report, '+inttostr(kNeighbormode)+'Neighbor impute) ['+inttostr(kDefMode)+']');
  writeln(' -n Number of replacement neighbors ['+inttostr(lOpts.ReplacementNeighbors)+']');
  writeln(' -o Min Observations (e.g. if 2, impute voxels observed at least twice) ['+inttostr(lOpts.MinObservations)+']');
  writeln(' -s Search radius (mm)  ['+realtostr(lOpts.SearchRadiusMM,2)+']');
  writeln('Examples:');
{$IFDEF UNIX}
writeln(' '+E+' -m 1 -s 12 ~/f1/p1.nii ~/f1/p2.nii ~/f1/p3.nii ~/f1/p4.nii ~/f1/p5.nii');
{$ELSE}
 writeln(' '+E+' -m 1 -s 12 c:\p1.nii c:\p2.nii c:\p3.nii c:\p4.nii c:\p5.nii');
{$ENDIF}
end; //writehelp

function StoInt(S: string; Default: integer): integer;
begin
  try
     	result := StrToInt(S);
  except
    on Exception : EConvertError do
      result := Default;
  end;
end;

function StoFloat(S: string; Default: single): single;
begin
  try
     	result := StrToFloat(S);
  except
    on Exception : EConvertError do
      result := Default;
  end;
end;

procedure DisplayMessages;
var
  i: integer;
begin
  if DebugStrings.Count < 1 then
     exit;
  for i := 0 to (DebugStrings.Count-1) do
  	  Writeln(DebugStrings[i]);
  DebugStrings.Clear;
end;

procedure ProcessParamStr;
var
  lOpts : TImputeOptions;
  lMode,i: integer;
  s,s2: string;
  c: char;
  lFileNames : TStringlist;
begin
  ExitCode := 1;//assume error - set to zero on successful completion
  lOpts := ImputeDefaults;
  lMode := kDefMode;
  lFileNames := TStringlist.Create;
  i := 1;
  while i <= ParamCount do begin
    s := ParamStr(i);
    if length(s)> 1 then begin
      if s[1]='-' then begin
        c := upcase(s[2]);
        if c='H' then
           Writehelp
        else if i < paramcount then begin
          inc(i);
          s2 := ParamStr(i);
          case c of
               'M': lMode:= StoInt(s2,lMode);
               'N': lOpts.ReplacementNeighbors:= StoInt(s2,lOpts.ReplacementNeighbors);
               'O': lOpts.MinObservations:= StoInt(s2,lOpts.MinObservations);
               'S': lOpts.SearchRadiusMM := StoFloat(s2,lOpts.SearchRadiusMM);
          end;//case
        end;
      end else// starts with '-'
      	  lFilenames.Add(s);
    end; //length > 1 char
    inc(i);
  end; //for each parameter
  if (lFilenames.Count < 1) or (not fileexists(lFilenames[0])) then
  	 writeln('Please specify valid input images')
  else begin
      GroupImpute(lFilenames, lOpts);
      DisplayMessages;
  end;
  lFilenames.free;
  ExitCode := 0;
end;

begin
  {$IFDEF GUI} showmsg('Compiler error: currently GUI is defined. Please edit isgui.inc');{$ENDIF}
  // parse parameters
  if (ParamCount = 0)  then
    WriteHelp
  else
      ProcessParamStr;
end.

