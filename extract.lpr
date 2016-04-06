program extract;

{$IFDEF FPC}{$mode objfpc}{$H+}{$ENDIF}
{$H+}
{$D-,O+,Q-,R-,S-}
{$include isgui.inc}
uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, dialogsx, define_types,nii_core, nii_extract,morphological;
procedure WriteHelp;
var
  P,E: string;
begin
  { add your help code here }
  E := extractfilename(ParamStr(0));
  writeln('Usage: ',E,'[options] input.nii');
  writeln('Version: '+ kVers+' by Chris Rorden');
  writeln(' Attempts to remove noise in the dark regions (air) around an object.');
  writeln('Options:');
  writeln(' -a apply extraction to this image');
  writeln(' -c minimum cluster size (mm^3) (if 0 than largest, default 0)');
  writeln(' -d number of voxels surface is dilated after extraction (0..12, default 2)');
  writeln(' -o name of output file (otherwise ''e'' prefix added to input name)');
  writeln(' -t threshold for edge (1..5, default 4)');
  writeln(' -k kind of extraction 0=DeleteAir,1=PreserveBright,2=PreserveBrightAndInclusions, Default=2');
  writeln(' -x explicit threshold (if not 0, then this value will be used instead of Otsus method)');

 // writeln(' -r create text regressor files instead of adjusted image)');
  writeln(' -h show these help instructions)');
  writeln('Examples:');
{$IFDEF UNIX}
p := ' ~/f1/';
{$ELSE}
p := ' c:\f1\';
  {$ENDIF}
  writeln(' '+E+p+'t1.nii');
  writeln('    extract image t1.nii using default options ');
    writeln(' '+E+p+' -a t2.nii t1.nii');
  writeln('    estimate extraction on t1, apply to t2');
  writeln(' '+E+p+'c1.nii c2.nii');
  writeln('    estimate extraction based on sum of c1/c2, apply to c1');

  writeln(' '+E+' -d 1 -t 3 -s n  '+p+'t1.nii');
  writeln('    extract t1.nii with a threshold of 3, 1 voxel dilation, multiple objects ');

end;

function StoInt(S: string; Default: integer): integer;
begin
  try
     	result := StrToInt(S);
  except
    on Exception : EConvertError do
      result := Default;
  end;
end;

(*function StoBool(S: string): boolean;
begin
  result := true;
  if (length(S)> 0) and ( (upcase(S[1])='F')  or (S[1]='0') ) then
     result := false;
end; *)

function StoFloat(S: string; Default: single): single;
begin
  try
     	result := StrToFloat(S);
  except
    on Exception : EConvertError do
      result := Default;
  end;
end;

(*procedure StrToMemo(lStr: String);
var
   lLen,lPos: integer;
   lOutStr: string;
begin
     lLen := length(lStr);
     if lLen < 1 then exit;
     lOutStr := '';
     for lPos := 1 to lLen do begin
         if lStr[lPos] = kCR then begin
            Writeln(lOutStr);
            lOutStr := '';
         end else
             lOutStr := lOutStr + lStr[lPos];
     end;
     if lOutStr <> '' then
        Writeln(lOutStr);
end;  *)

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
label 666;
var
  i,lOtsuLevels,lMode: integer;
  lDilateVox,lClusterMM,lExplicitThreshold: single;
  lImages: TStringlist;
  lModname,s,s2,lOutname: string;
  lCreateImg:boolean;
  c: char;
begin
  ExitCode := 1;//assume error - set to zero on successful completion
  lImages := TStringlist.create;
  lModname := '';
  lOtsuLevels := 4;
  lDilateVox := 2;
  lClusterMM := 0;
  lExplicitThreshold := 0;
  lMode := kMorphPreserveLargestBrightAndInclusions;
  //lNoise := false;
  lOutname := '';
  i := 1;
  while i <= ParamCount do begin
    s := ParamStr(i);
    if length(s)> 1 then begin
      if s[1]='-' then begin
        c := upcase(s[2]);
        if c='H' then
           Writehelp
        else {if c='R' then begin
              lCreateImg := false;
              lCreateText := true;
        end else} if i < paramcount then begin
          inc(i);
          s2 := ParamStr(i);
          case c of
               'A': lModname := s2;
               'C': lClusterMM := StoFloat(s2,lClusterMM);
               'D': lDilateVox := StoFloat(s2,lDilateVox);
               'K': lMode := StoInt(s2,lOtsuLevels);
               'O': lOutname := s2;
               'X' : lExplicitThreshold := StoFloat(s2,lExplicitThreshold);
               'T': lOtsuLevels := StoInt(s2,lOtsuLevels);
          end;//case
        end;
      end else// starts with '-'
      	  lImages.Add(s);//lImgFilename := s;
    end; //length > 1 char
    inc(i);
  end; //for each parameter
  if (lImages.count < 1) or (not fileexists(lImages[0])) {(lImgFilename='') or (not fileexists(lImgFilename))} then begin
  	 writeln('Please specify a valid input image');
     goto 666;
  end;
  if lExplicitThreshold >0 then
     lOtsuLevels := 0;
  nii_extract_object (lImages,lModname,lOutname, lOtsuLevels,lMode, lDilateVox, lClusterMM, lExplicitThreshold);
  DisplayMessages;
  ExitCode := 0;
666:
    lImages.free;
end;

{$R *.res}

begin
  // quick check parameters
  {$IFDEF GUI} showmsg('Compiler error: currently GUI is defined. Please edit isgui.inc');{$ENDIF}

  // parse parameters
  if (ParamCount = 0)  then
    WriteHelp
  else
  	  ProcessParamStr;
end.