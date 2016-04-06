program part;

{$IFDEF FPC}{$mode objfpc}{$H+}{$ENDIF}
{$H+}
{$D-,O+,Q-,R-,S-}
{$include isgui.inc}
uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, dialogsx, define_types,upart,nii_core;

const
   kDefaultBins = 20;
procedure WriteHelp;
var
  E: string;
begin
  { add your help code here }
  E := extractfilename(ParamStr(0));
  writeln('Usage: ',E,'[options] input.nii');
  writeln('Version: '+kVers+' by Chris Rorden');
  writeln(' Uses Siemens PULS/RESP data to remove variance in NIfTI images.');
  writeln(' For details, see Deckers et al (2006) www.pubmed.com/17011214.');
  writeln('Options:');
  writeln(' -1 name of first DICOM volume (else onset time should be stored in input.nii''s header)');
  writeln(' -b number of bins (otherwise '+inttostr(kDefaultBins)+')');
  writeln(' -d delete volumes');
  writeln(' -o name of output file (otherwise ''p'' prefix added to input name)');
  writeln(' -p name of physio file');
  writeln(' -s slice order (ascending/descending,sequential/interleaved): AS='+inttostr(kAscending)+' AI='+inttostr(kAscendingInterleavedPhilGE)+' DS='+inttostr(kDescending)+' DI='+inttostr(kDescendingInterleavedPhilGE)+' AI[Siemens]='+inttostr(kAscendingInterleavedSiemens)+' DI[Siemens]='+inttostr(kDescendingInterleavedSiemens));
  writeln(' -t TR in seconds (otherwise uses TR from input.nii''s header)');
 // writeln(' -r create text regressor files instead of adjusted image)');
  writeln(' -h show these help instructions)');
  writeln('Examples:');
{$IFDEF UNIX}
writeln(' '+E+' -p ~/f1/p1.resp -t 1.72 -b 30 -o ~/f1/fixedp1.nii ~/f1/i1.nii');
writeln('  Will automatically load ~/f1/p1.puls if file exists.');
writeln(' '+E+' -p ~/f1/p1.resp -p ~/f1/p1.puls ~/f1/i1.nii');
writeln('  Will create output ~/f1/pi1.nii');
writeln(' '+E+' -p ~/f1/p1.resp -r -p ~/f1/p1.puls ~/f1/i1.nii');
writeln('  Will create text file regressors instead of modified image');
writeln(' '+E+' ~/f1/i1.nii');
writeln('  Assumes a resp/puls file[s] exists with same name as input (~/f1/i1.resp)');
{$ELSE}
  writeln(' '+E+' -p c:\f1\p1.resp -t 1.72 -b 30 -o c:\f1\fixedp1.nii c:\f1\i1.nii');
  writeln('  Will automatically load c:\f1\p1.puls if file exists.');
  writeln(' '+E+' -p c:\f1\p1.resp -p c:\f1\p1.puls c:\f1\i1.nii');
  writeln('  Will create output c:\f1\pi1.nii');
  writeln(' '+E+' -p c:\f1\p1.resp -r -p c:\f1\p1.puls c:\f1\i1.nii');
  writeln('  Will create text file regressors instead of modified image');
  writeln(' '+E+' c:\f1\i1.nii');
  writeln('  Assumes a resp/puls file[s] exists with same name as input (c:\f1\i1.resp)');
  {$ENDIF}
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

function StoFloat(S: string; Default: single): single;
begin
  try
     	result := StrToFloat(S);
  except
    on Exception : EConvertError do
      result := Default;
  end;
end;

procedure StrToMemo(lStr: String);
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
  lPhysioNames : TStringlist;
  lImgFilename,lPhysioFilename2,lOutname,lComments,l1stDICOMname: string;
  lTRsec,lDeleteVols: single;
  bins,i,lSliceOrder: integer;
  s,s2: string;
  lCreateImg,lCreateText:boolean;
  c: char;
begin
  ExitCode := 1;//assume error - set to zero on successful completion
  lImgFilename := '';
  lOutname := '';
  l1stDICOMname := '';
  lSliceOrder := kAscending;
  lCreateImg := true;
  lCreateText := false;
  lPhysioNames := TStringlist.Create;
  i := 1;
  bins := 20;
  lTRsec := 0.0;
  lDeleteVols := 0.0;
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
               '1': l1stDICOMname := s2;
               'B': bins := StoInt(s2,bins);
               'O': lOutname := s2;
               'D': lDeleteVols := StoFloat(s2,lDeleteVols);
               'P': lPhysionames.Add(s2);
               'S': lSliceOrder := StoInt(s2,lSliceOrder);
               'T': lTRsec := StoFloat(s2,lTRsec);
          end;//case
        end;
      end else// starts with '-'
      	  lImgFilename := s;
    end; //length > 1 char
    inc(i);
  end; //for each parameter
  if (lImgFilename='') or (not fileexists(lImgFilename)) then begin
  	 writeln('Please specify a valid input image '+lImgFilename);
     exit;
  end;
  if lOutname = '' then
  	lOutname := ChangeFilePrefix(lImgFilename,'p');
  if lPhysioNames.count = 0 then
  	 lPhysioNames.add(ChangeFileExtX(lImgFilename,'.resp'));
  if lPhysioNames.count = 1 then begin
  	 if UpCaseExt(lPhysioNames[0]) = '.PULS' then
     lPhysioFilename2 := (changefileext(lPhysioNames[0],'.resp'))
  	 else
  	  lPhysioFilename2 := (changefileext(lPhysioNames[0],'.puls'));
     if fileexists(lPhysioFilename2) then
     	lPhysioNames.add(lPhysioFilename2);
  end;
  if not fileexists(lPhysioNames[0]) then begin
  	 writeln('Please specify a valid physio file '+lPhysioNames[0]);
     exit;
  end;
  lComments := ApplyPart( lImgFilename,lOutname,l1stDICOMname, lPhysioNames,bins,lSliceOrder,lTRsec,lDeleteVols,lCreateImg,lCreateText);
  //ApplyPart( lImg,Outname{opt},l1stDICOMname{optional}: string; lPhysionames: TStrings; lBins,lSliceOrder : integer; lTRsec,lDeleteVols: single; lImgOut,lTextOut:boolean): string;

  lPhysioNames.free;
  DisplayMessages;
  StrToMemo(lComments);

  ExitCode := 0;
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