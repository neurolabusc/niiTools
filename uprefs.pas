unit uprefs;
{$H+}

interface
uses IniFiles,SysUtils,Classes,define_types;

type

  Str255= string[255];
  TUPrefs = record
    Smooth: boolean;
    Permutations: integer;
  end;
  var
    gUPrefs: TUprefs;

procedure IniStr255(lRead: boolean; lIniFile: TIniFile; lIdent: string; var lValue: str255);
procedure IniFloat(lRead: boolean; lIniFile: TIniFile; lIdent: string;  var lValue: single);
procedure IniByte(lRead: boolean; lIniFile: TIniFile; lIdent: string;  var lValue: byte);
procedure IniInt(lRead: boolean; lIniFile: TIniFile; lIdent: string;  var lValue: integer);
procedure IniBool(lRead: boolean; lIniFile: TIniFile; lIdent: string;  var lValue: boolean);
procedure IniStr(lRead: boolean; lIniFile: TIniFile; lIdent: string; var lValue: string);

implementation

procedure USetDefaultPrefs (var lPrefs: TUPrefs);
var
  lI : integer;
begin
  with lPrefs do begin
    Smooth := true;
    Permutations := 1000;//1366;
  end;
end; //Proc SetDefaultPrefs

procedure IniFloat(lRead: boolean; lIniFile: TIniFile; lIdent: string;  var lValue: single);
//read or write an integer value to the initialization file
var
	lStr: string;
begin
        if not lRead then begin
           lIniFile.WriteString('FLT',lIdent,FloattoStr(lValue));
           exit;
        end;
	lStr := lIniFile.ReadString('FLT',lIdent, '');
	if length(lStr) > 0 then
		lValue := StrToFloat(lStr);
end; //IniFloat

procedure IniByte(lRead: boolean; lIniFile: TIniFile; lIdent: string;  var lValue: byte);
//read or write an integer value to the initialization file
var
	lStr: string;
begin
        if not lRead then begin
           lIniFile.WriteString('BYT',lIdent,InttoStr(lValue));
           exit;
        end;
	lStr := lIniFile.ReadString('BYT',lIdent, IntToStr(lValue));
	if length(lStr) > 0 then
		lValue := StrToInt(lStr);
end; //IniByte

procedure IniInt(lRead: boolean; lIniFile: TIniFile; lIdent: string;  var lValue: integer);
//read or write an integer value to the initialization file
var
	lStr: string;
begin
        if not lRead then begin
           lIniFile.WriteString('INT',lIdent,IntToStr(lValue));
           exit;
        end;
	lStr := lIniFile.ReadString('INT',lIdent, IntToStr(lValue));
	if length(lStr) > 0 then
		lValue := StrToInt(lStr);
end; //IniInt

procedure IniBool(lRead: boolean; lIniFile: TIniFile; lIdent: string;  var lValue: boolean);
//read or write a boolean value to the initialization file
var
	lStr: string;
begin
        if not lRead then begin
           lIniFile.WriteString('BOOL',lIdent,Bool2Char(lValue));
           exit;
        end;
	lStr := lIniFile.ReadString('BOOL',lIdent, Bool2Char(lValue));
	if length(lStr) > 0 then
		lValue := Char2Bool(lStr[1]);
end; //IniBool

procedure IniStr(lRead: boolean; lIniFile: TIniFile; lIdent: string; var lValue: string);
//read or write a string value to the initialization file
begin
  if not lRead then begin
    lIniFile.WriteString('STR',lIdent,lValue);
    exit;
  end;
	lValue := lIniFile.ReadString('STR',lIdent,lValue);
end; //IniStr

procedure IniStr255(lRead: boolean; lIniFile: TIniFile; lIdent: string; var lValue: str255);
//read or write a string value to the initialization file
begin
  if not lRead then begin
    lIniFile.WriteString('STR',lIdent,lValue);
    exit;
  end;
	lValue := lIniFile.ReadString('STR',lIdent,lValue);
end; //IniStr255


function UIniFile(lRead: boolean; lFilename: string; var lPrefs: TUPrefs): boolean;
//Read or write initialization variables to disk
var
  lI: integer;
  lIniFile: TIniFile;
begin
  result := false;
  if (lRead) then
    USetDefaultPrefs (lPrefs);
  if (lRead) and (not Fileexists(lFilename)) then
        exit;
  lIniFile := TIniFile.Create(lFilename);
	IniBool(lRead,lIniFile, 'Smooth',lPrefs.Smooth);

	IniInt(lRead,lIniFile, 'Permutations',lPrefs.Permutations);
  lIniFile.Free;
end;

{$IFDEF UNIX}
function DefaultsDir (lSubFolder: string): string;
//for Linux: DefaultsDir is ~/appname/SubFolder/, e.g. /home/username/mricron/subfolder/
//Note: Final character is pathdelim
const
     pathdelim = '/';
var
   lBaseDir: string;
begin
     lBaseDir := GetEnvironmentVariable ('HOME')+pathdelim+'.'+ FileNameNoExt(ExtractFilename(paramstr(0) ) );
     if not DirectoryExists(lBaseDir) then begin
        {$I-}
        MkDir(lBaseDir);
        if IOResult <> 0 then begin
               //Msg('Unable to create new folder '+lBaseDir);
        end;
        {$I+}
     end;
     lBaseDir := lBaseDir+pathdelim;
     if lSubFolder <> '' then begin
         lBaseDir := lBaseDir + lSubFolder;
         if not DirectoryExists(lBaseDir) then begin
            {$I-}
            MkDir(lBaseDir);
            if IOResult <> 0 then begin
               //you may want to show an error, e.g. showmessage('Unable to create new folder '+lBaseDir);
               exit;
            end;
            {$I+}
         end;
         result := lBaseDir + pathdelim;
     end else
         result := lBaseDir;
end;

function UIniName: string;
begin
  result := DefaultsDir('')+FileNameNoExt(extractfilename(paramstr(0)))+'.ini';
end;
{$ELSE}
function UIniName: string;
begin
  result := ExtractFilePath(paramstr(0))+'settings.ini';
end;
{$ENDIF}

initialization
  UIniFile(true, uininame,gUPrefs);

finalization
  UIniFile(false, uininame,gUPrefs);

end.
