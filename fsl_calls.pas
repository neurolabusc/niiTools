unit fsl_calls; 

{$mode objfpc}{$H+}
{$Include isgui.inc}
interface
uses
  {$IFDEF GUI}LResources, Forms,{$ENDIF}
  Classes, SysUtils,
  Process, define_types,nii_core, dialogsx;

procedure FSLcmd (lCmd: string);
function FSLbet (lFilename: string; lFrac: single): string;
function FSLslicetimer (lFilename4D: string; lOrder: integer; lTRsec: single): string;
function FSLmcflirt (lFilename4D: string): string;
function FSLmean(lFilename4D: string): string;
function FSLflirt (lFilenames: tstrings): string;

var gFSLbase: string;
implementation
function GetFSLdir: string;
//const FSLBase = '/usr/local/fsl';
begin
    result := gFSLBASE;
   if (length(result)<1) or (not DirExists(result)) then begin
      if direxists (GetEnvironmentVariable('FSLDIR')) then
         result:=GetEnvironmentVariable('FSLDIR');
   end;

end;

procedure FSLcmd (lCmd: string);
const
  FSLOUTPUTTYPE = 'FSLOUTPUTTYPE=NIFTI_GZ';
 var
 AProcess: TProcess;
 i: integer;
   AStringList: TStringList;
   PATH,FSLDIR,lS,FULL,FSLDIRBIN: string;
 begin
    {$IFNDEF UNIX}
    Showmsg('FSL not configured for Windows.');
    exit;
    {$ENDIF}
   AProcess := TProcess.Create(nil);
   AStringList := TStringList.Create;
   PATH:=GetEnvironmentVariable('PATH');
   FSLDIR := GetFSLdir;
   FSLDIRBIN :=  FSLDIR+'/bin' ;
    FULL :=   PATH+':'+FSLDIR+':'+FSLDIRBIN;
    lS := 'FSLDIR='+FSLDIR;
    AProcess.Environment.Add(lS);
    lS := 'LD_LIBRARY_PATH='+FSLDIRBIN;
    AProcess.Environment.Add(lS);
    lS := 'PATH='+FULL;
    AProcess.Environment.Add(lS);
    lS := 'FSLCLUSTER_MAILOPTS="n"';
    AProcess.Environment.Add(lS);
   AProcess.Environment.Add(FSLOUTPUTTYPE);
   AProcess.CommandLine := FSLDIRBIN+pathdelim+lCmd;
   AProcess.Options := AProcess.Options + [poWaitOnExit, poStderrToOutPut, poUsePipes];
   (*for i := 1 to AProcess.Environment.Count do
       riteln(AProcess.Environment.Strings[i-1]);*)
   riteln(AProcess.CommandLine);
   {$IFDEF GUI}application.processmessages;{$ENDIF}
   AProcess.Execute;
   AStringList.LoadFromStream(AProcess.Output);
   if AStringList.Count > 0 then
      for i := 1 to AStringList.Count do
       riteln(AStringList.Strings[i-1]);
   AStringList.Free;
   AProcess.Free;
end;

function FSLbet (lFilename: string; lFrac: single): string;
var
   lCmd: string;
begin
    result := ChangeFilePrefix(lFilename,'b');
    if not ExtGZ(result) then   //bet will turn a .nii image into .nii.gz
     result :=   ChangeFileExtX(result,'.nii.gz');

    lCmd := 'bet "'+lFilename+'" "'+result +'" -R -F -f '+floattostr(lFrac);
    FSLCmd (lCmd);
end;

function FSLmean (lFilename4D: string): string;
var
   lCmd: string;
begin
    result := ChangeFilePrefix(lFilename4D,'w');
    lCmd := 'fslmaths  "'+lFilename4D+'" -Tmean "'+result +'"';
    FSLCmd (lCmd);
end;

function FSLmcflirt (lFilename4D: string): string;
var
   lCmd: string;
begin
    result := ChangeFilePrefix(lFilename4D,'r');
    lCmd := 'mcflirt -in  "'+lFilename4D+'" -o "'+result +'"  -cost mutualinfo';
    FSLCmd (lCmd);
end;


(*kAscendingInterleavedPhilGE = 2;
kDescendingInterleavedPhilGE = 4;
kAscendingInterleavedSiemens = 5;
kDescendingInterleavedSiemens = 6;  *)

function FSLslicetimer (lFilename4D: string; lOrder: integer; lTRsec: single): string;
var
   lCmd,x: string;
begin
    if (lOrder = kSimultaneous) then begin
       result := lFilename4D;
       exit;
    end;
    result := ChangeFilePrefix(lFilename4D,'t');
    //https://www.jiscmail.ac.uk/cgi-bin/webadmin?A2=fsl;426e0378.1307
    if (lOrder =  kAscendingInterleavedSiemens) or (lOrder = kDescendingInterleavedSiemens) then
       Showmsg('************ WARNING - SOFTWARE DOES NOT YET SUPPORT SIEMENS SLICE TIMING ******');
    if (lOrder =  kAscendingInterleavedPhilGE) or (lOrder = kDescendingInterleavedPhilGE) or (lOrder =  kAscendingInterleavedSiemens) or (lOrder = kDescendingInterleavedSiemens) then begin
       Showmsg('Warning: please make sure FSL''s slicetimer "-odd" order is compatible with your scanner');
       x := ' --odd';
    end else
        x := '';
    if (lOrder = kDescending) or (lOrder = kDescendingInterleavedPhilGE) or (lOrder = kDescendingInterleavedSiemens) then
      x := x + ' --down';
    lCmd := 'slicetimer -i   "'+lFilename4D+'" -o "'+result +'"  -r '+floattostr(lTRsec)+x;
    FSLCmd (lCmd);
end;

function FSLflirt (lFilenames: tstrings): string;
var
   lCmd,Ref,lFilename,Mat,Warped: string;
   i: integer;
begin
     lFilename := lFilenames[0];
     Ref := GetFSLdir+'/data/standard/MNI152_T1_2mm_brain';
     Mat := ChangeFileExtX(lFilename,'.mat');
     result := ChangeFilePrefix(lFilename,'w');

     lCmd := 'flirt -in "'+lFilename+'" -ref '+ref+' -out '+result+'  -omat "'+Mat+ '" -bins 256 -cost mutualinfo -searchrx -90 90 -searchry -90 90 -searchrz -90 90 -dof 12  -interp trilinear';

     FSLCmd (lCmd);
     if lFilenames.count <= 1 then
       exit;
     for i := 2 to (lFilenames.count) do begin
         lFilename := lFilenames[i-1];
         Warped := ChangeFilePrefix(lFilename,'w');
         lCmd := 'flirt -in "'+lFilename+'" -ref "'+Ref+'" -applyxfm -init "'+Mat+'" -out "'+Warped+'"';
         FSLCmd (lCmd);
     end;
end;

initialization
  gFSLbase:= '/usr/local/fsl';
end.

