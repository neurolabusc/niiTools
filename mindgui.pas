unit mindgui;
{$mode objfpc}{$H+}

interface
uses
  LCLProc, LazHelpHTML, UTF8Process, //<- these are used to launch external web browser
  LCLIntf {<-GetTickCount}, Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  StdCtrls, ComCtrls, ExtCtrls, mindcore;

type
  { TForm1 }
  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Edit1: TMenuItem;
    Exit1: TMenuItem;
    Memo1: TMemo;
    MenuItem1: TMenuItem;
    Copy1: TMenuItem;
    MakeMask1: TMenuItem;
    Define1: TMenuItem;
    Impute1: TMenuItem;
    ImputeNeighbor1: TMenuItem;
    ImputeMultiple1: TMenuItem;
    ImputePool1: TMenuItem;
    Inspect1: TMenuItem;
    AppleMenu: TMenuItem;
    AppleAbout: TMenuItem;
    Help2: TMenuItem;
    Help1: TMenuItem;
    MotionCorrect1: TMenuItem;
    Normalize1: TMenuItem;
    Panel1: TPanel;
    ProgressBar1: TProgressBar;
    SliceTimeCorrect2: TMenuItem;
    SliceTimeCorrect1: TMenuItem;
    TemporalSmooth1: TMenuItem;
    OpenDialog1: TOpenDialog;
    SpatialSmooth1: TMenuItem;
    Timer1: TTimer;
    PredMissing1: TMenuItem;
    Logistic1: TMenuItem;
    procedure AppleAboutClick(Sender: TObject);
    procedure BestPracticesTutorial1Click(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
    procedure Define1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ImputeMultiple1Click(Sender: TObject);
    procedure ImputeNeighbor1Click(Sender: TObject);
    procedure ImputePool1Click(Sender: TObject);
    procedure Inspect1Click(Sender: TObject);
    procedure Logistic1Click(Sender: TObject);
    procedure MakeMask1Click(Sender: TObject);
    procedure MotionCorrect1Click(Sender: TObject);
    procedure Normalize1Click(Sender: TObject);
    procedure SliceTimeCorrect1Click(Sender: TObject);
    procedure SliceTimeCorrect2Click(Sender: TObject);
    procedure SpatialSmooth1Click(Sender: TObject);
    procedure TemporalSmooth1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure DisplayMessages;
    function SelectFiles(lCaption: string; AllowMultiSelect, AllowVOI: boolean):boolean;
    procedure Alpha;
  private
    { private declarations }
  public
    { public declarations }
  end;
var
  Form1: TForm1; 

implementation
{$R *.lfm}
uses
  nii_smooth, nii_math, nii_core, define_types, dialogsx, fsl_calls;
{ TForm1 }

procedure TForm1.Alpha;
begin
  Memo1.lines.clear;
  Memo1.lines.add('This function is not yet supported');
end;

function TForm1.SelectFiles(lCaption: string; AllowMultiSelect, AllowVOI: boolean):boolean;
begin
  OpenDialog1.title := lcaption;
  if AllowVOI then
  	 OpenDialog1.filter := kImgPlusVOIFilter
  else
   	  OpenDialog1.filter := kImgFilter;
  if AllowMultiSelect then
    OpenDialog1.Options := [ofAllowMultiSelect,ofFileMustExist]
  else
    OpenDialog1.Options := [ofFileMustExist];
  result := OpenDialog1.Execute;
end;

procedure TForm1.DisplayMessages;
begin
  Memo1.Lines.AddStrings(DebugStrings);
  DebugStrings.Clear;
end;

procedure TForm1.Copy1Click(Sender: TObject);
begin
  Memo1.SelectAll;
  Memo1.CopyToClipBoard;
end;

procedure TForm1.Define1Click(Sender: TObject);
var
   lOpts: TImputeOptions;
  lImgNames: TStringlist;
begin
  if not SelectFiles('Select contrast images to impute',true,true) then
     exit;
  lOpts :=  ImputeDefaults;
  lOpts.ReportOnly := true;
  lImgNames := TStringlist.Create;
  lImgNames.AddStrings(OpenDialog1.files);
  Timer1.enabled := true;    Application.processmessages;
  Memo1.Lines.clear;
  GroupImpute(lImgNames,lOpts);
  Timer1.enabled := false; ProgressBar1.position := 0;
    DisplayMessages;
  Memo1.lines.add('Group imputation estimation complete');
  lImgNames.Free;
end;

procedure TForm1.BestPracticesTutorial1Click(Sender: TObject);
var
  v: THTMLBrowserHelpViewer;
  BrowserPath, BrowserParams: string;
  p: LongInt;
  URL: String;
  BrowserProcess: TProcessUTF8;
begin
  v:=THTMLBrowserHelpViewer.Create(nil);
  try
    v.FindDefaultBrowser(BrowserPath,BrowserParams);
    debugln(['Path=',BrowserPath,' Params=',BrowserParams]);
    URL := ExtractFileDirWithPathDelim(paramstr(0))+'help.htm';
    if not fileexists(URL) then begin
      Memo1.lines.clear;
      Memo1.Lines.Add('Unable to find help file named '+URL);
      Memo1.lines.Add('Opening online page instead');
      URL:='http://www.mricro.com';
    end;
    p:=System.Pos('%s', BrowserParams);
    System.Delete(BrowserParams,p,2);
    System.Insert(URL,BrowserParams,p);
    BrowserProcess:=TProcessUTF8.Create(nil);
    try
      BrowserProcess.CommandLine:=BrowserPath+' '+BrowserParams;
      BrowserProcess.Execute;
    finally
      BrowserProcess.Free;
    end;
  finally
    v.Free;
  end;
end;

procedure TForm1.AppleAboutClick(Sender: TObject);
begin
  Showmsg(kVers);
end;

procedure TForm1.Exit1Click(Sender: TObject);
begin
  close;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  Memo1.lines.add(kVers);
  Application.ShowButtonGlyphs:= sbgNever;
  {$IFDEF Darwin}
    AppleMenu.visible := true;
    File1.Visible:= false;
  {$ENDIF}
end;

procedure TForm1.ImputeMultiple1Click(Sender: TObject);
begin
     if Timer1.enabled then exit;
     Alpha;
end;

procedure TForm1.ImputeNeighbor1Click(Sender: TObject);
var
  lImgNames: TStringlist;
  StartTime: DWord;
  lOpts: TImputeOptions;
begin
  if not SelectFiles('Select contrast images to impute',true,true) then
     exit;
  if OpenDialog1.files.count < 2 then begin
    showmessage('Group imputation requires at least two images.');
    exit;
  end;
  lOpts :=  ImputeDefaults;
  lOpts.SearchRadiusMM := GetFloat('Search Radius (mm)', 1,lOpts.SearchRadiusMM,128);
  lOpts.ReplacementNeighbors   := GetInt('Number of Replacement Neighbors', 1,lOpts.ReplacementNeighbors,128);
  lOpts.MinObservations := GetInt('Minimum observations (if 2, only impute voxels observed in at least 2 images)',1,lOpts.MinObservations,OpenDialog1.files.count-1);
  lImgNames := TStringlist.Create;
lImgNames.AddStrings(OpenDialog1.files);
  Timer1.enabled := true;    Application.processmessages;
  Memo1.Lines.clear;
  StartTime := GetTickCOunt;
  GroupImpute(lImgNames,lOpts);
  Timer1.enabled := false; ProgressBar1.position := 0;
    DisplayMessages;
  Memo1.lines.add('Group neighbor imputation correct complete: Time elapsed '+inttostr(GetTickCount-StartTime)+'ms');
  lImgNames.Free;
end;

procedure TForm1.ImputePool1Click(Sender: TObject);
begin
     if Timer1.enabled then exit;
     Alpha;
end;

procedure TForm1.Inspect1Click(Sender: TObject);
begin
  if Timer1.enabled then exit;
    Memo1.Lines.Clear;
  if not SelectFiles('Choose images to inspect',true,true) then exit;
   InspectVols(OpenDialog1.Files);
   DisplayMessages;
end;

procedure TForm1.Logistic1Click(Sender: TObject);
begin
     if Timer1.enabled then exit;
     Alpha;
end;

procedure TForm1.MakeMask1Click(Sender: TObject);
begin
     if Timer1.enabled then exit;
     Alpha;
end;

procedure TForm1.MotionCorrect1Click(Sender: TObject);
var
  lV: integer;
begin
  if not SelectFiles('Select 4D image[s] to motion correct',true,true) then
     exit;
  Timer1.enabled := true;    Application.processmessages;
  for lV := 1 to OpenDialog1.files.count do begin
      FSLmcflirt(OpenDialog1.files.Strings[lV-1]);
      DisplayMessages;
  end;
  Timer1.enabled := false; ProgressBar1.position := 0;
  Memo1.lines.add('Motion correct complete');
end;

procedure TForm1.Normalize1Click(Sender: TObject);
begin
  if not SelectFiles('Select image to normalize (if multiple, estimates from 1st are applied to all)',true,true) then
     exit;
  Timer1.enabled := true;    Application.processmessages;
  FSLflirt(OpenDialog1.files);
  DisplayMessages;
  Timer1.enabled := false; ProgressBar1.position := 0;
  Memo1.lines.add('Normalize complete');
end;

procedure TForm1.SliceTimeCorrect1Click(Sender: TObject);
var
  lSliceOrder: integer;
begin
     if Timer1.enabled then exit;
       Memo1.Lines.Clear;
     if not SelectFiles('Choose images to slice time correct',true,true) then exit;
     lSliceOrder := GetInt('Slice order: ascend='+inttostr(kAscending)+', ascendInterleaved='+inttostr(kAscendingInterleaved)+' descend='
  			  +inttostr(kDescending)+', DescendInterleaved='+inttostr(kDescendingInterleaved),kAscending,kAscendingInterleaved, kDescendingInterleaved);
     Timer1.enabled := true; Application.processmessages;
      nii_slicetimecorrect_cubic(OpenDialog1.Files,lSliceOrder);
     Timer1.enabled := false; ProgressBar1.position := 0;
     DisplayMessages;
end;

procedure TForm1.SliceTimeCorrect2Click(Sender: TObject);
  var
    lTR: single;
    lV,lSliceOrder: integer;
  begin
    if not SelectFiles('Select 4D image[s] to slice time correct',true,true) then
       exit;
    lSliceOrder := GetInt('Slice order: ascend='+inttostr(kAscending)+', ascendInterleaved='+inttostr(kAscendingInterleaved)+' descend='
  			  +inttostr(kDescending)+', DescendInterleaved='+inttostr(kDescendingInterleaved),kAscending,kAscendingInterleaved, kDescendingInterleaved);
    lTR := GetFloat('Time Per Volume (TR, sec)',0.1,3.0,20);
    Timer1.enabled := true;    Application.processmessages;
    for lV := 1 to OpenDialog1.files.count do begin
        FSLslicetimer(OpenDialog1.files.Strings[lV-1],lSliceOrder,lTR);
        DisplayMessages;
    end;
    Timer1.enabled := false; ProgressBar1.position := 0;
    Memo1.lines.add('Slice timer complete');
end;

procedure TForm1.SpatialSmooth1Click(Sender: TObject);
var
  lFWHM: single;
begin
  if Timer1.enabled then exit;
   Memo1.Lines.Clear;
 lFWHM := GetFloat('Amount to blur (FWHM (mm)', 0,6,128);
  if not SelectFiles('Choose images to smooth',true,true) then exit;
 Timer1.enabled := true; Application.processmessages;
  nii_smooth_gauss(OpenDialog1.Files,lFWHM);
  Timer1.enabled := false; ProgressBar1.position := 0;
 DisplayMessages;
end;

procedure TForm1.TemporalSmooth1Click(Sender: TObject);
var
  lFWHM: single;
begin
   if Timer1.enabled then exit;
    Memo1.Lines.Clear;
  lFWHM := GetFloat('Amount to blur FWHM (sec)', 0,6,128);
   if not SelectFiles('Choose images to smooth',true,true) then exit;
  Timer1.enabled := true; Application.processmessages;
   nii_smooth_gauss_sec(OpenDialog1.Files,lFWHM);
   Timer1.enabled := false; ProgressBar1.position := 0;
  DisplayMessages;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  DisplayMessages;
  ProgressBar1.position := round(1000*DebugFractionCompleted);
end;

end.

