unit uilaz;

{$mode objfpc}{$H+}
{$include isgui.inc}
interface

uses
  LCLType,
  morphological,Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  nii_mask,nii_smooth, StdCtrls, ExtCtrls, Menus, nii_math, nii_core, upart,//nii_perf,  uperfprefsform,
  nii_peak,nii_extract,define_types, nii_ttest, nii_intennorm, dialogsx, nii_wlsreg_perm,nii_gen, nii_modvalformat, fsl_calls;

type

  { TuiForm }

  TuiForm = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    MenuItem1: TMenuItem;
    ContinuousGroups1: TMenuItem;
    IntenNorm1: TMenuItem;
    Inspect1: TMenuItem;
    Add1: TMenuItem;
    CreateImages1: TMenuItem;
    AddLesionVol1: TMenuItem;
    FLIRT: TMenuItem;
    MCFLIRT: TMenuItem;
    BET: TMenuItem;
    Extract1: TMenuItem;
    Mask1: TMenuItem;
    Peak1: TMenuItem;
    SliceTimer: TMenuItem;
    SmoothSec1: TMenuItem;
    Mirror1: TMenuItem;
    Smooth2: TMenuItem;
    SliceTimeCorrection1: TMenuItem;
    Smooth1: TMenuItem;
    Perfusion1: TMenuItem;
    OpenDialog3: TOpenDialog;
    Part1: TMenuItem;
    Options1: TMenuItem;
    Voxelwise3: TMenuItem;
    ROI3: TMenuItem;
    ROI1: TMenuItem;
    Voxelwise1: TMenuItem;
    ROI2: TMenuItem;
    Voxelwise2: TMenuItem;
    TwoGroups1: TMenuItem;
    Exit1: TMenuItem;
    OpenDialog2: TOpenDialog;
    ProgressBar1: TProgressBar;
    Timer1: TTimer;
    ToolBar2: TToolBar;
    Memo1: TMemo;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    procedure Memo1Change(Sender: TObject);
    procedure Txx;
    procedure Bug;
    procedure AddBtnClick(Sender: TObject);
    procedure AddLesionVol1Click(Sender: TObject);
    procedure BETClick(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure Extract1Click(Sender: TObject);
    procedure FLIRTClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure InspectBtnClick(Sender: TObject);
    procedure DisplayMessages;
    procedure IntenNormBtnClick(Sender: TObject);
    procedure CreateImages1Click(Sender: TObject);
    procedure Mask1Click(Sender: TObject);
    procedure MCFLIRTClick(Sender: TObject);
    procedure Mirror1Click(Sender: TObject);
    procedure Peak1Click(Sender: TObject);
    procedure Perfusion1Click(Sender: TObject);
    procedure ROI3Click(Sender: TObject);
    procedure SliceTimeCorrection1Click(Sender: TObject);
    procedure SliceTimerClick(Sender: TObject);
    procedure Smooth1Click(Sender: TObject);
    procedure Smooth2Click(Sender: TObject);
    procedure SmoothSec1Click(Sender: TObject);
    procedure voipairedttestBtnClick(Sender: TObject);
    procedure PartBtnClick(Sender: TObject);
    function SelectFiles(lCaption: string; AllowMultiSelect, AllowVOI: boolean):boolean;
    procedure StrToMemo(lStr: String);
    procedure pairedttestBtnClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure voiunrelatedttestBtnClick(Sender: TObject);
    procedure unrelatedttestBtnClick(Sender: TObject);
    procedure Voxelwise3Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  uiForm: TuiForm;

implementation

{$R *.lfm}

function AcceptMsg(lTitle: string): boolean;
//returns Yes if user presses 'OK, else false
begin
	 result := false;
	 if MessageDlg('Question', lTitle, mtConfirmation,[mbYes, mbNo],'0') = mrYes then  //if you get an error here, edit isgui.inc
	 	result := true;

end;


{ TuiForm }
procedure TuiForm.DisplayMessages;
begin
  Memo1.Lines.AddStrings(DebugStrings);
  DebugStrings.Clear;
end;

procedure TuiForm.IntenNormBtnClick(Sender: TObject);
var
   lMask,lRegion1,lRegion2: string;
   lImgNames: TStringList;
begin
  if Timer1.enabled then exit;
  if SelectFiles('Select Mask [optional]',false,true) then
  	 lMask := OpenDialog1.filename
  else
   	  lMask := '';
  if not SelectFiles('Region to have rescaled intensity = 1',false,true) then exit;
  lRegion1 := OpenDialog1.filename;
  if not SelectFiles('Region to have rescaled intensity = 2',false,true) then exit;
  lRegion2 := OpenDialog1.filename;
  if not SelectFiles('Select images to rescale',true,false) then exit;
  lImgNames := TStringlist.Create;
  lImgNames.AddStrings(OpenDialog1.files);
  intensitynormalize(lMask,lRegion1,lRegion2, lImgNames);
  lImgNames.Free;
  DisplayMessages;
end;

procedure TuiForm.CreateImages1Click(Sender: TObject);
var
   d,i,ni: integer;
   Val: single;
begin
  if not SaveDialog1.Execute then exit;
  d := GetInt ('Image dimensions',1,32,128);
  ni := GetInt ('Images to create',1,7,100);
  if (ni < 1) or (d < 1) then exit;
  for i := 1 to ni do begin
      Val := GetInt ('Image intensity for volume '+inttostr(i),1,i,100);
      Generate_NII(ChangeFilePostfix(SaveDialog1.filename,inttostr(i)), d, Val);
  end;
  //zzz
end;

procedure TuiForm.Mask1Click(Sender: TObject);
var
  lMaskFilename : string;
  lThresh: single;
begin
  lThresh := GetFloat('Set threshold: voxels darker than this value will be masked', 0,0.005,99999);
  if not SelectFiles('Select mask image.',false,false) then exit;
  lMaskFilename := OpenDialog1.filename;
  if not SelectFiles('Choose images to inspect',true,true) then exit;
   Timer1.enabled := true; Application.processmessages;
   nii_masking(lMaskFilename,OpenDialog1.Files,lThresh);
   Timer1.enabled := false; ProgressBar1.position := 0;
   DisplayMessages;
end;

procedure TuiForm.Mirror1Click(Sender: TObject);
begin
     if Timer1.enabled then exit;
       Memo1.Lines.Clear;
     if not SelectFiles('Choose images to mirror',true,true) then exit;

     Timer1.enabled := true; Application.processmessages;
      nii_mirror(OpenDialog1.Files);
     Timer1.enabled := false; ProgressBar1.position := 0;
     DisplayMessages;
end;

procedure TuiForm.Peak1Click(Sender: TObject);
var
  lStepsize,lSmoothFWHM: single;
  lDesiredPeaks: integer;
begin
  if not SelectFiles('Select Image[s] to threshold',true,true) then
     exit;
  lDesiredPeaks :=  GetInt('Desired number of seed peaks',1,6,24);
  lStepsize :=  GetFloat('Step size',0,0.001,0.5);
  lSmoothFWHM  :=  GetFloat('Smoothing (FWHM mm)',0,0,32);
  Timer1.enabled := true;    Application.processmessages;
  //for lV := 1 to OpenDialog1.files.count do begin
      threshpeaks(OpenDialog1.files, lDesiredPeaks, lStepSize,lSmoothFWHM);
      //nii_extract_object(OpenDialog1.files.Strings[lV-1],'',lOtsuLevels,lMode, lDilateVox,lNoise);
      DisplayMessages;
  //end;
  Timer1.enabled := false; ProgressBar1.position := 0;
  Memo1.lines.add('Brain extract complete');


end;

procedure SaveText (lBasename: string);
var
   S: string;
begin
  // Now change the defaults
  //S := '';
  //DateSeparator      := '_';
  //TimeSeparator      := '_';
  ShortDateFormat    := 'ddmmyy';
  ShortTimeFormat    := 'hhnn';
  DateTimeToString(S, 'ddddd_t', Now);
  uiForm.memo1.lines.saveToFile(ExtractFileDirWithPathDelim(lBasename)+S+'perf.txt');
end;

procedure TuiForm.Perfusion1Click(Sender: TObject);
begin
  Showmessage('Please use the command line perfusion tool');
  (*if Timer1.enabled then exit;
    Memo1.Lines.Clear;
    //lOpts := defaultperf;
    PerfPrefsForm.writeprefs(gPerfPrefs);
    PerfPrefsForm.showmodal;
    PerfPrefsForm.readprefs(gPerfPrefs);
    //exit;
  if not SelectFiles('Choose images to convert',true,true) then exit;



  Timer1.enabled := true;    Application.processmessages;

  perfusionanalyze(OpenDialog1.Files,gPerfPrefs);
  Timer1.enabled := false; ProgressBar1.position := 0;
  DisplayMessages;
 SaveText(OpenDialog1.Files[0]);*)
end;

procedure TuiForm.Voxelwise3Click(Sender: TObject);
var
  lMask,lVAL: ansistring;
  lPerm: integer;
begin

  if Timer1.enabled then exit;
  Memo1.Lines.Clear;
  if not OpenDialog3.execute then
     	exit;
  lVAL := OpenDialog3.FileName;
  lMask := '';
  if SelectFiles('Select brain mask [optional]',false,true) then
  	 	 lMask := OpenDialog1.FileName;
  lPerm := GetInt ('Permutations',0,0,4000);
  Timer1.enabled := true;    Application.processmessages;
  maskedwls_perm(lVAL,lMask,changefileextx(lVAL,'.nii.gz'),lPerm,1,false,false{AcceptMsg('Smooth data?')} );
  Timer1.enabled := false; ProgressBar1.position := 0;
   DisplayMessages;
   Memo1.lines.SaveToFile(extractfilepath(lVAL)+'wls_z_notes.txt');
end;

procedure TuiForm.ROI3Click(Sender: TObject);
var
  lV,lPerm: integer;//lMask: ansistring;
  lVAL: ansistring;
  lVOIs: TStringlist;
begin
	 if Timer1.enabled then exit;
	 Memo1.Lines.Clear;
     if not OpenDialog3.execute then
     	exit;
     lVAL := OpenDialog3.FileName;
  	 if not SelectFiles('Select VOI[s]',true,true) then
        	exit;
  	 lVOIs := TStringlist.Create;
  	 lVOIs.AddStrings(OpenDialog1.files);
     lPerm := GetInt ('Permutations',0,0,4000);
     Timer1.enabled := true;    Application.processmessages;
     for lV := 1 to lVOIs.count do
     	 maskedwls_perm(lVAL,lVOIs[lV-1],changefileextx(lVAL,'.nii.gz'),lPerm,1,true,false{AcceptMsg('Smooth data?')} );
     Timer1.enabled := false; ProgressBar1.position := 0;
     DisplayMessages;
     Memo1.lines.SaveToFile(extractfilepath(lVAL)+'wls_voi_notes.txt');
     lVOIs.free;
end;

procedure TuiForm.SliceTimeCorrection1Click(Sender: TObject);
var
  lSliceOrder: integer;
begin
     if Timer1.enabled then exit;
       Memo1.Lines.Clear;
     if not SelectFiles('Choose images to slice time correct',true,true) then exit;
     lSliceOrder := GetInt('Slice order (Ascending,Descending,Sequential,Interleaved,siemens): AS='+inttostr(kAscending)+', AI='+inttostr(kAscendingInterleavedPhilGE)+' DS='
  			  +inttostr(kDescending)+', DI='+inttostr(kDescendingInterleavedPhilGE)+', AIs='+inttostr(kAscendingInterleavedSiemens)+', DIs='+inttostr(kDescendingInterleavedSiemens),kAscending,kAscending, kDescendingInterleavedSiemens);
     Timer1.enabled := true; Application.processmessages;
      nii_slicetimecorrect_cubic(OpenDialog1.Files,lSliceOrder);
     Timer1.enabled := false; ProgressBar1.position := 0;
     DisplayMessages;
end;

procedure TuiForm.Smooth1Click(Sender: TObject);
begin
  if Timer1.enabled then exit;
    Memo1.Lines.Clear;
  if not SelectFiles('Choose images to smooth',true,true) then exit;
   nii_smooth_cubic(OpenDialog1.Files);
  DisplayMessages;
end;

procedure TuiForm.Smooth2Click(Sender: TObject);
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

procedure TuiForm.SmoothSec1Click(Sender: TObject);
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

procedure TuiForm.voipairedttestBtnClick(Sender: TObject);
var
  lV: integer;//lMask: ansistring;
  lVOIs,lImgNames: TStringlist;
begin
	 if Timer1.enabled then exit;
	 Memo1.Lines.Clear;
     if not SelectFiles('Select VOI[s]',true,true) then exit;
     lVOIs := TStringlist.Create;
     lVOIs.AddStrings(OpenDialog1.files);
	 if not SelectFiles('Select difference images',true,false) then exit;
  lImgNames := TStringlist.Create;
  lImgNames.AddStrings(OpenDialog1.files);
  Timer1.enabled := true; Application.processmessages;
  for lV := 1 to lVOIs.count do
  	  maskedttest(lVOIs[lV-1],'',lImgNames,0,GetInt ('Permutations',0,1000,4000),true,false);
  Timer1.enabled := false; ProgressBar1.position := 0;
     DisplayMessages;
   Memo1.lines.SaveToFile(extractfilepath(lImgNames[0])+'ttest_pv_notes.txt');
  lImgNames.Free;
  lVOIs.free;
end;

function TuiForm.SelectFiles(lCaption: string; AllowMultiSelect, AllowVOI: boolean):boolean;
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

procedure TuiForm.StrToMemo(lStr: String);
var
   lLen,lPos: integer;
   lOutStr: string;
begin
     lLen := length(lStr);
     if lLen < 1 then exit;
     lOutStr := '';
     for lPos := 1 to lLen do begin
         if lStr[lPos] = kCR then begin
            Memo1.lines.add(lOutStr);
            lOutStr := '';
         end else
             lOutStr := lOutStr + lStr[lPos];
     end;
     if lOutStr <> '' then
        Memo1.lines.add(lOutStr);
end;

procedure TuiForm.Timer1Timer(Sender: TObject);
begin
  DisplayMessages;
  ProgressBar1.position := round(1000*DebugFractionCompleted);
end;

procedure TuiForm.voiunrelatedttestBtnClick(Sender: TObject);
var
  //lMask: ansistring;
  lVOIs,lImgNames: TStringlist;
  lnGroup1,lV: integer;
begin
  if Timer1.enabled then exit;
    Memo1.Lines.Clear;
  if not SelectFiles('Select VOI[s]',true,true) then exit;
  lVOIs := TStringlist.Create;
  lVOIs.AddStrings(OpenDialog1.files);
  if not SelectFiles('Select images from GROUP 1',true,false) then exit;
  lImgNames := TStringlist.Create;
  lImgNames.AddStrings(OpenDialog1.files);
  lnGroup1 := lImgNames.Count;
  if not SelectFiles('Select images from GROUP 2',true,false) then exit;
  lImgNames.AddStrings(OpenDialog1.files);
  Timer1.enabled := true;    Application.processmessages;
  for lV := 1 to lVOIs.count do
  	  maskedttest(lVOIs[lV-1],'',lImgNames,lnGroup1,GetInt ('Permutations',0,1000,4000),true,false);
  Timer1.enabled := false; ProgressBar1.position := 0;
   DisplayMessages;
  Memo1.lines.SaveToFile(extractfilepath(lImgNames[0])+'ttest_uv_notes.txt');
  lImgNames.Free;
  lVOIs.free;
end;

procedure TuiForm.pairedttestBtnClick(Sender: TObject);
var
  lMask: ansistring;
  lImgNames: TStringlist;
begin
	 if Timer1.enabled then exit;
	 Memo1.Lines.Clear;
     if not SelectFiles('Select brain mask [optional]',false,true) then
        lMask := ''
     else
  	 	 lMask := OpenDialog1.FileName;
  if not SelectFiles('Select difference images',true,false) then exit;
  lImgNames := TStringlist.Create;
  lImgNames.AddStrings(OpenDialog1.files);
  Timer1.enabled := true;    Application.processmessages;
   maskedttest(lMask,'',lImgNames,0,GetInt ('Permutations',0,1000,4000),false,AcceptMsg('Smooth data?') );
  Timer1.enabled := false; ProgressBar1.position := 0;
   DisplayMessages;
     DisplayMessages;
   Memo1.lines.SaveToFile(extractfilepath(lImgNames[0])+'ttest_p_notes.txt');
  lImgNames.Free;
end;

procedure TuiForm.unrelatedttestBtnClick(Sender: TObject);
var
  lMask: ansistring;
  lImgNames: TStringlist;
  lnGroup1: integer;
begin
  if Timer1.enabled then exit;
    Memo1.Lines.Clear;
  if not SelectFiles('Select brain mask [optional]',false,true) then
     lMask := ''
  else
    	 lMask := OpenDialog1.FileName;

  if not SelectFiles('Select images from GROUP 1',true,false) then exit;
  lImgNames := TStringlist.Create;
  lImgNames.AddStrings(OpenDialog1.files);
  lnGroup1 := lImgNames.Count;
  if not SelectFiles('Select images from GROUP 2',true,false) then exit;
  lImgNames.AddStrings(OpenDialog1.files);
  Timer1.enabled := true;    Application.processmessages;
   maskedttest(lMask,'',lImgNames,lnGroup1,GetInt ('Permutations',0,1000,4000),false, AcceptMsg('Smooth data?'));
  Timer1.enabled := false; ProgressBar1.position := 0;
   DisplayMessages;
  Memo1.lines.SaveToFile(extractfilepath(lImgNames[0])+'ttest_u_notes.txt');
  lImgNames.Free;
end;

procedure TuiForm.PartBtnClick(Sender: TObject);
var
   lPhysioNames: TStringlist;
  lImgFilename,lPhysioFilename2,lOutname,lComments: string;
  lSliceOrder,bins: integer;
begin
   //DemoPart;exit;
    if Timer1.enabled then exit;
	Memo1.Lines.Clear;
   if not SelectFiles('Select 4D image to correct.',false,false) then exit;
  lImgFilename := OpenDialog1.filename;
  if not OpenDialog2.execute then exit;
  lPhysioNames := TStringlist.Create;
  lPhysioNames.AddStrings(OpenDialog2.files);
  if lPhysioNames.count = 1 then begin
  	 if UpCaseExt(lPhysioNames[0]) = '.PULS' then
     lPhysioFilename2 := (changefileext(lPhysioNames[0],'.resp'))
  	 else
  	  lPhysioFilename2 := (changefileext(lPhysioNames[0],'.puls'));
     if fileexists(lPhysioFilename2) then
     	lPhysioNames.add(lPhysioFilename2);
  end;
  bins := 20;//GetInt ('Number of bins?', 5, 20,60);
  lSliceOrder := GetInt('Slice order (Ascending,Descending,Sequential,Interleaved,siemens): AS='+inttostr(kAscending)+', AI='+inttostr(kAscendingInterleavedPhilGE)+' DS='
  			  +inttostr(kDescending)+', DI='+inttostr(kDescendingInterleavedPhilGE)+', AIs='+inttostr(kAscendingInterleavedSiemens)+', DIs='+inttostr(kDescendingInterleavedSiemens),kAscending,kAscending, kDescendingInterleavedSiemens);

  //lSliceOrder := GetInt('Slice order: ascend='+inttostr(kAscending)+', ascendInterleaved='+inttostr(kAscendingInterleaved)+' descend='
  //			  +inttostr(kDescending)+', DescendInterleaved='+inttostr(kDescendingInterleaved),kAscending,kAscending, kDescendingInterleaved);
  lOutname := ChangeFilePrefix(lImgFilename,'p');
  lComments := ApplyPart( lImgFilename,lOutname,'', lPhysioNames,bins,lSliceOrder,0,0,true,false);
  lPhysioNames.free;
  DisplayMessages;
   StrToMemo(lComments);
end;

procedure TuiForm.InspectBtnClick(Sender: TObject);
begin
  Txx; exit;
  if Timer1.enabled then exit;
    Memo1.Lines.Clear;
  if not SelectFiles('Choose images to inspect',true,true) then exit;
   InspectVols(OpenDialog1.Files);
   DisplayMessages;
end;

procedure TuiForm.AddBtnClick(Sender: TObject);
begin
    if not SelectFiles('Choose images to add',true,true) then exit;
  if not SaveDialog1.Execute then exit;
  AddVols3D(OpenDialog1.Files,SaveDialog1.FileName);
  DisplayMessages;
end;

procedure TuiForm.AddLesionVol1Click(Sender: TObject);
begin
     if not OpenDialog3.execute then
     	exit;
     AddLesionVolumeToValFile( OpenDialog3.FileName);
     DisplayMessages;
end;

procedure TuiForm.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TuiForm.Extract1Click(Sender: TObject);
var
  lDilateVox: single;
  lNoise: boolean;
  lMode,lV,lOtsuLevels: integer;
begin
  if not SelectFiles('Select Image[s] to brain extract',true,true) then
     exit;
  lOtsuLevels :=  GetInt('Otsu levels: larger values for larger volumes',1,5,5);
  lDilateVox :=  GetInt('Edge dilation voxels: larger values for larger volumes',0,4,12);
  lNoise :=  OKMsg('Fill air with normally distributed noise?');
  if not  OKMsg('Only extract single largest object?') then
     lMode := kMorphDeleteLargestDark
  else begin
       if lNoise then
          lMode := kMorphPreserveLargestBrightAndInclusions
       else
        lMode := kMorphPreserveLargestBright;


  end;
  Timer1.enabled := true;    Application.processmessages;
  for lV := 1 to OpenDialog1.files.count do begin
      nii_extract_object(OpenDialog1.files.Strings[lV-1],lOtsuLevels,lMode, lDilateVox,0);
      DisplayMessages;
  end;
  Timer1.enabled := false; ProgressBar1.position := 0;
  Memo1.lines.add('Brain extract complete');
end;

(*function Random_Normal: Extended;
//http://www.netlib.org/random/amrandom.pas
{ Adapted from the following Fortran 77 code
	 ALGORITHM 712, COLLECTED ALGORITHMS FROM ACM.
	 THIS WORK PUBLISHED IN TRANSACTIONS ON MATHEMATICAL SOFTWARE,
	 VOL. 18, NO. 4, DECEMBER, 1992, PP. 434-435.

  The function random_normal() returns a normally distributed pseudo-random
  number with zero mean and unit variance.

  The algorithm uses the ratio of uniforms method of A.J. Kinderman
  and J.F. Monahan augmented with quadratic bounding curves. }
const
	VSmall = MinDouble;
const
	s = 0.449871;
	t = -0.386595;
	a = 0.19600;
	b = 0.25472;
	r1 = 0.27597;
	r2 = 0.27846;
var
	u, v, x, y, q: Extended;
	Done: Boolean;

begin
	Done := False;
	repeat
		u := Random;
		v := Random;
		v := 1.7156 * (v - 0.5);
		// Evaluate the quadratic form
		x := u - s;
		Y := abs (v) - t;
		q := Sqr (x) + y * (a * y - b * x);
		// Accept P if inside inner ellipse
		if (q < r1) then
			Done := True
		else if (q <= r2) and (Sqr (v) < -4.0 * Ln (u) * Sqr (u)) then
			Done := True;
	until Done;
	// Return ratio of P's coordinates as the normal deviate
	if u < VSmall then
		result := Random_Normal
        else
            Result := v / u;
end;

procedure IQ;
const
 k= 8000;
var
  i: integer;
  Q1, Q2, Q3, IQR: single;
  ra: array of single;
begin
  randomize;
  setlength(ra,k);
  for i := 0 to (k-1) do
      ra[i] := random(100)+1;
  Quartiles32(ra, Q1, Q2, Q3);
  UiForm.Memo1.lines.add(realtostr(Q1,1)+'x'+realtostr(Q2,1)+'x'+realtostr(Q3,1));
  IQR := Q3 -Q1;
  for i := 0 to (k-1) do
      ra[i] := Q2+ (Random_Normal * IQR * 0.67448);
  //50% of the distribution lies within 0.67448 standard deviations of the mean
  //http://www.regentsprep.org/Regents/math/algtrig/ATS2/NormalLesson.htm
      Quartiles32(ra, Q1, Q2, Q3);
  UiForm.Memo1.lines.add(realtostr(Q1,3)+'x'+realtostr(Q2,3)+'x'+realtostr(Q3,3));
end;   *)
procedure stack (lImages: Tstringlist; lHdr: string);
const
  kBytes = 256*256*2;
var
  nok,i,n,sz: integer;
  lOutname: string;
  lOut,lIn: file;
  lDat: bytep;
begin
  limages.Sort;
  n := lImages.count;
  nok := 0;
  for i := 0 to (n-1) do begin
      sz := FSize(lImages[i]);
      if sz <> kBytes then
         riteln(inttostr(sz)+' - '+lImages[i])
      else begin
          riteln('+'+lImages[i]);
          if nok = 0 then
               lOutname := changefileprefix(lImages[i],'m') ;
          inc(nok);
      end;
  end;
  if nok < 1 then exit;
  riteln(lHdr);
  riteln(  ChangeFileExt(lOutname,'.hdr'));
  nii_copyhdr(lHdr,ChangeFileExt(lOutname,'.hdr'),nok);
  riteln(inttostr(nok)+' images');
  Filemode := 2;
  AssignFile(lOut, lOutname);
  //if fileexists(lOutname) then
  //  Reset(lOut, 1)
  //else
    Rewrite(lOut, 1);
  getmem(lDat,kBytes);
  for i := 0 to (n-1) do begin
      sz := FSize(lImages[i]);
      if sz = kBytes then begin
          AssignFile(lIn, lImages[i]);
          Reset(lIn, 1);
          BlockRead(lIn, lDat^[1], kBytes);
          CloseFile(lIn);
          BlockWrite(lOut, lDat^[1], kBytes);
      end;
  end;
  freemem(lDat);
  CloseFile(lOut);
end;


(*&procedure TuiForm.Txx;
var
  lImages: TstringList;
begin
  lImages   := TStringlist.Create;
  OpenDialog1.filter := '*.*';
  //OpenDialog1.InitialDir:= '/Users/rorden/segdata/20Normals_T1/1_24';
    OpenDialog1.Options := [ofAllowMultiSelect,ofFileMustExist] ;
  IF NOT OpenDialog1.Execute then exit ;
  lImages.AddStrings(OpenDialog1.files);
  stack(lImages,'/Users/rorden/segdata/a.hdr');
  DisplayMessages;
  lImages.free;

end;*)
procedure MatchSlices(lSrc,lRef: string);
var
  lS,lR: TNIFTIimg;
begin
     createNII(lS);
     createNII(lR);
     ReadNIIHdr(lSrc, lS);
     ReadNIIHdr(lRef, lR);
     nii_cropslicesx (lSrc,lS.Hdr.dim[3]-lR.Hdr.dim[3],lR);
     freeNII(lS);
     freeNII(lR);
end;

function shiftslices (lName: string):boolean;
var
  lO, lnVox,lV: integer;
  lNII: TNIfTIimg;
begin
     result := false;
     if (not fileexists(lName)) then
      exit;
     lO := GetInt('Slice  to shift for '+lName,-100,0, 100);
     if lO = 0 then
        exit;
     CreateNII(lNII);
     if not Read4DF32(lName, lNII) then
        exit;

     lO := lNII.hdr.dim[1]*lNII.hdr.dim[2]*-lO;

     lnVox := lNII.hdr.dim[1]*lNII.hdr.dim[2]*lNII.hdr.dim[3];
     if lO > 0 then begin
        for lV := 1 to (lnVox-lO) do
            lNII.f32^[lV] := lNII.f32^[lV+lO];
         for lV := (lnVox-lO+1) to lnVox do
             lNII.f32^[lV] := 0;
     end else begin
         lO := abs(lO);
         for lV := (lnVox) downto lO do
             lNII.f32^[lV] := lNII.f32^[lV-lO]; //lO negative!
          for lV := 1 to (lO-1) do
              lNII.f32^[lV] := 0;

     end; //each vol
     WriteNII(changefileprefix(lNII.HdrName,'a'),lNII);
     FreeNII(lNII);
end;

procedure TuiForm.Txx;
var
  lImages: TstringList;
  lI: integer;
begin
       if not SelectFiles('Select nii',true,true) then  exit;
        for lI := 1 to OpenDialog1.files.count do begin
            ShiftSlices( OpenDialog1.files[lI-1]);
            //MatchSlices(CHANGEFILEEXT(OpenDialog1.files[lI-1],'.hdr'), OpenDialog1.files[lI-1])
            //MatchSlices('/Users/rorden/desktop/o/1_24.hdr','/Users/rorden/desktop/o/1_24.nii');

        end;
        exit;

  lImages   := TStringlist.Create;
  OpenDialog1.filter := '*.*';
  //OpenDialog1.InitialDir:= '/Users/rorden/segdata/20Normals_T1/1_24';
    OpenDialog1.Options := [ofAllowMultiSelect,ofFileMustExist] ;
  IF NOT OpenDialog1.Execute then exit ;
  lImages.AddStrings(OpenDialog1.files);
  stack(lImages,'/Users/rorden/segdata/a.hdr');
  DisplayMessages;
  lImages.free;

end;

procedure TuiForm.Memo1Change(Sender: TObject);
begin

end;

procedure To8Bit (lName: string; lBubbleFill: boolean);
var
  lNII: TNIFTIIMg;
begin
     CreateNII(lNII);
     Read4Dbyte(changefileprefix(lName,'sm'),lNII);
     //BubbleFill (var lImg: Bytep; lXi,lYi,lZi: integer; lAirValue,lFillVal: byte  );
       BubbleFill (lNII.i8, lNII.Hdr.dim[1],lNII.Hdr.dim[2],lNII.Hdr.dim[3],0 ,255);
     WriteNII(changefileprefix(lName,'s'),lNII);
     FreeNII(lNII);
end;
procedure TuiForm.Bug;
var OK: boolean;
  lMask: string;
  lI: integer;
  lImages: TstringList;
begin
  exit;
  makeTPM ('/Users/rorden/img/f/c1.nii','/Users/rorden/img/f/c2.nii','/Users/rorden/img/f/c3.nii','/Users/rorden/img/f/c4.nii','/Users/rorden/img/f/c5.nii','/Users/rorden/img/f/brainmask4.voi',0);

  nii_smooth_gauss('/Users/rorden/img/f/4dc1.nii',4);
  nii_4Dunity('/Users/rorden/img/f/s4dc1.nii',0.995);
 //maskTPM('/Users/rorden/img/final/motorthick3.voi','/Users/rorden/img/tpm0mm.nii', 0.85,-0.85,0,0,0,0,3); //-1: bone -> NonBrain and air

  //copybottom('/Users/rorden/img/c5.nii',4);
  //makeTPM ('/Users/rorden/img/f/c1.nii','/Users/rorden/img/f/c2.nii','/Users/rorden/img/f/c3.nii','/Users/rorden/img/f/c4.nii','/Users/rorden/img/f/c5.nii','/Users/rorden/img/f/brainmask3.voi',0);
  exit;
     if not SelectFiles('Select nii',true,true) then  exit;
        for lI := 1 to OpenDialog1.files.count do begin
            nii_pastehdr8 (OpenDialog1.files[lI-1],changefileprefix(OpenDialog1.files[lI-1],'s'));
            //To8bit( OpenDialog1.files[lI-1],true);
            //ShiftSlices( OpenDialog1.files[lI-1]);
            //MatchSlices(CHANGEFILEEXT(OpenDialog1.files[lI-1],'.hdr'), OpenDialog1.files[lI-1])
            //MatchSlices('/Users/rorden/desktop/o/1_24.hdr','/Users/rorden/desktop/o/1_24.nii');

        end;
  exit;
  if not SelectFiles('Select nii',true,true) then  exit;
   for lI := 1 to OpenDialog1.files.count do begin
       lMask := OpenDialog1.files[lI-1];
       MatchSlices(Changefileext(Lmask,'.hdr'),lMask);
       //MatchSlices('/Users/rorden/desktop/o/1_24.hdr','/Users/rorden/desktop/o/1_24.nii');

   end;
  //OK := nii_cropslices ('/Users/rorden/desktop/o/1_24.hdr',-8);
  exit;
  OK := nii_cropslices ('/Users/rorden/super/TPM5large.nii',78);
  nii_pastehdr ('/Users/rorden/super/TPM.nii','/Users/rorden/super/cTPM5large.nii');
  //maskTPM('/Users/rorden/a/not_bones.voi','/Users/rorden/a/template_2.nii', 0,0,0,-1,0.5,0); //-1: bone -> NonBrain and air

exit;
  if not SelectFiles('Select mask',false,true) then
       exit;
    lMask:= OpenDialog1.filename;
    if not SelectFiles('Select template to be masked',false,false) then
       exit;
    //maskTPM(lMask, OpenDialog1.filename, 0,-1,0,0,0,0);


end;

(*procedure TuiForm.Bug;
var OK: boolean;
  lImages: Tstrings;
begin
    if not SelectFiles('Select Image[s] to brain extract',true,true) then
     exit;
  exit;

  lImages   := TStringlist.Create;
   //nii_scale ('/Users/rorden/super/i4/noeyes/oldm5.nii','/Users/rorden/super/i4/noeyes/smarrow.nii',0.45);
  nii_4Dunity ('/Users/rorden/super/tpmx.nii',0.99);
  lImages.Add('/Users/rorden/super/utpmx.nii');;
  nii_smooth_gauss (lImages, 3);

  //OK := nii_cropslices ('/Users/rorden/super/sutpmx.nii',78);
  //nii_pastehdr ('/Users/rorden/super/TPM.nii','/Users/rorden/super/csutpmx.nii');
  lImages.free;
end;

var
  lDilateVox: single;
  lOneContiguousObject, lNoise: boolean;
  lV,lOtsuLevels: integer;
begin
  lOtsuLevels :=  5;
  lDilateVox :=  4;
  lNoise :=  true;
  lOneContiguousObject := true;
  Timer1.enabled := true;    Application.processmessages;

      nii_extract_object('/Users/rorden/Desktop/head_2.nii','',lOtsuLevels, lDilateVox,lNoise, lOneContiguousObject);
      DisplayMessages;

  Timer1.enabled := false; ProgressBar1.position := 0;
  Memo1.lines.add('Brain extract complete');


end; //end*)

procedure TuiForm.FormShow(Sender: TObject);
begin
   Application.ShowButtonGlyphs:= sbgNever;
   {$IFNDEF GUI}showmessage('Compile error: please define GUI in isgui.inc'); {$ENDIF}
   UiForm.Memo1.lines.add(kVers);
   Bug;
end;

procedure TuiForm.BETClick(Sender: TObject);
var
  lFrac: single;
  lV: integer;
begin
  if not SelectFiles('Select Image[s] to brain extract',true,true) then
     exit;
  lFrac := GetFloat('Brain extraction fraction (smaller values lead to larger brain volume)',0.1,0.3,0.9);
  Timer1.enabled := true;    Application.processmessages;
  for lV := 1 to OpenDialog1.files.count do begin
      FSLbet(OpenDialog1.files.Strings[lV-1],lFrac);
      DisplayMessages;
  end;
  Timer1.enabled := false; ProgressBar1.position := 0;
  Memo1.lines.add('Brain extract complete');
end;

procedure TuiForm.MCFLIRTClick(Sender: TObject);
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

procedure TuiForm.FLIRTClick(Sender: TObject);
begin
  if not SelectFiles('Select image to normalize (if multiple, estimates from 1st are applied to all)',true,true) then
     exit;
  Timer1.enabled := true;    Application.processmessages;
  FSLflirt(OpenDialog1.files);
  DisplayMessages;
  Timer1.enabled := false; ProgressBar1.position := 0;
  Memo1.lines.add('Normalize complete');
end;

procedure TuiForm.FormCreate(Sender: TObject);
begin

end;

procedure TuiForm.SliceTimerClick(Sender: TObject);
  var
    lTR: single;
    lV,lSliceOrder: integer;
  begin
    if not SelectFiles('Select 4D image[s] to slice time correct',true,true) then
       exit;
    lSliceOrder := GetInt('Slice order (Ascending,Descending,Sequential,Interleaved,siemens): AS='+inttostr(kAscending)+', AI='+inttostr(kAscendingInterleavedPhilGE)+' DS='
  			  +inttostr(kDescending)+', DI='+inttostr(kDescendingInterleavedPhilGE)+', AIs='+inttostr(kAscendingInterleavedSiemens)+', DIs='+inttostr(kDescendingInterleavedSiemens),kAscending,kAscending, kDescendingInterleavedSiemens);

    //lSliceOrder := GetInt('Slice order: ascend='+inttostr(kAscending)+', ascendInterleaved='+inttostr(kAscendingInterleaved)+' descend='
  //			  +inttostr(kDescending)+', DescendInterleaved='+inttostr(kDescendingInterleaved),kAscending,kAscendingInterleaved, kDescendingInterleaved);
    lTR := GetFloat('Time Per Volume (TR, sec)',0.1,3.0,20);
    Timer1.enabled := true;    Application.processmessages;
    for lV := 1 to OpenDialog1.files.count do begin
        FSLslicetimer(OpenDialog1.files.Strings[lV-1],lSliceOrder,lTR);
        DisplayMessages;
    end;
    Timer1.enabled := false; ProgressBar1.position := 0;
    Memo1.lines.add('Slice timer complete');

end;


end.

