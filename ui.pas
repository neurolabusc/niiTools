unit ui;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ToolWin, ComCtrls,nii_core,nii_math, upart, define_types,ezdicom, nii_ttest;

type
  Tuiform = class(TForm)
    ToolBar1: TToolBar;
    AddBtn: TButton;
    Memo1: TMemo;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    InspectBtn: TButton;
    PartBtn: TButton;
    OpenDialog2: TOpenDialog;
    ttestBtn: TButton;
    procedure DisplayMessages;
    function SelectFiles(lTitle: string; AllowMultiSelect: boolean):boolean;
    procedure AddBtnClick(Sender: TObject);
    procedure InspectBtnClick(Sender: TObject);
    procedure PartBtnClick(Sender: TObject);
    procedure StrToMemo(lStr: String);
    procedure ttestBtnClick(Sender: TObject);
    //procedure SelectFiles(AllowMultiSelect: boolean): boolean;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  uiform: Tuiform;

implementation

{$R *.dfm}
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

function GetInt (title: string; min, default,max: integer): integer;
var
s: string;
begin
  s := inttostr(default);
  InputQuery('Integer required',title,s);
  try
     	result := StrToInt(S);
  except
    on Exception : EConvertError do
      result := default;
  end;
end;

procedure Tuiform.DisplayMessages;
begin
  Memo1.Lines.Clear;
  Memo1.Lines.AddStrings(DebugStrings);
  DebugStrings.Clear;
end;

function Tuiform.SelectFiles(lTitle: string; AllowMultiSelect: boolean):boolean;
begin
  OpenDialog1.Title := lTItle;
  if AllowMultiSelect then
    OpenDialog1.Options := [ofAllowMultiSelect,ofFileMustExist]
  else
    OpenDialog1.Options := [ofFileMustExist];
  result := OpenDialog1.Execute;
end;

procedure Tuiform.AddBtnClick(Sender: TObject);
begin
  if not SelectFiles('Select images to combine',true) then exit;
  if not SaveDialog1.Execute then exit;
  AddVols3D(OpenDialog1.Files,SaveDialog1.FileName);
  DisplayMessages;
end;

procedure Tuiform.InspectBtnClick(Sender: TObject);
begin
  if not SelectFiles('Select image[s] to inspect',true) then exit;
  InspectVols(OpenDialog1.Files);
  DisplayMessages;

end;

procedure Tuiform.PartBtnClick(Sender: TObject);
var
   lPhysioNames: TStringlist;
  lImgFilename,lPhysioFilename2,lOutname,lComments,lDICOMname: string;
  bins: integer;
begin
   if not SelectFiles('Select images for correction',false) then exit;
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
  lOutname := ChangeFilePrefix(lImgFilename,'p');
  lComments := ApplyPart( lImgFilename,lOutname,'',lPhysioNames,bins,0,0,false,false);
  lPhysioNames.free;
  DisplayMessages;
   StrToMemo(lComments);
end;


procedure Tuiform.ttestBtnClick(Sender: TObject);
var
  lMask: ansistring;
  lImgNames: TStringlist;
begin
  (*if not SelectFiles('Select mask ',true) then exit;
  lMask := OpenDialog1.FileName;
  if not SelectFiles('Select images from patients with deficit',true) then exit;
  lImgNames := TStringlist.Create;
  lImgNames.AddStrings(OpenDialog1.files);
  //if not SaveDialog1.Execute then exit;
   maskedttest(lMask,'c:\pas\test22.nii.gz',lImgNames);
   DisplayMessages;
  lImgNames.Free; *)
end;

end.
