unit uperfprefsform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,//nii_perf,
  define_types, types, fsl_calls;

type

  { TPerfPrefsForm }

  TPerfPrefsForm = class(TForm)
    AIFpeak: TCheckBox;
    FSLdir: TButton;
    Normalize: TCheckBox;
    ComputeRaw: TCheckBox;
    ComputeFitted: TCheckBox;
    Label11: TLabel;
    Label6: TLabel;
    minR2: TEdit;
    BaselineVols: TEdit;
    Label10: TLabel;
    MotionCorrect: TCheckBox;
    BrainExtract: TCheckBox;
    FinalVol: TEdit;
    MaskThreshSD: TEdit;
    AIFVox: TEdit;
    Label9: TLabel;
    OKbtn: TButton;
    Label3: TLabel;
    DeleteVols: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    DefaultBtn: TButton;
    SmoothFWHMsec: TEdit;
    TRsec: TEdit;
    STCdrop: TComboBox;
    Label2: TLabel;
    SmoothFWHMmm: TEdit;
    Label1: TLabel;
    TEmsec: TEdit;
    procedure DefaultBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FSLdirClick(Sender: TObject);
    procedure writeprefs (var lPrefs: TPerfOpts);
    procedure readprefs (var lPrefs: TPerfOpts);

  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  PerfPrefsForm: TPerfPrefsForm;
  gPerfPrefs:  TPerfOpts;

implementation

{$R *.lfm}

{ TPerfPrefsForm }

procedure wFloatEdit (var f: single; E: TEdit);
begin
	 E.Caption:= realtostr(f,5);
end;

procedure wIntEdit (var i: integer; E: TEdit);
begin
	 E.Caption:= inttostr(i);
end;

procedure  TPerfPrefsForm.writeprefs (var lPrefs: TPerfOpts);
begin
  wFloatEdit(lPrefs.TEmsec,TEmsec);
  wFloatEdit(lPrefs.TRsec,TRsec);
  STCdrop.ItemIndex := lPrefs.SliceTimeCorrect;
  wFloatEdit(lPrefs.SmoothFWHMmm,SmoothFWHMmm);
  wFloatEdit(lPrefs.SmoothFWHMsec,SmoothFWHMsec);
  wIntEdit(lPrefs.DeleteVols,DeleteVols);
  wIntEdit(lPrefs.BaselineVols,BaselineVols);
  wIntEdit(lPrefs.FinalVol,FinalVol);
  //wIntEdit(lPrefs.MaskVox,MaskVox);
  wFloatEdit(lPrefs.MaskThreshSD,MaskThreshSD);
  wFloatEdit(lPrefs.MinR2,MinR2);
  wIntEdit(lPrefs.AIFVox,AIFVox);
  ComputeRaw.checked := lPrefs.ComputeRaw;
  ComputeFitted.Checked := lPrefs.ComputeFitted;
  BrainExtract.checked := lPrefs.BrainExtract;
  MotionCorrect.checked := lPrefs.MotionCorrect;
  Normalize.checked := lPrefs.Normalize;
  AIFpeak.checked := lPrefs.AIFpeak;
end;

procedure FloatEdit (var f: single; E: TEdit);
var
  S: string;
  V: single;
begin
	 S := E.text;
     try
        V := strtofloat(S);
     except
           on EConvertError do begin
              showmessage('FloatEdit Error - Unable to convert the string '+S+' to a float');
              exit;
           end;
     end;
     f := V;
end;

procedure IntEdit (var I: integer; E: TEdit);
var
  S: string;
  V: integer;
begin
	 S := E.text;
     try
        V := strtoint(S);
     except
           on EConvertError do begin
              showmessage('IntEdit Error - Unable to convert the string '+S+' to a integer');
              exit;
           end;
     end;
     I := V;
end;

procedure  TPerfPrefsForm.readprefs (var lPrefs: TPerfOpts);
begin
  FloatEdit(lPrefs.TRsec,TRsec);
  FloatEdit(lPrefs.TEmsec,TEMsec);

  lPrefs.SliceTimeCorrect:= STCdrop.ItemIndex;
  FloatEdit(lPrefs.SmoothFWHMmm,SmoothFWHMmm);
  FloatEdit(lPrefs.SmoothFWHMsec,SmoothFWHMsec);
  IntEdit(lPrefs.DeleteVols,DeleteVols);
  IntEdit(lPrefs.BaselineVols,BaselineVols);
  IntEdit(lPrefs.FinalVol,FinalVol);
  //IntEdit(lPrefs.MaskVox,MaskVox);
  FloatEdit(lPrefs.MaskThreshSD,MaskThreshSD);
  FloatEdit(lPrefs.MinR2,MinR2);
  IntEdit(lPrefs.AIFVox,AIFVox);
  lPrefs.ComputeRaw := ComputeRaw.checked;
  lPrefs.ComputeFitted := ComputeFitted.checked;
  lPrefs.BrainExtract := BrainExtract.Checked;
  lPrefs.MotionCorrect := MotionCorrect.Checked;

  lPrefs.Normalize := Normalize.Checked;
  lPrefs.AIFpeak := AIFpeak.Checked;
end;

procedure TPerfPrefsForm.DefaultBtnClick(Sender: TObject);
begin
  gPerfPrefs := defaultperf;
  writeprefs(gPerfPrefs);
end;


procedure TPerfPrefsForm.FormCreate(Sender: TObject);
begin
  {$IFNDEF UNIX}
  BrainExtract.enabled := false;
  MotionCorrect.enabled := false;
  Normalize.enabled := false;
  {$ENDIF}
  gPerfPrefs := defaultperf;
end;

procedure TPerfPrefsForm.FSLdirClick(Sender: TObject);
var
  S: string;
begin
    S := gFSLbase;
    if InputQuery('FSL dir','Please specify FSL directory ', S) then
       gFSLbase := S;
end;


end.
