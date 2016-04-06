unit uprefsform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,nii_perf;

type

  { TPerfPrefsForm }

  TPerfPrefsForm = class(TForm)
    BaselineVols: TEdit;
    FinalVol: TEdit;
    MaskVox: TEdit;
    MaskThreshSD: TEdit;
    AIFVox: TEdit;
    Label9: TLabel;
    OKbtn: TButton;
    Label3: TLabel;
    DeleteVols: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    TRsec: TEdit;
    STCdrop: TComboBox;
    Label2: TLabel;
    SmoothFWHM: TEdit;
    Label1: TLabel;
    procedure SmoothFWHMChange(Sender: TObject);
    procedure writeprefs (var lPrefs: TPerfOpts);
    procedure readprefs (var lPrefs: TPerfOpts);

  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  PerfPrefsForm: TPerfPrefsForm;

implementation

{$R *.lfm}

{ TPerfPrefsForm }
procedure  TPerfPrefsForm.writeprefs (var lPrefs: TPerfOpts);
begin
  //

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
  FloatEdit(lPrefs.TRsec,SmoothFWHM);
  lPrefs.SliceTimeCorrect:= STCdrop.ItemIndex;
  FloatEdit(lPrefs.SmoothFWHM,SmoothFWHM);
  IntEdit(lPrefs.DeleteVols,DeleteVols);
  IntEdit(lPrefs.BaselineVols,BaselineVols);
  IntEdit(lPrefs.FinalVol,FinalVol);
  IntEdit(lPrefs.MaskVox,FinalVol);
  FloatEdit(lPrefs.MaskThreshSD,MaskThreshSD);
  IntEdit(lPrefs.AIFVox,AIFVox);

end;

procedure TPerfPrefsForm.SmoothFWHMChange(Sender: TObject);
begin

end;

end.

