program partui;

uses
  Forms,
  ui in 'ui.pas' {uiform},
  nii_math,
  ezdicom in 'ezdicom.pas',
  nii_ttest in 'nii_ttest.pas',
  nii_intennorm in 'nii_intennorm.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(Tuiform, uiform);
  Application.Run;
end.
