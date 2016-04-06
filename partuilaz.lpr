program partuilaz;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, uilaz, nii_smooth, nii_wlsreg, nii_gen, nii_modvalformat,
  nii_wls_thread, gamma_powell, //uperfprefsform,
  nii_extract, nii_mask, arrayu,
  nii_peak;

{$R *.res}

begin
  Application.Title:='masked_stats';
  Application.Initialize;
  Application.CreateForm(TuiForm, uiForm);
  //Application.CreateForm(TPerfPrefsForm, PerfPrefsForm);
  Application.Run;
end.
