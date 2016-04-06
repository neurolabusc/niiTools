unit nii_gen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, nii_core;

procedure Generate_NII(lFilename: string; lDim: integer; lVal: single);

implementation

procedure Generate_NII(lFilename: string; lDim: integer; lVal: single);
var
  lNII:TNIfTIimg;
  i: integer;
begin
    if lDim < 1 then
       exit;
    CreateNII(lNII);
    Force3DNII(lNII);
    lNII.Hdr.dim[1] := lDim;
    lNII.Hdr.dim[2] := lDim;
    lNII.Hdr.dim[3] := lDim;
    CreateZeroedFloat3DNII(lNII,lNII, lFilename);
     for i := 1 to (lNII.VoxelsLoaded div 2) do
      lNII.f32^[i] := lVal;
    WriteNII(lFilename,lNII);
    FreeNII(lNII);
end;


end.

