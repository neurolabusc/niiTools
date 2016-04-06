unit nii_wls_thread;

{$mode objfpc}{$H+}

interface

uses

  classes, define_types,nii_core, sysutils,uregmult, nii_ttest,utypes;

type
TChatMemberThread = class(TThread)
private
  FName: string;


protected
  procedure Execute; override;
public
  constructor Create(Name: string);
  destructor Destroy; override;
end;



implementation

constructor TChatMemberThread.Create(Name: string);
begin
  inherited Create(True);
  FName := Name;
  FreeOnTerminate := True;
end;

destructor TChatMemberThread.Destroy;
begin
  //

  inherited;
end;

function PermutedZ (lnObservations,lnFactors,lFactorToTest,lnPermute: integer;var Xf: PMatrix; var Y: PVector; lPermOrder: PIntMatrix; var lMaxZ,lMinZ: PVector): double;
var
  lP,lF,lO,lDF : integer;
  X    : PMatrix;
  lPermT: double;
  lOutSlope,lOutT: DoubleP0;
begin
  lDF := lnObservations-lnFactors-1;
  if lDF < 1 then
     exit;
  Getmem(lOutSlope,(lnFactors+1)*sizeof(double));
  Getmem(lOutT,(lnFactors+1)*sizeof(double));
  DimMatrix(X, lnFactors, lnObservations);
  MultipleRegressionVec (lnObservations,lnFactors,Xf,Y,lOutT,lOutSlope);
  result := TtoZ(lOutT^[lFactorToTest-1],lDF);
  for lP := 1 to lnPermute do begin
    for lF := 1 to (lnFactors) do begin
      for lO := 1 to lnObservations do
        X^[lF]^[lO] := Xf^[lF]^[lPermOrder^[lP]^[lO] ];
    end;
    MultipleRegressionVec (lnObservations,lnFactors,X,Y,lOutT,lOutSlope);
    lPermT := TtoZ(lOutT^[lFactorToTest-1],lDF);
    if lPermT < lMinZ^[lP] then
       lMinZ^[lP] := lPermT;
    if lPermT > lMaxZ^[lP] then
       lMaxZ^[lP] := lPermT;
  end;//for each Permutation
  DelMatrix(X, lnFactors,lnObservations);
  Freemem(lOutT);
  Freemem(lOutSlope);
end;

function  ReturnResiduals (lnObservations,lnFactors: integer; X: PMatrix; Y: PVector; var Yresidual: PVector; lOutSlope: DoubleP0): boolean;
var
  lEstimate: double;
  lF,lO: integer;
begin
    result := false;
    if (lnObservations < 1) or (lnfactors < 1) then
      exit;
    for lO := 1 to lnObservations do begin
      lEstimate :=  lOutSlope^[lnFactors];//constant
      for lF := 1 to (lnFactors) do
        lEstimate := lEstimate+ (X^[lF]^[lO] *lOutSlope^[lF-1]);
      //residual
      Yresidual^[lO] := Y^[lO]-lEstimate ;
    end;
    result := true;
end;

function MultipleRegressionPermutedZ (lnObservations,lnFactors,lFactorToTest,lnPermute: integer;  var Xf: PMatrix; var Y: PVector; lPermOrder: PIntMatrix; var lMaxZ,lMinZ: PVector): double;
label
  666;
var
  lO,lF,lDF,lFx : integer;
  Yresidual: PVector;
  X    : PMatrix;
  lOutSlope,lOutT: DoubleP0;
begin
  result := 0;
  lDF := lnObservations-lnFactors-1;
  if (lDF < 1) or (lFactorToTest < 1) or (lFactorToTest >lnFactors) then
    exit;
  if lnFactors < 2 then begin
     result := PermutedZ (lnObservations,lnFactors,lFactorToTest,lnPermute, Xf, Y,lPermOrder, lMaxZ,lMinZ);
      exit;
  end;
  //regress out all other factors...
  DimMatrix(X, lnFactors-1, lnObservations);
  DimVector(Yresidual, lnObservations);
  Getmem(lOutSlope,(lnFactors+1)*sizeof(double));
  Getmem(lOutT,(lnFactors+1)*sizeof(double));
  lFx := 0;
  for lF := 1 to (lnFactors) do begin
    if lF <> lFactorToTest then begin
      inc(lFx);
      for lO := 1 to lnObservations do
        X^[lFx]^[lO] := Xf^[lF]^[lO];
    end;
  end;

  if not MultipleRegressionVec (lnObservations,lnFactors-1,X,Y,lOutT,lOutSlope) then
    goto 666;
  if not ReturnResiduals (lnObservations,lnFactors-1, X, Y, Yresidual, lOutSlope) then
    goto 666;
  //permute here...
  result := PermutedZ (lnObservations,lnFactors,lFactorToTest,lnPermute, Xf, Yresidual,lPermOrder, lMaxZ,lMinZ);
  //PermutedP (lnObservations,lnFactors,lFactorToTest,lnPermute, Xf,Yresidual,l1Tailed,l2Tailed);

666:
  DelVector(Yresidual, lnObservations);
  DelMatrix(X, lnFactors-1,lnObservations);
  freemem(lOutSlope);
  freemem(lOutT);
end;

procedure TChatMemberThread.Execute;
var
  I: Integer;
begin
  for I := 0 to 20 do
      Sleep(Random(100));
end;

end.

