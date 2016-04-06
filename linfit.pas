unit linfit;
{$IFDEF FPC} {$mode objfpc}{$H+}{$ENDIF}
//from Statistics Library  Earl F. Glynn, August 1997
interface

uses
  Classes, SysUtils; 
procedure LinearRegression(const x, y: array of Double; const nPoints: integer; var Slope,Intercept,R: double); overload;
procedure LinearRegression(const x, y: array of Double; const lB,uB: integer; var Slope,Intercept,R: double); overload;

implementation

procedure LinearRegression(const x, y: array of Double; const nPoints: integer; var Slope,Intercept,R: double);
begin
  LinearRegression(x, y, 0,nPoints-1,Slope,Intercept,R);

end;

procedure LinearRegression(const x, y: array of Double; const lB,uB: integer; var Slope,Intercept,R: double); overload;
//Input: arrays x,y each with nPoints observations
//   Will report linear regression y[i] := Slope*x[i]+Intercept
//   Will also return correlation coefficient R as a measure for goodness of fit
var
  FSumXSquared,FSumYSquared,FSumX,FSumY,FSumXY: double;
  i,n: integer;
begin
  //set default values if incase input is invalid
  Slope := 0;
  Intercept := 0;
  r := 0;
  n := uB-lB+1;
  if n = 1 then begin
    Intercept := y[lB];
    Slope := 0;
    R := 0;
    exit;  //special case - single observation

  end;
  if n < 2 then
      exit;
  //initialize
  FSumXSquared := 0;
  FSumYSquared := 0;
  FSumX := 0;
  FSumY := 0;
  FSumXY := 0;
  //accumulate
  for i := lB to (uB) do begin
         FSumX        := FSumX + x[i];
         FSumXSquared := FSumXSquared + x[i]*x[i];
         FSumY        := FSumY + y[i];
         FSumYSquared := FSumYSquared + y[i]*y[i];
         FSumXY       := FSumXY + x[i]*y[i];
  end;
  //calculate
  Slope   := FSumXY - FSumX*FSumY/n;                   // Sxy * (n - 1)
  Intercept := SQRT( (FSumXSquared - FSumX*FSumX/n)*     // Sx * Sy * (n - 1)
                       (FSumYSquared - FSumY*FSumY/n) );
  if Intercept <> 0 then
      r :=  Slope/Intercept;                          // r = Sxy / (Sx * Sy)
  Slope := (FSumXY - FSumX*FSumY/n) /
            (FSumXSquared - FSumX*FSumX/n);
  Intercept := (FSumY - Slope*FSumX)/n;
end;

end.

