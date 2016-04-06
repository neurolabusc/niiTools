unit gamma_powell;
interface
{$ifdef fpc}{$mode delphi}{$H+}{$endif}
uses  math,dialogsx,sysutils, linfit;

type
       Float = double;
       TGamma = record
         x0,xMax,yMax,Alpha, // <-final guess for four parameters describe gamma function
         ix0,ixMax,iyMax,iAlpha, // <-initial guess for four parameters describe gamma function
         RawxMax,RawyMax: Float; //<- raw peak height with no fitting
         //   x0=ArrivalTime, xMax=PeakTime, yMax=PeakHeight, Alpha=Kurtosis
         Beta,Mean,MTT,RSquare,RSquareRaw: Float;// <- these are descriptive that are calculated using four parameters
         //  Beta=Shape(GammaPDF), Mean='FirstMoment', MTT=MeanTransitTime(FM-x0)
         Lb,Ub: integer; //Lb/Ub are lower and upper bounds to be sampled. e.g. Ub=0,Lb=9 will only evaluate first 10 observations
         Error: boolean;
       end;
       function FitGamma (x,y: array of double; Lb,Ub: integer; var g: TGamma; SlowButPrecise: boolean): boolean;
       function ToGamma(X : Float; G:TGamma) : Float;
       procedure ComputeDescriptiveGamma (var g: TGamma); overload;

implementation

(*uses mainform;

function f2s(lR: double; lDec: integer): string;
begin
     result := FloatToStrF(lR, ffFixed,7,lDec);
end;  *)

procedure AllPositive (var y: array of double; n: integer);
//make negative values = zero
var
  i: integer;
begin
     for i := 0 to (n-1) do
         if y[i] < 0 then
            y[i] := 0;
end;

procedure ComputeDescriptiveGamma (var g: TGamma); overload;
//input: four parameter Gamma variate based on Madsen's parameterization
//output:
//For details see http://en.wikipedia.org/wiki/Gamma_distribution
begin
     if g.Alpha = 0 then
        g.Beta := 0
     else
         g.Beta := (g.xMax-g.x0)/g.Alpha;
     g.MTT := (g.Alpha+1) * g.Beta;
     g.Mean := g.MTT + g.x0;
     g.Rsquare := 0;
     g.RSquareRaw := 0;
end;

function SumSqrGamma(X, Y: Array of Double; lB, Ub: integer; var g : TGamma) : Float;
//SumSquare for model B
// Our fitting function will attempt to minimize this value ("Least Squares" cost function)
var
  i: integer;
begin
  result := 0;
  for i := Lb to Ub do
    result := result + sqr(Y[i]-ToGamma(X[i],g) );
end;

function RSqr (X, Y: Array of Double; lB, Ub: integer; var g: TGamma): Double;
var
   i: integer;
   mean, SStot: double;
begin
     result := 0;
	 //compute mean
	 mean := 0;
	 for i := Lb to Ub do
  	 	 mean := mean + Y[i];
	 mean := mean/(Ub-Lb+1);
	 //compute SumSquare Total...
	 SStot := 0;
	 for i := Lb to Ub do
  	 	 SStot := SStot + sqr(Y[i]- mean) ;
	 if SStot = 0 then
  	 	exit;
	 result := 1-( SumSqrGamma(X, Y,lB,Ub,g)/ SStot);
end;
procedure ComputeDescriptiveGamma (X, Ypos: Array of Double; lB, Ub: integer; var g: TGamma); overload;

begin
      ComputeDescriptiveGamma (g);
      g.Rsquare := RSqr(X,YPos,lB,Ub,g)
end;


function LinFitSS(x,y: array of double; iT0,iTMax: integer): double;
//used for initial estimate of gamma
var
    i: integer;
    Slope,Intercept,R: double;
begin
  result := maxint;
  if iT0 >= iTMax then exit;
  LinearRegression(x, y, iT0,iTMax, Slope,Intercept,R);
     result :=0;
     for i := 0 to iT0 do
          result := result+ sqr(y[i]);
     for i := (iT0+1) to iTMax do
          result := result + sqr(y[i]- (x[i]*Slope+Intercept) );
end;

function Madsen (t,ymax,tmax,alpha,t0: double): double;
var
   tprime: double;
begin
  result := 0;
  if t <= t0 then
     exit;
  tprime := ((t-t0) /(tmax-t0));
  result := ymax*power(tprime,alpha)*exp(alpha*(1-tprime));
end;

procedure MadsenFit (x,y: double; var xln,yln : double; xMax,x0: double);
//warning: ln(0)=  -inf : this is a problem if observed values = 0!
var
   tprime: double;
begin
  if (xMax=x0)  then begin
     xln := 0;
     yln := 0;
     exit;
  end;
  tprime := (x-x0) /(xMax-x0);
  yln:=ln(y);
  xln := (1+ln(tprime)-tprime);
end;

function PrepGamma (Lb,Ub: integer): TGamma;
begin
    result.Lb := Lb;
    result.Ub := Ub;
end;

procedure CropTail(y: array of double; var g: TGamma);
//noisy tails can lead to a poor fit -
// inspect samples after peak and stop when values are <20% peak height or when the signal rises for two samples...
var
   i,iXMax:integer;
   YThresh: double;
begin
     iXMax := 0;
     for i := g.Lb to g.Ub do
         if (y[i] >y[iXMax]) then
            iXMax := i;
     if (iXMax+3) >= g.Ub then
        exit;
     YThresh := y[iXMax] * 0.2; //ignore tail
     for i := (iXMax+1) to (g.Ub) do begin
         if (y[i]< YThresh) then begin
            g.Ub := i-1;
            exit;
         end;
         if ((i+2) <= g.Ub) and (y[i]<y[i+1]) and (y[i+1]<y[i+2]) then begin //consistent signal rise after peak - recirculation?
            g.Ub := i-1;
            exit;
         end;
     end;
end;

procedure CropHead(x: array of double; var g: TGamma);
//noisy initial samples can lead to poor convergence and slow Powell function...
var
   i: integer;
begin
     if (g.Ub-g.Lb) < 3 then
        exit;
     for i := g.Lb to g.Ub do
         if (x[i] >= g.x0) then begin
            if (i > 0) and (i > g.Lb) then
               g.Lb := i - 1;
            exit;
         end;
end;

(*

procedure OutTab (A,B,C,D,E,F: float);
const
   kTab = chr(9);
var
  i: integer;
  myFile : TextFile;
begin
  AssignFile(myFile, 'f:\perf\vat.tab');
  if fileexists('f:\perf\vat.tab') then
  	 Append(myFile)
  else
  	  ReWrite(myFile);
  Write(myFile,f2s(A,8),kTab);
  Write(myFile,f2s(B,8),kTab);
  Write(myFile,f2s(C,8),kTab);
  Write(myFile,f2s(D,8),kTab);
  Write(myFile,f2s(E,8),kTab);
  Write(myFile,f2s(F,8),kTab);
  Writeln(myFile);
  Flush(myFile);
  closefile(myFile);
end;  *)
function Centroid(PrevHt,CenterHt,NextHt: double): double;
//find centroid for three bars  http://en.wikipedia.org/wiki/Centroid
var
  Min,PrevTriMass,PrevSqrMass,NextTriMass,NextSqrMass,PrevTriCentroid,NextTricentroid: double;
begin
  result := 0;
  if PrevHt = NextHt then
    exit;
  if PrevHt < NextHt then
    Min := PrevHt
  else
    Min := NextHt;
  if PrevHt > CenterHt then
    PrevTriCentroid := -2/3
  else
    PrevTriCentroid := -1/3;
  if NextHt > CenterHt then
    NextTriCentroid := 2/3
  else
    NextTriCentroid := 1/3;
  PrevTriMass := (0.5*abs(PrevHt-CenterHt));
  NextTriMass := (0.5*abs(NextHt-CenterHt));
  PrevSqrMass := abs(PrevHt-Min);
  NextSqrMass := abs(NextHt-Min);
  result := (-0.5*PrevSqrMass+PrevTriCentroid*PrevTriMass+0.5*NextSqrMass+NextTriCentroid*NextTriMass)/(PrevSqrMass+PrevTriMass+NextSqrMass+NextTriMass);
end;

function LERP (frac,v0,v1: double): double;
//linear interpolate: function returns v0 if frac is 0, and v1 if frac is 1...
begin
  result := v0;
  if v0 <> v1 then
     result := result + frac*(v1-v0);
end;

procedure InitialEstimateGamma (x,y: array of double; var g: TGamma);
//approximate fit for x0, xMax, yMax, alpha
var
  xln,yln: array of double;
  r,SS,SSmin,Ctr: double;
  nx,nOK,i, iX0,iXMax: integer;
begin
     g.Error := true;
     nx := (g.Ub-g.Lb)+1;
    if nx < 3 then exit;
    //find time of peak (TMax) as index number
    iXMax := 0;
    for i := g.Lb to g.Ub do
        if (y[i] >y[iXMax]) then
           iXMax := i;
    g.xMax := x[iXMax]; //peak time - in seconds
    g.RawxMax := x[iXMax];
    g.RawyMax := y[ixMax];
    //  we need samples before and after peak to estimate x0 and alpha respectively...
    if (iXMax <= 0) or (iXMax <= g.Lb) or (iXMax >= g.Ub) then
       exit;
    // Madsen suggests using centroid to estimate xMax with sub-sample accuracy...
    //OutTab(y[ixMax-1],y[ixMax],y[ixMax+1],x[ixMax-1],x[ixMax],x[ixMax+1]);
    Ctr := Centroid(y[ixMax-1],y[ixMax],y[ixMax+1]);
    if Ctr < 0 then
       g.xMax := Lerp(1+Ctr,x[ixMax-1],x[ixMax])
    else if Ctr > 0 then
    	 g.xMax := Lerp(Ctr,x[ixMax],x[ixMax+1]);
    //find  arrival time (x0) as index number
    iX0 := 0;
    SSmin := LinFitSS(x,y,iX0,iXMax);
    for i := 0 to (iXmax-1) do begin
        SS := LinFitSS(x,y,i,iXMax);
        if SS < SSMin then begin
           SSMin := SS;
           iX0 := i
        end;
    end;
    g.x0 := x[iX0];//arrival time, in seconds
    //estimate alpha and ymax using linear regression, see Madsen
    setlength (xln,nx);
    setlength(yln,nx);
    nOK := 0;
    for i := (iX0+1) to (g.Ub) do begin
      if y[i] > 0 then begin //ln 0 = inf!
         //MadsenFit (x[i],y[i],xln[nOK],yln[nOK],iXMax,iX0);
         MadsenFit (x[i],y[i],xln[nOK],yln[nOK],g.xMax,g.x0);
         inc(nOK);
      end; //if y[i] > 0
    end; //for each sample
    if nOK < 2 then
       exit;
    LinearRegression(xln,yln,0,nOK-1,g.alpha,g.ymax,R);
    //fx(result.alpha,result.yMax);
    xln := nil;
    yln := nil;
    if g.alpha < 0.15 then
       g.alpha := 0.15;
    g.yMax:= exp(g.yMax);
    //we have now completed our initial fit... at this stage the initial fit is our best guess of the final fit
    g.ix0 := g.X0;
    g.ixMax := g.xMax;
    g.iyMax := g.yMax;
    g.iAlpha := g.Alpha;
    g.Error := false;
end;
(*gives same answer, but is ~15% slower
function ToGamma (x: double; g: TGamma): double;
var
   t: double;
begin
     result := 0;
    if (x <= g.x0) or (g.xMax <= g.x0) then
       exit;
    t := (x-g.x0)/(g.xMax-g.x0);
    result := g.ymax * power(t,g.alpha)*exp(g.alpha*(1-t));
end;
*)
function ToGamma(X : Float; G:TGamma) : Float;
{ ------------------------------------------------------------------
  Computes the regression function at point X
  P is the vector of parameters, such that :
  P[1] = x0     P[2] = xmax     P[3] = ymax     P[4] = alpha
  ------------------------------------------------------------------ }
const
  MinLog = -87.33655;
var
  W, Z : Float;
begin
     result := 0.0;
  if (X <= G.x0) or ( (G.Xmax - G.x0) <= 0) then
      Exit;
  W := (X - G.X0) / (G.xMax - G.X0);  { (x - x0) / (xmax - x0) }
  Z := G.alpha* (1.0 + Ln(W) - W);    { alpha * (1 + ln(w) - w }
  if Z > MinLog then
    result := G.Ymax * Exp(Z);  { ymax * w^alpha * exp[alpha * (1 - w)] }
end; //GammaFit_Func

procedure GammaPowell(X, Y    : Array of Double;Lb, Ub  : Integer;MaxIter : Integer;Tol     : Float; var g: TGamma);
Const NP=4;//number of parameters
label 666; //fatal error
Type
      pMAT = ^MAT;
       MAT = Array[1..NP,1..NP] of Double;
      pVEC = ^VEC;
       VEC = Array[1..NP] of Double;
Var
      XI: pMAT;
      XT,P,PCOM, XICOM: pVEC;    {PCOM,XICOM,NCOM are common variables}
      ///N,
        NCOM:Integer;  {for LINMIN and F1DIM only}
      MinX,MaxX,FRET,FTOL: Double;

function GammaFit_Func(X : Float; P:pVEC) : Float;
{ ------------------------------------------------------------------
  Computes the regression function at point X
  P is the vector of parameters, such that :
  P[1] = x0     P[2] = xmax     P[3] = ymax     P[4] = alpha
  ------------------------------------------------------------------ }
const
  MinLog = -87.33655;
var
  W, Z : Float;
begin
  result := 0;
  if (X <= P[1]) then // or (P[1]<MinX) or (P[1]>P[2])or ( (P[2] - P[1]) <= 0) or (P[2]>MaxX) or (P[4]<=0) then
    Exit;
  W := (X - P[1]) / (P[2] - P[1]);  { (x - x0) / (xmax - x0) }
  Z := P[4] * (1.0 + Ln(W) - W);    { alpha * (1 + ln(w) - w }
  if Z > MinLog then
    result := P[3] * Exp(Z);  { ymax * w^alpha * exp[alpha * (1 - w)] }
end; //GammaFit_Func

(*
procedure OutVec (P: pVec);
const
   kTab = chr(9);
var
  i: integer;
  myFile : TextFile;
begin
  AssignFile(myFile, 'f:\perf\vax.tab');
  if fileexists('f:\perf\vax.tab') then
  	 Append(myFile)
  else
  	  ReWrite(myFile);
  for i := 1 to 4 do
  	  Write(myFile,f2s(P[i],8),kTab);
  Writeln(myFile);
  Flush(myFile);
  closefile(myFile);
end;

procedure OutF (P: Float);
const
   kTab = chr(9);
var
  i: integer;
  myFile : TextFile;
begin
  AssignFile(myFile, 'f:\perf\vax.tab');
  if fileexists('f:\perf\vax.tab') then
  	 Append(myFile)
  else
  	  ReWrite(myFile);

  	  Write(myFile,f2s(P,8),kTab);
  Writeln(myFile);
  Flush(myFile);
  closefile(myFile);
end;*)

{user defined function to minimize}
function Func(P:pVEC) : Float;
//SumSquare for model B
// Our fitting function will attempt to minimize this value ("Least Squares" cost function)
//P[1] = x0     P[2] = xmax     P[3] = ymax     P[4] = alpha
var
  i: integer;
begin
  result := maxint;
  //OutVec(P);
  if  (P[1]< MinX) or (P[1]>P[2])or ( (P[2] - P[1]) <= 0) or (P[2]>=MaxX) or (P[4]>MaxInt)or (P[4]<=0.000001) or (P[3]=0) then
    exit;
  result := 0;
  for i := Lb to Ub do
    result := result + sqr(Y[i]-GammaFit_Func(X[i],P) );
  //OutF(result);
end;

FUNCTION MAX(a,b:DOUBLE):DOUBLE;
Begin
  if a>=b then MAX:=a else MAX:=b
End;

FUNCTION MIN(a,b:DOUBLE):DOUBLE;
Begin
  if a<=b then MIN:=a else MIN:=b
End;

FUNCTION Sign(a,b:DOUBLE):DOUBLE;
Begin
  if b>=0 then Sign:=ABS(a)
          else Sign:=-ABS(a)
End;

FUNCTION F1DIM(X:Double):Double;
Var
  //XT:pVEC;
  J:Integer;
Begin
  //New(XT);
  For J:=1 to NCOM do
    XT^[J]:=PCOM^[J] + X*XICOM^[J];
  F1DIM := FUNC(XT);
  //Dispose(XT);
End;

Procedure MNBRAK(Var AX,BX,CX,FA,FB,FC:Double);
{----------------------------------------------------------------------
 Given a Function F1DIM(X), and given distinct initial points AX and
 BX, this routine searches in the downhill direction (defined by the
 Function as evaluated at the initial points) and returns new points
 AX, BX, CX which bracket a minimum of the Function. Also returned
 are the Function values at the three points, FA, FB and FC.
----------------------------------------------------------------------}
Label 1;
Const GOLD=1.618034; GLIMIT=100.0; TINY=1e-20; HUGE=1e16;
{The first parameter is the default ratio by which successive intervals
{are magnified; the second is the maximum magnification allowed for
{a parabolic-fit step.}
Var
  DUM,FU,Q,R,U,ULIM {,QX}: Double;
Begin
  FA:=F1DIM(AX);
  FB:=F1DIM(BX);
  IF FB > FA THEN
  begin
    DUM:=AX;
    AX:=BX;
    BX:=DUM;
    DUM:=FB;
    FB:=FA;
    FA:=DUM
  end;
  CX:=BX+GOLD*(BX-AX);
  FC:=F1DIM(CX);
1:IF FB >= FC THEN
  begin
    R:=(BX-AX)*(FB-FC);
    Q:=(BX-CX)*(FB-FA);
    U:=BX-((BX-CX)*Q-(BX-AX)*R)/(2.*Sign(MAX(ABS(Q-R),TINY),Q-R));
    try
      ULIM:=BX+GLIMIT*(CX-BX);
    except
        on E : Exception do  ULIM:=1;
    end;
    IF (BX-U)*(U-CX) > 0.0 THEN begin
      FU:=F1DIM(U);
      IF FU < FC THEN
      begin
        AX:=BX;
        FA:=FB;
        BX:=U;
        FB:=FU;
        GOTO 1
      end
      ELSE IF FU > FB THEN
      begin
        CX:=U;
        FC:=FU;
        GOTO 1
      end;
      U:=CX+GOLD*(CX-BX);
      FU:=F1DIM(U)
    end
    ELSE IF (CX-U)*(U-ULIM) > 0.0 THEN
    begin
      FU:=F1DIM(U);
      IF FU < FC THEN
      begin
        BX:=CX;
        CX:=U;
        U:=CX+GOLD*(CX-BX);
        FB:=FC;
        FC:=FU;
        FU:=F1DIM(U)
      end
    end
    ELSE IF (U-ULIM)*(ULIM-CX) >= 0.0 THEN
    begin
      U:=ULIM;
      FU:=F1DIM(U)
    end
    ELSE
    begin
      U:=CX+GOLD*(CX-BX);
      FU:=F1DIM(U)
    end;
    AX:=BX;
    BX:=CX;
    CX:=U;
    FA:=FB;
    FB:=FC;
    FC:=FU;
    GOTO 1
  end
End;

Function  BRENT(Var AX,BX,CX,TOL,XMIN:Double): Double;
{-------------------------------------------------------------------
 Given a function F1DIM, and a bracketing triplet of abscissas
 AX,BX,CX (such that BX is between AX and CX, and F(BX) is less
 than both F(AX) and F(CX)), this routine isolates the minimum
 to a fractional precision of about TOL using Brent's method.
 The abscissa of the minimum is returned in XMIN, and the minimum
 function value is returned as BRENT, the returned function value.
-------------------------------------------------------------------}
Label 1,2,3;
Const ITMAX=100; CGOLD=0.3819660; ZEPS=1e-10;
{Maximum allowed number of iterations; golden ratio; and a small
 number which protects against trying to achieve fractional accuracy
 for a minimum that happens to be exactly zero}
Var
  A,B,D,E,ETEMP,FX,FU,FV,FW,P,Q,R,TOL1,TOL2,U,V,W,X,XM: Double;
  ITER: Integer;
Begin
  A:=MIN(AX,CX);
  B:=MAX(AX,CX);
  V:=BX;
  W:=V;
  X:=V;
  D := 0;
  E:=0.0;
  FX:=F1DIM(X);
  FV:=FX;
  FW:=FX;
  For ITER:=1 to ITMAX do                              {main loop}
  begin
    XM:=0.5*(A+B);
    TOL1:=TOL*ABS(X)+ZEPS;
    TOL2:=2.0*TOL1;
    IF ABS(X-XM) <= (TOL2-0.5*(B-A)) Then GOTO 3;  {Test for done here}
    IF ABS(E) > TOL1 THEN             {Construct a trial parabolic fit}
    begin
      R:=(X-W)*(FX-FV);
      Q:=(X-V)*(FX-FW);
      P:=(X-V)*Q-(X-W)*R;
      Q:=0.2*(Q-R);
      IF Q > 0.0 Then  P:=-P;
      Q:=ABS(Q);
      ETEMP:=E;
      E:=D;
      IF (ABS(P) >= ABS(0.5*Q*ETEMP)) OR (P <= Q*(A-X)) OR (P >= Q*(B-X)) Then GOTO 1;
{   The above conditions determine the acceptability of the
    parabolic fit. Here it is o.k.:}
      D:=P/Q;
      U:=X+D;
      IF (U-A < TOL2) OR (B-U < TOL2) Then D:=Sign(TOL1,XM-X);
      GOTO 2
    end;
1:  IF X >= XM THEN
      E:=A-X
    ELSE
      E:=B-X;
    D:=CGOLD*E;
2:  IF ABS(D) >= TOL1 THEN
      U:=X+D
    ELSE
      U:=X+Sign(TOL1,D);
    FU:=F1DIM(U);  {This the one function evaluation per iteration}
    IF FU <= FX THEN
    begin
      IF U >= X THEN
        A:=X
      ELSE
        B:=X;
      V:=W;
      FV:=FW;
      W:=X;
      FW:=FX;
      X:=U;
      FX:=FU
    end
    ELSE
    begin
      IF U < X THEN
        A:=U
      ELSE
        B:=U;
      IF (FU <= FW) OR (W =X) THEN
      begin
        V:=W;
        FV:=FW;
        W:=U;
        FW:=FU
      end
      ELSE IF (FU <= FV) OR (V = X) OR (V = W) THEN
      begin
        V:=U;
        FV:=FU
      end
    end
  end;
  showmsg(' Brent exceed maximum iterations.');
3:XMIN:=X;   {exit section}
  BRENT:=FX
End;

Procedure LINMIN(Var P, XI:pVEC; N:Integer; Var FRET:Double);
{----------------------------------------------------------
  Given an N dimensional point P and a N dimensional direc-
  tion XI, moves and resets P to where the function FUNC(P)
  takes on a minimum along the direction XI from P, and
  replaces XI by the actual vector displacement that P was
  moved. Also returns as FRET the value of FUNC at the
  returned location P. This is actually all accomplished by
  calling the routines MNBRAK and BRENT.
----------------------------------------------------------}
Var
  AX,BX,FA,FB,FX,TOL,XMIN,XX: Double;
  J: Integer;
Begin
  TOL:=1e-4;
  NCOM:=N;
  For J:=1 to N do
  begin
    PCOM^[J]:=P^[J];
    XICOM^[J]:=XI^[J]
  end;
  AX:=0.0;
  XX:=1.0;
  BX:=2.0;
  MNBRAK(AX,XX,BX,FA,FX,FB);
  FRET:=BRENT(AX,XX,BX,TOL,XMIN);
  For J:=1 to N do
  begin
    XI^[J]:=XMIN*XI^[J];
    P^[J]:=P^[J] + XI^[J]
  end
End;

Procedure POWELL(Var P:pVEC; XI:pMAT; {N,}NP:Integer; FTOL:Double;
                  Var FRET:Double);
{-----------------------------------------------------------
  Minimization of a function  FUNC of N variables  (FUNC is
  not an argument, it is a fixed function name). Input con-
  sists of an initial starting point P  that is a vector of
  length N; an initial matrix XI  whose  logical dimensions
  are N by N, physical dimensions NP by NP, and whose columns
  contain the initial set of directions (usually the N unit
  vectors); and FTOL, the fractional tolerance in the func-
  tion value such that failure to decrease by more than this
  amount on one iteration signals doneness. On output, P is
  set to the best point found, XI is the then-current direc-
  tion set,  FRET is the returned function value at P,  and
  ITER is the number of iterations taken. The routine LINMIN
  is used.
-----------------------------------------------------------}
Label 1, 10;
Const
  ITMAX=300;
Var
  PT, PTT, XIT:pVEC;
  DEL,FP,FPTT,T: Double;
  ITER,I,IBIG,J:Integer;
Begin
  New(PT); New(PTT); New(XIT);
  FRET:=FUNC(P);
  For J:=1 to NP do
    PT^[J]:=P^[J];    {Save initial point}
  ITER:=0;
1:ITER:=ITER+1;
  FP:=FRET;
  IBIG:=0;
  DEL:=0.0;           {Will be the biggest function decrease.}
  For I:=1 to NP do    {In each iteration, loop over all directions in the set.}
  begin               {Copy the direction.}
    For J:=1 to NP do
      XIT^[J]:=XI^[J,I];
    FPTT:=FRET;
    LINMIN(P,XIT,NP,FRET);  {Minimize along it}
    IF ABS(FPTT-FRET) > DEL THEN
    begin
      DEL:=ABS(FPTT-FRET);
      IBIG:=I
    end
  end;
  IF 2.0*ABS(FP-FRET) <= FTOL*(ABS(FP)+ABS(FRET)) then goto 10; {Termination criterion}
  IF ITER = ITMAX Then
  begin
    showmsg(' Powell exceeding maximum iterations: ');
    goto 10
  end;
  For J:=1 to NP do
  begin
    PTT^[J]:=2.0*P^[J]-PT^[J]; {Construct the extrapolated point and the average}
    XIT^[J]:=P^[J]-PT^[J];     {direction moved. Save the old starting point}
    PT^[J]:=P^[J]
  end;
  FPTT:=FUNC(PTT);             {Function value at extrapolated point.}
  IF  FPTT >= FP Then GOTO 1;  {One reason not to use new direction.}
  T:=2.0*(FP-2.0*FRET+FPTT)*Sqr(FP-FRET-DEL)-DEL*Sqr(FP-FPTT);
  IF T >= 0.0 Then GOTO 1;     {Other reason not to use new direction.}
  LINMIN(P,XIT,NP,FRET);        {Move to the minimum of the new direction.}
  For J:=1 to NP do             {and save the new direction.}
    XI^[J,IBIG]:=XIT^[J];
  GOTO 1;
10:Dispose(PT); Dispose(PTT); Dispose(XIT)
End;

//(X, Y    : PVector; Lb, Ub  : Integer; MaxIter : Integer; Tol     : Float)
{main function: doPowell}
BEGIN
  New(P); New(XI); New(PCOM); New(XICOM);New(XT);
  MinX := X[Lb];
  MaxX := X[Ub];
  //N:=4;
  // P[1] = x0     P[2] = xmax     P[3] = ymax     P[4] = alpha
  P^[1]:=g.x0; P^[2]:=g.xMax; P^[3]:=g.Ymax; P^[4]:=g.alpha;
  XI^[1,1]:=0.5; XI^[1,2]:=0; XI^[1,3]:=0;XI^[1,4]:=0;
  XI^[2,1]:=0; XI^[2,2]:=0.5; XI^[2,3]:=0;XI^[2,4]:=0;
  XI^[3,1]:=0; XI^[3,2]:=0; XI^[3,3]:=0.5;XI^[3,4]:=0;
  XI^[4,1]:=0; XI^[4,2]:=0; XI^[4,3]:=0;XI^[4,4]:=0.5;
  FTOL:=Tol;//1e-8;
  POWELL(P,XI,{N,}NP,FTOL,FRET);
   g.x0:=P^[1];
   g.xMax:=P^[2];
   g.yMax:=P^[3];
   g.alpha:=P^[4];
666:
  Dispose(P); Dispose(XI); Dispose(PCOM); Dispose(XICOM); Dispose(XT);
END;

function Valid(var g: TGamma): boolean;
begin
     result := true;
     if ((g.Ub-g.Lb) < 4) then begin
     	g.Error := true;
        result := false;
     end;
end;

function FitGamma (x,y: array of double; Lb,Ub: integer; var g: TGamma; SlowButPrecise: boolean): boolean;
label
  666;
var
   Ypos    : Array of Double;
   i,n: integer;
begin
     g.Error := true;
     result := false;
     if (Ub < 3) or ((Ub-Lb)<3) then
        exit;
     setlength(Ypos,Ub+1);
     for i := 0 to Ub do
         Ypos[i] := Y[i];
     AllPositive(yPos,Ub);
     g:= PrepGamma (Lb,Ub);
     CropTail(yPos,g);
     InitialEstimateGamma(x,ypos,g); //note: fit cropped range
     if g.Error then goto 666;

     CropHead(x, g);
     if not Valid(g) then goto 666;
     if SlowButPrecise then
     	GammaPowell(x,yPos,g.Lb,g.Ub,1000,1e-3,g); //fit cropped range g.Lb..g.Ub
     ComputeDescriptiveGamma (x,ypos,g.lB,g.Ub,g); //fit cropped range g.Lb..g.Ub
     g.RSquareRaw :=  RSqr (x,y, lB, Ub, g);  //note: use original data [not yPos], use full range, not cropped g.Lb...g.Ub
     if (g.MTT > x[g.Ub]) then
        g.Error := true;
     //ComputeDescriptiveGamma (x,ypos,lB,Ub,g); //note: use full range, not cropped g.Lb...g.Ub
     if g.Error then goto 666;
     result := true;
 666:
 	 YPos := nil;
end;

end.
