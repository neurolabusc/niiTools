unit arrayu;

{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils;

procedure sort32(lower, upper : integer; var Data: array of single);
procedure Quartiles32(var Data: array of single; var Q1, Q2, Q3: single);


implementation

procedure Quartiles32(var Data: array of single; var Q1, Q2, Q3: single);
//25th, 50th (median) and 75th percentile
//not changes order of array
var
   f: single;
   t,n: integer;
begin
     n := length(Data);
     if n < 4 then exit;
     sort32(0,n-1,Data);
     f := (n-2)/4;
     t := trunc(f);
     f := f-t;
     Q1 := ((1-f)*Data[t]) + (f * Data[t+1]);


     f := (n-1)/2;
     t := trunc(f);
     f := f-t;
     Q2 := ((1-f)*Data[t]) + (f * Data[t+1]);

     f := (n-1)- ((n-2)/4);
     t := trunc(f);
     f := f-t;
     Q3 := ((1-f)*Data[t]) + (f * Data[t+1]);

end;

procedure sort32(lower, upper : integer; var Data: array of single);
//40ms - fast but very recursive...
var
       left, right : integer;
       pivot,lswap: single;
begin
     pivot:=Data[(lower+upper) div 2];
     left:=lower;
     right:=upper;
     while left<=right do begin
             while Data[left]  < pivot do left:=left+1;  { Parting for left }
             while Data[right] > pivot do right:=right-1;{ Parting for right}
             if left<=right then begin   { Validate the change }
                 lswap := Data[left];
                 Data[left] := Data[right];
                 Data[right] := lswap;
                 left:=left+1;
                 right:=right-1;
             end; //validate
     end;//while left <=right
     if right>lower then sort32(lower,right,Data); { Sort the LEFT  part }
     if upper>left  then sort32(left ,upper,data); { Sort the RIGHT part }
end;


end.

