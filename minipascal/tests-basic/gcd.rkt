#lang minipascal
{ Tests recursive function calls.
  Expected result: 2*3*5 = 30 }
program gcd;
function rem(x,y:integer):integer;
  begin
    rem:= x-((x div y)*y);
  end;
function gcd(x,y:integer):integer;
  begin
   { write('x',' ',x,' ',' ','y',' ',y); writeln; }
    if x>y then
      gcd:=gcd(y,x)
    else
      if x=0 then
        gcd:=y
      else
        gcd:=gcd(rem(y,x),x)
 end;
begin
  writeln(gcd(2*2*3*3*5*7,2*3*5*11));
end.
