#lang minipascal simple
{http://stackoverflow.com/questions/14673601/
 how-to-write-numbers-separated-with-commas-in-a-loop-on-one-line }
program prime;
var
  P:integer;
  I:integer;
  J:integer;
  A:integer;
  
function mod(x,y:integer):integer;
begin
  mod:= x- (x div y)*y;
end;

begin
  writeln('Prime number program');
  writeln;
  writeln('Insert number');
  read(P);

  for I:=2 to P-1 do
  begin
      J:=Mod(P, I);
      if (J=0) then
      begin
        writeln(P,' divides with ',I);
        a:=a+1
      end;
  end;

  if a=0 then
  begin
  writeln(P,' is prime number');
  end;
end.