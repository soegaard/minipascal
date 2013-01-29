#lang minipascal
{This test demonstrates nested functions.
 The expected result is 7.
 Original examples from Wikipedia.}
program nested;
type  
  int=integer;  
var   
  n:integer;
function E(x: int): int;
  function F(y: int): int;
    begin
    F := x + y  
    end;
  begin
  E := F(3) + F(2)
  end;
begin
  writeln(E(1));
end.
