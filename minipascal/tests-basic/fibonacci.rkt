#lang minipascal
program fib;
type 
 int=integer;
function fib(n:int):int;
  begin
    if n=0 then
      fib:=1
    else
      fib:=n*fib(n-1)
  end;
begin
  writeln(fib(10));
end.

