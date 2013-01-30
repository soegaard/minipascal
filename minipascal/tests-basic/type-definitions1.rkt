#lang minipascal
{ Tests type definitions equivalent to base types.}
{ Expected result: a,1,true }
program types;
type
 ch=char;
 int=integer;
 bool=boolean;
var 
  c:ch;
  i:int;
  b:bool;
begin
  c:='a';
  i:=1;
  b:=true;
  writeln(c,i,b);
end.

