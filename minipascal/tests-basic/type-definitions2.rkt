#lang minipascal
{ Tests type definitions equivalent to array of base type.}
{ Expected result: 42 }
program types;
type
 int=integer;
 arr = array[1..10] of int;
 ints = arr;
var 
  a:ints;
begin
  a[3]:=42;
  writeln(a[3]);
end.

