#lang minipascal
{Tests the array functions low and high}
program lowhigh;
type
  string = array [ 0 ..255] of char;
  counts = array ['a'..'z'] of integer;
var
  deadbeef: string;
  i,n:integer;
  c:char;
  count:counts;
begin
  deadbeef:='deadbeef';
  write('low:  ', low(deadbeef));
  writeln;
  write('high: ', high(deadbeef));
  writeln;
  { count number of occurences of each letter }
  // count:=makearray('a','z',0); // for simple
  for i:=1 to high(deadbeef) do
    begin
    c:=deadbeef[i];
    count[c]:=count[c]+1;
    end;    
  { print counts }
  for c:='a' to 'f' do
    begin
    write(c,': ',count[c]);
    writeln;
    end;    
end.
