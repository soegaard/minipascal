#lang minipascal
program strings;
type
  string = array [0..255] of char;
var
  s:string;
  t:string;
begin
  s:='Hello world!';
  t:='Hello world!';
  writeln(s);
  writeln(ord(s[0]));
  writeln(s=t);
end.
