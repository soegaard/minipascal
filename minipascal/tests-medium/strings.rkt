#lang minipascal
program strings;
type
  string = array [0..255] of char;
var
  s,t,u,v:string;
begin
  s:='Hello world!';
  t:='Hello world!';
  u:='Hello worle!';
  v:='Hello worlc!';
  write('length of s: ');
  writeln(ord(s[0]));
  
  writeln('These comparisons are all true:');
  writeln(s=t);
  writeln(s<u);
  writeln(s>v);
  writeln(s<=t);
  writeln(s<=u);
  writeln(s>=v);
  writeln(s<>u);
end.
