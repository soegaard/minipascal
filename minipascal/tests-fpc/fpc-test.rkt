#lang minipascal simple (input "abcdef")
program fpctest;
var s:string;
begin
  writeln('Enter string:');
  readln(s);
  writeln('You wrote:');
  writeln(s);
end.
