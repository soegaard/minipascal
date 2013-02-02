#lang minipascal 
{ Tests negative constants }
program negativeconstants;
const
  c=42;
begin
  writeln(-12,-c);
end.
