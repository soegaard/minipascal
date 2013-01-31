#lang minipascal
program WriteNatural;

function mod(x,y:integer):integer;
  begin
  mod:=x-(x div y)*y;
  end;

procedure WriteNatural(i:integer);
  begin
  if i<10 then
    write(chr(i+ord('0')))
  else
    begin
    WriteNatural(i div 10);
    write(chr(mod(i,10)+ord('0')));
    end
  end;
begin
  WriteNatural(12345);
end.

