#lang minipascal
program hcf;
{ Highest common factor.
  Expeced result: 4 }
var
  r:integer;
  
function mod(x,y:integer):integer;
  begin
    mod := x-(x div y)*y;
  end;

function hcf(p,q:integer):integer;
  begin
    r:=mod(p,q);
    while r<>0 do
      begin
      p:=q;
      q:=r;
      r:=mod(p,q);
      end;
    hcf:=q;
  end;
begin
  hcf(2*2*3,2*2*5);
end.
