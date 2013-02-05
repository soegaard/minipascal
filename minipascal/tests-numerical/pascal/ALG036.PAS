program ALG036;
{  BEZIER CURVE ALGORITHM 3.6

   To construct the cubic Bezier curves C0, ..., Cn-1 in
   parameter form, where Ci is represented by

   (xi(t),yi(t)) = ( a0(i) + a1(i)*t + a2(i)*t^2 + a3(i)*t^3,
                     b0(i) + b1(i)*t + b2(i)*t^2 + b3(i)*t^3)

   for 0 <= t <= 1 as determined by the left endpoint (x(i),y(i)),
   left guidepoint (x+(i),y+(i)), right endpoint (x(i+1),y(i+1)) and
   right guidepoint (x-(i+1),y-(i+1)) for each i = 0, 1, ... , n-1;

   INPUT  n, ( (x(i),y(i)), i = 0,...,n ),
             ( (x+(i),y+(i)), i = 0,...,n-1 ),
             ( (x-(i),y-(i)), i = 1,...,n ).

   OUTPUT coefficients ( a0(i), a1(i), a2(i), a3(i),
                         b0(i), b1(i), b2(i), b3(i), i = 0, ... , n-1 ).
                                                                       }
   var
      A0, A1, A2, A3, B0, B1, B2, B3 : array[0..24] of real;
      X, Y : array[0..25] of real;
      XPL, YPL : array[0..24] of real;
      XMI, YMI : array[1..25] of real;
      I, N, FLAG : integer;
      OK : boolean;
      A : char;
      INP, OUP : text;
      NAME : string [ 30 ];
procedure INPUT;
   begin
      writeln('This is the Bezier Curve Algorithm.');
      OK := false;
      while ( not OK ) do
         begin
            writeln ('Choice of input method: ');
            writeln ('1. Input entry by entry from keyboard ');
            writeln ('2. Input data from a text file ');
            writeln ('Choose 1 or 2 please ');
            readln ( FLAG );
            if ( FLAG = 1 ) or ( FLAG = 2 ) then OK := true
         end;
      case FLAG of
         1 : begin
                OK := false;
                while ( not OK ) do
                   begin
                      writeln ('Input n ');
                      readln ( N );
                      if ( N > 0 ) then
                         begin
                            OK := true;
                            writeln('Input X[0],Y[0],X+[0],Y+[0]');
                            writeln('separated by a space');
                            readln(X[0],Y[0],XPL[0],YPL[0]);
                            for I := 1 to N-1 do
                               begin
                                  writeln('Input X(',I,'),Y(',I,') ');
                                  writeln ('separated by space ');
                                  readln ( X[I], Y[I] );
                                  writeln('Input X-(',I,'),Y-(',I,') ');
                                  writeln ('separated by space ');
                                  readln ( XMI[I], YMI[I] );
                                  writeln('Input X+(',I,'),Y+(',I,') ');
                                  writeln ('separated by space ');
                                  readln ( XPL[I], YPL[I] );
                               end;
                            writeln('Input X[n],Y[n],X-[n],Y-[n]');
                            writeln('separated by a space');
                            readln(X[N],Y[N],XMI[N],YMI[N])
                        end
                     else writeln ('Number must be a positive integer ')
                  end
             end;
         2 : begin
                write ('Has a text file been created with the data as ');
                writeln ('follows ? ');
                writeln;
                writeln('X[0]    Y[0]    X+[0]    Y+[0]');
                writeln('X[1]    Y[1]    X-[1]    Y-[1]    X+[1]    Y+[1]');
                writeln('...');
                writeln('X[n-1]  Y[n-1]  X-[n-1]  Y-[n-1]  X+[n-1]  Y+[n-1]');
                writeln('X[n]    Y[n]    X-[n]    Y-[n]');
                writeln;
                writeln ('Enter Y or N ');
                readln ( A );
                if ( A = 'Y' ) or ( A = 'y' ) then
                   begin
                      write ('Input the file name in the form - ');
                      writeln ('drive:name.ext ');
                      writeln ('for example:   A:DATA.DTA ');
                      readln ( NAME );
                      assign ( INP, NAME );
                      reset ( INP );
                      OK := false;
                      while ( not OK ) do
                         begin
                            writeln ('Input n');
                            readln ( N );
                            if ( N > 0 ) then
                               begin
                                  OK := true;
                                  readln(INP,X[0],Y[0],XPL[0],YPL[0]);
                                  for I := 1 to N-1 do
                                     begin
                                        read (INP, X[I], Y[I] );
                                        read (INP, XMI[I], YMI[I] );
                                        readln (INP, XPL[I], YPL[I] );
                                     end;
                                  readln(INP,X[N],Y[N],XMI[N],YMI[N]);
                                  close ( INP )
                               end
                            else writeln ('Number must be a positive integer ')
                         end
                   end
                else
                   begin
                      writeln ('Please create the input file as indicated. ');
                      write ('The program will end so the input file can ');
                      writeln ('be created. ');
                      OK := false
                   end
             end;
       end
   end;
procedure OUTPUT;
   begin
      writeln ('Select output destination ');
      writeln ('1. Screen ');
      writeln ('2. Text file ');
      writeln ('Enter 1 or 2 ');
      readln ( FLAG );
      if ( FLAG = 2 ) then
         begin
            write ('Input the file name in the form - ');
            writeln ('drive:name.ext ');
            writeln ('for example:   A:OUTPUT.DTA ');
            readln ( NAME );
            assign ( OUP, NAME )
         end
      else assign ( OUP, 'CON');
      rewrite ( OUP );
      writeln(OUP,'BEZIER CURVE ALGORITHM');
      writeln(OUP);
      writeln(OUP,'A0':12,'A1':12,'A2':12,'A3':12,' on the first line');
      writeln(OUP,'B0':12,'B1':12,'B2':12,'B3':12,' on the second line');
   end;
begin
   INPUT;
   if (OK) then
      begin
         OUTPUT;
{        STEP 1                                                        }
         For I := 0 to N-1 do
            begin
{              STEP 2                                                  }
               A0[I] := X[I];
               B0[I] := Y[I];
               A1[I] := 3*(XPL[I] - X[I]);
               B1[I] := 3*(YPL[I] - Y[I]);
               A2[I] := 3*(X[I]+XMI[I+1]-2*XPL[I]);
               B2[I] := 3*(Y[I]+YMI[I+1]-2*YPL[I]);
               A3[I] := X[I+1]-X[I]+3*XPL[I]-3*XMI[I+1];
               B3[I] := Y[I+1]-Y[I]+3*YPL[I]-3*YMI[I+1];
{              STEP 3                                                  }
               writeln(OUP,A0[I]:12:6,A1[I]:12:6,A2[I]:12:6,A3[I]:12:6);
               writeln(OUP,B0[I]:12:6,B1[I]:12:6,B2[I]:12:6,B3[I]:12:6);
               writeln(OUP);
            end;
         close(OUP)
      end
{  STEP 4                                                              }
end.

