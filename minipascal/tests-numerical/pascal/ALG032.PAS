program ALG032;
{  NEWTON'S INTERPOLATORY DIVIDED-DIFFERENCE FORMULA ALGORITHM 3.2

   To obtain the divided-difference coefficients of the interpolatory
   polynomial P on the (n+1) distinct numbers x(0), x(1), ..., x(n)
   for the function f:

   INPUT:   numbers x(0), x(1), ..., x(n); values f(x(0)), f(x(1)), ...,
            f(x(n)) as the first column Q(0,0), Q(1,0), ..., Q(N,0) OF Q,
            or may be computed if function f is supplied.

   OUTPUT:  the numbers Q(0,0), Q(1,1), ..., Q(N,N) where
            P(x) = Q(0,0) + Q(1,1)*(x - x(0)) + Q(2,2)*(x - x(0))*(x - x(1))
            +... + Q(N,N)*(x - x(0))*(x - x(1))*...*(x - x(N - 1)).
                                                                       }
var
   Q : array [ 0..25, 0..25 ] of real;
   X : array [ 0..25 ] of real;
   I,J,N,FLAG : integer;
   OK : boolean;
   A : char;
   INP, OUP : text;
   NAME : string [ 30 ];
{  Change F if program is to calculate the first column of Q           }
function F ( X : real ) : real;
   begin
      F := 1.0 / X
   end;
procedure INPUT;
   begin
      writeln('Newtons form of the interpolation polynomial');
      OK := false;
      while ( not OK ) do
         begin
            writeln ('Choice of input method: ');
            writeln ('1. Input entry by entry from keyboard ');
            writeln ('2. Input data from a text file ');
            writeln ('3. Generate data using a function F ');
            writeln ('Choose 1, 2, or 3 please ');
            readln ( FLAG );
            if ( FLAG = 1 ) or ( FLAG = 2 ) or ( FLAG = 3 ) then OK := true
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
                            for I := 0 to N do
                               begin
                                  write ('Input X(',I,') and F(X(',I,')) ');
                                  writeln ('separated by space ');
                                  readln ( X[I], Q[I,0] )
                               end
                        end
                     else writeln ('Number must be a positive integer ')
                  end
             end;
         2 : begin
                write ('Has a text file been created with the data in two ');
                writeln ('columns ? ');
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
                                  for I := 0 to N do
                                     readln ( INP, X[I], Q[I,0]);
                                  close ( INP )
                               end
                            else writeln ('Number must be a positive integer ')
                         end
                   end
                else
                   begin
                      write ('Please create the input file in two column ');
                      writeln ('form with the X values and ');
                      writeln ('F(X) values in the corresponding columns ');
                      write ('The program will end so the input file can ');
                      writeln ('be created. ');
                      OK := false
                   end
             end;
         3 : begin
                write ('Has the function F been created in the program ');
                writeln ('immediately preceding ');
                writeln ('the INPUT procedure? ');
                writeln ('Enter Y or N ');
                readln ( A );
                if ( A = 'Y' ) or ( A = 'y' ) then
                   begin
                      OK := false;
                      while ( not OK ) do
                         begin
                            writeln ('Input n');
                            readln ( N );
                            if ( N > 0 ) then
                               begin
                                  OK := true;
                                  for I := 0 to N do
                                     begin
                                        writeln  ('Input X(',I,') ');
                                        readln ( X[I] );
                                        Q[I,0] := F( X[I] )
                                     end
                               end
                            else writeln ('Number must be a positive integer ')
                         end
                   end
                else
                   begin
                      write ('The program will end so that the function F ');
                      writeln ('can be created. ');
                      OK := false
                   end
             end
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
      writeln(OUP,'NEWTONS INTERPOLATION POLYNOMIAL');
      writeln(OUP)
   end;
   begin
      INPUT;
      if ( OK ) then
         begin
            OUTPUT;
{           STEP 1                                                     }
            for I := 1 to N do
               for J := 1 to I do
                  Q[I,J] := ( Q[I,J-1] - Q[I-1,J-1] ) /
                            ( X[I] - X[I-J] );
{           STEP 2                                                     }
            writeln (OUP,'Input data follows: ');
            for I := 0 to N do
               writeln(OUP,'X(',I,') = ',X[I]:12:8,'  F(X(',I,')) = ',
                         Q[I,0]:12:8 );
            writeln(OUP);
            writeln (OUP,'The coefficients Q(0,0), ..., Q(N,N) are: ');
            for I := 0 to N do writeln (OUP,Q[I,I]:12:8 )
         end;
      close(OUP)
   end.
