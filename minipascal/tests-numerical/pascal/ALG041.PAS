program ALG041;
{  SIMPSON'S COMPOSITE ALGORITHM 4.1

   To approximate I = integral ( ( f(x) dx ) ) from a to b:

   INPUT:   endpoints a, b; even positive integer n.

   OUTPUT:  approximation XI to I.
                                                                       }
var
   A,B,XI0,XI1,XI2,H,XI,X : real;
   N,I,NN : integer;
   OK : boolean;
   AA : char;
{  Change function F for a new problem                                 }
function F ( X : real ) : real;
   begin
      F := sin ( X )
   end;
procedure INPUT;
   begin
      writeln('This is Simpsons Method.');
      writeln(' ');
      write ('Has the function F been created in the program ');
      writeln ('immediately preceding ');
      writeln ('the INPUT procedure? ');
      writeln ('Enter Y or N ');
      readln ( AA );
      if ( AA = 'Y' ) or ( AA = 'y' ) then
         begin
            OK := false;
            while ( not OK ) do
               begin
                  write ('Input lower limit of integration and ');
                  writeln ('upper limit of integration ');
                  writeln ('separated by a blank ');
                  readln ( A, B );
                  if ( A >= B ) then
                     begin
                        write ('Lower limit must be less ');
                        writeln ('than upper limit ')
                     end
                     else OK := true
               end;
            OK := false;
            while ( not OK ) do
               begin
                  writeln ('Input an even positive integer N.');
                  readln ( N );
                  if ((N > 0) and ((N div 2) * 2 = N))
                     then OK := true
                     else
                        writeln('Input must be even and positive ')
               end
         end
      else
         begin
            write ('The program will end so that the function F ');
            writeln ('can be created ');
            OK := false
         end
   end;
   procedure OUTPUT;
      begin
         writeln;
         writeln ('The integral of F from ',A:12:8,' to ',B:12:8,' is ');
         writeln ( XI:12:8 )
      end;
   begin
      INPUT;
      if (OK) then
         begin
{        STEP 1                                                        }
            H := ( B - A ) / N;
{           STEP 2                                                     }
            XI0 := F( A ) + F( B );
{           summation of f(x(2*I-1))                                   }
            XI1 := 0.0;
{           summation of f(x(2*I))                                     }
            XI2 := 0.0;
{           STEP 3                                                     }
            NN := N - 1;
            for I := 1 to NN do
               begin
{              STEP 4                                                  }
               X := A + I * H;
{              STEP 5                                                  }
               if ( I = I div 2 * 2 ) then  XI2 := XI2 + F( X )
               else XI1 := XI1 + F( X )
            end;
{           STEP 6                                                     }
            XI := ( XI0 + 2.0 * XI2 + 4.0 * XI1 ) * H / 3.0;
{           STEP 7                                                     }
            OUTPUT
         end
   end.
