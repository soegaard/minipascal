program ALG042;
{  ROMBERG ALGORITHM 4.2

   To approximate I = integral( ( f(x) dx ) ) from a to b:

   INPUT:   endpoints a, b; integer n.

   OUTPUT:  an array R. ( R(2,n) is the approximation to I. )

   R is computed by rows; only 2 rows saved in storage                 }
var
   R : array [ 1..2 , 1..15 ] of real;
   X,A,B,H,SUM : real;
   I,J,K,L,M,N : integer;
   OK : boolean;
   AA : char;
{  Change function F for a new problem                                 }
function F ( X : real ) : real;
   begin
      F := sin( X )
   end;
procedure INPUT;
   begin
      writeln('This is Romberg integration.');
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
                  writeln ('Input number of rows - no decimal point ');
                  readln ( N );
                  if ( N > 0 ) then OK := true
                  else writeln ('Number must be a positive integer ')
               end
         end
      else
         begin
            write ('The program will end so that the function F ');
            writeln ('can be created ');
            OK := false
         end
   end;
begin
   INPUT;
{  STEP 1                                                              }
   if (OK) then
      begin
         H := B - A;
         R[1,1] := ( F( A ) + F( B ) ) / 2.0 * H;
{        STEP 2                                                        }
         writeln ('Initial Data: ');
         writeln ('Limits of integration = [',A:12:8,', ',B:12:8,']');
         writeln ('Number of rows = ',N:3 );
         writeln; writeln ('Romberg Integration Table: ');
         writeln; writeln ( R[1,1]:12:8 ); writeln;
{        STEP 3                                                        }
         for I := 2 to N do
            begin
{              STEP 4                                                  }
{              approximation from Trapezoidal method                   }
               SUM := 0.0;
               M := round( exp( ( I - 2 ) * ln( 2.0 ) ) );
               for K := 1 to M do SUM := SUM + F( A + ( K - 0.5 ) * H );
               R[2,1] := ( R[1,1] + H * SUM ) / 2.0;
{              STEP 5                                                  }
{              extrapolation                                           }
               for J := 2 to I do
                  begin
                     L := round( exp( 2 * ( J - 1 ) * ln( 2.0 ) ) );
                     R[2,J] := R[2,J-1]+(R[2,J-1]-R[1,J-1])/(L-1.0)
                  end;
{              STEP 6                                                  }
               for K := 1 to I do write ( R[2,K]:12:8 );
               writeln; writeln;
{              STEP 7                                                  }
               H := H / 2.0;
{              STEP 8                                                  }
{              since only two rows are kept in storage, this step      }
{              is to prepare for the next row.                         }
{              update row 1 of R                                       }
               for J := 1 to I do R[1,J] := R[2,J]
            end
      end
{  STEP 9                                                              }
   end.