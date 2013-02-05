program ALG045;
{  GAUSSIAN DOUBLE INTEGRAL ALGORITHM 4.5

   To approximate I = double integral ( ( f(x,y) dy dx ) ) with limits
   of integration from a to b for x and from c(x) to d(x) for y:

   INPUT:   endpoints a, b; positive integers m, n.  (Assume that the
            roots r(i,j) and coefficients c(i,j) are available for
            i equals m and n for 1<= j <= i.)

   OUTPUT:  approximation J to I.
}
var
   r,co : array [1..5,1..5] of real;
   A,B,H1,H2,AJ,JX,D1,C1,K1,K2,X,Y,Q : real;
   N,M,I,J : integer;
   OK : boolean;
   AA : char;
{  Change functions F,C,D for a new problem                            }
function F ( X, Y : real ) : real;
{  F is the integrand                                                  }
   begin
      F := exp( Y / X )
   end;
function C ( X : real ) : real;
{   C(X) is the lower limit of Y                                       }
   begin
      C := X * X * X
   end;
function D ( X : real ) : real;
{   D(X) is the upper limit of Y                                       }
   begin
      D := X * X
   end;
procedure INPUT;
   begin
      writeln('This is Gaussian Quadrature for double integrals.');
      write ('Have the functions F, C and D been created in the ');
      writeln ('program immediately ');
      writeln ('preceding the INPUT procedure? ');
      writeln ('Enter Y or N. ');
      readln ( AA );
      if ( AA = 'Y' ) or ( AA = 'y' ) then
         begin
            OK := false;
            while ( not OK ) do
               begin
                  write ('Input lower and upper limits of integration ');
                  writeln ('of the outer integral separated ');
                  writeln ('by a blank. ');
                  readln ( A, B );
                  if ( A >= B ) then
                     begin
                        write ('Lower limit must be less ');
                        writeln ('than upper limit. ')
                     end
                  else OK := true
               end;
            OK := false;
            while ( not OK ) do
               begin
                  writeln;
                  writeln ('Input two integers M,N. ');
                  write('Both must be less than or equal to 5');
                  writeln(' and greater than 1');
                  writeln(' for this implementation.');
                  writeln ('Gaussian quadrature uses M for outer ');
                  write ('integral and N for inner ');
                  writeln ('integral - separate with blank. ');
                  readln ( M, N );
                  if ( ( M <= 1 ) or ( N <= 1 ) ) then
                     writeln ('Integers must be greater than 1. ')
                  else
                      if ( (M > 5) or (N > 5) ) then
                         writeln('Integers must be less than or equal to 5')
                      else  OK := true;
               end
         end
      else
         begin
            write ('The program will end so that the functions F,C,D');
            writeln (' can be created. ');
            OK := false
         end
   end;
procedure OUTPUT;
   begin
      writeln;
      writeln ('The integral of F from ',A:12:8,' to ',B:12:8,' is ');
      write ( AJ:14:10 );
      writeln (' obtained with M = ',M:3,' and N = ',N:3 );
   end;
begin
   INPUT;
   if (OK) then
      begin
{ Initialize coefficients c[i,j] and roots r[i,j]                      }
         r[2,1] := 0.5773502692; r[2,2] := -r[2,1]; co[2,1] := 1.0;
         co[2,2] := 1.0; r[3,1] := 0.7745966692; r[3,2] := 0.0;
         r[3,3] := -r[3,1]; co[3,1] := 0.5555555556; co[3,2] := 0.8888888889;
         co[3,3] := co[3,1]; r[4,1] := 0.8611363116; r[4,2] := 0.3399810436;
         r[4,3] := -r[4,2]; r[4,4] := -r[4,1]; co[4,1] := 0.3478548451;
         co[4,2] := 0.6521451549; co[4,3] := co[4,2]; co[4,4] := co[4,1];
         r[5,1] := 0.9061798459; r[5,2] := 0.5384693101; r[5,3] := 0.0;
         r[5,4] := -r[5,2]; r[5,5] := -r[5,1]; co[5,1] := 0.2369268850;
         co[5,2] := 0.4786286705; co[5,3] := 0.5688888889; co[5,4] := co[5,2];
         co[5,5] := co[5,1];
{        STEP 1                                                        }
         H1 := ( B - A ) / 2.0;
         H2 := ( B + A ) / 2.0;
         AJ := 0.0;                             { Use AJ instead of J. }
{        STEP 2                                                        }
         for I := 1 to M do
            begin
{              STEP 3                                                  }
               X := H1 * r[M,I] + H2;
               JX := 0.0;
               C1 := C( X );
               D1 := D( X );
               K1 := ( D1 - C1 ) / 2.0 ;
               K2 := ( D1 + C1 ) / 2.0;
{              STEP 4                                                  }
               for J := 1 to N do
                  begin
                     Y := K1 * r[N,J] + K2;
                     Q := F( X, Y );
                     JX := JX + co[N,J] * Q
                  end;
{              STEP 5                                                  }
               AJ := AJ + co[M,I] * K1 * JX
            end;
{        STEP 6                                                        }
         AJ := AJ * H1;
{        STEP 7                                                        }
         OUTPUT
      end
   end.


