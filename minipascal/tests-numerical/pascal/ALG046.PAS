program ALG046;
{  GAUSSIAN TRIPLE INTEGRAL ALGORITHM 4.6

   To approximate I = triple integral ( ( f(x,y,z) dz dy dx ) ) with limits
   of integration from a to b for x, from c(x) to d(x) for y, and from
   alpha(x,y) to beta(x,y) for z.

   INPUT:   endpoints a, b; positive integers m, n, p.  (Assume that the
            roots r(i,j) and coefficients c(i,j) are available for i
            equals m, n, and p and for 1 <= j <= i.)

   OUTPUT:  approximation J TO I.
}
var
   r,co : array [1..5,1..5] of real;
   A,B,H1,H2,AJ,JX,K1,K2,D1,C1,JY,Z1,Z2,L1,L2,X,Y,Z,Q : real;
   N,M,I,J,P,K : integer;
   OK : boolean;
   AA : char;
{  Change functions F,C,D,ALPHA,BETA for a new problem                 }
function F ( X, Y, Z : real ) : real;
{   F is the integrand                                                 }
   begin
      F := sqrt( X * X + Y * Y )
   end;
function C ( X : real ) : real;
{  C is the lower limit for Y                                          }
   begin
      C := 0.0
   end;
function D ( X : real ) : real;
{  D is the upper limit for Y                                          }
   begin
      D := sqrt( 4 - X * X )
   end;
function ALPHA ( X, Y : real ) : real;
{  ALPHA is the lower limit for Z                                      }
   begin
      ALPHA := sqrt( X * X + Y * Y )
   end;
function BETA ( X, Y : real ) : real;
{  BETA is the upper limit for Z                                       }
   begin
      BETA := 2
   end;
procedure INPUT;
   begin
      writeln('This is Gaussian Quadrature for triple integrals.');
      write ('Have the functions F, C, D, ALPHA, ');
      writeln ('and BETA been created in ');
      writeln ('the program immediately preceding the INPUT procedure? ');
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
                  writeln ('Input three integers M, N, P. ');
                  writeln ('They must be less than or equal to 5 ');
                  writeln (' and greater than 1 for');
                  writeln ('this implementation.  They');
                  writeln ('will be used for Gaussian quadrature in ');
                  writeln ('first, second, and third dimensions, resp. ');
                  writeln (' - separate with blank. ');
                  readln ( M, N, P );
                  if ( ( N <= 1 ) or ( M <= 1 ) or ( P <= 1 ) ) then
                     writeln ('Integers must be positive. ')
                  else
                     if ((M > 5) or (N > 5) or (P > 5)) then
                     writeln('Integers must be less than or equal to 5')
                  else OK := true;
               end
         end
      else
         begin
            write ('The program will end so that the functions F,');
            writeln ('C,D,ALPHA,BETA can be created. ');
            OK := false
         end
   end;
procedure OUTPUT;
   begin
      writeln;
      write ('The integral of F from ',A:12:8,' to ',B:12:8,' is ');
      writeln ( AJ:14:10 );
      writeln (' obtained with M = ',M:3,' , N = ',N:3,' and P = ',P:3 );
   end;
begin
   INPUT;
   if (OK) then
      begin
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
         AJ := 0.0;                           {Use AJ instead of J.}
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
{                    STEP 5                                            }
                     Y := K1 * r[N,J] + K2;
                     JY := 0.0;
{                    Use Z1 for Beta and Z2 for alpha.                 }
                     Z1 := BETA(X,Y);
                     Z2 := ALPHA(X,Y);
                     L1 := (Z1-Z2)/2.0;
                     L2 := (Z1+Z2)/2.0;
{                    STEP6                                             }
                     for K := 1 to P do
                        begin
                           Z := L1 * r[P,K] + L2;
                           Q := F( X, Y, Z );
                           JY := JY + co[P,K] * Q
                        end;
{                    STEP 7                                            }
                     JX := JX + co[N,J] * L1 * JY
                  end;
{              STEP 8                                                  }
               AJ := AJ + co[M,I] * K1 * JX
            end;
{        STEP 9                                                        }
         AJ := AJ * H1;
{        STEP 10                                                       }
         OUTPUT
      end
   end.


