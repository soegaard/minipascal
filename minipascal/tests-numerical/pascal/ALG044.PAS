program ALG044;
{  DOUBLE INTEGRAL ALGORITHM 4.4

   To approximate I = double integral ( ( f(x,y) dy dx ) ) with limits
   of integration from a to b for x and from c(x) to d(x) for y:

   INPUT:   endpoints a, b; positive integers m, n.

   OUTPUT:  approximation J to I.
}
var
   A,B,H,AN,AE,AO,X,HX,BN,YA,YB,BE,BO,Y,Z,A1,AC : real;
   N,M,NN,MM,I,J : integer;
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
      writeln('This is Simpsons Method for double integrals.');
      writeln(' ');
      write ('Have the functions F, C and D been created in the ');
      writeln ('program immediately ');
      writeln ('preceding the INPUT procedure? ');
      writeln ('Enter Y or N ');
      readln ( AA );
      if ( AA = 'Y' ) or ( AA = 'y' ) then
         begin
            OK := false;
            while ( not OK ) do
               begin
                  write ('Input lower and upper limits of integration ');
                  writeln ('of the outer integral separated ');
                  writeln ('by a blank ');
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
                  write ('Input two positive integers N, M; there will ');
                  writeln ('be 2N subintervals for outer ');
                  write ('integral and 2M subintervals for inner ');
                  writeln ('integral - separate with blank ');
                  readln ( N, M );
                  if ( ( M <= 0 ) or ( N <= 0 ) ) then
                     writeln ('Integers must be positive ')
                  else OK := true
               end
         end
      else
         begin
            write ('The program will end so that the functions F,C,D');
            writeln (' can be created ');
            OK := false
         end
   end;
procedure OUTPUT;
   begin
      writeln ('The integral of F from ',A:12:8,' to ',B:12:8,' is ');
      write ( AC:12:8 );
      writeln (' obtained with N = ',N:3,' and M = ',M:3 )
   end;
begin
   INPUT;
   if (OK) then
      begin
         NN := 2 * N;
         MM := 2 * M - 1;
{        STEP 1                                                        }
         H := ( B - A ) / NN;
{        use AN, AE, AO for J(1), J(2), J(3) resp.                     }
{        end terms                                                     }
         AN := 0.0;
{        even terms                                                    }
         AE := 0.0;
{        odd terms                                                     }
         AO := 0.0;
{        STEP 2                                                        }
         for I := 0 to NN do
            begin
{              STEP 3                                                  }
{              Composite Simpson's Method for X                        }
               X := A + I * H;
               YA := C( X );
               YB := D( X );
               HX := ( YB - YA ) / ( 2.0 * M );
{              use BN, BE, BO for K(1), K(2), K(3) resp.               }
{              end terms                                               }
               BN := F( X, YA ) + F( X, YB );
{              even terms                                              }
               BE := 0.0;
{              odd terms                                               }
               BO := 0.0;
{              STEP 4                                                  }
               for J := 1 to MM do
                  begin
{                    STEP 5                                            }
                     Y := YA + J * HX;
                     Z := F( X, Y );
{                    STEP 6                                            }
                     if ( J = J div 2 * 2 ) then BE := BE + Z
                     else BO := BO + Z
                  end;
{              STEP 7                                                  }
{              use A1 for L, which is the integral of F(X(I),Y) from C(X(I))
               to D(X(I)) by Composite Simpson's method                }
               A1 := ( BN + 2.0 * BE + 4.0 * BO ) * HX / 3.0;
{              STEP 8                                                  }
               if ( ( I = 0 ) or ( I = NN ) ) then AN := AN + A1
               else
                  if ( I = I div 2 * 2 ) then AE := AE + A1
                  else AO := AO + A1
            end;
{        STEP 9                                                        }
{        Use AC for J                                                  }
         AC := ( AN + 2.0 * AE + 4.0 * AO ) * H / 3.0;
{        STEP 10                                                       }
         OUTPUT
      end
   end.


