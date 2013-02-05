program ALG054;
{  ADAMS-FORTH ORDER PREDICTOR-CORRECTOR ALGORITHM 5.4

   To approximate the solution of the initial value problem
          y' = f(t,y), a <= t <= b, y(a) = alpha,
   at N+1 equally spaced points in the interval [a,b].

   INPUT:   endpoints a,b; initial condition alpha; integer N.

   OUTPUT:  approximation w to y at the (N+1) values of t.
}
var
   T,W : array [ 1..4 ] of real;
   A,B,ALPHA,H,T0,W0 : real;
   FLAG,I,N,J : integer;
   OK : boolean;
   NAME : string [ 30 ];
   OUP : text;
   AA : char;
{  Change function F for a new problem.                                }
function F ( T, Y : real ) : real;
   begin
      F := Y - T*T + 1.0
   end;
procedure RK4 ( H, T0, W0 : real; var T1, W1 : real );
   var
      K1, K2, K3, K4 : real;
   begin
      T1 := T0 + H;
      K1 := H * F( T0, W0 );
      K2 := H * F( T0 + 0.5 * H, W0 + 0.5 * K1 );
      K3 := H * F(T0 + 0.5 * H, W0 + 0.5 * K2 );
      K4 := H * F( T1, W0 + K3 );
      W1 := W0 + ( K1 + 2.0 * ( K2 + K3 ) + K4 ) / 6.0
   end;
procedure INPUT;
   begin
      writeln('This is Adams-Bashforth Predictor Corrector Method');
      writeln('Has the function F been created in the program immediately');
      writeln('preceding the INPUT procedure?  Enter Y or N.');
      readln(AA);
      if ((AA = 'Y') or (AA = 'y')) then
         begin
            OK := false;
            while ( not OK ) do
               begin
                  writeln('Input left and right endpoints separated by blank.');
                  readln ( A, B );
                  if ( A >= B ) then
                    writeln('Left endpoint must be less than right endpoint.')
                  else OK := true
               end;
            writeln ('Input the initial condition. ');
            readln ( ALPHA );
            OK := false;
            while ( not OK ) do
               begin
                  writeln ('Input an integer greater than or equal to 4 ');
                  writeln ('for the number of subintervals. ');
                  readln ( N );
                  if ( N < 4 ) then
                     writeln ('Number must be greater than or equal to 4. ')
                  else OK := true
               end;
         end
      else
         begin
            writeln('The program will end so that F can be created.');
            OK := false
         end
   end;
procedure OUTPUT;
   begin
      writeln ('Choice of output method: ');
      writeln ('1. Output to screen ');
      writeln ('2. Output to text file ');
      writeln ('Please enter 1 or 2. ');
      readln ( FLAG );
      if ( FLAG = 2 ) then
         begin
            writeln ('Input the file name in the form - drive:name.ext, ');
            writeln('for example:   A:OUTPUT.DTA');
            readln ( NAME );
            assign ( OUP, NAME )
         end
      else assign ( OUP, 'CON' );
      rewrite ( OUP );
      writeln(OUP,'ADAMS-BASHFORTH FOURTH ORDER PREDICTOR CORRECTOR METHOD');
      writeln ( OUP );
      writeln(OUP,'t':5,'w':12);
   end;
begin
   INPUT;
   if (OK) then
      begin
         OUTPUT;
{        STEP 1                                                        }
{        The subscripts are shifted to avoid zero subscripts           }
         H := ( B - A ) / N;
         T[1] := A;
         W[1] := ALPHA;
         writeln(OUP,T[1]:5:3,W[1]:12:7);
{        STEP 2                                                        }
         for I := 1 to 3 do
            begin
{              STEP 3 AND 4                                            }
{              compute starting values using Runge-Kutta method
               given in a subroutine                                   }
               RK4( H, T[I], W[I], T[I+1], W[I+1] );
               writeln(OUP,T[I+1]:5:3,W[I+1]:12:7);
{              STEP 5                                                  }
            end;
{        STEP 6                                                        }
         for I := 4 to N do
            begin
{              STEP 7                                                  }
{              T0, W0 will be used in place of t, w resp.              }
               T0 := A + I * H;
{              predict W(I)                                            }
               W0 := W[4]+H*(55.0*F(T[4],W[4])-59.0*F(T[3],W[3])
                     +37.0*F(T[2],W[2])-9.0*F(T[1],W[1]))/24.0;
{              correct W(I)                                            }
               W0 := W[4]+H*(9.0*F(T0,W0)+19.0*F(T[4],W[4])
                     -5.0*F(T[3],W[3])+F(T[2],W[2]))/24.0;
{              STEP 8                                                  }
               writeln(OUP,T0:5:3,W0:12:7);
{              STEP 9                                                  }
{              prepare for next iteration                              }
               for J := 1 to 3 do
                  begin
                     T[J] := T[J+1];
                     W[J] := W[J+1]
                  end;
{              STEP 10                                                 }
               T[4] := T0;
               W[4] := W0
            end;
{        STEP 11                                                       }
         close ( OUP )
      end
end.