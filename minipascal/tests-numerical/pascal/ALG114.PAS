program ALG114;
{
   NONLINEAR FINITE-DIFFERENCE ALGORITHM 11.4

   To approximate the solution to the nonlinear boundary-value problem
      Y'' = F(X,Y,Y'), A<=X<=B, Y(A) = ALPHA, Y(B) = BETA:

   INPUT:   Endpoints A,B; boundary conditions ALPHA, BETA;
            integer N; tolerance TOL; maximum number of iterations M.

   OUTPUT:  Approximations W(I) TO Y(X(I)) for each I=0,1,...,N+1
            or a message that the maximum number of iterations was
            exceeded.
}
var
   W,A,B,C,D,L,U,Z,V : array [ 1..25 ] of real;
   AA,BB,ALPHA,BETA,TOL,H,X,T,VMAX : real;
   N1,NN,K,N,J,I,FLAG : integer;
   OK : boolean;
   AB : char;
   NAME : string [ 30 ];
   OUP : text;
{  Change functions F, FY and FYP for a new problem                    }
function F ( X,Y,Z : real ) : real;
   begin
      F := (32+2*X*X*X-Y*Z)/8
   end;
{   FY represents partial of F with respect to Y                       }
function FY ( X,Y,Z : real ) : real;
   begin
      FY := -Z/8
   end;
{   FYP represents partial of F with respect to Y'                     }
function FYP ( X,Y,Z : real ) : real;
   begin
      FYP := -Y/8
   end;
procedure INPUT;
   begin
      writeln('This is the Nonlinear Finite-Difference Method.');
      OK := false;
      writeln('Have the functions F, FY ( partial of F with respect to y ),');
      writeln ('FYP ( partial of F with respect to y'') been created ');
      writeln ('immediately preceding the INPUT procedure?');
      writeln ('Answer Y or N. ');
      readln ( AB );
      if ( AB = 'Y' ) or ( AB = 'y' ) then
         begin
            while ( not OK ) do
               begin
                  write ('Input left and right endpoints ');
                  writeln('separated by blank. ');
                  readln ( AA, BB );
                  if ( AA >= BB ) then
                    writeln('Left endpoint must be less than right endpoint.')
                  else OK := true
               end;
            writeln ('Input Y(',AA,'). ');
            readln ( ALPHA );
            writeln ('Input Y(',BB,'). ');
            readln ( BETA );
            OK := false;
            while ( not OK ) do
               begin
                  writeln ('Input an integer > 1 for the number of ');
                  writeln ('subintervals.  Note that h = (b-a)/(n+1) ');
                  readln ( N );
                  if ( N <= 1 ) then
                     writeln ('Number must be a positive integer. ')
                  else OK := true
               end;
            OK := false;
            while ( not OK ) do
               begin
                  writeln ('Input Tolerance.');
                  readln ( TOL );
                  if ( TOL <= 0.0 ) then
                     writeln ('Tolerance must be positive. ')
                  else Ok := true
               end;
            OK := false;
            while ( not OK ) do
               begin
                  writeln ('Input maximum number of iterations. ');
                  readln ( NN );
                  if ( NN <= 0 ) then writeln ('Must be positive integer. ')
                  else OK := true
               end
         end
      else writeln ('The program will end so that F, FP, FPY can be created.')
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
      writeln(OUP,'NONLINEAR FINITE-DIFFERENCE METHOD');
      writeln(OUP);
      writeln ( OUP, 'I':3,'X(I)':14,'W(I)':14);
      writeln ( OUP );
   end;
   begin
      INPUT;
      if ( OK ) then
         begin
            OUTPUT;
{           STEP 1                                                     }
            N1 := N - 1;
            H := ( BB - AA ) / ( N + 1 );
{           STEP 2                                                     }
            for I := 1 TO N do
               W[I] := ALPHA+I*H*(BETA-ALPHA)/(BB-AA);
{           STEP 3                                                     }
            K := 1;
{           STEP 4                                                     }
            while ( ( K <= NN ) and ( OK ) ) do
               begin
{                 STEP 5                                               }
                  X := AA + H;
                  T := ( W[2] - ALPHA ) / ( 2.0 * H );
                  A[1] := 2.0 + H * H * FY( X, W[1], T );
                  B[1] := -1.0 + H * FYP( X, W[1], T ) / 2.0;
                  D[1] := - ( 2.0 * W[1] - W[2] - ALPHA + H * H *
                          F( X, W[1], T ) );
{                 STEP 6                                               }
                  for I := 2 TO N1 DO
                     begin
                        X := AA + I * H;
                        T := ( W[I+1] - W[I-1] ) / ( 2.0 * H );
                        A[I] := 2.0 + H * H * FY( X, W[I], T );
                        B[I] := -1.0 + H * FYP( X, W[I], T ) / 2.0;
                        C[I] := -1.0 - H * FYP( X, W[I], T ) / 2.0;
                        D[I] := -(2.0*W[I]-W[I+1]-W[I-1]+H*H*
                                F( X, W[I], T ) )
                     end;
{                 STEP 7                                               }
                  X := BB - H;
                  T := ( BETA - W[N-1] ) / ( 2.0 * H );
                  A[N] := 2.0 + H * H * FY( X, W[N], T );
                  C[N] := -1.0 - H * FYP( X, W[N], T ) / 2.0;
                  D[N] := - ( 2.0 * W[N] - W[N-1] - BETA + H * H *
                          F( X, W[N], T ) );
{                 STEP 8
                  STEPS 8 through 12 solve a tridiagonal linear system
                  using Algorithm 6.7                                  }
                  L[1] := A[1];
                  U[1] := B[1] / A[1];
                  Z[1] := D[1] / L[1];
{                 STEP 9                                               }
                  for I := 2 to N1 do
                     begin
                        L[I] := A[I] - C[I] * U[I-1];
                        U[I] := B[I] / L[I];
                        Z[I] := ( D[I] - C[I] * Z[I-1] ) / L[I];
                     end;
{                 STEP 10                                              }
                  L[N] := A[N] - C[N] * U[N-1];
                  Z[N] := ( D[N] - C[N] * Z[N-1] ) / L[N];
{                 STEP 11                                              }
                  V[N] := Z[N];
                  VMAX := abs( V[N] );
                  W[N] := W[N] + V[N];
{                 STEP 12                                              }
                  for J := 1 to N1 do
                     begin
                        I := N - J;
                        V[I] := Z[I] - U[I] * V[I+1];
                        W[I] := W[I] + V[I];
                        if (abs( V[I] ) > VMAX) then VMAX := abs(V[I])
                     end;
{                 STEP 13
                  test for accuracy                                    }
                  if ( VMAX <= TOL ) then
                     begin
                        I := 0;
                        writeln(OUP,I:3,AA:14:8,ALPHA:14:8);
                        for I := 1 to N DO
                           begin
                              X := AA + I * H;
                              writeln(OUP,I:3,X:14:8,W[I]:14:8)
                           end;
                        I := N + 1;
                        writeln(OUP,I:3,BB:14:8,BETA:14:8);
                        OK := false
                     end
                  else
{                    STEP 14                                           }
                     K := K + 1
               end;
{           STEP 15                                                    }
            if ( K > NN ) then
               writeln ( OUP, 'No convergence in ', NN, ' iterations ');
            close( OUP )
         end
   end.

