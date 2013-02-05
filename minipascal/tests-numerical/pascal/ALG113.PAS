program ALG113;
{
   LINEAR FINITE-DIFFERENCE ALGORITHM 11.3

   To approximate the solution of the boundary-value problem

      Y'' = P(X)Y' + Q(X)Y + R(X), A<=X<=B, Y(A) = ALPHA, Y(B) = BETA:

   INPUT:   Endpoints A, B; boundary conditions ALPHA, BETA;
            integer N.

   OUTPUT:  Approximations W(I) to Y(X(I)) for each I=0,1,...,N+1.
}
var
   A,B,C,D,L,U,Z,W : array [ 1..24 ] of real;
   AA,BB,ALPHA,BETA,X,H : real;
   N,FLAG,I,M,J : integer;
   OK : boolean;
   AB : char;
   NAME : string [ 30 ];
   OUP : text;
{  Change functions P, Q and R for a new problem                       }
function P ( X : real ) : real;
   begin
      P := -2/X
   end;
function Q ( X : real ) : real;
   begin
      Q := 2/(X*X)
   end;
function R ( X : real ) : real;
   begin
      R := sin(ln(X))/(X*X)
   end;
procedure INPUT;
   begin
      writeln('This is the Linear Finite-Difference Method.');
      OK := false;
      writeln ('Have the functions P, Q and R been created immediately');
      writeln ('preceding the INPUT procedure? ');
      writeln ('Answer Y or N. ');
      readln ( AB );
      if ( AB = 'Y' ) or ( AB = 'y' ) then
         begin
            while ( not OK ) do
               begin
                  write('Input left and right endpoints ');
                  writeln('separated by blank. ');
                  readln ( AA, BB );
                  if ( AA >= BB ) then
                    writeln ('Left endpoint must be less than right endpoint.')
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
                     writeln ('Number must exceed 1. ')
                  else OK := true
               end
         end
      else writeln ('The program will end so that P, Q, R can be created. ')
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
      writeln(OUP,'LINEAR FINITE DIFFERENCE METHOD');
      writeln(OUP);
      write ( OUP, 'I':3,'X(I)':14,'W(I)':14);
      writeln ( OUP );
   end;
   begin
      INPUT;
      if ( OK ) then
         begin
            OUTPUT;
{           STEP 1                                                     }
            H := ( BB - AA ) / ( N + 1.0 );
            X := AA + H;
            A[1] := 2.0 + H * H * Q( X );
            B[1] := -1.0 + 0.5 * H * P( X );
            D[1] := -H*H*R(X)+(1.0+0.5*H*P(X))*ALPHA;
            M := N - 1;
{           STEP 2                                                     }
            for I := 2 to M do
               begin
                  X := AA + I * H;
                  A[I] := 2.0 + H * H * Q( X );
                  B[I] := -1.0 + 0.5 * H * P( X );
                  C[I] := -1.0 - 0.5 * H * P( X );
                  D[I] := -H * H * R( X )
               end;
{           STEP 3                                                      }
            X := BB - H;
            A[N] := 2.0 + H * H * Q( X );
            C[N] := -1.0 - 0.5 * H * P( X );
            D[N] := -H*H*R(X)+(1.0-0.5*H*P(X))*BETA;
{           STEP 4
            STEPS 4 through 8 solve a triagiagonal linear system using
            Algorithm 6.7                                              }
            L[1] := A[1];
            U[1] := B[1] / A[1];
            Z[1] := D[1] / L[1];
{           STEP 5                                                     }
            for I := 2 to M do
               begin
                  L[I] := A[I] - C[I] * U[I-1];
                  U[I] := B[I] / L[I];
                  Z[I] := (D[I] - C[I] * Z[I-1])/L[I];
               end;
{           STEP 6                                                     }
            L[N] := A[N] - C[N] * U[N-1];
            Z[N] := (D[N] - C[N] * Z[N-1])/L[N];
{           STEP 7                                                     }
            W[N] := Z[N];
{           STEP 8                                                     }
            for J := 1 to M do
               begin
                  I := N - J;
                  W[I] := Z[I] - U[I] * W[I+1]
               end;
            I := 0;
{           STEP 9                                                     }
            writeln (OUP,I:3,AA:14:8,ALPHA:14:8);
            for I := 1 to N do
               begin
                  X := AA + I * H;
                  writeln(OUP,I:3,X:14:8,W[I]:14:8)
               end;
            I := N + 1;
            writeln(OUP,I:3,BB:14:8,BETA:14:8);
{           STEP 10                                                    }
            close( OUP )
         end
   end.

