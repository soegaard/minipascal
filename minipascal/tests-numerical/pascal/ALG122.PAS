program ALG122;
{
   HEAT EQUATION BACKWARD-DIFFERENCE ALGORITHM 12.2

   To approximate the solution to the parabolic partial-differential
   equation subject to the boundary conditions
                  u(0,t) = u(l,t) = 0, 0 < t < T = max t,
   and the initial conditions
                  u(x,0) = F(x), 0 <= x <= l:

   INPUT:   endpoint l; maximum time T; constant ALPHA; integers m, N.

   OUTPUT:  approximations W(I,J) to u(x(I),t(J)) for each
            I = 1, ..., m-1 and J = 1, ..., N.
}
var
   W,L,U,Z : array [ 1..25 ] of real;
   FT,FX,ALPHA,H,K,VV,T,X : real;
   N,M,M1,M2,N1,FLAG,I1,I,J : integer;
   OK : boolean;
   AA : char;
   NAME : string [ 30 ];
   OUP : text;
{  Change F for a new problem                                          }
function F( X : real ) : real;
   begin
      F := sin( PI * X )
   end;
procedure INPUT;
   begin
      writeln('This is the Backward-Difference Method for Heat Equation.');
      writeln ('Has the function F been created immediately ');
      writeln ('preceding the INPUT procedure? Answer Y or N. ');
      readln ( AA );
      if ( AA = 'Y' ) or ( AA = 'y' ) then
         begin
            writeln ('The lefthand endpoint on the X-axis is 0. ');
            OK := false;
            while ( not OK ) do
               begin
                  writeln ('Input the righthand endpoint on the X-axis. ');
                  readln ( FX );
                  if ( FX <= 0.0 ) then
                     writeln ('Must be positive number. ')
                  else
                     OK := true
               end;
            OK := false;
            while ( not OK ) do
               begin
                  writeln ('Input the maximum value of the time variable T. ');
                  readln ( FT );
                  if ( FT <= 0.0 ) then
                     writeln ('Must be positive number. ')
                  else
                     Ok := true
               end;
            writeln ('Input the constant alpha. ');
            readln ( ALPHA );
            OK := false;
            while ( not OK ) do
               begin
                  writeln('Input integer m = number of intervals on X-axis');
                  write ('and N = number of time intervals ');
                  writeln ('- separated by a blank. ');
                  writeln('Note that m should be 3 or larger.');
                  readln ( M, N );
                  if ( ( M <= 2 ) or ( N <= 0 ) ) then
                     writeln ('Numbers not in the correct range. ')
                  else
                     OK := true
               end
         end
      else
         begin
            write ('The program will end so that the function ');
            writeln ('F can be created. ');
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
      writeln(OUP,'THIS IS THE BACKWARD-DIFFERENCE METHOD');
      writeln(OUP);
      write ( OUP,'I':3,'X(I)':12,'     W(X(I),',FT:12,')');
      writeln ( OUP );
      for I := 1 to M1 do
         begin
            X := I * H;
            writeln ( OUP, I:3, X:12:8,'     ', W[I]:14:8)
         end;
      close ( OUP )
   end;
   begin
      INPUT;
      if ( OK ) then
         begin
            M1 := M - 1;
            M2 := M - 2;
            N1 := N - 1;
{           STEP 1                                                     }
            H := FX / M;
            K := FT / N;
            VV := ALPHA * ALPHA * K / ( H * H );
{           STEP 2                                                     }
            for I := 1 to M1 do W[I] := F( I * H );
{           STEP 3
            STEPS 3 through 11 solve a tridiagonal linear system
            using Algorithm 6.7                                        }
            L[1] := 1.0 + 2.0 * VV;
            U[1] := -VV / L[1];
{           STEP 4                                                     }
            for I := 2 to M2 do
               begin
                  L[I] := 1.0 + 2.0 * VV + VV * U[I-1];
                  U[I] := -VV / L[I]
               end;
{           STEP 5                                                     }
            L[M1] := 1.0 + 2.0 * VV + VV * U[M2];
{           STEP 6                                                     }
            for J := 1 to N do
               begin
{                 STEP 7                                               }
{                 current t(j)                                         }
                  T := J * K;
                  Z[1] := W[1] / L[1];
{                 STEP 8                                               }
                  for I := 2 to M1 do
                      Z[I] := ( W[I] + VV * Z[I-1] ) / L[I];
{                 STEP 9                                               }
                  W[M1] := Z[M1];
{                 STEP 10                                              }
                  for I1 := 1 to M2 do
                     begin
                        I := M2 - I1 + 1;
                        W[I] := Z[I] - U[I] * W[I+1]
                     end
               end;
{           STEP 11                                                    }
            OUTPUT
         end
{        STEP 12                                                       }
   end.


