

program ALG123;
{  CRANK-NICOLSON ALGORITHM 12.3

   To approximate the solution of the parabolic partial-differential
   equation subject to the boundary conditions
              u(0,t) = u(l,t) = 0, 0 < t < T = max t
   and the initial conditions
               u(x,0) = F(x), 0 <= x <= l:

   INPUT:   endpoint l; maximum time T; constant ALPHA; integers m, N:

   OUTPUT:  approximations W(I,J) to u(x(I),t(J)) for each
            I = 1,..., m-1 and J = 1,..., N.
}
var
   V,L,U,Z : array [ 1..25 ] of real;
   FT,FX,ALPHA,H,K,VV,T,X : real;
   N,M,M1,M2,N1,FLAG,I1,I,J : integer;
   OK : boolean;
   AA : char;
   NAME : string [ 30 ];
   OUP : text;
{  Change function F for a new problem                                 }
function F( X : real ) : real;
   begin
      F := sin(pi*X)
   end;
procedure INPUT;
   begin
      writeln('This is the Crank-Nicolson Method.');
      writeln ('Has the function F(x) been created immediately');
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
            writeln ('Input the constant alpha.  ');
            readln ( ALPHA );
            OK := false;
            while ( not OK ) do
               begin
                  writeln ('Input integer m = number of intervals on X-axis ');
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
      writeln(OUP,'CRANK-NICOLSON METHOD');
      writeln(OUP);
      write ( OUP,'I':3,'X(I)':12,'     W(X(I),',FT:12,')' );
      writeln ( OUP );
      for I := 1 to M1 do
         begin
            X := I * H;
            writeln(OUP,I:3,X:12:8,V[I]:14:8)
         end;
      close ( OUP )
   end;
   begin
      INPUT;
      if ( OK ) then
         begin
            M1 := M - 1;
            M2 := M - 2;
{           STEP 1                                                     }
            H := FX / M;
            K := FT / N;
{           VV is used for lambda                                      }
            VV := ALPHA * ALPHA * K / ( H * H );
{           set V(M) = 0                                               }
            V[M] := 0.0;
{           STEP 2                                                     }
            for I := 1 to M1 do V[I] := F( I * H );
{           STEP 3
            STEPS 3 through 11 solve a tridiagonal linear system
            using Algorithm 6.7                                        }
            L[1] := 1.0 + VV;
            U[1] := -VV / ( 2.0 * L[1] );
{           STEP 4                                                     }
            for I := 2 to M2 do
               begin
                  L[I] := 1.0 + VV + VV * U[I-1] / 2.0;
                  U[I] := -VV / ( 2.0 * L[I] )
               end;
{           STEP 5                                                     }
            L[M1] := 1.0 + VV + 0.5 * VV * U[M2];
{           STEP 6                                                     }
            for J := 1 to N do
               begin
{                 STEP 7                                               }
{                 current t(j)                                         }
                  T := J * K;
                  Z[1] := ((1.0-VV)*V[1]+VV*V[2]/2.0)/L[1];
{                 STEP 8                                               }
                  for I := 2 to M1 do
                     Z[I] := ((1.0-VV)*V[I]+0.5*VV*(V[I+1]+
                             V[I-1]+Z[I-1]))/L[I];
{                 STEP 9                                               }
                  V[M1] := Z[M1];
{                 STEP 10                                              }
                  for I1 := 1 to M2 do
                     begin
                        I := M2 - I1 + 1;
                        V[I] := Z[I] - U[I] * V[I+1]
                     end
               end;
{           STEP 11                                                    }
            OUTPUT
         end
{  STEP 12                                                             }
   end.


