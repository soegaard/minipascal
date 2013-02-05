program ALG112;
{
   NONLINEAR SHOOTING ALGORITHM 11.2

   To approximate the solution of the nonlinear boundary-value problem

          Y'' = F(X,Y,Y'), A<=X<=B, Y(A) = ALPHA, Y(B) = BETA:


   INPUT:   Endpoints A,B; boundary conditions ALPHA, BETA; number of
            subintervals N; tolerance TOL; maximum number of iterations M.

   OUTPUT:  Approximations W(1,I) TO Y(X(I)); W(2,I) TO Y'(X(I))
            for each I=0,1,...,N or a message that the maximum
            number of iterations was exceeded.
}
var
   W1,W2 : array [ 1..26 ] of real;
   X,T,A,B,ALPHA,BETA,TOL,H,TK,U1,U2 : real;
   K11,K12,K21,K22,K31,K32,K41,K42 : real;
   NN,K,N,J,FLAG,I : integer;
   OK : boolean;
   AA : char;
   NAME : string [ 30 ];
   OUP : text;
{  Change functions F, FY and FYP for a new problem                    }
function F ( X,Y,Z : real ) : real;
   begin
      F := (32+2*X*X*X-Y*Z)/8
   end;
{   FY REPRESENTS PARTIAL OF F WITH RESPECT TO Y                       }
function FY ( X,Y,Z : real ) : real;
   begin
      FY := -Z/8
   end;
{   FYP REPRESENTS PARTIAL OF F WITH RESPECT TO Y'                     }
function FYP ( X,Y,Z : real ) : real;
   begin
      FYP := -Y/8
   end;
procedure INPUT;
   begin
      writeln('This is the Nonlinear Shooting Method.');
      OK := false;
      writeln ('Have the functions F, FY ( partial of F with respect to y ),');
      writeln ('FYP ( partial of F with respect to y'') ');
      writeln('been created immediately preceding the INPUT procedure?');
      writeln ('Answer Y or N. ');
      readln ( AA );
      if ( AA = 'Y' ) or ( AA = 'y' ) then
         begin
            while ( not OK ) do
               begin
                  write ('Input left and right endpoints ');
                  writeln('separated by blank. ');
                  readln ( A, B );
                  if ( A >= B ) then
                    writeln('Left endpoint must be less than right endpoint.')
                  else OK := true
               end;
            writeln ('Input Y(',A,'). ');
            readln ( ALPHA );
            writeln ('Input Y(',B,'). ');
            readln ( BETA );
            TK := ( BETA - ALPHA ) / ( B - A );
            writeln('TK = ',TK);
            writeln('input new TK if desired');
            readln(TK);
            OK := false;
            while ( not OK ) do
               begin
                  write ('Input an integer > 1 for the number of ');
                  writeln ('subintervals. ');
                  readln ( N );
                  if ( N <= 1 ) then
                     writeln ('Number must exceed 1. ')
                  else OK := true
               end;
            OK := false;
            while ( not OK ) do
               begin
                  writeln ('Input Tolerance. ');
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
      else writeln ('The program will end so that F, FP, FPY can be created. ')
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
      writeln(OUP,'NONLINEAR SHOOTING METHOD');
      writeln(OUP);
      write ( OUP, 'I':3,'X(I)':14,'W1(I)':14,'W2(I)':14);
      writeln ( OUP );
   end;
   begin
      INPUT;
      if ( OK ) then
         begin
{           STEP 1                                                     }
            OUTPUT;
            H := ( B - A ) / N;
            K := 1;
{           TK already computed.                                       }
            OK := false;
{           STEP 2                                                     }
            while ( ( K <= NN ) and ( not OK ) ) do
               begin
{                 STEP 3                                               }
                  W1[1] := ALPHA;
                  W2[1] := TK;
                  U1 := 0.0 ;
                  U2 := 1.0;
{                 STEP 4
                  Runge-Kutta method for systems is used in STEPS 5, 6 }
                  for I := 1 to N DO
                     begin
{                       STEP 5                                         }
                        X := A + ( I - 1.0 ) * H;
                        T := X + 0.5 * H;
{                       STEP 6                                         }
                        K11 := H * W2[I];
                        K12 := H * F( X, W1[I], W2[I] );
                        K21 := H * ( W2[I] + 0.5 * K12 );
                        K22 := H * F( T, W1[I] + 0.5 * K11, W2[I] +
                               0.5 * K12);
                        K31 := H * ( W2[I] + 0.5 * K22 );
                        K32 := H * F( T, W1[I] + 0.5 * K21, W2[I] +
                               0.5 * K22 );
                        K41 := H * ( W2[I] + K32 );
                        K42 := H * F( X + H, W1[I] + K31, W2[I] + K32 );
                        W1[I+1] := W1[I] + ( K11 + 2.0 * ( K21 + K31 ) +
                                   K41 ) / 6.0;
                        W2[I+1] := W2[I] + ( K12 + 2.0 * ( K22 + K32 ) +
                                   K42 ) / 6.0;
                        K11 := H * U2;
                        K12 := H * ( FY( X, W1[I], W2[I] ) * U1 +
                               FYP( X, W1[I], W2[I] ) * U2 );
                        K21 := H * ( U2 + 0.5 * K12 );
                        K22 := H*(FY(T,W1[I],W2[I])*
                               (U1+0.5*K11)+FYP(T,W1[I],W2[I])*
                               (U2+0.5*K21));
                        K31 := H * ( U2 + 0.5 * K22 );
                        K32 := H * ( FY( T, W1[I], W2[I] ) *
                               (U1+0.5*K21)+FYP(T,W1[I],W2[I])*
                               (U2+0.5*K22));
                        K41 := H * ( U2 + K32 );
                        K42 := H*(FY(X+H,W1[I],W2[I])*(U1+K31)+
                               FYP(X+H,W1[I],W2[I])*(U2+K32));
                        U1 := U1+(K11+2.0*(K21+K31)+K41)/6.0;
                        U2 := U2+(K12+2.0*(K22+K32)+K42)/6.0
                     end;
{                 STEP 7
                  test for accuracy                                    }
                  if ( abs( W1[N+1] - BETA ) < TOL ) then
                     begin
{                       STEP 8                                         }
                        I := 0;
                        writeln(OUP,I:3,A:14:8,ALPHA:14:8,TK:14:8);
                        for I := 1 to N do
                           begin
                              J := I + 1;
                              X := A + I * H;
                              writeln (OUP,I:3,X:14:8,W1[J]:14:8,W2[J]:14:8)
                           end;
                        writeln (OUP, 'Convergence in ',K, ' iterations');
                        writeln(OUP,' t = ',TK:14);
{                       STEP 9                                         }
                        OK := true;
                     end
                  else
                     begin
{                       STEP 10
                        Newton's method applied to improve TK          }
                        TK := TK - ( W1[N+1] - BETA ) / U1;
                        K := K + 1
                     end
               end;
{           STEP 11
            method failed                                              }
            if ( not OK ) then
               writeln (OUP, 'Method failed after ', NN, ' iterations')
         end;
      close( OUP )
   end.

