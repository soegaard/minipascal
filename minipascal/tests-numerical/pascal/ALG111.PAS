program ALG111;
{
   LINEAR SHOOTING ALGORITHM 11.1

   To approximate the solution of the boundary-value problem

   -Y'' + P(X)Y' + Q(X)Y + R(X) = 0, A<=X<=B, Y(A)=ALPHA, Y(B)=BETA:


   INPUT: Endpoints A,B; boundary conditions ALPHA, BETA; number of
          subintervals N.

   OUTPUT: Approximations W(1,I) to Y(X(I)); W(2,I) to Y'(X(I))
           for each I=0,1,...,N.

}
var
   U,V : array [ 1..2, 1..25 ] of real;
   T,A,B,ALPHA,BETA,X,H,U1,U2,V1,V2,W1,W2,Z : real;
   K11,K12,K21,K22,K31,K32,K41,K42 : real;
   N,FLAG,I : integer;
   OK : boolean;
   AA : char;
   NAME : string [ 30 ];
   OUP : text;
{  Change functions P, Q, R for a new problem                          }
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
      writeln('This is the Linear Shooting Method.');
      OK := false;
      writeln ('Have the functions P, Q, and R been created immediately ');
      writeln ('preceding the INPUT procedure? ');
      writeln ('Answer Y or N. ');
      readln ( AA );
      if ( (AA = 'Y') or (AA = 'y') ) then
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
            OK := false;
            while ( not OK ) do
               begin
                  write ('Input a positive integer for the number of ');
                  writeln ('subintervals. ');
                  readln ( N );
                  if ( N <= 0 ) then
                     writeln ('Number must be a positive integer. ')
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
      writeln(OUP,'LINEAR SHOOTING METHOD');
      writeln(OUP);
      write ( OUP, 'I':3,'X(I)':12,'W(1,I)':12,'W(2,I)':12);
      writeln ( OUP );
   end;
   begin
      INPUT;
      if ( OK ) then
         begin
            OUTPUT;
{           STEP 1                                                     }
            H := ( B - A ) / N;
            U1 := ALPHA;
            U2 := 0.0;
            V1 := 0.0;
            V2 := 1.0;
{           STEP 2                                                     }
            for I := 1 to N do
               begin
{                 STEP 3                                               }
                  X := A + ( I - 1.0 ) * H;
                  T := X + 0.5 * H;
{                 STEP 4                                               }
                  K11 := H * U2;
                  K12 := H * ( P( X ) * U2 + Q( X ) * U1 + R( X ) );
                  K21 := H * ( U2 + 0.5 * K12 );
                  K22 := H * ( P( T ) * ( U2 + 0.5 * K12 ) + Q( T ) *
                         ( U1 + 0.5 * K11 ) + R( T ) );
                  K31 := H * ( U2 + 0.5 * K22 );
                  K32 := H * ( P( T ) * ( U2 + 0.5 * K22 ) + Q( T ) *
                         ( U1 + 0.5 * K21 ) + R( T ) );
                  T := X + H;
                  K41 := H * ( U2 + K32 );
                  K42 := H * ( P( T ) * ( U2 + K32 ) + Q(T) * ( U1 + K31 ) +
                         R( T ) );
                  U1 := U1 + ( K11 + 2.0 * ( K21 + K31 ) + K41 ) / 6.0;
                  U2 := U2 + ( K12 + 2.0 * ( K22 + K32 ) + K42 ) / 6.0;
                  K11 := H * V2;
                  K12 := H * ( P( X ) * V2 + Q( X ) * V1 );
                  T := X + 0.5 * H;
                  K21 := H * ( V2 + 0.5 * K12 );
                  K22 := H * ( P( T ) * ( V2 + 0.5 * K12 ) + Q( T ) *
                         ( V1 + 0.5 * K11 ) );
                  K31 := H * ( V2 + 0.5 * K22 );
                  K32 := H * ( P( T ) * ( V2 + 0.5 * K22 ) + Q( T ) *
                         ( V1 + 0.5 * K21 ) );
                  T := X + H;
                  K41 := H * ( V2 + K32 );
                  K42 := H * ( P( T ) * ( V2 + K32 ) + Q(T) * ( V1 + K31 ));
                  V1 := V1 + ( K11 + 2.0 * ( K21 + K31 ) + K41 ) / 6.0;
                  V2 := V2 + ( K12 + 2.0 * ( K22 + K32 ) + K42 ) / 6.0;
                  U[1,I] := U1;
                  U[2,I] := U2;
                  V[1,I] := V1;
                  V[2,I] := V2
               end;
{           STEP 5                                                     }
            W1 := ALPHA;
            Z := ( BETA - U[1,N] ) / V[1,N];
            X := A;
            I := 0;
            writeln (OUP,I:3,X:12:8,W1:12:8,Z:12:8);
{           STEP 6                                                     }
            for I := 1 to N do
                begin
                   X := A + I * H;
                   W1 := U[1,I] + Z * V[1,I];
                   W2 := U[2,I] + Z * V[2,I];
                   writeln (OUP,I:3,X:12:8,W1:12:8,W2:12:8);
                end;
           close (oup)
         end
{   STEP 7                                                             }
   end.
