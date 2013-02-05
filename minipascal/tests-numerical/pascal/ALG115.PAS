program ALG115;
{
   PIECEWISE LINEAR RAYLEIGH-RITZ ALGORITHM 11.5

   To approximate the solution of the boundary-value problem

        -D(P(X)Y')/DX + Q(X)Y = F(X), 0 <= X <= 1,
               Y(0) = Y(1) = 0,

   with a piecewise linear function:

   INPUT:   integer N; mesh points X(0) = 0 < X(1) < ...
            < X(N) < X(N+1) = 1

   OUTPUT:  coefficients C(1),...,C(N) of the basis functions
}
var
   X : array [ 0..26 ] of real;
   H : array [ 0..25 ] of real;
   A,B,ALPHA,BETA,ZETA,Z,C : array [ 1..25 ] of real;
   Q : array [ 1..6, 1..26 ] of real;
   HQ,HC : real;
   N1,J1,FN,N,J,FLAG : integer;
   OK : boolean;
   AA : char;
   NAME : string [ 30 ];
   INP,OUP : text;
{  Change functions P, QQ and F for a new problem                      }
function P( X : real ) : real;
   begin
      P := 1.0
   end;
function QQ( X : real ) : real;
   begin
      QQ := PI * PI
   end;
function F( X : real ) : real;
   begin
      F := 2.0 * PI * PI * sin( PI * X )
   end;
procedure INPUT;
   begin
      writeln('This is the Piecewise Linear Rayleigh-Ritz Method.');
      writeln ('This program requires functions P, QQ, F to be created and ');
      writeln ('X(0), ..., X(N+1) to be supplied. ');
      writeln ('Are the preparations complete? Answer Y or N. ');
      readln ( AA );
      if ( ( AA = 'Y' ) or ( AA = 'y' ) ) then
         begin
            OK := false;
            while ( not OK ) do
               begin
                  writeln('Input integer N where X(0) = 0, X(N+1) = 1.');
                  readln ( N );
                  if ( N < 2 ) then writeln ('N must be greater than 1. ')
                  else OK := true
               end;
            X[0] := 0.0;
            X[N+1] := 1.0;
            writeln ('Choice of method to input X(1), ..., X(N): ');
            writeln ('1.  Input from keyboard at the prompt ');
            writeln ('2.  Equally spaced nodes to be calculated ');
            writeln ('3.  Input from text file ');
            writeln ('Please enter 1, 2, or 3. ');
            readln ( FLAG );
            if ( FLAG = 2 ) then
               begin
                  HC := 1.0 / ( N + 1.0 );
                  for J := 1 to N do
                     begin
                        X[J] := J * HC;
                        H[J-1] := HC
                     end;
                  H[N] := HC
               end
            else
               begin
                  if ( FLAG = 3 ) then
                     begin
                        writeln ('Has the input file been created?');
                        writeln (' - enter Y or N. ');
                        readln ( AA );
                        if ( AA = 'Y' ) or ( AA = 'y' ) then
                           begin
                              write ('Input the file name in the form -');
                              writeln(' drive:name.ext, ');
                              writeln ('for example: A:DATA.DTA ');
                              readln ( NAME );
                              assign ( INP, NAME );
                              reset ( INP );
                              for J := 1 to N do read ( INP, X[J] );
                              for J := 0 to N do H[J] := X[J+1] - X[J];
                              close ( INP )
                           end
                        else
                           begin
                              write ('The program will end so');
                              writeln(' the input file can be created. ');
                              OK := false
                           end
                     end
                  else
                     begin
                        for J := 1 to N do
                           begin
                              writeln ('Input X(',J,'). ');
                              readln ( X[J] );
                              H[J-1] := X[J] - X[J-1]
                           end;
                        H[N] := X[N+1] - X[N]
                     end
               end
         end
      else
         begin
            writeln ('The program will end so that the functions ');
            writeln ('can be created. ');
            OK := false
         end
   end;
function SIMPSON( FN : integer; A,B : real ) : real;
   var
      Z : array [ 0..4 ] of real;
      Y,H : real;
      I : integer;
   begin
      H := ( B - A ) / 4.0;
      for I := 0 to 4 do
         begin
            Y := A + I * H;
            case FN of
               1: Z[I] := ( 4.0 - I ) * I * sqr( H ) * QQ( Y );
               2: Z[I] := sqr( I * H ) * QQ( Y );
               3: Z[I] := sqr( H * ( 4.0 - I ) ) * QQ( Y );
               4: Z[I] := P( Y );
               5: Z[I] := I * H * F( Y );
               6: Z[I] := ( 4.0 - I ) * H * F( Y )
            end
         end;
      SIMPSON := ( Z[0] + Z[4] + 2.0 * Z[2] + 4.0 * ( Z[1] + Z[3] ) ) *
                 H / 3.0
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
      writeln(OUP,'PIECEWISE LINEAR RAYLEIGH-RITZ METHOD');
      writeln(OUP);
      writeln ( OUP,'I':3,'X(I-1)':12,'X(I)':12,'X(I+1)':12,'C(I)':14);
      writeln ( OUP );
      for J := 1 to N do
         writeln(OUP,J:3,X[J-1]:12:8,X[J]:12:8,X[J+1]:12:8,C[J]:14:8);
      close ( OUP )
   end;
   begin
      INPUT;
{     Step 1 is done within the input procedure                        }
      if ( OK ) then
         begin
            N1 := N - 1;
{           STEP 3                                                     }
            for J := 1 to N1 do
               begin
                  Q[1,J] := SIMPSON( 1, X[J], X[J+1] ) / sqr( H[J] );
                  Q[2,J] := SIMPSON( 2, X[J-1], X[J] ) / sqr( H[J-1] );
                  Q[3,J] := SIMPSON( 3, X[J], X[J+1] ) / sqr( H[J] );
                  Q[4,J] := SIMPSON( 4, X[J-1], X[J] ) / sqr( H[J-1] );
                  Q[5,J] := SIMPSON( 5, X[J-1], X[J] ) / H[J-1] ;
                  Q[6,J] := SIMPSON( 6, X[J], X[J+1] ) / H[J]
               end;
            Q[2,N] := SIMPSON( 2, X[N-1], X[N] ) / sqr( H[N-1] );
            Q[3,N] := SIMPSON( 3, X[N], X[N+1] ) / sqr( H[N] );
            Q[4,N] := SIMPSON( 4, X[N-1], X[N] ) / sqr( H[N-1] );
            Q[4,N+1] := SIMPSON( 4, X[N], X[N+1] ) / sqr( H[N] );
            Q[5,N] := SIMPSON( 5, X[N-1], X[N] ) / H[N-1] ;
            Q[6,N] := SIMPSON( 6, X[N], X[N+1] ) / H[N] ;
{           STEP 4                                                     }
            for J := 1 to N1 do
               begin
                  ALPHA[J] := Q[4,J]+Q[4,J+1]+Q[2,J]+Q[3,J];
                  BETA[J] := Q[1,J]-Q[4,J+1];
                  B[J] := Q[5,J]+Q[6,J]
               end;
{           STEP 5                                                     }
            ALPHA[N] := Q[4,N]+Q[4,N+1]+Q[2,N]+Q[3,N];
            B[N] := Q[5,N]+Q[6,N];
{           STEP 6                                                     }
{           STEPS 6-10 solve a symmetric tridiagonal linear system
            using Algorithm 6.7.                                       }
            A[1] := ALPHA[1];
            ZETA[1] := BETA[1] / ALPHA[1];
            Z[1] := B[1] / A[1];
{           STEP 7                                                     }
            for J := 2 to N1 do
               begin
                  A[J] := ALPHA[J] - BETA[J-1] * ZETA[J-1];
                  ZETA[J] := BETA[J] / A[J];
                  Z[J] := ( B[J] - BETA[J-1] * Z[J-1] ) / A[J];
               end;
{           STEP 8                                                     }
            A[N] := ALPHA[N] - BETA[N-1] * ZETA[N-1];
            Z[N] := ( B[N] - BETA[N-1] * Z[N-1] ) / A[N];
{           STEP 9                                                     }
            C[N] := Z[N];
{           STEP 10                                                    }
            for J := 1 to N1 do
               begin
                  J1 := N - J;
                  C[J1] := Z[J1] - ZETA[J1] * C[J1+1]
               end;
            OUTPUT
         end
{        STEP 11                                                       }
   end.



