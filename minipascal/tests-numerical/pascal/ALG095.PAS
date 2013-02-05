program ALG095;
{  HOUSEHOLDER'S ALGORITHM 9.5

   To obtain a symmetric tridiagonal matrix A(n-1) similar
   to the symmetric matrix A = A(1), construct the following
   matrices A(2),A(3),...,A(n-1) where A(K) = A(I,J)**K, for
   each K = 1,2,...,n-1:

   INPUT:   Dimension n; matrix A.

   OUTPUT:  A(n-1) (At each step, A can be overwritten.)
}
const
   ZERO = 1.0E-20;
var
   A : array [ 1..10, 1..10 ] of real;
   V,U,Z : array [ 1..10 ] of real;
   S,Q,RSQ,PROD : real;
   FLAG,N,I,J,K,KK,L : integer;
   OK : boolean;
   AA : char;
   NAME : string [ 30 ];
   INP,OUP : text;
procedure INPUT;
   begin
      writeln('This is the Householder Method.');
      OK := false;
      writeln ('The symmetric array A will be input from a text file ');
      writeln ('in the order: ');
      writeln ('              A(1,1), A(1,2), A(1,3), ..., A(1,n), ');
      writeln ('                      A(2,2), A(2,3), ..., A(2,n), ');
      writeln ('                              A(3,3), ..., A(3,n), ');
      writeln ('                                      ..., A(n,n)  ');
      writeln;
      write ('Place as many entries as desired on each line, but separate ');
      writeln ('entries with ');
      writeln ('at least one blank. ');
      writeln; writeln;
      writeln ('Has the input file been created? - enter Y or N. ');
      readln ( AA );
      if ( AA = 'Y' ) or ( AA = 'y' ) then
         begin
            writeln ('Input the file name in the form - drive:name.ext, ');
            writeln ('for example: A:DATA.DTA ');
            readln ( NAME );
            assign ( INP, NAME );
            reset ( INP );
            OK := false;
            while ( not OK ) do
               begin
                  writeln ('Input the dimension n. ');
                  readln ( N );
                  if ( N > 1 ) then
                     begin
                        for I := 1 to N do
                           for J := I to N do
                              begin
                                 read ( INP, A[I,J] );
                                 A[J,I] := A[I,J]
                              end;
                           OK := true
                     end
                  else writeln ('Dimension must be greater then 1. ')
               end
         end
      else
         begin
            write ('The program will end so that the input file can be ');
            writeln ('created. ')
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
      else  assign ( OUP, 'CON' );
      rewrite ( OUP );
      writeln(OUP,'HOUSEHOLDER METHOD');
      writeln(OUP);
      write ( OUP, 'The similar tridiagonal matrix follows ');
      writeln ( OUP, '- output by rows ');
      writeln ( OUP );
      for I := 1 to N do
         begin
            for J := 1 to N do write ( OUP, A[I,J]:12:8);
            writeln ( OUP );
            writeln ( OUP );
         end;
      close ( OUP )
   end;
   begin
      INPUT;
      if ( OK ) then
         begin
{           STEP 1                                                     }
            for K := 1 to N - 2 do
               begin
                  Q := 0.0;
                  KK := K + 1;
{                 STEP 2                                               }
                  for I := KK to N do Q := Q + A[I,K] * A[I,K];
{                 STEP 3                                               }
{                 S is used in place of ALPHA                          }
                  if ( abs(A[K+1,K]) <= ZERO ) then
                     S := sqrt( Q )
                  else
                     S := A[K + 1,K] / abs( A[K + 1,K] ) * sqrt( Q );
{                 STEP 4                                               }
                  RSQ := ( S + A[K + 1,K ] ) * S ;
{                 STEP 5                                               }
                  V[K] := 0.0;
                  V[K+1] := A[K+1,K]+S;
                  for J := K+2 to N do V[J] := A[J,K];
{                 STEP 6                                               }
                  for J := K to N do
                     begin
                        U[J] := 0.0;
                        for I := KK to N do U[J] := U[J] + A[J,I]*V[I];
                        U[J] := U[J] / RSQ
                     end;
{                 STEP 7                                               }
                  PROD := 0.0;
                  for I := K+1 to N do PROD := PROD + V[I]*U[I];
{                 STEP 8                                               }
                  for J := K to N do Z[J] := U[J] - 0.5*PROD*V[J]/RSQ;
{                 STEP 9                                               }
                  for L := K+1 to N-1 do
                     begin
{                       STEP 10                                        }
                        for J := L+1 to N do
                           begin
                              A[J,L] := A[J,L]-V[L]*Z[J]-V[J]*Z[L];
                              A[L,J] := A[J,L]
                           end;
{                       STEP 11                                        }
                        A[L,L] := A[L,L] - 2.0*V[L]*Z[L]
                     end;
{                 STEP 12                                              }
                  A[N,N] := A[N,N]-2.0*V[N]*Z[N];
{                 STEP 13                                              }
                  for J := K+2 to N do
                     begin
                        A[K,J] := 0.0;
                        A[J,K] := 0.0
                     end;
{                 STEP 14                                              }
                  A[K+1,K] := A[K+1,K]-V[K+1]*Z[K];
                  A[K,K+1] := A[K+1,K]
               end;
{              STEP 15                                                 }
               OUTPUT
         end
   end.