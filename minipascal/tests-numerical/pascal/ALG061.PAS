program ALG061;
{  GAUSSIAN ELIMINATION WITH BACKWARD SUBSTITUTION ALGOTITHM 6.1

   To solve the n by n linear system

   E1:  A[1,1] X[1] + A[1,2] X[2] +...+ A[1,n] X[n] = A[1,n+1]
   E2:  A[2,1] X[1] + A[2,2] X[2] +...+ A[2,n] X[n] = A[2,n+1]
   :
   .
   EN:  A[n,1] X[1] + A[n,2] X[2] +...+ A[n,n] X[n] = A[n,n+1]

   INPUT:   number of unknowns and equations n; augmented
            matrix A = (A(I,J)) where 1<=I<=n and 1<=J<=n+1.

   OUTPUT:  solution x(1), x(2),...,x(n) or a message that the
            linear system has no unique solution.
                                                                       }
const
   ZERO = 1.0E-20;
var
   A : array [ 1..12, 1..13 ] of real;
   X : array [ 1..12 ] of real;
   C,XM,SUM : real;
   FLAG,N,M,I,J,ICHG,NN,IP,JJ,K,L,KK : integer;
   OK : boolean;
   AA : char;
   NAME : string [ 30 ];
   INP,OUP : text;
procedure INPUT;
   begin
      writeln('This is Gaussian Elimination to solve a linear system.');
      writeln ('The array will be input from a text file in the order: ');
      writeln('A(1,1), A(1,2), ..., A(1,N+1), A(2,1), A(2,2), ..., A(2,N+1),');
      writeln ('..., A(N,1), A(N,2), ..., A(N,N+1) '); writeln;
      write ('Place as many entries as desired on each line, but separate ');
      writeln ('entries with ');
      writeln ('at least one blank. ');
      writeln; writeln;
      writeln ('Has the input file been created? - enter Y or N. ');
      readln ( AA );
      OK := false;
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
                  writeln ('Input the number of equations - an integer. ');
                  readln ( N );
                  if ( N > 0 ) then
                     begin
                        for I := 1 to N do
                           for J := 1 to N + 1 do read ( INP, A[I,J] );
                        OK := true;
                        close ( INP )
                     end
                  else writeln ('The number must be a positive integer. ')
               end
         end
      else writeln ('The program will end so the input file can be created. ')
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
      writeln(OUP,'GAUSSIAN ELIMINATION');
      writeln(OUP);
      writeln ( OUP, 'The reduced system - output by rows: ');
      for I := 1 to N do
         begin
            for J := 1 to M do write ( OUP, A[I,J]:12:8 );
            writeln ( OUP )
         end;
      writeln ( OUP ); writeln ( OUP );
      writeln ( OUP, 'Has solution vector: ');
      for I := 1 to N do write ( OUP, '':2, X[I]:12:8 );
      writeln ( OUP ); writeln ( OUP );
      writeln ( OUP, 'with ',ICHG,' row interchange(s) ');
      close ( OUP )
   end;
   begin
      INPUT;
      if ( OK ) then
         begin
{           STEP 1                                                     }
            NN := N - 1;
            M := N + 1;
            ICHG := 0;
            I := 1;
            while ( OK ) and ( I <= NN ) do
               begin
{                 STEP 2                                               }
{                 use IP instead of p                                  }
                  IP := I;
                  while ( abs( A[IP,I] ) <= ZERO ) and ( IP <= N ) do
                     IP := IP + 1;
                  if ( IP = M ) then OK := false
                  else
                     begin
{                       STEP 3                                         }
                        if ( IP <> I ) then
                           begin
                              for JJ := 1 to M do
                                 begin
                                    C := A[I,JJ];
                                    A[I,JJ] := A[IP,JJ];
                                    A[IP,JJ] := C
                                 end;
                              ICHG := ICHG + 1
                           end;
{                       STEP 4                                         }
                        JJ := I + 1;
                        for J := JJ to N do
                           begin
{                             STEP 5                                   }
{                             use XM in place of m(J,I)                }
                              XM := A[J,I] / A[I,I];
{                             STEP 6                                   }
                              for K := JJ to M do
                                 A[J,K] := A[J,K] - XM * A[I,K];
{                             multipliers XM could be saved in A[J,I]  }
                              A[J,I] := 0.0
                           end
                     end;
                  I := I + 1
               end;
            if ( OK ) then
               begin
{                 STEP 7                                               }
                  if ( abs( A[N,N] ) <= ZERO ) then OK := false
                  else
                     begin
{                       STEP 8                                         }
{                       start backward substitution                            }
                        X[N] := A[N,M] / A[N,N];
{                       STEP 9                                         }
                        for K := 1 to NN do
                           begin
                              I := NN - K + 1;
                              JJ := I + 1;
                              SUM := 0.0;
                              for KK := JJ to N do
                                 SUM := SUM - A[I,KK] * X[KK];
                              X[I] := (A[I,M] + SUM) / A[I,I]
                           end;
{                       STEP 10                                        }
{                       procedure completed successfully               }
                        OUTPUT
                     end
               end;
            if ( not OK ) then writeln ('System has no unique solution ')
         end
   end.

