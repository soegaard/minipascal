program ALG062;
{  GAUSSIAN ELIMINATION WITH PARTIAL PIVOTING ALGORITHM 6.2

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
   INP,OUP : text;
   A : array [ 1..10, 1..11 ] of real;
   X : array [ 1..10] of real;
   NROW : array [ 1..10 ] of integer;
   AMAX,XM,SUM : real;
   FLAG,N,M,ICHG,I,NN,IMAX,J,JJ,IP,JP,NCOPY,I1,J1,N1,K,N2,LL,KK : integer;
   OK : boolean;
   AA : char;
   NAME : string [ 30 ];
procedure INPUT;
   begin
      writeln('This is Gauss Elimination with Partial Pivoting.');
      writeln ('The array will be input from a text file in the order: ');
      writeln('A(1,1), A(1,2), ..., A(1,N+1), A(2,1), A(2,2), ..., A(2,N+1),');
      writeln ('..., A(N,1), A(N,2), ..., A(N,N+1) '); writeln;
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
      else
         begin
            write('The program will end so');
            writeln(' the input file can be created.');
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
      else  assign ( OUP, 'CON' );
      rewrite ( OUP );
      writeln(OUP,'GAUSSIAN ELIMINATION WITH PARTIAL PIVOTING');
      writeln(OUP);
      writeln ( OUP, 'The reduced system - output by rows: ');
      for I := 1 to N do
         begin
            for J := 1 to N do write ( OUP, A[I,J]:12:8 );
            writeln ( OUP )
         end;
      writeln ( OUP ); writeln ( OUP );
      writeln ( OUP, 'Has solution vector: ');
      for I := 1 to N do
         begin
            write ( OUP, '':2, X[I]:12:8 )
         end;
      writeln ( OUP );
      writeln ( OUP, 'with ',ICHG,' row interchange(s) ');
      writeln ( OUP );
      writeln ( OUP, 'The rows have been logically re-ordered to: ');
      for I := 1 to N do write ( OUP, NROW[I]:3 ); writeln(OUP);
      close ( OUP )
   end;
   begin
      INPUT;
      if ( OK ) then
         begin
            M := N + 1;
{           STEP 1                                                     }
            for I := 1 to N do NROW[I] := I;
{           initialize row pointer                                     }
            NN := N - 1;
            ICHG := 0;
            I := 1;
{           STEP 2                                                     }
            while ( OK ) and ( I <= NN ) do
               begin
{                 STEP 3                                               }
                  IMAX := NROW[I];
                  AMAX := abs( A[IMAX,I] );
                  IMAX := I;
                  JJ := I + 1;
                  for IP := JJ to N do
                     begin
                        JP := NROW[IP];
                        if ( abs( A[JP,I] ) > AMAX ) then
                           begin
                              AMAX := abs( A[JP,I] );
                              IMAX := IP
                           end
                     end;
{                    STEP 4                                            }
                     if ( AMAX <= ZERO ) then OK := false
                     else
                        begin
{                          STEP 5                                      }
{                          simulate row interchange                    }
                           if ( NROW[I] <> NROW[IMAX] ) then
                              begin
                                 ICHG := ICHG + 1;
                                 NCOPY := NROW[I];
                                 NROW[I] := NROW[IMAX];
                                 NROW[IMAX] := NCOPY
                              end;
                           I1 := NROW[I];
{                          STEP 6                                      }
                           for J := JJ to N do
                              begin
                                 J1 := NROW[J];
{                                STEP 7                                }
                                 XM := A[J1,I] / A[I1,I];
{                                STEP 8                                }
                                 for K := JJ to M do
                                    A[J1,K] := A[J1,K] - XM * A[I1,K];
{                                Multiplier XM could be saved in A[J1,I] }
                                 A[J1,I] := 0.0
                              end
                        end;
                   I := I + 1
               end;
            if ( OK ) then
               begin
{                 STEP 9                                               }
                  N1 := NROW[N];
                  if ( abs( A[N1,N] ) <= ZERO ) then OK := false
{                 system has no unique solution                        }
                  else
                     begin
{                       STEP 10                                        }
{                       start backward substitution                    }
                        X[N] := A[N1,M] / A[N1,N];
{                       STEP 11                                        }
                        for K := 1 to NN do
                           begin
                              I := NN - K + 1;
                              JJ := I + 1;
                              N2 := NROW[I];
                              SUM := 0.0;
                              for KK := JJ to N do
                                 begin
                                    SUM := SUM - A[N2,KK] * X[KK]
                                 end;
                              X[I] := (A[N2,M] + SUM) / A[N2,I]
                           end;
{                       STEP 12                                        }
{                       procedure completed successfully               }
                        OUTPUT
                     end
               end;
            if ( not OK ) then writeln ('System has no unique solution ')
         end
   end.

