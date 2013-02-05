program ALG096;
{  QR ALGORITHM 9.6

   To obtain the eigenvalues of a symmetric, tridiagonal n by n matrix

            a(1)   b(2)
            b(2)   a(2)   b(3)
               .      .      .
                 .      .      .
                   .      .      .
                   b(n-1)  a(n-1)  b(n)
                              b(n)   a(n)


   INPUT:   n; A(1),...,A(n) (diagonal of A); B(2),...,B(n)
            (off-diagonal of A); maximum number of iterations M, tolerance TOL.

   OUTPUT:  Eigenvalues of A or recommended splitting of A, or a message that
            the maximum number of iterations was exceeded.
}
var
   A,B,C,D,Q,S,X : array [ 1..10 ] of real;
   R,Y,Z : array [ 1..9 ] of real;
   TOL,B1,C1,D1,X1,X2,SHIFT : real;
   FLAG,N,I,J,K,M,MM,L : integer;
   OK,DONE : boolean;
   AA : char;
   NAME : string [ 30 ];
   INP,OUP : text;
procedure INPUT;
   begin
      writeln('This is the QR Method.');
      OK := false;
      write ('The tridiagonal symmetric array A will be input from ');
      writeln ('a text file in the order: ');
      writeln ('    (diagonal): A(1), A(2), ..., A(n), ');
      writeln ('    (subdiagonal): B(2), B(3), ..., B(n). ');
      writeln;
      write ('Place as many entries as desired on each line, but separate ');
      writeln ('entries with ');
      writeln ('at least one blank.');
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
                        for I := 1 to N do read ( INP, A[I] );
                        for I := 2 to N do read ( INP, B[I] );
                        OK := true
                     end
                  else writeln ('Dimension must be greater then 1. ')
               end;
            OK := false;
            while ( not OK ) do
               begin
                  writeln ('Input the tolerance. ');
                  readln ( TOL );
                  if ( TOL > 0.0 ) then OK := true
                  else writeln ('Tolerance must be a positive real number. ')
               end;
            OK := false;
            while ( not OK ) do
               begin
                  writeln ('Input the maximum number of iterations. ');
                  readln ( L );
                  if ( L > 0 ) then OK := true
                  else writeln ('Number must be a positive integer. ')
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
      writeln(OUP,'QR METHOD');
      writeln(OUP)
   end;
begin
   INPUT;
   if ( OK ) then
      begin
         OUTPUT;
{        STEP 1                                                        }
         SHIFT := 0.0;
         K := 1;
{        STEP 2                                                        }
         while ( ( K <= L ) and OK ) do
            begin
               writeln ( OUP, 'Iteration number ',K,'    N = ',N );
               writeln ( OUP, 'The array A is now as follows: ');
               writeln ( OUP, 'Diagonal: ');
               for I := 1 to N do write ( OUP, A[I]:12:8 );
               writeln ( OUP );
               writeln ( OUP, 'Off diagonal: ');
               for I := 2 to N do write ( OUP, B[I]:12:8 );
               writeln ( OUP );
{              Steps 3-7 test for success.                             }
{              STEP 3                                                  }
               if ( abs( B[N] ) <= TOL ) then
                  begin
                     A[N] := A[N] + SHIFT;
                     writeln ( OUP, 'Eigenvalue = ', A[N]:12:8 );
                        N := N - 1
                  end;
{              STEP 4                                                  }
               if ( abs( B[2] ) <= TOL ) then
                  begin
                     A[1] := A[1] + SHIFT;
                     writeln( OUP, 'Eigenvalue = ', A[1]:12:8 );
                     N := N - 1;
                     A[1] := A[2];
                     for J := 2 to N do
                        begin
                           A[J] := A[J+1];
                           B[J] := B[J+1]
                        end
                  end;
{              STEP 5                                                  }
               if N = 0 then OK := false;
{              STEP 6                                                  }
               if N = 1 then
                  begin
                     A[1] := A[1] + SHIFT;
                     writeln(OUP,'Eigenvalue = ',A[1]:12:8);
                     OK := false;
                  end;
{              STEP 7                                                  }
               if OK then
                  begin
                     M := N - 1;
                     if ( M >= 2 ) then
                        begin
                           for I := 2 to M do
                              begin
                                if ( abs( B[I] ) <= TOL ) then
                                   begin
                                      J := I;
                                      OK := false
                                   end
                              end;
                           if ( not OK ) then
                              begin
                              writeln(OUP,'Split the matrix into');
                              writeln(OUP,'Diagonal');
                              for I := 1 to J-1 do write(OUP,A[I]:12:8);
                              writeln(OUP);
                              writeln(OUP,'Off-diagonal');
                              for I := 2 to J-1 do write(OUP,B[I]:12:8);
                              writeln(OUP);
                              writeln(OUP,'and');
                              writeln(OUP,'Diagonal');
                              for I := J to N do write(OUP,A[I]:12:8);
                              writeln(OUP);
                              writeln(OUP,'Off-diagonal');
                              for I := J+1 to N do write(OUP,B[I]:12:8);
                              writeln(OUP);
                              end
                        end;
                  end;
               if ( OK ) then
                  begin
{                    STEP 8                                            }
{                    compute shift                                     }
                     B1 := -( A[N] + A[N - 1] );
                     C1 := A[N] * A[N - 1] - B[N] * B[N];
                     D1 := B1 * B1 - 4.0 * C1;
                     if ( D1 < 0.0 ) then
                        begin
                           write ( OUP, 'Problem with complex roots, D1 ');
                           writeln ( OUP, '= ', D1 );
                           OK := false
                        end
                     else
                        begin
                           D1 := sqrt( D1 );
{                          STEP 9                                      }
                           if ( B1 > 0.0 ) then
                              begin
                                 X1 := -2.0 * C1 / ( B1 + D1 );
                                 X2 := -( B1 + D1 ) / 2.0
                              end
                           else
                              begin
                                 X1 := ( D1 - B1 ) / 2.0;
                                 X2 := 2.0 * C1 / ( D1 - B1 )
                              end;
{                          if N = 2 then the 2 eigenvalues have
                           been computed                               }
{                          STEP 10                                     }
                           if ( N = 2 ) then
                              begin
                                 X1 := X1 + SHIFT;
                                 X2 := X2 + SHIFT;
                                 write ( OUP, 'The last two eigenvalues ');
                                 writeln (OUP,'are: ',X1:12:8,X2:12:8);
                                 OK := false
                              end
                           else
                              begin
{                                STEP 11                               }
                                 if (abs(A[N]-X1) > abs(A[N]-X2))
                                    then X1 := X2;
{                                accumulate shift                      }
{                                Step 12                               }
                                 SHIFT := SHIFT + X1;
{                                STEP 13                               }
{                                perform shift                         }
                                 for I := 1 to N do D[I] := A[I] - X1;
{                                STEPS 14 and 15 compute R(K)          }
{                                STEP 14                               }
                                 X[1] := D[1];
                                 Y[1] := B[2];
{                                STEP 15                               }
                                 for J := 2 to N do
                                    begin
                                       Z[J-1] := sqrt(sqr(X[J-1])+
                                                      sqr(B[J]));
                                       C[J] := X[J-1]/Z[J-1];
                                       S[J] := B[J]/Z[J-1];
                                       Q[J-1] := C[J]*Y[J-1]+S[J]*D[J];
                                       X[J] := C[J]*D[J]-S[J]*Y[J-1];
                                       if ( J <> N ) then
                                          begin
                                             R[J-1] := S[J]*B[J+1];
                                             Y[J] := C[J]*B[J+1]
                                          end
                                    end;
                                 M := N - 1;
                                 MM := N - 2;
{                                STEP 16-18 COMPUTE A(K+1)             }
{                                STEP 16                               }
                                 Z[N] := X[N];
                                 A[1] := C[2]*Z[1]+S[2]*Q[1];
                                 B[2] := S[2]*Z[2];
                                 if ( N > 2 ) then
                                    begin
{                                      STEP 17                         }
                                       for J := 2 to M do
                                          begin
                                             A[J] := C[J+1]*C[J]*Z[J]
                                                     +S[J+1]*Q[J];
                                             B[J+1] := S[J+1]*Z[J+1]
                                          end
                                    end;
{                                STEP 18                               }
                                 A[N] := C[N] * Z[N]
                              end
                        end
                  end;
{              STEP 19                                                 }
               K := K+1
            end;
{        STEP 20                                                       }
         if ( ( OK ) and ( K > L ) ) then
            writeln ( OUP, 'Maximum Number of Iterations Exceeded.');
{        the process is complete                                       }
         close ( OUP )
      end
end.
