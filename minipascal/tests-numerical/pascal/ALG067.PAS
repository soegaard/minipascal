program ALG067;
{  CROUT REDUCTION FOR TRIDIAGONAL LINEAR SYSTEMS ALGORITHM 6.7

   To solve the n x n linear system

   E1:  A[1,1] X[1] + A[1,2] X[2]                  = A[1,n+1]
   E2:  A[2,1] X[1] + A[2,2] X[2] + A[2,3] X[3]    = A[2,n+1]
   :
   .
   E(n):          A[n,n-1] X[n-1] + A[n,n] X[n]    = A[n,n+1]

   INPUT:   the dimension n; the entries of A.

   OUTPUT:  the solution X(1), ..., X(N).
                                                                       }
var
   A,B,C,BB,Z,X : array [ 1..10 ] of real;
   FLAG,N,I,J,NN,II : integer;
   OK : boolean;
   AA : char;
   NAME : string [ 30 ];
   INP,OUP : text;
procedure INPUT;
   begin
      writeln('This is Crout Method for tridiagonal linear systems.');
      writeln ('The array will be input from a text file in the order: ');
      write ('all diagonal entries, all lower sub-diagonal entries, all ');
      writeln ('upper sub-diagonal ');
      writeln ('entries, inhomogeneous term. '); writeln;
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
{                       A(I,I) is stored in A(I), 1 <= I <= n          }
                        for I := 1 to N do read ( INP, A[I] );
{                       the lower sub-diagonal A(I,I-1) is stored
                        in B(I), 2 <= I <= n                           }
                        for I := 2 to N do read ( INP, B[I] );
{                       the upper sub-diagonal A(I,I+1) is stored
                        in C(I), 1 <= I <= n-1                         }
                        NN := N - 1;
                        for I := 1 to NN do read ( INP, C[I] );
{                       A(I,N+1) is stored in BB(I), 1 <= I <= n       }
                        for I := 1 to N do read ( INP, BB[I] );
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
      writeln(OUP,'CROUT METHOD FOR TRIDIAGONAL LINEAR SYSTEMS');
      writeln(OUP);
      writeln ( OUP, 'The solution is ');
      for I := 1 to N do write ( OUP, '':2, X[I]:12:8 );
      writeln(OUP);
      close ( OUP )
   end;
   begin
      INPUT;
      if ( OK ) then
         begin
{           STEPS 1-3 set up and solve LZ = B                           }
{           STEP 1                                                     }
{           the entries of U overwrite C and
            the entries of L overwrite A                               }
            C[1] := C[1] / A[1];
            Z[1] := BB[1] / A[1];
{           STEP 2                                                     }
            for I := 2 to NN do
               begin
                  A[I] := A[I] - B[I] * C[I-1];
                  C[I] := C[I] / A[I];
                  Z[I] := (BB[I]-B[I]*Z[I-1])/A[I]
               end;
{           STEP 3                                                     }
            A[N] := A[N] - B[N] * C[N-1];
            Z[N] := (BB[N]-B[N]*Z[N-1])/A[N];
{           STEP 4                                                     }
{           STEPS 4, 5 solve UX = Z                                    }
            X[N] := Z[N];
{           STEP 5                                                     }
            for II := 1 to NN do
               begin
                  I := NN - II + 1;
                  X[I] := Z[I] - C[I] * X[I+1]
               end;
{           STEP 6                                                     }
            OUTPUT
         end
   end.

