program ALG064;
{  DIRECT FACTORIZATION ALGORITHM 6.4

   To factor the n by n matrix A = (A(I,J)) into the product of the
   lower triangular matrix L = (L(I,J)) and U = (U(I,J)), that is
   A = LU, where the main diagonal of either L or U consists of
   all ones:

   INPUT:   dimension n; the entries A(I,J), 1<=I, J<=n, of A;
            the diagonal L(1,1), ..., L(N,N) of L or the diagonal
            U(1,1), ..., U(N,N) of U.

   OUTPUT:  the entries L(I,J), 1<=J<=I, 1<=I<=n of L and the entries
            U(I,J), I<=J<=n, 1<=I<=n of U.
                                                                       }
const
   ZERO = 1.0E-20;
var
   A : array [ 1..10, 1..10 ] of real;
   XL : array [ 1..10 ] of real;
   S,SS : real;
   FLAG,N,M,I,J,ISW,JJ,K,KK : integer;
   OK : boolean;
   AA : char;
   NAME : string [ 30 ];
   INP,OUP : text;
procedure INPUT;
   begin
      writeln('This is the general LU factorization method.');
      writeln ('The array will be input from a text file in the order: ');
      writeln('A(1,1), A(1,2), ..., A(1,N), A(2,1), A(2,2), ..., A(2,N),');
      writeln ('..., A(N,1), A(N,2), ..., A(N,N) '); writeln;
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
                  writeln ('Input the dimension n - an integer. ');
                  readln ( N );
                  if ( N > 0 ) then
                     begin
                        for I := 1 to N do
                           for J := 1 to N do read ( INP, A[I,J] );
                        OK := true;
                        close ( INP )
                     end
                  else writeln ('The number must be a positive integer. ')
               end;
            writeln ('Choice of diagonals: ');
            writeln ('1. Diagonal of L consists of ones ');
            writeln ('2. Diagonal of U consists of ones ');
            writeln ('Please enter 1 or 2. ');
            readln ( FLAG );
            if ( FLAG = 1 ) then ISW := 0
            else ISW := 1
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
      writeln(OUP,'GENERAL LU FACTORIZATION');
      writeln(OUP);
      if ( ISW = 0 ) then
         writeln ( OUP,'The diagonal of L consists of all entries = 1.0 ')
      else
         writeln ( OUP,' The diagonal of U consists of all entries = 1.0 ');
      writeln ( OUP );
      write ( OUP, 'Entries of L below/on diagonal and entries of U above');
      writeln ( OUP, '/on diagonal ');
      writeln ( OUP, '- output by rows in overwrite format: ');
      for I := 1 to N do
         begin
            for J := 1 to N do write ( OUP, A[I,J]:12:8 );
            writeln ( OUP )
         end;
      close ( OUP )
   end;
   begin
      INPUT;
      if ( OK ) then
         begin
            for I := 1 to N do XL[I] := 1.0;
{           STEP 1                                                     }
            if ( abs( A[1,1] ) <= ZERO ) then OK := false
            else
               begin
{                 the entries of L below the main diagonal will be
                  placed in the corresponding entries of A; the
                  entries of U above the main diagonal will be
                  placed in the corresponding entries of A; the
                  main diagonal which was NOT input will become
                  the main diagonal of A; the input main diagonal
                  of L or U is, of course, placed in XL.               }
                  A[1,1] := A[1,1] / XL[1];
{                 STEP 2                                               }
                  for J := 2 to N do
                     begin
                        if ( ISW = 0 ) then
                           begin
{                             first row of U                           }
                              A[1,J] := A[1,J] / XL[1];
{                             first column of L                        }
                              A[J,1] := A[J,1] / A[1,1]
                           end
                        else
                           begin
{                             first row of U                           }
                              A[1,J] := A[1,J] / A[1,1];
{                             first column of L                        }
                              A[J,1] := A[J,1] / XL[1]
                           end
                     end;
{                 STEP 3                                               }
                  M := N - 1;
                  I := 2;
                  while ( I <= M ) and ( OK ) do
                     begin
{                       STEP 4                                         }
                        KK := I - 1;
                        S := 0.0;
                        for K := 1 to KK do S := S - A[I,K] * A[K,I];
                        A[I,I] := ( A[I,I] + S ) / XL[I];
                        if ( abs( A[I,I] ) <= ZERO ) then OK := false
                        else
                           begin
{                             STEP 5                                   }
                              JJ := I + 1;
                              for J := JJ to N do
                                 begin
                                    SS := 0.0;
                                    S := 0.0;
                                    for K := 1 to KK do
                                       begin
                                          SS := SS - A[I,K] * A[K,J];
                                          S := S - A[J,K] * A[K,I]
                                       end;
                                    if ( ISW = 0 ) then
                                       begin
{                                         Ith row of U                 }
                                          A[I,J] := ( A[I,J] + SS ) / XL[I];
{                                         Ith column of L              }
                                          A[J,I] := ( A[J,I] + S ) / A[I,I]
                                       end
                                    else
                                       begin
{                                         Ith row of U                 }
                                          A[I,J] := ( A[I,J] + SS ) / A[I,I];
{                                         Ith column of L              }
                                          A[J,I] := ( A[J,I] + S ) / XL[I]
                                       end
                                 end
                           end;
                        I := I + 1
                     end;
                  if ( OK ) then
                     begin
{                       STEP 6                                         }
                        S := 0.0;
                        for K := 1 to M do S := S - A[N,K] * A[K,N];
                        A[N,N] := ( A[N,N] + S ) / XL[N];
{                       If A(N,N) = 0 then A = LU but the matrix is singular.
                        Process is complete, all entries of A have been
                        determined.
                        STEP 7                                         }
                        OUTPUT
                     end
               end;
            if ( not OK ) then writeln ('Factorization impossible ')
         end
   end.

