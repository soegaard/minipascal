program ALG066;
{  CHOLESKI'S ALGORITHM 6.6

   To factor the positive definite n by n matrix A into LL**T,
   where L is lower triangular.

   INPUT:   the dimension n; entries A(I,J), 1<=I, J<=n of A.

   OUTPUT:  the entries L(I,J), 1<=J<=I, 1<=I<=n of L.

   the entries of U = L**T are U(I,J) = L(J,I), I<=J<=n, 1<=I<=n
}
var
   A : array [ 1..10, 1..10 ] of real;
   S : real;
   FLAG,N,I,J,K,NN,JJ,KK : integer;
   OK : boolean;
   AA : char;
   NAME : string [ 30 ];
   INP,OUP : text;
procedure INPUT;
   begin
      writeln('This is Choleski Factorization Method.');
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
                  writeln ('Input the dimension - an integer. ');
                  readln ( N );
                  if ( N > 0 ) then
                     begin
                        for I := 1 to N do
                           for J := 1 to N do read ( INP, A[I,J] );
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
      else  assign ( OUP, 'CON' );
      rewrite ( OUP );
      writeln(OUP,'CHOLESKI FACTORIZATION');
      writeln(OUP);
      writeln ( OUP, 'The matrix l output by rows: ');
      for I := 1 to N do
         begin
            for J := 1 to I do write ( OUP, '':2, A[I,J]:12:8 );
            writeln ( OUP )
         end;
      close ( OUP )
   end;
   begin
      INPUT;
      if ( OK ) then
         begin
{           STEP 1                                                     }
            A[1,1] := sqrt( A[1,1] );
{           STEP 2                                                     }
            for J := 2 to N do A[J,1] := A[J,1] / A[1,1];
{           STEP 3                                                     }
            NN := N - 1;
            for I := 2 to NN do
               begin
{                 STEP 4                                               }
                  KK := I - 1;
                  S := 0.0;
                  for K := 1 to KK do S := S - A[I,K] * A[I,K];
                  A[I,I] := sqrt( A[I,I] + S );
{                 STEP 5                                               }
                  JJ := I + 1;
                  for J := JJ to N do
                     begin
                        S := 0.0;
                        KK := I - 1;
                        for K := 1 to KK do S := S - A[J,K] * A[I,K];
                        A[J,I] := ( A[J,I] + S ) / A[I,I]
                     end
               end;
{           STEP 6                                                     }
            S := 0.0;
            for K := 1 to NN do S := S - A[N,K] * A[N,K];
            A[N,N] := sqrt( A[N,N] + S );
{           STEP 7                                                     }
            OUTPUT
         end
   end.

