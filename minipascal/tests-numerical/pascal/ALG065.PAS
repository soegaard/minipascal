program ALG065;
{  LDL-t ALGORITHM 6.5

   To factor the positive definite n by n matrix A into LDL**T,
   where L is a lower triangular matrix with ones along the diagonal
   and D is a diagonal matrix with positive entries on the
   diagonal.

   INPUT:   the dimension n; entries A(I,J), 1<=I, J<=n of A.

   OUTPUT:  the entries L(I,J), 1<=J<I, 1<=I<=N of L and D(I),
            1<=I<=n of D.
}
var
   A : array [ 1..10, 1..11 ] of real;
   D,V : array [ 1..10 ] of real;
   S : real;
   FLAG,N,I,J,K,NN,JJ,KK : integer;
   OK : boolean;
   AA : char;
   NAME : string [ 30 ];
   INP,OUP : text;
procedure INPUT;
   begin
      writeln('This is the LDL^t Method for Positive Definite Matrices.');
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
                        OK := true
                     end
                  else writeln ('The number must be a positive integer. ')
               end;
            close ( INP )
         end
      else
         begin
            write ('The program will end so');
            writeln(' the input file can be created. ');
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
      writeln(OUP,'LDL^t FACTORIZATION');
      writeln(OUP);
      writeln ( OUP, 'The matrix L output by rows: ');
      for I := 1 to N do
         begin
            for J := 1 to I-1 do write ( OUP, '':2, A[I,J]:12:8 );
            writeln ( OUP )
         end;
      writeln( OUP, 'The diagonal of D: ');
      for I := 1 to N do write ( OUP, '':2, D[I]:12:8 );
      writeln ( OUP )
   end;
   begin
      INPUT;
      if ( OK ) then
         begin
{           STEP 1                                                     }
            for I := 1 to N do
               begin
{                 STEP 2                                               }
                  for J := 1 to I-1 do V[J] := A[I,J] * D[J];
{                 STEP 3                                               }
                  D[I] := A[I,I];
                  for J := 1 to I-1 do D[I] := D[I] - A[I,J] * V[J];
{                 STEP 4                                               }
                  for J := I+1 to N do
                     begin
                        for K := 1 to I-1 do
                           A[J,I] := A[J,I] - A[J,K] * V[K];
                        A[J,I] := A[J,I] / D[I]
                     end
               end;
{           STEP 5                                                     }
            OUTPUT;
            close ( OUP )
         end
   end.

