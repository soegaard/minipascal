program ALG071;
{  JACOBI ITERATIVE ALGORITHM 7.1

   To solve Ax = b given an initial approximation x(0).

   INPUT:   the number of equations and unknowns n; the entries
            A(I,J), 1<=I, J<=n, of the matrix A; the entries
            B(I), 1<=I<=n, of the inhomogeneous term b; the
            entries XO(I), 1<=I<=n, of x(0); tolerance TOL;
            maximum number of iterations N.

    OUTPUT: the approximate solution X(1),...,X(n) or a message
            that the number of iterations was exceeded.
}
var
   INP,OUP : text;
   A : array [ 1..10, 1..11 ] of real;
   X1,X2 : array [ 1..10 ] of real;
   S,ERR,TOL : real;
   FLAG,N,I,J,NN,K : integer;
   OK : boolean;
   AA : char;
   NAME : string [ 30 ];
procedure INPUT;
   begin
      writeln('This is the Jacobi Method for Linear Systems.');
      OK := false;
      writeln ('The array will be input from a text file in the order: ');
      writeln('A(1,1), A(1,2), ..., A(1,n+1), A(2,1), A(2,2), ..., A(2,n+1),');
      write ('..., A(n,1), ');
      writeln ('A(n,2), ..., A(n,n+1) '); writeln;
      write ('Place as many entries as desired on each line, but separate ');
      writeln ('entries with ');
      writeln ('at least one blank.');
      writeln ('The initial approximation should follow in same format.' );
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
                        for I := 1 to N do read ( INP, X1[I]);
{                       use X1 for XO                                  }
                        OK := true;
                        close ( INP )
                     end
                  else writeln ('The number must be a positive integer. ')
               end;
            OK := false;
            while ( not OK) do
               begin
                  writeln ('Input the tolerance.');
                  readln ( TOL );
                  if (TOL > 0) then OK := true
                  else writeln('Tolerance must be a positive number.')
               end;
            OK := false;
            while ( not OK) do
               begin
                  writeln('Input maximum number of iterations.');
                  readln ( NN );
{                 use NN for N                                         }
                  if (NN > 0) then OK := true
                  else writeln('Number must be a positive integer.')
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
         writeln(OUP,'JACOBI ITERATIVE METHOD FOR LINEAR SYSTEMS');
         writeln ( OUP );
         writeln ( OUP, 'The solution vector is : ');
         for I := 1 to N do write ( OUP, X2[I]:12:8 );
         writeln ( OUP ); writeln ( OUP, 'using ',K,' iterations ');
         writeln ( OUP, 'with Tolerance',TOL,' in infinity-norm');
         close ( OUP )
      end;
   begin
      INPUT;
      if ( OK ) then
         begin
{           STEP 1                                                     }
            K := 1;
            OK := false;
{           STEP 2                                                     }
            while ( not OK ) and ( K <= NN ) do
               begin
{                 err is used to test accuracy - it measures the
                  infinity-norm }
                  ERR := 0.0;
{                 STEP 3                                               }
                  for I := 1 to N do
                     begin
                        S := 0.0;
                        for J := 1 to N do S := S - A[I,J] * X1[J];
                        S := ( S + A[I,N + 1] ) / A[I,I];
                        if (abs(S) > ERR) then ERR := abs(S);
{                       use X2 for X                                   }
                        X2[I] := X1[I] + S
                     end;
{                 STEP 4                                                       }
                  if ( ERR <= TOL ) then OK := true
{                 process is complete                                  }
                  else
                     begin
{                       STEP 5                                         }
                        K := K + 1;
{                       STEP 6                                         }
                        FOR I := 1 TO N DO X1[I] := X2[I]
                     end
               end;
            if ( not OK ) then writeln
                ('Maximum Number of Iterations Exceeded ')
{           STEP 7                                                     }
{           procedure completed unsuccessfully                         }
            else OUTPUT
        end
   end.

