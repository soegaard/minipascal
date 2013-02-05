program ALG091;
{  POWER METHOD ALGORITHM 9.1

   To approximate the dominant eigenvalue and an associated
   eigenvector of the n by n matrix A given a nonzero vector x:

   INPUT:   Dimension n; matrix A; vector x; tolerance TOL; maximum
            number of iterations N.

   OUTPUT:  Approximate eigenvalue MU; approximate eigenvector x
            or a message that the maximum number of iterations was
            exceeded.                                                  }
const
   ZERO = 1.0E-20;
var
   A : array [ 1..10, 1..10 ] of real;
   X,Y : array [ 1..10 ] of real;
   T,AMAX,YMU,ERR,TOL : real;
   FLAG,N,I,J,NN,K,LP : integer;
   OK : boolean;
   AA : char;
   NAME : string [ 30 ];
   INP, OUP : text;
procedure INPUT;
   begin
      writeln('This is the Power Method.');
      OK := false;
      writeln ('The array will be input from a text file in the order: ');
      writeln('A(1,1), A(1,2), ..., A(1,n), A(2,1), A(2,2), ..., A(2,n),');
      write ('..., A(n,1), ');
      writeln ('A(n,2), ..., A(n,n) '); writeln;
      write ('Place as many entries as desired on each line, but separate ');
      writeln ('entries with ');
      writeln ('at least one blank. ');
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
                  writeln ('Input the dimension n. ');
                  readln ( N );
                  if ( N > 0 ) then
                     begin
                        for I := 1 to N do
                           for J := 1 to N do read ( INP, A[I,J] );
                        for I := 1 to N do read ( INP, X[I]);
                        close ( INP );
                        while ( not OK ) do
                           begin
                              writeln ('Input the tolerance. ');
                              readln ( TOL );
                              if ( TOL > 0.0 ) then OK := true
                              else
                                 writeln ('Tolerance must be positive number.')
                           end;
                        OK := false;
                        while ( not OK ) do
                           begin
                              write ('Input maximum number of iterations ');
                              writeln ('- integer.');
                              readln ( NN );
{                             use NN for  N                            }
                              if ( NN > 0 ) then OK := true
                              else
                                 writeln ('Number must be positive integer. ')
                           end
                     end
                  else writeln ('The dimension must be a positive integer. ')
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
         writeln(OUP,'POWER METHOD');
         writeln(OUP);
         writeln(OUP,'iter  approx        approx eigenvector');
         writeln(OUP,'      eigenvalue');
      end;
   begin
      INPUT;
      if ( OK ) then
         begin
            OUTPUT;
{           STEP 1                                                     }
            K := 1;
{           STEP 2                                                     }
            LP := 1;
            AMAX := abs( X[1] );
            for I := 2 to N do
               begin
                  if ( abs( X[I] ) > AMAX ) then
                     begin
                        AMAX := abs( X[I] );
                        LP := I
                     end
               end;
{           STEP 3                                                     }
            for I := 1 to N do X[I] := X[I] / AMAX;
{           STEP 4                                                     }
            while ( ( K <= NN ) and OK ) do
               begin
{                 STEP 5                                               }
                  for I := 1 to N do
                     begin
                        Y[I] := 0.0;
                        for J := 1 to N do Y[I] := Y[I] + A[I,J] * X[J]
                     end;
{                 STEP 6                                               }
                  YMU := Y[LP];
{                 STEP 7                                               }
                  LP := 1;
                  AMAX := abs( Y[1] );
                  for I := 2 to N do
                     begin
                        if ( abs( Y[I] ) > AMAX ) then
                           begin
                              AMAX := abs( Y[I] );
                              LP := I
                           end
                     end;
{                 STEP 8                                               }
                  if ( AMAX <= ZERO ) then
                     begin
                        write ('Zero eigenvalue - select another ');
                        writeln ('initial vector and begin again ');
                        OK := false
                     end
                  else
                     begin
{                       STEP 9                                         }
                        ERR := 0.0;
                        for I := 1 to N do
                           begin
                              T := Y[I] / Y[LP];
                              if ( abs( X[I] - T ) > ERR ) then
                                 ERR := abs( X[I] - T );
                              X[I] := T
                           end;
                        write ( OUP,K,' ',YMU:12:8);
                        for I := 1 to N do write ( OUP, X[I]:12:8 );
                        writeln(OUP);
{                       STEP 10                                        }
                        if ( ERR <= TOL ) then
                           begin
                              writeln ( OUP ); writeln ( OUP );
                              write ( OUP, 'The eigenvalue =', YMU:12:8);
                              writeln ( OUP, ' to tolerance =', TOL );
                              writeln(OUP,'on iteration number = ',K);
                              writeln ( OUP );
                              writeln(OUP,'Unit eigenvector is :');
                              writeln ( OUP ) ;
                              for I := 1 to N do write ( OUP, X[I]:12:8 );
                              OK := false
                           end;
{                       STEP 11                                        }
                        K := K + 1;
                     end
               end;
{           STEP 12                                                    }
            if ( K > NN ) then
               begin
                  write ('Method did not converge within ', NN );
                  writeln (' iterations ')
               end;
            close(OUP)
         end;
   end.

