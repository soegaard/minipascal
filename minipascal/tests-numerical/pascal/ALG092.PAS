program ALG092;
{ SYMMETRIC POWER METHOD ALGORITHM 9.2

   To approximate the dominant eigenvalue and an associated
   eigenvector of the n by n symmetric matrix A given a nonzero vector x:

   INPUT:   Dimension n; matrix A; vector x; tolerance TOL;
            maximum number of iterations N.

   OUTPUT:  Approximate eigenvalue MU; approximate eigenvector x or
            a message that the maximum number of iterations was
            exceeded.
}
const
   ZERO = 1.0E-20;
var
   A : array [ 1..10, 1..10 ] of real;
   X,Y : array [ 1..10 ] of real;
   YMU,ERR,TOL : real;
   FLAG,N,I,J,NN,K : integer;
   OK : boolean;
   AA : char;
   NAME : string [ 30 ];
   INP,OUP : text;
procedure INPUT;
begin
   writeln('This is the Symmetric Power Method.');
   OK := false;
   writeln ('The array will be input from a text file in the order: ');
   writeln('A(1,1), A(1,2), ..., A(1,n), A(2,1), A(2,2), ..., A(2,n),');
   write ('..., A(n,1), ');
   writeln ('A(n,2), ..., A(n,n) '); writeln;
   write ('Place as many entries as desired on each line, but separate ');
   writeln ('entries with ');
   writeln ('at least one blank. ');
   writeln ('The initial approximation should follow in same format. ');
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
{                    Because of the way in which the subroutine
                     norm computes the l2 norm of a vector,
                     the initial input of X is into Y and X
                     is initialized at the zero vector.             }
                     for I := 1 to N do read ( INP, Y[I] );
                     for I := 1 to N do X[I] := 0.0;
                     close ( INP );
                     while ( not OK ) do
                        begin
                           writeln ('Input the tolerance. ');
                           readln ( TOL );
                           if ( TOL > 0.0 ) then OK := true
                           else
                              writeln ('Tolerance must be positive number. ')
                        end;
                     OK := false;
                     while ( not OK ) do
                        begin
                           write ('Input maximum number of iterations ');
                           writeln ('- integer. ');
                           readln ( NN );
{                          Use NN for capital N for the maximum number of
                           iterations                                     }
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
      writeln(OUP,'SYMMETRIC POWER METHOD');
      writeln(OUP);
      writeln(OUP,'iter   approx        approx eigenvector');
      writeln(OUP,'       eigenvalue');
   end;
procedure NORM;
   var
      XL,T : real;
      I : integer;
   begin
      XL := 0.0;
      for I := 1 to N do XL := XL + sqr( Y[I] );
{     2-norm of Y    }
      XL := sqrt( XL );
      ERR := 0.0;
      if ( XL > ZERO ) then
         begin
            for I := 1 to N do
               begin
                  T := Y[I] / XL;
                  ERR := ERR + sqr(X[I]-T);
                  X[I] := T
               end;
{              X has a 2-norm of 1.0             }
            ERR := sqrt(ERR)
         end
      else
         begin
            write ('A has a zero eigenvalue - select new vector and begin ');
            writeln ('again ');
            OK := false
         end
   end;
   begin
      INPUT;
      if ( OK ) then
         begin
            OUTPUT;
{           STEP 1                                                     }
            K := 1;
{           Norm computes the norm of the vector X and
            returns X divided by its norm in the variable Y            }
            NORM;
            if ( OK ) then
               begin
{                 STEP 2                                               }
                  while ( ( K <= NN ) and OK ) do
                     begin
{                       STEPS 3 AND 4                                  }
                        YMU := 0.0;
                        for I := 1 to N do
                           begin
                              Y[I] := 0.0;
                              for J := 1 to N do
                                 Y[I] := Y[I] + A[I,J] * X[J];
                              YMU := YMU + X[I] * Y[I]
                           end;
{                       STEP 5 (This step is accomplished in
                                subroutine norm.)                      }
{                       STEP 6                                         }
                        NORM;
                        write ( OUP,K,' ', YMU:12:8,'   ');
                        for I := 1 to N do write ( OUP, X[I]:12:8 );
                        writeln(OUP);
                        if ( OK ) then
                           begin
{                             STEP 7                                   }
                              if ( ERR < TOL ) then
                                 begin
{                                   procedure completed successfully   }
                                    writeln ( OUP ); writeln ( OUP );
                                    write(OUP,'The eigenvalue = ',YMU:12:8);
                                    writeln(OUP,' to tolerance = ',TOL);
                                    writeln(OUP,'on iteration number = ',K);
                                    writeln ( OUP );
                                    writeln(OUP,'Unit eigenvector is:');
                                    writeln ( OUP ) ;
                                    for I := 1 to N do write(OUP,X[I]:12:8);
                                    writeln(OUP);
                                    OK := false
                                 end
                              else
{                                STEP 8                                }
                                 K := K + 1
                           end
                     end;
{                 STEP 9                                               }
                  if ( K > NN ) then
                     writeln ('No convergence within ', NN, ' iterations ')
               end;
               close(OUP)
         end;
   end.

