program ALG094;
{  WIELANDT'S DEFLATION ALGORITHM 9.4

   To approximate the second most dominant eigenvalue and an
   associated eigenvector of the n by n matrix A given an
   approximation LAMBDA to the dominant eigenvalue, an
   approximation V to a corresponding eigenvector and a vector X
   belonging to R**(n-1).

   INPUT:   Dimension n; matrix A; approximate eigenvalue LAMBDA;
            approximate eigenvector V belonging to R**n; vector X
            belonging to R**(n-1), tolerance TOL, maximum number
            of iterations N.

   OUTPUT:  Approximate eigenvalue MU; approximate eigenvector U or
            a message that the method fails.
}
const
   ZERO = 1.0E-20;
var
   A : array [ 1..10, 1..10 ] of real;
   B : array [ 1..9, 1..9 ] of real;
   V,W,VV : array [ 1..10 ] of real;
   X,Y : array [ 1..10 ] of real;
   S,AMAX,YMU,XMU,ERR,TOL : real;
   NUM,FLAG,N,I,J,K,M,I1,N1,I2,L1,L2,NN : integer;
   OK : boolean;
   AA : char;
   NAME : string [ 30 ];
   INP,OUP : text;
procedure INPUT;
   begin
      writeln('This is Wielandt Deflation.');
      OK := false;
      writeln ('The array will be input from a text file in the order: ');
      writeln('A(1,1), A(1,2), ..., A(1,n), A(2,1), A(2,2), ..., A(2,n),');
      write ('..., A(n,1), ');
      write ('A(n,2), ..., A(n,n).'); writeln; writeln;
      write('Next place the approximate eigenvector V(1), ..., ');
      writeln ('V(n) and follow it  ');
      write ('by the approximate eigenvalue. Finally, an ');
      writeln ('initial approximate ');
      write ('eigenvector of dimension n-1: X(1), ..., X(n-1) ');
      writeln ('should follow. ');
      writeln;
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
                  writeln ('Input the dimension n. ');
                  readln ( N );
                  if ( N > 1 ) then OK := true
                  else writeln ('Dimension must be greater than 1. ')
               end;
            OK := false;
            while ( not OK ) do
               begin
                  writeln ('Input a positive tolerance for the power method.');
                  readln ( TOL );
                  if ( TOL > 0.0 ) then OK := true
                  else writeln ('Tolerance must be a positive number. ')
               end;
            OK := false;
            while ( not OK ) do
               begin
                  write ('Input the maximum number of iterations for the ');
                  writeln ('power method. ');
                  readln ( NN );
                  if ( NN > 0 ) then OK := true
                  else writeln ('The number must be a positive integer. ')
               end;
            for I := 1 to N do
               for J := 1 to N do read ( INP, A[I,J] );
            OK := false;
            for I := 1 to N do
               begin
                  read ( INP, V[I] );
                  if ( abs( V[I] ) > ZERO ) then OK := true
               end;
            read ( INP, XMU );
            M := N - 1;
            if ( OK ) then
               begin
                  OK := false;
                  for I := 1 to M do
                     begin
                        read ( INP, X[I] );
                        if ( abs( X[I] ) > ZERO ) then OK := true
                     end
               end;
            if ( not OK ) then writeln ('All vectors must be nonzero. ');
            close ( INP );
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
      writeln(OUP,'WIELANDT DEFLATION');
      writeln(OUP);
   end;
procedure POWER;
   var
      AMAX,T,ERR : real;
      K,LP,I,J : integer;
      DONE : boolean;
   begin
      K := 1;
      LP := 1;
      AMAX := abs( X[1] );
      for I := 2 to M do
         begin
            if ( abs( X[I] ) > AMAX ) then
               begin
                  AMAX := abs( X[I] );
                  LP := I
               end
         end;
      DONE := false;
      for I := 1 to M do X[I] := X[I] / AMAX;
      while ( ( K <= NN ) and ( OK ) and ( not DONE ) ) do
         begin
            for I := 1 to M do
               begin
                  Y[I] := 0.0;
                  for J := 1 to M do Y[I] := Y[I] + B[I,J] * X[J]
               end;
            YMU := Y[LP];
            LP := 1;
            AMAX := abs( Y[1] );
            for I := 2 to M do
               begin
                  if ( abs( Y[I] ) > AMAX ) then
                     begin
                        AMAX := abs( Y[I] );
                        LP := I
                     end
               end;
            if ( AMAX <= ZERO ) then
               begin
                  writeln ('Zero eigenvalue - B is singular ');
                  OK := false
               end
            else
               begin
                  ERR := 0.0;
                  for I := 1 to M do
                     begin
                        T := Y[I] / Y[LP];
                        if ( abs( X[I] - T ) > err ) then
                           ERR := abs( X[I] - T );
                        X[I] := T
                     end;
                  if ( ERR < TOL ) then
                     begin
                        for I := 1 to M do Y[I] := X[I];
                        DONE := true
                     end
                  else K := K + 1
               end
         end;
      if ( ( K > NN ) and OK ) then
         begin
            writeln ('Power method failed to converge in ',NN,' iterations');
            OK := false
         end
      else writeln(OUP,'Number Iterations for Power Method = ',K);
   end;
   begin
      INPUT;
      if ( OK ) then
         begin
            OUTPUT;
{           STEP 1                                                     }
            I := 1;
            AMAX := abs( V[1] );
            for J := 2 to N do
                begin
                   if ( abs( V[J] ) > AMAX ) then
                      begin
                         I := J;
                         AMAX := abs( V[J] )
                      end
                end;
{           STEP 2                                                     }
            if ( I <> 1 ) then
               begin
                  for K := 1 to I-1 do
                      for J := 1 to I-1 do
                          B[K,J] := A[K,J] - V[K] * A[I,J] / V[I]
               end;
{           STEP 3                                                     }
            if ( ( I <> 1 ) and ( I <> N ) ) then
               begin
                  for K := I to N-1 do
                      for J := 1 to I-1 do
                          begin
                             B[K,J] := A[K+1,J]-V[K+1]*A[I,J]/V[I];
                             B[J,K] := A[J,K+1]-V[J]*A[I,K+1]/V[I]
                          end
               end;
{           STEP 4                                                     }
            if ( I <> N ) then
               begin
                  for K := I to N-1 do
                      for J := I to N-1 do
                          B[K,J] := A[K+1,J+1]-V[K+1]*A[I,J+1]/V[I]
               end;
{           STEP 5                                                     }
            POWER;
            if ( OK ) then
               begin
{                 STEP 6                                               }
                  if ( I <> 1 ) then
                     begin
                        for K := 1 to I-1 do W[K] := Y[K]
                     end;
{                 STEP 7                                               }
                  W[I] := 0.0;
{                 STEP 8                                               }
                  if ( I <> N ) then
                     begin
                        for K := I+1 to N do W[K] := Y[K - 1]
                     end;
{                 STEP 9                                               }
                  S := 0.0;
                  for J := 1 to N do S := S + A[I,J] * W[J];
                  S := S / V[I];
                  for K := 1 to N do
{                     Compute eigenvector
                      VV is used in place of U here                    }
                      VV[K] := ( YMU - XMU ) * W[K] + S * V[K];
                  writeln ( OUP, 'The reduced matrix B: ' );
                  for L1 := 1 to M do
                     begin
                         for L2 := 1 to M do write ( OUP, B[L1,L2] );
                         writeln ( OUP )
                     end;
                  writeln ( OUP );
                  write ( OUP, 'The Eigenvalue =', YMU:12:8 );
                  writeln ( OUP, ' to Tolerance =', TOL );
                  writeln ( OUP );
                  writeln ( OUP, 'Eigenvector is: ');
                  for I := 1 to N do write ( OUP, VV[I]:12:8 );
                  writeln(OUP);
               end;
            close(OUP)
         end
   end.