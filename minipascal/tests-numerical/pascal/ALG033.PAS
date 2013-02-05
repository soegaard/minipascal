program ALG033;
{     HERMITE INTERPOLATION ALGORITHM 3.3

      TO OBTAIN THE COEFFICIENTS OF THE HERMITE INTERPOLATING
      POLYNOMIAL H ON THE (N+1) DISTINCT NUMBERS X(0), ..., X(N)
      FOR THE FUNCTION F:

      INPUT:   NUMBERS X(0), X(1), ..., X(N); VALUES F(X(0)), F(X(1)),
               ..., F(X(N)) AND F'(X(0)), F'(X(1)), ..., F'(X(N)).

      OUTPUT:  NUMBERS Q(0,0), Q(1,1), ..., Q(2N + 1,2N + 1) WHERE

               H(X) = Q(0,0) + Q(1,1) * ( X - X(0) ) + Q(2,2) *
                      ( X - X(0) )**2 + Q(3,3) * ( X - X(0) )**2 *
                      ( X - X(1) ) + Q(4,4) * ( X - X(0) )**2 *
                      ( X - X(1) )**2 + ... + Q(2N + 1,2N + 1) *
                      ( X - X(0) )**2 * ( X - X(1) )**2 * ... *
                      ( X - X(N - 1) )**2 * (X - X(N) ).
                                                                               }
type
   FILENAME = string [ 30 ];
var
   Q : array [ 0..25 , 0..25 ] of real;
   X : array [ 0..12 ] of real;
   Z : array [ 0..25 ] of real;
   XX, S : real;
   I,J,K,N,FLAG : integer;
   INP,OUP : text;
   NAME : FILENAME;
   OK : boolean;
   A : char;
{ Change functions F and FP if program is to evaluate first column and
  part of second column of Q. }
function F ( X : real ) : real;
   begin
      F := 1.0/X
   end;
{ Note FP is the derivative of F      }
function FP ( X : real ) : real;
   begin
      FP := -1.0/(X*X)
   end;
procedure INPUT;
   begin
      writeln('This is Hermite interpolation.');
      OK := false;
      while ( not OK ) do
         begin
            writeln ('Choice of input method: ');
            writeln ('1. Input entry by entry from keyboard ');
            writeln ('2. Input data from a text file ');
            writeln ('3. Generate data using a function F ');
            writeln ('Choose 1, 2, or 3 please ');
            readln ( FLAG );
            if ( FLAG = 1 ) or ( FLAG = 2 ) or ( FLAG = 3 ) then OK := true
         end;
      case FLAG of
         1 : begin
                OK := false;
                while ( not OK ) do
                   begin
                      writeln ('Input the number of data points minus 1 ');
                      readln ( N );
                      if ( N > 0 ) then
                         begin
                            OK := true;
                            for I := 0 to N do
                               begin
                                  write ('Input X(',I,'), F(X(',I,')), and ');
                                  writeln ('F''(X(',I,')) separated by spaces ');
                                  readln ( X[I], Q[2 * I,0], Q[2 * I + 1,1] )
                               end
                         end
                      else writeln ('Number must be a positve integer ')
                   end
             end;
         2 : begin
                write ('Has a text file been created with the data in three ');
                writeln ('columns? ');
                writeln ('Enter Y or N ');
                readln ( A );
                if ( A = 'Y' ) or ( A = 'y' ) then
                   begin
                      write ('Input the file name in the form - ');
                      writeln ('drive:name.ext ');
                      writeln ('as example:   A:DATA.DTA ');
                      readln ( NAME );
                      assign ( INP, NAME );
                      reset ( INP );
                      OK := false;
                      while ( not OK ) do
                         begin
                            writeln ('Input the number of data points minus 1 ');
                            readln ( N );
                            if ( N > 0 ) then
                               begin
                                  for I := 0 to N do
                                     readln ( INP, X[I], Q[2 * I,0],
                                              Q[2 * I + 1,1] );
                                  close ( INP );
                                  OK := true
                               end
                            else writeln ('Number must be a positive integer ')
                         end
                   end
                else
                   begin
                      write ('Please create the input file in three column ');
                      writeln ('form with the X values, F(X), and ');
                      writeln ('F''(X) values in the corresponding columns. ');
                      write ('The program will end so the input file can ');
                      writeln ('be created. ');
                      OK := false;
                   end
             end;
         3 : begin
                write ('Have the functions F and FP been created in ');
                writeln ('the program immediately preceding ');
                writeln ('the INPUT procedure? ');
                writeln ('Enter Y or N ');
                readln ( A );
                if ( A = 'Y' ) or ( A = 'y' ) then
                   begin
                      OK := false;
                      while ( not OK ) do
                         begin
                            writeln ('Input the number of data points minus 1 ');
                            readln ( N );
                            if ( N > 0 ) then
                               begin
                                  for I := 0 to N do
                                     begin
                                        writeln ('Input X(',I,') ');
                                        readln ( X[I] );
                                        Q[2 * I,0] := F( X[I] );
                                        Q[2 * I + 1,1] := FP( X[I] )
                                     end;
                                  OK := true
                               end
                            else writeln ('Number must be a positive integer ')
                         end
                   end
                else
                   begin
                      write ('The program will end so that the functions F ');
                      writeln ('and FP can be created. ');
                      OK := false
                   end
             end
      end
   end;
procedure OUTPUT;
      begin
         writeln ('Choice of output method: ');
         writeln ('1. Output to screen ');
         writeln ('2. Output to text file ');
         writeln ('Please enter 1 or 2 ');
         readln ( FLAG );
         if ( FLAG = 2 ) then
            begin
               writeln ('Input the file name in the form - drive:name.ext ');
               readln ( NAME );
               assign ( OUP , NAME )
            end
         else  assign ( OUP , 'CON' );
         rewrite ( OUP );
         writeln(OUP,'HERMITE INTERPOLATING POLYNOMIAL');
         writeln(OUP)
      end;
procedure OUTPUT2;
   begin
      writeln (OUP,'The input data follows: ');
      writeln (OUP,'  X, F(X), F''(X) ');
      for I := 0 to N do
         writeln (OUP, X[I], Q[2 * I,0], Q[2 * I + 1,1] );
      writeln(OUP);
      writeln (OUP,'The Coefficients of the Hermite Interpolation Polynomial ');
      writeln (OUP,'in order of increasing exponent follow: '); writeln(OUP);
      for I := 0 to K do writeln (OUP, Q[I,I] )
   end;
   begin
      INPUT;
      if ( OK ) then
         begin
{           STEP 1                                                             }
            for I := 0 to N do
               begin
{                 STEP 2                                                       }
                  Z[2 * I] := X[I];
                  Z[2 * I + 1] := X[I];
                  Q[2 * I + 1,0] := Q[2 * I,0];
{                 STEP 3                                                       }
                  if ( I <> 0 ) then
                     Q[2 * I,1] := ( Q[2 * I,0] - Q[2 * I - 1, 0] )
                                    / ( Z[2 * I] - Z[2 * I - 1] );
               end;
{           STEP 4                                                             }
            K := 2 * N + 1;
            for I := 2 to K do
               for J := 2 to I do
                 Q[I,J] := ( Q[I,J - 1] - Q[I - 1,J - 1] )
                            / ( Z[I] - Z[I - J] );
{           STEP 5                                                             }
            OUTPUT;
            OUTPUT2;
            writeln('Do you wish to evaluate this polynomial?');
            writeln('Enter Y or N');
            readln(A);
            if (A = 'Y') or (A = 'y') then
               begin
                  writeln('Enter a point at which to evaluate');
                  readln(XX);
                  S := Q[K,K]*(XX-Z[K-1]);
                  for I := 2 to K do
                     begin
                        J := K - I + 1;
                        S := (S + Q[J,J])*(XX-Z[J-1])
                     end;
                  S := S + Q[0,0];
                  writeln(OUP,'x-value and interpolated-value');
                  writeln(OUP,XX,S)
               end
         end;
         close(OUP)
   end.