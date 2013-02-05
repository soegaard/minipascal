program ALG031;
{  NEVILLE'S ITERATED INTERPOLATION ALGORITHM 3.1

   To evaluate the interpolating polynomial P on the
   (n+1) distinct numbers x(0), ..., x(n) at the number x
   for the function f:

   INPUT:   numbers x(0),..., x(n) as XX(0),...,XX(N);
            number x; values of f as the first column of Q
            or may be computed if function f is supplied.

   OUTPUT:  the table Q with P(x) = Q(N+1,N+1).                        }

var
   Q : array [ 0..25, 0..25 ] of real;
   XX,D : array [ 0..25 ] of real;
   X : real;
   I,J,N,FLAG : integer;
   OK : boolean ;
   A : char;
   INP,OUP : text;
   NAME : string [ 30 ];
{  Change F if program is to calculate the first column of Q           }
function F ( X : real ) : real;
   begin
      F := 1.0 / X
   end;
procedure INPUT;
   begin
      writeln('This is Nevilles Method.');
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
                      writeln ('Input n');
                      readln ( N );
                      if ( N > 0 ) then
                         begin
                            OK := true;
                            for I := 0 to N do
                               begin
                                  write ('Input X(',I,') and F(X(',I,')) ');
                                  writeln ('separated by space ');
                                  readln ( XX[I], Q[I,0] )
                               end
                         end
                      else writeln ('Number must be a positive integer ')
                   end
             end;
         2 : begin
                write ('Has a text file been created with the data in two ');
                writeln ('columns ? ');
                writeln ('Enter Y or N ');
                readln ( A );
                if ( A = 'Y' ) or ( A = 'y' ) then
                   begin
                      write ('Input the file name in the form - ');
                      writeln ('drive:name.ext ');
                      writeln ('for example:   A:DATA.DTA ');
                      readln ( NAME );
                      assign ( INP, NAME );
                      reset ( INP );
                      OK := false;
                      while ( not OK ) do
                         begin
                            writeln ('Input n');
                            readln ( N );
                            if ( N > 0 ) then
                               begin
                                  for I := 0 to N do
                                     readln ( INP, XX[I], Q[I,0]);
                                  close ( INP );
                                  OK := true
                               end
                            else writeln ('Number must be a positive integer ')
                         end
                   end
                else
                   begin
                      write ('Please create the input file in two column ');
                      writeln ('form with the X values and ');
                      writeln ('F(X) values in the corresponding columns. ');
                      write ('The program will end so the input file can ');
                      writeln ('be created. ');
                      OK := false
                   end
             end;
         3 : begin
                write ('Has the function F been created in the program ');
                writeln ('immediately preceding ');
                writeln ('the INPUT procedure? ');
                writeln ('Enter Y or N ');
                readln ( A );
                if ( A = 'Y' ) or ( A = 'y' ) then
                   begin
                      OK := false;
                      while ( not OK ) do
                         begin
                            writeln ('Input n');
                            readln ( N );
                            if ( N > 0 ) then
                               begin
                                  for I := 0 to N do
                                  begin
                                     writeln ('Input X(',I,') ');
                                     readln ( XX[I] );
                                     Q[I,0] := F( XX[I] )
                                  end;
                                  OK := true
                               end
                            else writeln ('Number must be a positive integer ')
                         end
                   end
                else
                   begin
                      write ('The program will end so that the function F ');
                      writeln ('can be created. ');
                      OK := false
                   end
             end
      end;
      if OK then
         begin
            write ('Input the point at which the polynomial is to be ');
            writeln ('evaluated ');
            readln ( X )
         end
   end;
procedure OUTPUT;
   begin
      writeln ('Select output destination ');
      writeln ('1. Screen ');
      writeln ('2. Text file ');
      writeln ('Enter 1 or 2 ');
      readln ( FLAG );
      if ( FLAG = 2 ) then
         begin
            write ('Input the file name in the form - ');
            writeln ('drive:name.ext ');
            writeln ('for example:   A:OUTPUT.DTA ');
            readln ( NAME );
            assign ( OUP, NAME )
         end
      else assign ( OUP, 'CON');
      rewrite ( OUP );
      writeln(OUP,'NEVILLES METHOD');
      writeln (OUP, 'Table for P evaluated at X = ',X:12:8,' follows: ');
      write (OUP, 'Entries are XX(I), Q(I,0), ..., Q(I,I) ');
      writeln (OUP, 'for each I = 0, ..., N where N = ',N:3 );
      writeln (OUP, ' ');
      for I := 0 to N do
         begin
            write ( OUP, XX[I]:12:8 );
            for J := 0 to I do write ( OUP, Q[I,J]:12:8 );
            writeln ( OUP )
         end;
      close ( OUP )
   end;
   begin
      INPUT;
      if ( OK ) then
         begin
{           STEP 1                                                     }
            D[0] := X - XX[0];
            for I := 1 to N do
               begin
                  D[I] := X - XX[I];
                  for J := 1 to I do
                     Q[I,J] := ( D[I] * Q[I-1,J-1] - D[I-J] *
                               Q[I,J-1] ) / ( D[I] - D[I-J] )
               end;
{           STEP 2                                                     }
            OUTPUT
         end
   end.