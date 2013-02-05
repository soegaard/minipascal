program ALGO22;
{  FIXED-POINT ALGORITHM 2.2

   To find a solution to p = g(p) given an
   initial approximation p0:

   INPUT:   initial approximation p0; tolerance TOL;
            maximum number of iterations N0.

   OUTPUT:  approximate solution p or
            a message that the method fails.
                                                                               }
var
   TOL,P0,P : real;
   I,N0,FLAG : integer;
   OK : boolean;
   AA : char;
   OUP : text;
   NAME : string [ 30 ];
{  Change function G for a new problem                                 }
function G ( X : real ) : real;
   begin
      G := sqrt( 10.0 / ( 4.0 + X ) )
   end;
procedure INPUT;
   begin
      writeln('This is the Fixed-Point Method.');
      write ('Has the function G been created in the program ');
      writeln ('immediately preceding ');
      writeln ('the INPUT procedure? ');
      writeln ('Enter Y or N ');
      readln ( AA );
      if ( AA = 'Y' ) or ( AA = 'y' ) then
         begin
            OK := false;
            writeln ('Input initial approximation ');
            readln ( P0 );
            while ( not OK ) do
               begin
                  writeln ('Input tolerance ');
                  readln ( TOL );
                  if ( TOL <= 0.0 ) then writeln ('Tolerance must be positve ')
                  else OK := true
               end;
            OK := false;
            while ( not OK ) do
               begin
                  write('Input maximum number of iterations');
                  writeln(' - no decimal point ');
                  readln ( N0 );
                  if ( N0 <= 0 ) then writeln ('Must be positive integer ')
                  else OK := true
               end
         end
      else
         begin
            write ('The program will end so that the function G ');
            writeln ('can be created ');
            OK := false
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
      writeln ('Select amount of output ');
      writeln ('1. Answer only ');
      writeln ('2. All intermediate approximations ');
      writeln ('Enter 1 or 2 ');
      readln (FLAG);
      writeln(OUP,'FIXED-POINT METHOD');
      if FLAG = 2 then
         begin
            writeln(OUP,'I':3,'   ','P':14)
         end
   end;
begin
   INPUT;
   if (OK) then
      begin
         OUTPUT;
{        STEP 1                                                        }
         I := 1; OK := true;
{        STEP 2                                                        }
         while ( ( I <= N0 ) and OK ) do
            begin
{              STEP 3                                                  }
{              compute P(I)                                            }
               P := G( P0 );
               if (FLAG = 2) then
                  begin
                     writeln(OUP,I:3,'   ',P:14)
                  end;
{              STEP 4                                                  }
               if ( abs( P - P0 ) < TOL ) then
{              procedure completed successfully                        }
                  begin
                     writeln (OUP);
                     writeln (OUP,'Approximate solution P = ',P:12:8 );
                     write (OUP,'Number of iterations = ',I:3 );
                     writeln (OUP,'    Tolerance = ',TOL:14 );
                     OK := false
                  end
               else
                  begin
{                    STEP 5                                            }
                     I := I + 1;
{                    STEP 6                                            }
{                    update P0                                         }
                     P0 := P
                  end
            end;
         if OK then
{           STEP 7                                                     }
{           procedure completed unsuccessfully                         }
            begin
               write(OUP,'Iteration number ',N0:3);
               writeln(OUP,' gave approximation ',P:12:8 );
               writeln (OUP,'not within tolerance ',TOL:14 )
            end;
         close(OUP);
      end
end.


