program ALGO26;
{  STEFFENSEN'S ALGORITHM 2.6

   To find a solution to g(x) = x
   given an initial approximation p0:

   INPUT:   initial approximation p0; tolerance TOL;
            maximum number of iterations N0.

   OUTPUT:  approximate solution p or
            a message that the method fails.
                                                                       }
var
   TOL,P0,P1,P2,P,D,ZERO : real;
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
      writeln('This is Steffensens Method.');
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
      writeln(OUP,'STEFFENSENS METHOD');
      if FLAG = 2 then
         begin
            writeln(OUP,'I':3,'   ','P':14)
         end;
   end;
begin
   ZERO := 1.0E-20;
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
{              compute P(1) with superscript ( I - 1 )                 }
               P1 := G( P0 );
{              compute P(2) with superscript ( I - 1 )                 }
               P2 := G( P1 );
               if (abs(P2-2.0*P1+P0) < ZERO) then
                  begin
                     FLAG := 1;
                     OK := false;
                     D := 10.0;
                     writeln(OUP,'Denominator = 0, method fails');
                     writeln(OUP,'best possible is P2(',I,')= ',P2:14)
                  end
               else
                  D := (P1-P0)*(P1-P0)/(P2-2.0*P1+P0);
{              compute P(0) with superscript ( I - 1 )                 }
               P := P0 - D;
               if (FLAG = 2) then
                  begin
                     writeln(OUP,I:3,'   ',P:14)
                  end;
{              STEP 4                                                  }
               if ( abs( D ) < TOL ) then
{                 procedure completed successfully                     }
                  begin
                     writeln (OUP);
                     writeln (OUP,'Approximate solution = ',P:12:8 );
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
               writeln(OUP);
               write (OUP,'Iteration number ',N0:3);
               writeln(OUP,' gave approximation ',P:12:8 );
               writeln (OUP,'not within tolerance ',TOL:14 )
            end;
         close(OUP);
      end
   end.


