program ALG024;
{  SECANT ALGORITHM 2.4

   To find a solution to the equation f(x) = 0
   given initial approximations p0 and p1:

   INPUT:   initial approximations p0, p1; tolerance TOL;
            maximum number of iterations N0.

   OUTPUT:  approximate solution p or
            a message that the algorithm fails.
                                                                       }
var
   P0,F0,P1,F1,P,FP,TOL : real;
   I,N0,FLAG : integer;
   OK : boolean;
   AA : char;
   OUP : text;
   NAME : string [ 30 ];
{  Change function F for a new problem                                 }
function F ( X : real ) : real;
   begin
      F := cos( X ) - X
   end;
procedure INPUT;
   begin
      writeln('This is the Secant Method.');
      write ('Has the function F been created in the program ');
      writeln ('immediately preceding ');
      writeln ('the INPUT procedure? ');
      writeln ('Enter Y or N ');
      readln ( AA );
      if ( AA = 'Y' ) or ( AA = 'y' ) then
         begin
            OK := false;
            while ( not OK ) do
               begin
                  write ('Input initial approximations P0 and P1 separated ');
                  writeln ('by blank ');
                  readln ( P0 , P1 );
                  if ( P0 = P1 ) then writeln ('P0 cannot equal P1 ')
                  else OK := true
               end;
            OK := false;
            while ( not OK ) do
               begin
                  writeln ('Input tolerance ');
                  readln ( TOL );
                  if (TOL <= 0.0) then writeln ('Tolerance must be positive ')
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
            write ('The program will end so that the function F ');
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
      writeln(OUP,'SECANT METHOD');
      writeln ('Select amount of output ');
      writeln ('1. Answer only ');
      writeln ('2. All intermediate approximations ');
      writeln ('Enter 1 or 2 ');
      readln (FLAG);
      if FLAG = 2 then
         begin
            writeln(OUP,'I':3,'   ','P':14,'   ','F(P)':14)
         end;
   end;
begin
   INPUT;
   if (OK) then
      begin
         OUTPUT;
{        STEP 1                                                        }
         I := 2;
         F0 := F( P0 );
         F1 := F( P1 );
         OK := true;
{        STEP 2                                                        }
         while ( ( I <= N0 ) and OK ) do
            begin
{              STEP 3                                                  }
{              compute P(I)                                            }
               P := P1 - F1 * ( P1 - P0 ) / ( F1 - F0 );
{              STEP 4                                                  }
               FP := F( P );
               if (FLAG = 2) then
                  begin
                     writeln(OUP,I:3,'   ',P:14,'   ',FP:14)
                  end;
               if ( abs( P - P1 ) < TOL )  then
{                 procedure completed successfully                     }
                  begin
                     writeln (OUP);
                     writeln (OUP,'Approximate solution = ',P:12:8 );
                     writeln (OUP,'with F(P) =', FP:12:8 );
                     write (OUP,'Number of iterations = ',I:3 );
                     writeln (OUP,'    Tolerance = ',TOL:14 );
                     OK := false
                  end
               else
                  begin
{                    STEP 5                                            }
                     I := I + 1;
{                    STEP 6                                            }
{                    update P0, F0, P1, F1                             }
                     P0 := P1;
                     F0 := F1;
                     P1 := P;
                     F1 := FP
                  end
            end;
         if OK then
{           STEP 7                                                     }
{           procedure completed unsuccessfully                         }
            begin
               writeln(OUP);
               write(OUP,'Iteration number ',N0:3);
               writeln(OUP,' gave approximation ',P:12:8 );
               writeln (OUP,'F(P) = ',FP:12:8,
                            ' not within tolerance : ',TOL:14 )
            end;
         close(OUP);
      end
   end.



