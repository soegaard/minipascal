program ALG023;
{  NEWTON-RAPHSON ALGORITHM 2.3

   To find a solution to f(x) = 0 given an
   initial approximation p0:

   INPUT:   initial approximation p0; tolerance TOL;
            maximum number of iterations NO.

   OUTPUT:  approximate solution p or
            a message that the algorithm fails.
                                                                       }
var
   TOL,P0,D,P,F0,FP0 : real;
   I,N0,FLAG : integer;
   OK : boolean;
   AA : char;
   OUP : text;
   NAME : string [ 30 ];
{  Change functions F and FP for a new problem                         }
function F(X : real) : real;
   begin
      F := cos(X) - X
   end;
{ FP is the derivative of F                                            }
function FP ( X : real ) : real;
   begin
      FP := -sin(X) - 1
   end;
procedure INPUT;
   begin
      writeln('This is Newtons Method');
      write ('Have the functions F and F'' been created in the program ');
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
            write ('The program will end so that the functions F and F'' ');
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
      writeln(OUP,'NEWTONS METHOD');
      writeln ('Select amount of output ');
      writeln ('1. Answer only ');
      writeln ('2. All intermediate approximations ');
      writeln ('Enter 1 or 2 ');
      readln (FLAG);
      if FLAG = 2 then
         begin
            writeln(OUP,'I':3,'   ','P':14,'   ','F(P)':14)
         end
   end;
begin
   INPUT;
   if (OK) then
      begin
         OUTPUT;
         F0 := F( P0 );
{        STEP 1                                                        }
         I := 1; OK := true;
{        STEP 2                                                        }
         while ( ( I <= N0 ) and OK ) do
            begin
{              STEP 3                                                  }
{              compute P(I)                                            }
               FP0 := FP( P0 );
               D := F0 / FP0;
{              STEP 6                                                  }
               P0 := P0 - D;
               F0 := F( P0 );
               if (FLAG = 2) then
                  begin
                     writeln(OUP,I:3,'   ',P0:14,'   ',F0:14)
                  end;
{              STEP 4                                                  }
               if  ( abs( D ) < TOL )  then
{                 procedure completed successfully                     }
                  begin
                     writeln (OUP);
                     writeln (OUP,'Approximate solution = ',P0 );
                     writeln (OUP,'with F(P) = ',F0 );
                     writeln (OUP,'Number of iterations = ',I );
                     writeln (OUP,'Tolerance = ',TOL );
                     OK := false
                  end
               else
{                 STEP 5                                               }
                  I := I + 1;
            end;
         if OK then
            begin
{              STEP 7                                                  }
{              procedure completed unsuccessfully                      }
               writeln (OUP,'Iteration number ',N0,
                            ' gave approximation ',P0 );
               writeln (OUP,'with F(P) = ',F0,' not within tolerance ',TOL )
            end;
         close(OUP);
      end
end.
