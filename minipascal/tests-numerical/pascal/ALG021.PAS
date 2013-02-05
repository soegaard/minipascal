program ALG021;
{  BISECTION ALGORITHM 2.1

   To find a solution to f(x) = 0 given the continuous function
   f on the interval [a,b], where f(a) and f(b) have
   opposite signs:

   INPUT:   endpoints a,b; tolerance TOL;
            maximum number of iterations N0.

   OUTPUT:  approximate solution p or
            a message that the algorithm fails.

                                                                       }
const ZERO = 1.0E-20;
var
   A,FA,B,FB,C,P,FP,TOL,X : real;
   I,N0,FLAG : integer;
   OK : boolean;
   AA : char;
   OUP : text;
   NAME : string [ 30 ];
{  Change function F for a new problem                                 }
function F ( X : real ) : real;
   begin
      F := ( X + 4.0 ) * X * X - 10.0
   end;
procedure INPUT;
   begin
      writeln('This is the Bisection Method.');
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
                  writeln ('Input endpoints A < B  separated by blank ');
                  readln ( A , B );
                  if ( A > B ) then
                     begin
                        X := A; A := B; B := X
                     end;
                  if ( A = B ) then writeln ('A cannot equal B ')
                  else
                     begin
                        FA := F( A );
                        FB := F( B );
                        if ( FA * FB > 0.0 ) then
                           writeln ('F(A) and F(B) have same sign ')
                        else OK := true
                     end
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
                  write('Input maximum number of iterations ');
                  writeln('- no decimal point ');
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
      writeln(OUP,'BISECTION METHOD');
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
{        STEP 1                                                        }
         I := 1;
         OK := true;
{        STEP 2                                                        }
         while ( ( I <= N0 ) and OK ) do
            begin
{              STEP 3                                                  }
{              compute P(I)                                            }
               C := ( B - A ) / 2.0;
               P := A + C;
{              STEP 4                                                  }
               FP := F( P );
               if (FLAG = 2) then
                  begin
                     writeln(OUP,I:3,'   ',P:14,'   ',FP:14)
                  end;
               if ( abs(FP) < ZERO ) or ( C < TOL )  then
{                 procedure completed successfully                     }
                  begin
                     writeln(OUP);
                     writeln (OUP,'Approximate solution P = ',P:12:8 );
                     writeln (OUP,'with F(P) = ',FP:12:8 );
                     write (OUP,'Number of iterations = ',I:3 );
                     writeln (OUP,'    Tolerance = ',TOL:14 );
                     OK := false
                  end
               else
                  begin
{                    STEP 5                                            }
                     I := I + 1;
{                    STEP 6                                            }
{                    compute A(I) and B(I)                             }
                     if ( FA * FP > 0.0 ) then
                        begin
                           A := P; FA := FP
                        end
                     else
                        begin
                           B := P; FB := FP
                        end
                  end
            end;
         if OK then
{           STEP 7                                                     }
{           procedure completed unsuccessfully                         }
            begin
               writeln(OUP);
               write (OUP,'Iteration number ',N0:3);
               writeln(OUP,' gave approximation ',P:12:8 );
               writeln (OUP,'F(P) = ',FP:12:8,' not within tolerance : ',TOL:14 )
            end;
         close(OUP);
      end
end.



