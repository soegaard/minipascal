program ALG025;
{  METHOD OF FALSE POSITION ALGORITHM 2.5

   To find a solution to f(x) = 0 given the continuous function
   f on the interval [p0,p1], where f(p0) and f(p1) have
   opposite signs:

   INPUT:   endpoints p0, p1; tolerance TOL;
            maximum number of iterations N0.

   OUTPUT:  approximate solution p or
            a message that the algorithm fails.

                                                                       }
const ZERO = 1.0E-20;
var
   Q,P0,Q0,P1,Q1,C,P,FP,TOL,X : real;
   I,N0 : integer;
   OK : boolean;
   AA : char;
   OUP : text;
   FLAG : integer;
   NAME : string [ 30 ];
{ Change function F for a new problem. }
function F ( X : real ) : real;
   begin
      F := cos(X) - X
   end;
procedure INPUT;
   begin
      writeln('This is the Method of False Position.');
      write ('Has the function F been created in the program ');
      writeln ('immediately preceding ');
      writeln ('the INPUT procedure? ');
      writeln ('Enter Y or N. ');
      readln ( AA );
      if ( AA = 'Y' ) or ( AA = 'y' ) then
         begin
            OK := false;
            while ( not OK ) do
               begin
                  writeln ('Input endpoints P0 < P1  separated by blank. ');
                  readln ( P0 , P1 );
                  if ( P0 > P1 ) then
                     begin
                        X := P0; P0 := P1; P1 := X
                     end;
                  if ( P0 = P1 ) then writeln ('P0 cannot equal P1. ')
                  else
                     begin
                        Q0 := F( P0 );
                        Q1 := F( P1 );
                        if ( Q0 * Q1 > 0.0 ) then
                           writeln ('F(P0) and F(P1) have same sign. ')
                        else   OK := true
                     end
               end;
            OK := false;
            while ( not OK ) do
               begin
                  writeln ('Input tolerance. ');
                  readln ( TOL );
                  if (TOL <= 0.0) then writeln ('Tolerance must be positive. ')
                  else OK := true
               end;
            OK := false;
            while ( not OK ) do
               begin
                  write('Input maximum number of iterations ');
                  writeln('- no decimal point. ');
                  readln ( N0 );
                  if ( N0 <= 0 ) then writeln ('Must be positive integer. ')
                  else OK := true
               end
         end
      else
         begin
            write ('The program will end so that the function F ');
            writeln ('can be created. ');
            OK := false
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
            assign ( OUP, NAME )
         end
      else assign ( OUP, 'CON' );
      writeln ('Select amount of output ');
      writeln ('1. Answer only ');
      writeln ('2. All intermediate approximations ');
      writeln ('Enter 1 or 2 ');
      readln (FLAG);
      rewrite ( OUP );
      writeln(OUP,'METHOD OF FALSE POSITION OR REGULA FALSII');
      writeln ( OUP );
      if FLAG = 2 then
         begin
            writeln(OUP,'I':3,'   ','P':14,'   ','F(P)':14)
         end
   end;
begin
   INPUT;
   if (OK) then
      begin
{        STEP 1                                                        }
         OUTPUT;
         I := 2;
         OK := true;
         Q0 := F( P0 );
         Q1 := F( P1 );
{        STEP 2                                                        }
         while ( ( I <= N0 ) and OK ) do
            begin
{              STEP 3                                                  }
{              compute P(I)                                            }
               P := P1 - Q1 * ( P1 - P0 ) / ( Q1 - Q0 );
               Q := F( P );
               if (FLAG = 2) then
                  begin
                     writeln(OUP,I:3,'   ',P:14,'   ',Q:14)
                  end;
{              STEP 4                                                  }
               if  ( abs(P-P1) < TOL ) then
                  begin
{                    Procedure completed successfully.                 }
                     writeln (OUP);
                     writeln (OUP,'Approximate solution P = ',P:12:8 );
                     writeln (OUP,'with F(P) = ',Q:12:8 );
                     write (OUP,'Number of iterations = ',I:3 );
                     writeln (OUP,'    Tolerance = ',TOL:14 );
                     OK := false
                  end
               else
                  begin
{              STEP 5                                                  }
                     I := I + 1;
{                    STEP 6                                            }
{                    compute P0(I) and P1(I)                           }
                     if ( Q * Q1 < 0.0 ) then
                        begin
                           P0 := P1; Q0 := Q1
                        end;
{                    STEP 7                                            }
                     P1 := P; Q1 := Q;
                  end
            end;
         if OK then
{           STEP 8                                                     }
{           procedure completed unsuccessfully                         }
            begin
                writeln(OUP);
                write(OUP,'Iteration number ',N0:3);
                writeln(OUP,' gave approximation ',P:12:8 );
                writeln (OUP,'F(P) = ',Q:12:8,
                ' not within tolerance : ',TOL:14 );
                writeln('failed');
            end;
      close(OUP)
      end
end.
