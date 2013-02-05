program ALG058;
{     TRAPEZOIDAL WITH NEWTON ITERATION ALGORITHM 5.8

      TO APPROXIMATE THE SOLUTION OF THE INITIAL VALUE PROBLEM:
                 Y' = F(T,Y), A <= T <= B, Y(A) = ALPHA,
      AT (N+1) EQUALLY SPACED NUMBERS IN THE INTERVAL [A,B].

      INPUT:   ENDPOINTS A,B; INITIAL CONDITION ALPHA; INTEGER N.
               TOLERANCE TOL; MAXIMUM NUMBER OF ITERATIONS M AT ANY ONE STEP.

      OUTPUT:  APPROXIMATION W TO Y AT THE (N+1) VALUES OF T
               OR A MESSAGE OF FAILURE.
}
var
   A,B,ALPHA,TOL,W,T,H,XK1,W0,Y : real;
   FLAG,N,M,I,J,IFLAG : integer;
   OK : boolean;
   AA : char;
   NAME : string [ 30 ];
   OUP : text;
{  Change functions F and FYP for a new problem.   }
function F ( T, Y : real ) : real;
   begin
      F :=  5*exp(5*T)*sqr(Y-T)+1
   end;
{  Function FYP is the partial derivative of F with respect to Y.   }
function FYP ( T, Y : real ) : real;
   begin
      FYP := 10*exp(5*T)*(Y-T)
   end;
procedure INPUT;
begin
   writeln('This is the Implicit Trapezoidal Method.');
   OK := false;
   write ('Have the functions F and FYP been defined? ');
   writeln ('Answer Y or N. ');
   readln ( AA );
   if ( AA = 'Y' ) or ( AA = 'y' ) then
      begin
         OK := false;
         while ( not OK ) do
            begin
               writeln ('Input left and right endpoints separated by blank ');
               readln ( A, B );
               if ( A >= B ) then
                  writeln ('Left endpoint must be less than right endpoint ')
               else OK := true
            end;
         writeln ('Input the initial condition ');
         readln ( ALPHA );
         OK := false;
         while ( not OK ) do
            begin
               write ('Input a positive integer for the number of ');
               writeln ('subintervals ');
               readln ( N );
               if ( N <= 0 ) then
                  writeln ('Number must be a positive integer ')
               else OK := true
            end;
         OK := false;
         while ( not OK ) do
            begin
               writeln ('Input tolerance ');
               readln ( TOL );
               if ( TOL > 0.0 ) then OK := true
               else writeln ('Tolerance must be a positve real number ')
            end;
         OK := false;
         while ( not OK ) do
            begin
               writeln ('Input the maximum number of iterations ');
               readln ( M );
               if ( M > 0 ) then OK := true
               else writeln ('Number of iterations must be positive ')
            end
      end
   else
      writeln ('The program will end so that the functions can be created.')
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
      rewrite ( OUP );
      writeln(OUP,'IMPLICIT TRAPEZOIDAL METHOD USING NEWTONS METHOD');
      writeln ( OUP );
      writeln ( OUP,'t':5,'w':12,' #iter');
   end;
begin
   INPUT;
   if OK then
      begin
         OUTPUT;
{     STEP 1                                                                   }
         W := ALPHA;
         T := A;
         H := ( B - A ) / N;
         writeln ( OUP,T:5:3,W:12:8,'0':4);
         I := 1;
         OK:= true;
{     STEP 2                                                                   }
         while( ( I <= N ) and OK ) do
            begin
{           STEP 3                                                             }
               XK1 := W + 0.5 * H * F( T, W );
               W0 := XK1;
               J := 1;
               IFLAG := 0;
{           STEP 4                                                             }
               while ( ( IFLAG = 0 ) and OK ) do
                  begin
{                 STEP 5                                                       }
                     W := W0 - ( W0 - XK1 - 0.5 * H * F( T + H, W0 ) ) /
                          ( 1.0 - 0.5 * H * FYP( T + H, W0 ) );
{                 STEP 6                                                       }
                     if ( abs( W - W0 ) < TOL ) then
                        begin
                           IFLAG := 1;
{                       STEP 7                                                 }
                           T := A + I * H;
                           writeln(OUP,T:5:3,W:12:8,J:4);
                           I := I + 1;
                        end
                     else
                        begin
                           J := J + 1;
                           W0 := W;
                           if ( J > M ) then OK := false
                        end
                  end
            end;
         if ( not OK ) then
            writeln ( OUP, 'Maximum number of iterations ',M,' exceeded.');
{     STEP 8                                                                   }
         close ( OUP )
      end
end.