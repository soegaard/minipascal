program ALG051;
{     EULER'S ALGORITHM 5.1

      TO APPROXIMATE THE SOLUTION OF THE INITIAL VALUE PROBLEM:
                 Y' = F(T,Y), A<=T<=B, Y(A) = ALPHA,
      AT N+1 EQUALLY SPACED POINTS IN THE INTERVAL [A,B].

      INPUT:   ENDPOINTS A,B; INITIAL CONDITION ALPHA; INTEGER N.

      OUTPUT:  APPROXIMATION W TO Y AT THE (N+1) VALUES OF T.
}
var
   OUP : text;
   A,B,ALPHA,H,T,W : real;
   FLAG,I,N : integer;
   OK : boolean;
   AA : char;
   NAME : string [ 30 ];
{  Change function F for a new problem.   }
function F ( T, Y : real ) : real;
   begin
      F := Y - T*T + 1.0
   end;
procedure INPUT;
begin
   writeln('This is Eulers Method.');
   write ('Has the function F been defined? ');
   writeln ('Answer Y or N. ');
   readln ( AA );
   OK := false;
   if ( AA = 'Y' ) or ( AA = 'y' ) then
      begin
         OK := false;
         while ( not OK ) do
            begin
               writeln('Input left and right endpoints separated by blank ');
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
      end
   else
      writeln ('The program will end so that the function can be created.')
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
      writeln ( OUP,'EULERS METHOD'  );
      writeln ( OUP );
      writeln ( OUP,'t':5,'w':12);
      writeln (OUP)
   end;
begin
   INPUT;
   if OK then
      begin
         OUTPUT;
{     STEP 1                                                                   }
         H := ( B - A ) / N;
         T := A;
         W := ALPHA;
         writeln ( OUP,T:5:3,W:12:7);
{     STEP 2                                                                   }
         for I := 1 to N do
            begin
{           STEP 3                                                             }
{           COMPUTE W(I)                                                       }
               W := W + H * F( T, W );
{           COMPUTE T(I)                                                       }
               T := A + I * H;
{           STEP 4                                                             }
               writeln ( OUP,T:5:3,W:12:7);
            end;
{     STEP 5                                                                   }
         close ( OUP )
      end
end.