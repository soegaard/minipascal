program ALG053;
{     RUNGE-KUTTA-FEHLBERG ALGORITHM 5.3

      TO APPROXIMATE THE SOLUTION OF THE INITIAL VALUE PROBLEM:
                 Y' = F(T,Y), A<=T<=B, Y(A) = ALPHA,
      WITH LOCAL TRUNCATION ERROR WITHIN A GIVEN TOLERANCE.

      INPUT:   ENDPOINTS A,B; INITIAL CONDITION ALPHA; TOLERANCE TOL;
               MAXIMUM STEPSIZE HMAX; MINIMUM STEPSIZE HMIN.

      OUTPUT:  T, W, H WHERE W APPROXIMATES Y(T) AND STEPSIZE H WAS
               USED OR A MESSAGE THAT MINIMUM STEPSIZE WAS EXCEEDED.
}
var
   OUP : text;
   A,B,TOL,ALPHA,HMAX,HMIN,H,T,W,K1,K2,K3,K4,K5,K6,R,DELTA : real;
   FLAG,I,N : integer;
   OK : boolean;
   AA : char;
   NAME : string [ 30 ];
{  Change function F for a new problem.    }
function F ( T, Y : real ) : real;
   begin
      F := Y - T*T + 1.0
   end;
procedure INPUT;
begin
   writeln('This is the Runge-Kutta-Fehlberg Method.');
   OK := false;
   write ('Has the function F been defined? ');
   writeln ('Answer Y or N. ');
   readln ( AA );
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
               writeln ('Input tolerance ');
               readln ( TOL );
               if ( TOL <= 0.0 ) then
                  writeln ('Tolerance must be positive. ')
               else OK := true
            end;
         OK := false;
         while ( not OK ) do
            begin
               write ('Input minimum and maximum mesh spacing separated by ');
               writeln ('blank ');
               readln ( HMIN, HMAX );
               if ( HMIN < HMAX ) and ( HMIN > 0.0 ) then OK := true
               else
                  begin
                     write ('Minimum mesh spacing must be a positive real ');
                     writeln ('number and less than ');
                     writeln ('the maximum mesh spacing ')
                  end
            end
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
      writeln(OUP,'RUNGE-KUTTA-FEHLBERG METHOD');
      writeln(OUP);
      writeln ( OUP,'T(I)':12,'W(I)':12,'H':12,'R':12);
      writeln ( OUP )
   end;
begin
   INPUT;
   if OK then
      begin
         OUTPUT;
{     STEP 1                                                                   }
         H := HMAX;
         T := A;
         W := ALPHA;
         writeln ( OUP,T:12:7,W:12:7,'0':12,'0':12);
         OK := true;
{     STEP 2                                                                   }
         while ( ( T < B ) and OK ) do
            begin
{           STEP 3                                                             }
               K1 := H*F(T,W);
               K2 := H*F(T+H/4,W+K1/4);
               K3 := H*F(T+3*H/8,W+(3*K1+9*K2)/32);
               K4 := H*F(T+12*H/13,W+(1932*K1-7200*K2+7296*K3)/2197);
               K5 := H*F(T+H,W+439*K1/216-8*K2+3680*K3/513-845*K4/4104);
               K6 := H*F(T+H/2,W-8*K1/27+2*K2-3544*K3/2565
                         +1859*K4/4104-11*K5/40);
{           STEP 4                                                             }
               R := abs(K1/360-128*K3/4275-2197*K4/75240.0
                         +K5/50+2*K6/55)/H;
{           STEP 5                                                             }
               if ( R <= TOL ) then
                  begin
{                 STEP 6                                                       }
{                 APPROXIMATION ACCEPTED                                       }
                     T := T + H;
                     W := W+25*K1/216+1408*K3/2565+2197*K4/4104-K5/5;
{                 STEP 7                                                       }
                     writeln(OUP,T:12:7,W:12:7,H:12:7,R:12:7)
                  end;
{           STEP 8                                                             }
{           TO AVOID UNDERFLOW                                                 }
               if ( R > 1.0E-20 ) then
                  DELTA := 0.84 * exp( 0.25 * ln( TOL / R ) )
               else DELTA := 10.0;
{           STEP 9                                                             }
{           CALCULATE NEW H                                                    }
               if ( DELTA <= 0.1 ) then H := 0.1 * H
               else
                  if ( DELTA >= 4.0 ) then H := 4.0 * H
                  else H := DELTA * H;
{           STEP 10                                                            }
               if ( H > HMAX ) then H := HMAX;
{           STEP 11                                                            }
               if ( H < HMIN ) then OK := false
               else
                  if ( T + H > B ) then
                    if (abs(B-T) < TOL) then T := B
                    else H := B - T
            end;
         if ( not OK ) then writeln ( OUP, 'Minimal H exceeded ');
{     STEP 12                                                                  }
{     PROCESS IS COMPLETE                                                      }
         close ( OUP )
      end
end.
