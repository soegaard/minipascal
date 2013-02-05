program ALG055;
{  ADAMS VARIABLE STEP-SIZE PREDICTOR-CORRECTOR ALGORITHM 5.5

   To approximate the solution of the initial value problem

          y' = f( t, y ), a <= t <= b, y(a) = ALPHA,

    with local truncation error within a given tolerance:

    INPUT:   endpoints a, b; initial condition ALPHA; tolerance TOL;
             maximum step size HMAX; minimum step size HMIN.

    OUTPUT:  I, T(I), W(I), H where at the Ith step W(I) approximates
             y(T(I)) and step size H was used or a message that the
             minimum step size was exceeded.
}
var
   T,W : array[1..100] of real;
   A,B,ALPHA,TOL,HMAX,HMIN,H,TT,WP,WC,SIG,Q : real;
   FLAG,KK,NFLAG,MFLAG,I,K,J : integer;
   DONE,OK : boolean;
   NAME : string[30];
   OUP : text;
   AA : char;
{  Change function F for a new problem                                 }
function F( T, Y : real ) : real;
   begin
      F := Y - T*T + 1.0
   end;
procedure INPUT;
   begin
      writeln('This is the Adams Variable Step-size');
      writeln('Predictor-Corrector Method');
      writeln('Has the function F been created in the program immediately');
      writeln('preceding the INPUT procedure?  Enter Y or N.');
      readln(AA);
      if ((AA = 'Y') or (AA = 'y')) then
         begin
            OK := false;
            while ( not OK ) do
               begin
                  writeln('Input left and right endpoints separated by blank.');
                  readln ( A, B );
                  if ( A >= B ) then
                    writeln('Left endpoint must be less than right endpoint.')
                  else OK := true
               end;
            OK := false;
            writeln ('Input the initial condition. ');
            readln ( ALPHA );
            while ( not OK ) do
               begin
                  writeln ('Input tolerance. ');
                  readln ( TOL );
                  if ( TOL <= 0.0 ) then
                     writeln ('Tolerance must be positive. ')
                  else OK := true
               end;
            OK := false;
            while ( not OK ) do
               begin
                  write ('Input minimum and maximum mesh spacing  ');
                  writeln ('separated by a blank. ');
                  readln ( HMIN, HMAX );
                  if ( HMIN < HMAX ) and ( HMIN > 0.0 ) then OK := true
                  else
                     begin
                        write ('Minimum mesh spacing must be a  ');
                        writeln ('positive real number and less than ');
                        writeln ('the maximum mesh spacing. ')
                     end
               end
         end
      else
         begin
            writeln('The program will end so that F can be created.');
            OK := false
         end
   end;
procedure OUTPUT;
   begin
      writeln ('Choice of output method: ');
      writeln ('1. Output to screen ');
      writeln ('2. Output to text file ');
      writeln ('Please enter 1 or 2. ');
      readln ( FLAG );
      if ( FLAG = 2 ) then
         begin
            writeln ('Input the file name in the form - drive:name.ext, ');
            writeln('for example:   A:OUTPUT.DTA');
            readln ( NAME );
            assign ( OUP, NAME )
         end
      else assign ( OUP, 'CON' );
      rewrite ( OUP );
      writeln ( OUP,'ADAMS VARIABLE STEP-SIZE PREDICTOR CORRECTOR METHOD');
      writeln ( OUP );
      writeln ( OUP,'t':12,'w':12,'h':12,'sigma':12)
   end;
procedure RK4( H,X,Y : real; var T,W : real );
   var
      K1, K2, K3, K4 : real;
   begin
{     STEP 1                                                           }
      K1 := H * F( X, Y );
      K2 := H * F( X + 0.5 * H, Y + 0.5 * K1 );
      K3 := H * F( X + 0.5 * H, Y + 0.5 * K2 );
      K4 := H * F( X + H, Y + K3 );
      W := Y + ( K1 + 2.0 * ( K2 + K3 ) + K4 ) / 6.0;
      T := X + H
   end;
begin
   INPUT;
   if (OK) then
      begin
         OUTPUT;
{        STEP 2                                                        }
{        subscripts are shifted to avoid zero subscripts               }
         T[1] := A;
         W[1] := ALPHA;
         H := HMAX;
{        OK is used in place of FLAG to exit the loop in step 4        }
         OK := true;
{        DONE is used in place of LAST to indicate when the last value
         is calculated.                                                }
         DONE := false;
{        STEP 3                                                        }
         for KK := 1 to 3 do
            RK4( H, T[KK], W[KK], T[KK + 1], W[KK + 1] );
{        NFLAG indicates computation from RK4                          }
         NFLAG := 1;
         I := 5;
{        use TT in place of t                                          }
         TT := T[4] + H;
{        STEP 4                                                        }
         while ( not DONE ) do
            begin
{              STEP 5                                                  }
{              predict W(I)                                            }
               WP := W[I-1]+H*(55.0*F(T[I-1],W[I-1])-59.0*
                     F(T[I-2],W[I-2])+37.0*F(T[I-3],W[I-3])-
                     9.0*F(T[I-4],W[I-4]))/24.0;
{              correct W(I)                                            }
               WC := W[I-1]+H*(9.0*F(TT,WP)+19.0*
                     F(T[I-1],W[I-1])-5.0*F(T[I-2],W[I-2])+
                     F(T[I-3],W[I-3]))/24.0;
               SIG := 19.0*abs(WC-WP)/(270.0*H);
{              STEP 6                                                  }
               if ( SIG <= TOL ) then
                  begin
{                    STEP 7                                            }
{                    result accepted                                   }
                     W[I] := WC;
                     T[I] := TT;
{                    STEP 8                                            }
                     if ( NFLAG = 1 ) then
                        begin
                           K := I - 3;
                           KK := I - 1;
{                          Previous results are also to be accepted.   }
                           for J := K to KK do
                              writeln(OUP,T[J]:12:8,W[J]:12:8,H:12:8,SIG:12:8);
                           writeln(OUP,T[I]:12:8,W[I]:12:8,H:12:8,SIG:12:8)
                        end
                     else
                        begin
{                          Previous results were already accepted.     }
                           writeln(OUP,T[I]:12:8,W[I]:12:8,H:12:8,SIG:12:8);
                        end;
{                    STEP 9                                            }
                     if ( not OK ) then DONE := true
                     else
                        begin
{                          STEP 10                                     }
                           I := I + 1;
                           NFLAG := 0;
{                          STEP 11                                     }
                           if ((SIG <= 0.1*TOL) or (T[I-1]+H > B)) then
                              begin
{                             Increase H if there is more accuracy than
                              is required or decrease H to include b as
                              a mesh point                             }
{                                STEP 12                               }
{                                to avoid underflow                    }
                                 if (SIG <= 1.0E-20) then Q := 4.0
                                 else Q := exp(0.25*ln(0.5*TOL/SIG));
{                                STEP 13                               }
                                 if (Q > 4.0) then H := 4.0*H
                                 else H := Q * H;
{                                STEP 14                               }
                                 if ( H > HMAX ) then H := HMAX;
{                                STEP 15                               }
                                 if (T[I-1]+4.0*H > B) then
                                    begin
                                       H := 0.25*(B-T[I-1]);
                                       if (H < TOL)
                                          then DONE := true;
                                       OK := false
                                    end;
{                                STEP 16                               }
                                 for KK := I - 1 to I + 2 do
                                    RK4(H,T[KK],W[KK],T[KK+1],W[KK+1]);
                                 NFLAG := 1;
                                 I := I + 3
                              end
                        end
                  end
               else
                  begin
{                    STEP 17                                           }
{                    False branch for Step 6 - result is rejected.     }
                     Q := exp( 0.25 * ln( 0.5 * TOL / SIG ) );
{                    STEP 18                                           }
                     if (Q < 0.1) then H := 0.1 * H
                     else H := Q * H;
{                    STEP 19                                           }
                     if ( H < HMIN ) then
                        begin
                           writeln ( OUP, 'HMIN exceeded ');
                           DONE := true
                        end
                     else
                        begin
                           if (T[I-1]+4.0*H > B) then H := 0.25*(B-T[I-1]);
                           if ( NFLAG = 1 ) then I := I - 3;
                           for KK := I - 1 to I + 2 do
                              RK4(H,T[KK],W[KK],T[KK+1],W[KK+1]);
                           I := I + 3;
                           NFLAG := 1
                        end
                  end;
{              STEP 20                                                 }
               TT := T[I-1] + H
            end;
{        STEP 21                                                       }
         close ( OUP )
      end
end.
