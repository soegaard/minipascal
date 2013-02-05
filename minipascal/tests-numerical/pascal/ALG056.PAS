program ALG056;
{  EXTRAPOLATION ALGORITHM 5.6

   To approximate the solution of the initial value problem:
                 y' = f(t,y), a <= t <= b, y(a) = ALPHA,
   with local truncation error within a given tolerance:

   INPUT:   endpoints a,b; initial condition ALPHA; tolerance TOL;
            maximum stepsize HMAX; minimum stepsize HMIN.

   OUTPUT:  T, W, H where W approximates y(T) and stepsize H was
            used or a message that minimum stepsize was exceeded.
}
var
   Q : array [ 1..7, 1..7 ] of real;
   Y : array [ 1..8 ] of real;
   TOL,ALPHA,A,B,HMIN,HMAX,T0,W0,H,HK,W2,W3,T,W1,V : real;
   ZERO : real;
   NK : array [ 1..8 ] of integer;
   FLAG,P,I,J,K,NFLAG,M,N : integer;
   OK,DONE : boolean;
   NAME : string [ 30 ];
   OUP : text;
   AA : char;
{  Change function F for a new problem                                 }
function F (T, Y: real ) : real;
   begin
      F := Y - T*T + 1.0
   end;
procedure INPUT;
   begin
      writeln('This is Gragg Extrapolation');
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
                    writeln ('Left endpoint must be less than right endpoint.')
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
      writeln ( OUP, 'GRAGG EXTRAPOLATION');
      writeln (OUP);
      writeln ( OUP, 'T':5, 'W':20, 'H':14, 'K':3 );
      writeln ( OUP )
   end;
begin
   INPUT;
   if (OK) then
      begin
         OUTPUT;
         ZERO := 1.0E-20;
{        STEP 1                                                        }
         NK[1] := 2;
         NK[2] := 4;
         for J := 1 to 3 do
            begin
                I := 2*J;
                NK[I+1] := 3 * NK[I] div 2;
                NK[I+2] := 2 * NK[I];
            end;
{        STEP 2                                                        }
         T0 := A;
         W0 := ALPHA;
         H := HMAX;
{        DONE is used in place of FLAG to exit the loop in Step 4.     }
         DONE := false;
{        STEP 3                                                        }
         for I := 1 to 7 do
            for J := 1 to I do Q[I,J] := sqr( NK[I+1] * 1.0 / NK[J] );
{        STEP 4                                                        }
         while ( not DONE ) do
            begin
{              STEP 5                                                  }
               K := 1;
{              when desired accuracy achieved, NFLAG is set to 1       }
               NFLAG := 0;
{              STEP 6                                                  }
               while ( ( K <= 8 ) and ( NFLAG = 0 ) ) do
                  begin
{                    STEP 7                                            }
                     HK := H / NK[K];
                     T := T0;
                     W2 := W0;
{                    Euler first step                                  }
                     W3 := W2 + HK * F( T, W2 );
                     T := T0 + HK;
{                    STEP 8                                            }
                     M := NK[K] - 1;
                     for J := 1 to M do
                        begin
                           W1 := W2;
                           W2 := W3;
{                          midpoint method                             }
                           W3 := W1 + 2.0 * HK * F( T, W2 );
                           T := T0 + ( J + 1 ) * HK
                        end;
{                    STEP 9                                            }
{                    Endpoint correction to compute Y(K,1)             }
                     Y[K] := ( W3 + W2 + HK * F( T, W3 ) ) / 2.0;
{                    STEP 10                                           }
{                    NOTE: Y(K-1)=Y(K-1,1),Y(K-2)=Y(K-1,2),...,
                     Y(1)=Y(K-1,K-1) since only previous row of the
                     table is saved                                    }
                     if ( K >= 2 ) then
                        begin
{                          STEP 11                                     }
                           J := K;
{                          Save Y[K-1,K-1]                             }
                           V := Y[1];
{                          STEP 12                                     }
                           while ( J >= 2 ) do
                              begin
{                                extrapolation to compute
                                         Y(J-1) = Y(K,K-J+2)           }
                                 Y[J-1] := Y[J] + ( Y[J] - Y[J-1] ) /
                                           ( Q[K-1,J-1] - 1.0 );
                                 J := J - 1
                              end;
{                          STEP 13                                     }
                           if ( abs( Y[1] - V ) <= TOL ) then NFLAG := 1
{                          Y(1) accepted as new w                      }
                        end;
{                    STEP 14                                           }
                     K := K + 1
                  end;
{              STEP 15                                                 }
               K := K - 1;
{              STEP 16                                                 }
               if ( NFLAG = 0 ) then
                  begin
{                    STEP 17                                           }
{                    new value for w rejected, decrease H              }
                     H := H / 2.0;
{                    STEP 18                                           }
                     if ( H < HMIN ) then
                        begin
                           writeln( OUP , 'HMIN exceeded ');
                           DONE := true
                        end
                  end
               else
                  begin
{                    STEP 19                                           }
{                    new value for w accepted                          }
                     W0 := Y[1];
                     T0 := T0 + H;
                     writeln (OUP,T0:5:2,W0:20:16,'  ',H:12,K:3);
{                    STEP 20                                                      }
{                    increase H if possible                            }
                     if ( abs( T0 - B ) < ZERO ) then DONE := true
                     else if ( T0 + H > B ) then H := B - T0
                        else if ( K <= 3 ) then H := 2.0 * H;
                     if ( H > HMAX ) then H := H / 2.0
                  end
            end;
{        STEP 21                                                       }
         close ( OUP )
      end
end.
