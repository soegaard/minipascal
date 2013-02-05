program ALG043;
{  ADAPTIVE QUADRATURE ALGORITHM 4.3

   To approximate I = integral( ( f(x) dx ) ) from a to b to within
   a given tolerance TOL:

   INPUT:   endpoints a, b; tolerance TOL; limit N to number of levels

   OUTPUT:  approximation APP or message that N is exceeded.
}
var
   TOL,A,H,FA,FC,FB,S : array [1..20] of real;
   V : array [1..7] of real;
   L : array [1..20] of integer;
   AA,BB,EPS,APP,FD,FE,S1,S2  : real;
   CNT,N,I,LEV : integer;
   OK : boolean;
   AB : char;
{  Change function F for a new problem                                 }
function F ( X : real ) : real;
   begin
      CNT := CNT+1;
      F := 100.0 / ( X * X ) * sin( 10.0 / X )
   end;
procedure INPUT;
   begin
      writeln('This is Adaptive Quadrature with Simpsons Method.');
      writeln(' ');
      write ('Has the function F been created in the program ');
      writeln ('immediately preceding ');
      writeln ('the INPUT procedure? ');
      writeln ('Enter Y or N ');
      readln ( AB );
      if ( AB = 'Y' ) or ( AB = 'y' ) then
         begin
            OK := false;
            while ( not OK ) do
               begin
                  write ('Input lower limit of integration and upper limit of ');
                  writeln ('integration ');
                  writeln ('separated by a blank ');
                  readln ( AA, BB );
                  if ( AA >= BB ) then
                     writeln ('Lower limit must be less than upper limit. ')
                  else OK := true
               end;
            OK := false;
            while ( not OK ) do
               begin
                  writeln ('Input tolerance ');
                  readln ( EPS );
                  if ( EPS > 0.0 ) then OK := true
                  else writeln ('Tolerance must be positive. ')
               end;
            OK := false;
            while ( not OK ) do
               begin
                  writeln ('Input the maximum number of levels ');
                  readln(N);
                  if ( N > 0 ) then OK := true
                  else writeln ('Number must be positive. ')
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
      writeln;
      writeln ('The integral of F from ',AA:12:8,' to ',BB:12:8,' is ');
      writeln ( APP:12:8,' to within ',EPS:14 );
      writeln('The number of function evaluations is: ',CNT)
   end;
begin
   INPUT;
   if (OK) then
      begin
         CNT := 0;
         OK := true;
{        STEP 1                                                        }
         APP := 0.0;
         I := 1;
         TOL[I] := 10.0 * EPS;
         A[I] := AA;
         H[I] := 0.5 * ( BB - AA );
         FA[I] := F( AA );
         FC[I] := F( AA + H[I] );
         FB[I] := F( BB );
{        Approximation from Simpson's method for entire interval       }
         S[I] := H[I] * ( FA[I] + 4.0 * FC[I] + FB[I] ) / 3.0;
         L[I] := 1;
{        STEP 2                                                        }
         while ( ( I > 0 ) and OK ) do
            begin
{           STEP 3                                                     }
               FD := F( A[I] + 0.5 * H[I] );
               FE := F( A[I] + 1.5 * H[I] );
{              Approximations from Simpson's method for halves of
               subintervals                                            }
               S1 := H[I] * ( FA[I] + 4.0 * FD + FC[I] ) / 6.0;
               S2 := H[I] * ( FC[I] + 4.0 * FE + FB[I] ) / 6.0;
{              Save data at this level                                 }
               V[1] := A[I];
               V[2] := FA[I];
               V[3] := FC[I];
               V[4] := FB[I];
               V[5] := H[I];
               V[6] := TOL[I];
               V[7] := S[I];
               LEV := L[I];
{              STEP 4                                                  }
{              Delete the level.                                       }
               I := I - 1;
{              STEP 5                                                  }
               if ( abs( S1 + S2 - V[7] ) < V[6] ) then
                  APP := APP + ( S1 + S2 )
               else
                  begin
                     if ( LEV >= N ) then OK := false  { Procedure fails }
                     else
                        begin
{                          Add one level                               }
{                          Data for right half subinterval             }
                           I := I + 1;
                           A[I] := V[1] + V[5];
                           FA[I] := V[3];
                           FC[I] := FE;
                           FB[I] := V[4];
                           H[I] := 0.5 * V[5];
                           TOL[I] := 0.5 * V[6];
                           S[I] := S2;
                           L[I] := LEV + 1;
{                          Data for left half subinterval              }
                           I := I + 1;
                           A[I] := V[1];
                           FA[I] := V[2];
                           FC[I] := FD;
                           FB[I] := V[3];
                           H[I] := H[I-1];
                           TOL[I] := TOL[I-1];
                           S[I] := S1;
                           L[I] := L[I-1]
                        end
                  end
            end;
         if ( not OK ) then
            begin
               writeln ('Level exceeded.  Method failed to give accurate ');
               writeln ('approximation.');
            end
         else
{           STEP 6                                                     }
            OUTPUT
      end
  end.


