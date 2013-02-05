program ALG103;
{  STEEPEST DESCENT ALGORITHM 10.3

   To approximate a solution P to the minimization problem
                  G(P) = MIN( G(X) : X in R(n) )
   given an initial approximation X:

   INPUT:   Number n of variables; initial approximation X;
            tolerance TOL; maximum number of iterations N.

   OUTPUT:  Approximate solution X or a message of failure.
}
   const
      ZERO = 1.0E-20;
   type
      VEC = array [ 1 .. 10] of real;
   var
      X,Z,C : VEC;
      G,A : array [1 .. 4] of real;
      Z0,X0,G0,H1,H2,H3,A0,TOL : real;
      N,NN,K,I,FLAG1 : integer;
      FLAG,OK : boolean;
      AA : char;
      OUP : text;
      NAME : string [ 30 ];
{     Change procedures CF and P for a new problem                     }
function CF ( I : integer; var X : VEC ) : real;
   begin
      case I of
         1 : CF := 3*x[1]-cos(x[2]*x[3]) - 0.5;
         2 : CF := sqr(x[1])-81*sqr(x[2]+0.1)+sin(x[3])+1.06;
         3 : CF := exp(-X[1]*x[2])+20*X[3] + (10*pi-3)/3
      end
   end;
function P ( I : integer; var X : VEC ) : real;
   begin
      case I of
         1 : P := 2 * ( 3 ) * CF( 1, X )
                  + 2 * ( 2*X[1] ) * CF( 2, X )
                  + 2 * ( -X[2]*exp(-X[1]*X[2]) ) * CF( 3, X );
         2 : P := 2 * ( X[3]*sin(x[2]*X[3]) ) * CF( 1, X )
                  + 2 * ( -162*(x[2]+0.1) ) * CF( 2, X )
                  + 2 * ( -X[1]*exp(-X[1]*X[2]) ) * CF( 3, X );
         3 : P := 2 * ( X[2]*sin(X[2]*X[3]) ) * CF( 1, X )
                  + 2 * ( cos(x[3]) ) * CF( 2, X )
                  + 2 * ( 20 ) * CF( 3, X )
      end
   end;
function F ( var X : VEC ) : real;
   var
      D : real;
      I : integer;
   begin
      D := 0.0;
      for I := 1 to N do D := D + sqr( CF( I, X ) );
      F := D
   end;
procedure INPUT;
   begin
      writeln('This is the Steepest Descent Method.');
      OK := false;
      writeln;
      writeln('NOTE THAT THE FUNCTION DEFINITIONS ARE VERY COMPLICATED');
      writeln ('Have the functions been set up as follows: '); writeln;
      writeln ('   1. F( var X : VEC ) : real ');
      writeln ('        which is the function to be minimized ( function G ) ');
      writeln ('        or the sum of the squares of the component');
      writeln ('        functions in a nonlinear system as in the example; ');
      writeln;
      writeln ('   2. CF( I : integer; var X : VEC ) : real ');
      writeln ('        which is the Ith component function of a nonlinear ');
      writeln ('        system - not used for simple minimization problems ');
      writeln;
      writeln ('   3. P( I : integer; var X : VEC ) : real ');
      writeln ('        which is the partial derivative of F with respect ');
      writeln ('        to the Ith variable '); writeln;
      writeln ('Answer Y or N. ');  writeln;
      readln ( AA );
      if ( AA = 'Y' ) or ( AA = 'y' ) then
         begin
            while ( not OK ) do
               begin
                  writeln ('Input the number n of equations. ');
                  readln ( N );
                  if ( N >= 2 ) then OK := true
                  else writeln ('n must be an integer greater than 1. ')
               end;
            OK := false;
            while ( not OK ) do
               begin
                  writeln ('Input the Tolerance. ');
                  readln ( TOL );
                  if ( TOL > 0.0 ) then OK := true
                  else writeln ('Tolerance must be positive. ')
               end;
            OK := false;
            while ( not OK ) do
               begin
                  writeln ('Input the maximum number of iterations. ');
                  readln ( NN );
                  if ( NN > 0 ) then OK := true
                  else writeln ('Must be a positive integer. ')
               end;
            for I := 1 to N do
               begin
                  writeln ('Input initial approximation X(', I, ').' );
                  readln ( X[I] )
               end
         end
      else
         writeln ('The program will end so that the functions can be defined.')
   end;
procedure OUTPUT;
   begin
      writeln ('Select output destination ');
      writeln ('1. Screen ');
      writeln ('2. Text file ');
      writeln ('Enter 1 or 2 ');
      readln ( FLAG1 );
      if ( FLAG1 = 2 ) then
         begin
            write ('Input the file name in the form - ');
            writeln ('drive:name.ext ');
            writeln ('for example:   A:OUTPUT.DTA ');
            readln ( NAME );
            assign ( OUP, NAME )
         end
      else assign ( OUP, 'CON');
      rewrite ( OUP );
      writeln ('Select amount of output ');
      writeln ('1. Answer only ');
      writeln ('2. All intermediate approximations ');
      writeln ('Enter 1 or 2 ');
      readln (FLAG1);
      writeln(OUP,'STEEPEST DESCENT METHOD FOR NONLINEAR SYSTEMS');
      writeln(OUP);
      if FLAG1 = 2 then
         begin
            writeln(OUP,'Iteration, Approximation')
         end
   end;
begin
   INPUT;
   if ( OK ) then
      begin
         OUTPUT;
{        STEP 1                                                        }
         K := 1;
{        STEP 2                                                        }
         while ( ( OK ) and ( K <= NN ) ) do
            begin
{              STEP 3                                                  }
               G[1] := F( X );
               Z0 := 0.0;
               for I := 1 to N do
                  begin
                     Z[I] := P( I, X );
                     Z0 := Z0 + sqr( Z[I] )
                  end;
               Z0 := sqrt( Z0 );
{              STEP 4                                                  }
               if ( Z0 <= ZERO ) then
                  begin
                     OK := false;
                     writeln (OUP,'Zero qradient - may have a minimum ');
                  end
               else
                  begin
{                    STEP 5                                            }
                     for I := 1 to N do Z[I] := Z[I] / Z0;
                     A[1] := 0.0;
                     X0 := 1.0;
                     for I := 1 to N do C[I] := X[I] - X0 * Z[I];
                     G0 := F( C );
{                    STEP 6                                            }
                     FLAG := true;
                     If (G0 < G[1]) then FLAG := false;
                     while ( FLAG and OK ) do
                        begin
{                          STEPS 7 AND 8                               }
                           X0 := 0.5 * X0;
                           if ( X0 <= ZERO ) then
                              begin
                                 OK := false;
                                 writeln (OUP,'No likely improvement - may ');
                                 writeln (OUP,'have a minimum ');
                              end
                           else
                              begin
                                 for I := 1 to N do C[I] := X[I]-X0*Z[I];
                                 G0 := F( C );
                              end;
                           if G0 < G[1] then FLAG := false;
                        end;
                     if ( OK ) then
                        begin
                           A[3] := X0;
                           G[3] := G0;
{                          STEP 9                                      }
                           X0 := 0.5 * X0;
                           for I := 1 to N do C[I] := X[I]-X0*Z[I];
                           A[2] := X0;
                           G[2] := F( C );
{                          STEP 10                                     }
                           H1 := (G[2]-G[1])/(A[2]-A[1]);
                           H2 := (G[3]-G[2])/(A[3]-A[2]);
                           H3 := (H2-H1)/(A[3]-A[1]);
{                          STEP 11                                     }
                           X0 := 0.5*(A[1]+A[2]-H1/H3);
                           for I := 1 to N do C[I] := X[I]-X0*Z[I];
                           G0 := F( C );
{                          STEP 12                                     }
                           A0 := X0;
                           for I := 1 to N do
                              if ( abs( G[I] ) < abs( G0 ) ) then
                                 begin
                                    A0 := A[I];
                                    G0 := G[I]
                                 end;
                              if ( abs( A0 ) <= ZERO ) then
                                 begin
                                    OK := false;
                                    writeln (OUP,'No change likely ');
                                    write (OUP,'- probably rounding error ');
                                    writeln (OUP,'problems ');
                                 end
                              else
                                 begin
{                                   STEP 13                            }
                                    for I := 1 to N do
                                          X[I] := X[I]-A0*Z[I];
{                                   STEP 14                            }
                                    if (FLAG1 = 2) then
                                      begin
                                         write(OUP,K:3);
                                         for I := 1 to N do
                                            write(OUP,X[I]:12:8);
                                         writeln(OUP);
                                      end;
                                    if ((abs(G0) < TOL) or
                                       (abs(G0-G[1]) < TOL)) then
                                       begin
                                          OK := false;
                                          writeln(OUP,'Iteration number ',K);
                                          writeln(OUP,'gives solution');
                                          writeln(OUP);
                                          for I := 1 to N do
                                              write(OUP,X[I]:12:8);
                                          writeln(OUP); writeln(OUP);
                                          writeln(OUP,'to within ',TOL);
                                          writeln(OUP);
                                          writeln (OUP,'Process is complete ');
                                       end
                                    else
{                                      STEP 15                         }
                                       K := K + 1
                                 end
                           end
                        end
                  end;
               if ( K > NN ) then
                  begin
{                    STEP 16                                           }
                     writeln (OUP,'Process does not converge in ',NN );
                     write (OUP,' iterations ')
                  end;
               close(OUP)
            end
   end.
