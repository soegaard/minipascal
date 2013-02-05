program ALG102;
{  BROYDEN ALGORITHM 10.2

   To approximate the solution of the nonlinear system F(X) = 0
   given an initial approximation X.

   INPUT:   Number n of equations and unknowns; initial
            approximation X = (X(1),...,X(n)); tolerance TOL;
            maximum number of iterations N.

   OUTPUT:  Approximate solution X = (X(1),...,X(n)) or a message
            that the number of iterations was exceeded.
}
   const
      ZERO = 1.0E-20;
   type
      VEC = array [1 .. 10] of real;
   var
      A,B : array [1 .. 10, 1 .. 10] of real;
      X : array [1 .. 10] of real;
      U,V,S,Y,Z : VEC;
      SN,TOL,VV,ZN,P : real;
      N,NN,I,J,L,K,KK,FLAG : integer;
      OK : boolean;
      AA : char;
      OUP : text;
      NAME : string [ 30 ];
{     Change procedures F and PD for a new problem                     }
function F( I : integer ) : real;
   begin
      case I of
         1 : F := 3*X[1]-cos(X[2]*X[3])-0.5;
         2 : F := sqr(X[1])-81*sqr(X[2]+0.1)+sin(X[3])+1.06;
         3 : F := exp(-X[1]*X[2])+20*X[3]+(10*pi-3)/3
      end
   end;
{  PD represents the Jacobian J(X)  }
function PD( I,J : integer ) : real;
   begin
      case I of
         1 : case J of
                1 : PD := 3;
                2 : PD := X[3]*sin(X[2]*X[3]);
                3 : PD := X[2]*sin(X[2]*X[3])
             end;
         2 : case J of
                1 : PD := 2*X[1];
                2 : PD := -162*(X[2]+0.1);
                3 : PD := cos(X[3])
             end;
         3 : case J of
                1 : PD := -X[2]*exp(-X[1]*X[2]);
                2 : PD := -X[1]*exp(-X[1]*X[2]);
                3 : PD := 20
             end
      end
   end;
procedure INPUT;
   begin
      writeln('This is the Broyden Method for Nonlinear Systems.');
      OK := false;
      write ('Has the function F been defined and have the partial ');
      writeln ('derivatives been ');
      writeln ('defined as follows: '); writeln;
      writeln ('   1. function F( I:integer ) : real ');
      writeln ('      where I is the number of the component function; ');
      writeln; writeln ('   2. function PD( I,J : integer ) : real ');
      writeln ('      where I is the number of the component function ');
      writeln ('      and J is the number of the variable with respect ');
      writeln ('      to which partial differentiation is performed; ');
      writeln;
      writeln ('Answer Y or N. ');
      readln ( AA );
      if ( AA = 'Y' ) or ( AA = 'y' ) then
         begin
            while ( not OK ) do
               begin
                  writeln ('Input the number n of equations. ');
                  readln ( N );
                  if ( N >= 2 ) then OK := true
                  else writeln ('N must be an integer greater than 1. ')
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
         writeln ('The program will end so that the functions can be created.')
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
      writeln ('Select amount of output ');
      writeln ('1. Answer only ');
      writeln ('2. All intermediate approximations ');
      writeln ('Enter 1 or 2 ');
      readln (FLAG);
      writeln(OUP,'BROYDENS METHOD FOR NONLINEAR SYSTEMS');
      writeln(OUP);
      if FLAG = 2 then
         begin
            writeln(OUP,'Iteration, Approximation, Error')
         end
   end;
procedure MULT ( var SN : real; var V,S : VEC );
{  MULT computes S = -A*V and SN = 2-norm of S  }
   begin
      SN := 0.0;
      for I := 1 to N do
         begin
            S[I] := 0.0;
            for J := 1 to N do S[I] := S[I] - A[I,J] * V[J];
            SN := SN + S[I] * S[I]
         end;
      SN := sqrt( SN )
   end;
procedure INVERT;
{  Procedure INVERT finds the inverse, if possible, of the matrix A
   and returns it in A.  The procedure is called only once and uses
   the Gauss-Jordan method with Partial Pivoting.  }
   var
      I,J,I1,I2,K : integer;
      C : real;
   begin
      for I := 1 to N do
         begin
            for J := 1 to N do B[I,J] := 0.0;
            B[I,I] := 1.0
         end;
      I := 1;
      while (( I <= N) and OK ) do
         begin
            I1 := I + 1;
            I2 := I;
            if ( I <> N ) then
               begin
                  C := abs( A[I,I] );
                  for J := I1 to N do
                     if ( abs( A[J,I] ) > C ) then
                        begin
                           I2 := J;
                           C := abs( A[J,I] )
                        end;
                  if ( C <= ZERO ) then OK := false
                     else
                        begin
                           if ( I2 <> I ) then
                              for J := 1 to N do
                                 begin
                                    C := A[I,J];
                                    A[I,J] := A[I2,J];
                                    A[I2,J] := C;
                                    C := B[I,J];
                                    B[I,J] := B[I2,J];
                                    B[I2,J] := C
                                 end;
                         end
               end
            else if ( abs( A[N,N] ) <= ZERO ) then OK := false;
            if ( OK ) then
               for J := 1 to N do
                  begin
                     if ( J <> I ) then
                        begin
                            C := A[J,I] / A[I,I];
                            for K := 1 to N do
                               begin
                                  A[J,K] := A[J,K] - C * A[I,K];
                                  B[J,K] := B[J,K] - C * B[I,K]
                               end
                        end
                  end;
            I := I + 1
         end;
      if ( OK ) then
         for I := 1 to N do
            begin
               C := A[I,I];
               for J := 1 to N do A[I,J] := B[I,J] / C
            end
      else writeln ('Jacobian has no inverse ')
   end;
   begin
      INPUT;
      if ( OK ) then
         begin
            OUTPUT;
{           STEP 1                                                     }
{           A will hold the Jacobian for the initial approximation
            the subprogram PD computes the entries of the Jacobian     }
            for I := 1 to N do
               begin
                  for J := 1 to N do
                  begin
                     A[I,J] := PD( I, J );
                  end;
{                 Compute V = F( X(0) )
                  The subprogram F( I ) computes the Ith
                  component of F at X                                  }
                  V[I] := F( I );
               end;
{           STEP 2                                                     }
            INVERT;
{           INVERT finds the inverse of the N by N matrix A and
            returns it in A                                            }
            if ( OK ) then
               begin
{                 STEP 3                                               }
                  K := 2;
{                 NOTE: S = S(1)                                       }
                  MULT( SN, V, S );
{                 MULT computes the product S = -A * V
                  and the L2-norm SN of S                              }
                  for I := 1 to N do X[I] := X[I] + S[I];
                  If (FLAG = 2) then
                     begin
                        writeln(OUP,1);
                        for I := 1 to N do write(OUP,' ',X[I]);
                        writeln(OUP);
                     end;
{                 STEP 4                                               }
                  while (( K < NN) and OK ) do
                     begin
{                       STEP 5                                         }
{                       The vector W is not used since the
                        function F is evaluated component
                        by component                                   }
                        for I := 1 to N do
                           begin
                              VV := F( I );
                              Y[I] := VV - V[I];
                              V[I] := VV;
                           end;
{                          NOTE: V = F( X(K) ) AND Y = Y(K)            }
{                       STEP 6                                         }
                        MULT( ZN, Y, Z );
{                       NOTE : Z = -A(K-1)**-1 * Y(K)                  }
{                       STEP 7                                         }
                        P := 0.0;
{                       P WILL BE S(K)**T * A(K-1)**-1 * Y(K)          }
                        for I := 1 to N do
                           begin
                              P := P - S[I] * Z[I];
                           end;
{                       STEP 8                                         }
                        for I := 1 to N do
                           begin
                              U[I] := 0.0;
                              for J := 1 to N do U[I] := U[I]+S[J]*A[J,I];
                           end;
{                       STEP 9                                         }
                        for I := 1 to N do
                           for J := 1 to N do
                               A[I,J] := A[I,J] + (S[I]+Z[I])*U[J]/P;
{                       STEP 10                                        }
                        MULT( SN, V, S );
{                       NOTE: A = A(K)**-1 and
                        S = -A(K)**-1 * F( X(K) )                      }
{                       STEP 11                                        }
                        for I := 1 to N do X[I] := X[I] + S[I];
{                       NOTE: X = X(K+1)                               }
                        KK := K + 1;
                        if (FLAG = 2) then
                           begin
                              write(OUP,K:3);
                              for I := 1 to N do write(OUP,X[I]:14:10);
                              writeln(OUP);
                              writeln(OUP,SN:12)
                           end;
{                       STEP 12                                        }
                        if ( SN <= TOL ) then
                           begin
{                             procedure completed successfully         }
                              OK := false;
                              write (OUP,'Iteration number ',K);
                              writeln (OUP,' gives solution:');
                              writeln(OUP);
                              for I := 1 to N do write (OUP,X[I]:14:10);
                              writeln(OUP); writeln(OUP);
                              writeln(OUP,'to within tolerance ',TOL);
                              writeln(OUP);
                              writeln (OUP,'Process is complete ')
                           end
                        else
{                          STEP 13                                     }
                           K := KK
                     end;
                  if ( K >= NN ) then
{                    STEP 14                                           }
                     begin
                        write (OUP,'Procedure does not converge in ', NN );
                        writeln (OUP,' iterations ')
                     end
               end;
            close(OUP)
         end
   end.
