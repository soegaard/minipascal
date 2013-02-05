program ALG101;
{
   NEWTON'S METHOD FOR SYSTEMS ALGORITHM 10.1

   To approximate the solution of the nonlinear system F(X)=0 given
   an initial approximation X:

   INPUT:   Number n of equations and unknowns; initial approximation
            X=(X(1),...,X(n)); tolerance TOL; maximum number of
            iterations N.

   OUTPUT:  Approximate solution X=(X(1),...,X(n)) or a message
            that the number of iterations was exceeded.                }
   const
      ZERO = 1.0E-20;
   var
      A : array [1 .. 10, 1 .. 11] of real;
      X,Y : array [1 .. 10] of real;
      TOL,R : real;
      N,NN,I,J,K,FLAG : integer;
      OK : boolean;
      AA : char;
      OUP : text;
      NAME : string [ 30 ];
{     Change procedures F and P for a new problem                      }
function F( I : integer ) : real;
   begin
      case I of
         1 : F := 3*x[1] - cos(x[2]*x[3]) -0.5;
         2 : F := x[1]*x[1] - 81*sqr(x[2]+0.1) + sin(x[3]) + 1.06;
         3 : F := exp(-x[1]*x[2]) + 20*x[3] + (10*pi-3)/3
      end
   end;
{  P is the Jacobian J(X)    }
function P( I,J : integer ) : real;
   begin
      case I of
         1 : case J of
                1 : P := 3;
                2 : P := x[3]*sin(x[2]*x[3]);
                3 : P := x[2]*sin(x[2]*x[3])
             end;
         2 : case J of
                1 : P := 2*x[1];
                2 : P := -162*(x[2]+0.1);
                3 : P := cos(x[3])
             end;
         3 : case J of
                1 : P := -x[2]*exp(-x[1]*x[2]);
                2 : P := -x[1]*exp(-x[1]*x[2]);
                3 : P := 20
             end
      end
   end;
procedure INPUT;
   begin
      writeln('This is the Newton Method for Nonlinear Systems.');
      OK := false;
      write ('Has the function F been defined and have the partial ');
      writeln ('derivatives been ');
      writeln ('defined as follows: '); writeln;
      writeln ('   1. function F( I:integer ) : real ');
      writeln ('      where I is the number of the component function; ');
      writeln; writeln ('   2. function P( I,J : integer ) : real ');
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
            while ( not OK) do
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
      writeln(OUP,'NEWTONS METHOD FOR NONLINEAR SYSTEMS');
      writeln(OUP);
      if FLAG = 2 then
         begin
            writeln(OUP,'Iteration, Approximation, Error')
         end
   end;
procedure LINSYS;
{  Procedure LINSYS solves the linear system  J(X) Y = -F(X) using
   Gaussian elimination with Partial Pivoting   }
   var
      L,I,K,IR,IA,J,JA : integer;
      Z,C : real;
   begin
      K := N - 1;
      OK := true;
      I := 1;
      while ( ( OK ) and ( I <= K ) ) do
         begin
            Z := abs( A[I,I] );
            IR := I;
            IA := I + 1;
            for J := IA to N do
               if ( abs( A[J,I] ) > Z ) then
                  begin
                     IR := J;
                     Z := abs( A[J,I] )
                  end;
            if ( Z <= ZERO ) then OK := false
            else
               begin
                  if ( IR <> I ) then
                     for J := I to N + 1 do
                        begin
                           C := A[I,J];
                           A[I,J] := A[IR,J];
                           A[IR,J] := C
                        end;
                  for J := IA to N do
                     begin
                        C := A[J,I] / A[I,I];
                        if ( abs( C ) <= ZERO ) then C := 0.0;
                        for L := I to N + 1 do
                           A[J,L] := A[J,L] - C * A[I,L]
                     end
               end;
            I := I + 1
         end;
      if ( OK ) then
         begin
            if ( abs( A[N,N] ) <= ZERO ) then OK := false
            else
               begin
                  Y[N] := A[N,N + 1] / A[N,N];
                  for I := 1 to K do
                     begin
                        J := N - I;
                        JA := J + 1;
                        C := A[J,N + 1];
                        for L := JA to N do C := C - A[J,L] * Y[L];
                        Y[J] := C / A[J,J]
                     end
               end
         end;
      if ( not OK ) then writeln ('Linear system is singular ')
   end;
   begin
      INPUT;
      if ( OK ) then
         begin
            OUTPUT;
{           STEP 1                                                     }
            K := 1;
{           STEP 2                                                     }
            while ( ( OK ) and ( K <= NN ) ) do
               begin
{                 STEP 3                                               }
                  for I := 1 to N do
                     begin
                        for J := 1 to N do A[I,J] := P( I, J );
                        A[I,N + 1] := -F( I )
                     end;
{                 STEP 4                                               }
                  LINSYS;
                  if ( OK ) then
                     begin
{                       STEP 5                                         }
                        R := 0.0;
                        for I := 1 to N do
                           begin
                              if ( abs( Y[I] ) > R ) then R := abs( Y[I] );
                              X[I] := X[I] + Y[I]
                           end;
                        if (FLAG = 2) then
                           begin
                              write(OUP,K:3);
                              for I := 1 to N do write(OUP,X[I]:12:8);
                              writeln(OUP);
                              writeln(OUP,R:12)
                           end;
{                       STEP 6                                         }
                        if ( R < TOL ) then
                           begin
                              OK := false;
                              writeln(OUP,'Iteration ',K,' gives solution:');
                              writeln(OUP);
                              for I := 1 to N do write(OUP,X[I]:12:8);
                              writeln(OUP); writeln(OUP);
                              writeln(OUP,'to within tolerance ',TOL);
                           end
{                       STEP 7                                         }
                        else K := K + 1
                     end
               end;
            if ( K > NN ) then
               begin
{                 STEP 8                                               }
                  write (OUP,'Procedure does not converge in ', NN );
                  writeln (OUP,' iterations ');
               end;
            close(OUP)
         end
   end.
