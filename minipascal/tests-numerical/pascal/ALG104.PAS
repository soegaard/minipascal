program ALG104;
{
   CONTINUATION METHOD FOR SYSTEMS ALGORITHM 10.4

   To approximate the solution of the nonlinear system F(X)=0 given
   an initial approximation X:

   INPUT:   Number n of equations and unknowns; initial approximation
            X=(X(1),...,X(n)); number of Runge-Kutta
            iterations N.

   OUTPUT:  Approximate solution X=(X(1),...,X(n)).                    }
   const
      ZERO = 1.0E-20;
   var
      A : array [1 .. 10, 1 .. 11] of real;
      K1 : array [1..4,1..10] of real;
      x,Y,B,X1 : array [1 .. 10] of real;
      MM : array [1..4] of real;
      H : real;
      N,I,J,K,FLAG,N1,KK : integer;
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
      writeln('This is the Continuation Method for Nonlinear Systems.');
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
            while ( not OK ) do
               begin
                  writeln ('Input the number of Runge-Kutta iterations. ');
                  readln ( N1 );
                  if ( N1 > 0 ) then OK := true
                  else writeln ('Must be a positive integer. ')
               end;
            for I := 1 to N do
               begin
                  writeln ('Input initial approximation X(', I, ').' );
                  readln ( X1[I] )
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
      writeln(OUP,'CONTINUATION METHOD FOR NONLINEAR SYSTEMS');
      writeln(OUP);
      writeln(OUP,'Iteration, Approximation')
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
            H := 1.0/N1;
            K := 1;
            MM[1] := 0.5; MM[2] := 0.5; MM[3] := 1.0; MM[4] := 0.0;
            for I := 1 to N do
                 x[I] := X1[I];
            for I := 1 to N do B[I] := -H*F(I);
{           STEP 2                                                   }
            while ( ( OK ) and ( K <= N1 ) ) do
               begin
{                 STEPS 3 - 6                                         }
                  for I := 1 to N do
                     begin
                       x[I] := X1[I];
                     end;
                  KK := 1;
                  while ( (KK <= 4) and OK) do
                     begin
                       for I := 1 to N do
                         begin
                           for J := 1 to N do A[I,J] := P( I, J );
                           A[I,N + 1] := B[I];
                         end;
{                 STEP 4                                               }
                     LINSYS;
                     if ( OK ) then
                        begin
{                       STEP 5                                         }
                           for I := 1 to N do
                              begin
                                 K1[KK,I] := Y[I];
                                 x[I] := X1[I] + mm[KK]*K1[KK,I]
                              end;
                        end;
                        KK := KK + 1
                  end;
              { STEP 7 }
                  if (OK)  then
                     begin
                     writeln(OUP,'Iteration',K);
                     for I := 1 to N do
                        X1[I]:=X1[I]+(K1[1,I]+2*K1[2,I]+2*K1[3,I]+K1[4,I])/6;
                        for I := 1 to N do write(OUP,X1[I]:12:8);
                        writeln(OUP);
                     end;
                  K := K + 1;
            end;
            close(OUP);
         end
   end.
