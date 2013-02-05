program ALG074;
{  ITERATIVE REFINEMENT ALGORITHM 7.4

   To approximate the solution to the linear system Ax=b when A is
   suspected to be ill-conditioned:

   INPUT:  The number of equations and unknowns n; the entries
           A(i,j), 1<=i, j<=n, of the matrix A; the entries b(i),
           1<=i<=n, of the inhomogeneous term b; the maximum number
           of iterations N.

   OUTPUT: The approximation XX(1),...,XX(N) or a message that the
           number of iterations was exceeded.                          }
const
   ZERO = 1.0E-20;
var
   INP,OUP : text;
   B,A : array [ 1..10, 1..11 ] of real;
   X,XX,R : array [ 1..10 ] of real;
   NROW : array [1..10] of integer;
   ERR1,TEMP,S,C,TOL,COND,XXMAX,YMAX : real;
   RND,IS,FLAG,N,M,I,J,NN,K,L,KK,LL,D,I1,J1 : integer;
   OK : boolean;
   A1 : char;
   NAME : string [ 30 ];
procedure INPUT;
   begin
      writeln('This is the Iterative Refinement Method.');
      writeln ('The array will be input from a text file in the order ');
      writeln('A(1,1), A(1,2), ..., A(1,N+1), A(2,1), A(2,2), ..., A(2,N+1),');
      writeln ('..., A(N,1), A(N,2), ..., A(N,N+1) ');
      writeln ('Place as many entries as desired on each line, but separate ');
      writeln ('entries with at least one blank. ');
      writeln; writeln;
      writeln ('Has the input file been created? - enter Y or N. ');
      readln ( A1 );
      OK := false;
      if ( A1 = 'Y' ) or ( A1 = 'y' ) then
         begin
            writeln ('Input the file name in the form - drive:name.ext, ');
            writeln ('for example: A:DATA.DTA ');
            readln ( NAME );
            assign ( INP , NAME );
            reset ( INP );
            OK := false;
            while ( not OK ) do
               begin
                  writeln ('Input the number of equations - an integer. ');
                  readln ( N );
                  if ( N > 0 ) then
                     begin
                        for I := 1 to N do
                           for J := 1 to N + 1 do read ( INP , A[I,J] );
                        OK := true;
                        close ( INP )
                     end
                  else writeln ('The number must be a positive integer. ')
               end;
{           NN is used for the maximum number of iterations            }
            OK := false;
            while (not OK) do
               begin
                  writeln('Input the maximum number of iterations.');
                  readln(NN);
                  if (NN > 0) then OK := true
                  else writeln('The number must be a positive integer.')
               end;
            writeln('Choice of rounding or chopping:');
            writeln('1. Rounding');
            writeln('2. Chopping');
            writeln('Please enter 1 or 2');
            readln(RND);
            OK := false;
            while (not OK) do
               begin
                  writeln('Input number of digits D <= 8 of rounding.');
                  readln(D);
                  if (D > 0) then OK := true
                  else writeln('D must be a positive integer.')
               end;
            OK := false;
            while (not OK) do
               begin
                  writeln('Input tolerance, which is usually 10 ** (-D).');
                  readln(TOL);
                  if (TOL > 0.0) then OK := true
                  else writeln('Tolerance must be positive.')
               end
         end
      else writeln ('The program will end so the input file can be created. ')
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
               assign ( OUP , NAME )
            end
         else  assign ( OUP , 'CON' );
         rewrite ( OUP );
         writeln(OUP,'ITERATIVE REFINEMENT METHOD');
         writeln(OUP)
      end;
function CHIP (R : integer; X : real) : real;
{ The function chip rounds or chops the number x to r digits.          }
   var
      SL,Z,Z1,Z2,Y,TEMP,TEMP1,TEMP2,Z3,Z4,TN : real;
      I, R1 : integer;
      OK : boolean;
   begin
      if ( abs(X) < ZERO ) then Z := 0.0
      else
         begin
            I := 0;
            Y := X;
            OK := true;
            Z1 := exp(R*ln(10.0));
            Z2 := exp((R-1.0)*ln(10.0));
            if ( abs(X) >= Z1 ) then
               begin
                  while (OK) do
                     begin
                        Y := Y/10.0;
                        I := I+1;
                        if ( abs(Y) < Z1 ) then OK := false
                     end
               end
            else
               if ( abs(X) < Z2) then
                  begin
                     while (OK) do
                        begin
                           Y := Y*10.0;
                           I := I - 1;
                           if (abs(Y) >= Z2) then OK := false
                        end
                  end;
             SL := exp(-R*ln(10.0));
             if RND = 1 then
                begin
                   if (Y >= 0.0) then
                       TEMP := Y+0.5+0.1*SL
                   else
                       TEMP := Y-0.5-0.1*SL
                end
             else
                begin
                   if (Y >= 0.0) then
                       TEMP := Y+0.1*SL
                   else
                       TEMP := Y-0.1*SL
                end;
             if R >= 6 then
                begin
                   R1 := R div 2;
                   TN := exp(R1*ln(10.0));
                   TEMP1 := TEMP/TN;
                   Z3 := int(TEMP1);
                   TEMP2 := TEMP-Z3*TN;
                   Z4 := int(TEMP2);
                   Z := (Z3*TN+Z4)*exp(I*ln(10.0))
                end
             else Z := int(TEMP)*exp(I*ln(10.0))
         end;
         CHIP := Z;
   end;
   begin
      INPUT;
      if ( OK ) then
         begin
            OUTPUT;
            M := N + 1;
            writeln(OUP,'Original system');
            for I := 1 to N do
               begin
                  for J := 1 to M do write(OUP,A[I,J]);
                  writeln(OUP);
               end;
            if RND = 1 then write(OUP,'Rounding to ')
                       else write(OUP,'Chopping to ');
            writeln(OUP,D,' digitis');
            writeln(OUP,'Modified System');
            for I := 1 to N do
               begin
                  NROW[I] := I;
                  for J := 1 to M do
                     begin
                        A[I,J] := chip(D,A[I,J]);
                        B[I,J] := A[I,J];
                        write(OUP,A[I,J])
                     end;
                  writeln(OUP)
               end;
{           NROW has been initialized,
            Gauss elimination will begin                               }
{           Step 0                                                     }
            I := 1;
            while ((I <= N-1) and (OK)) do
               begin
                  KK := I;
                  while ((abs(A[KK,I]) < ZERO) and (KK <= N)) do
                     KK := KK + 1;
                  if (KK > N) then
                     begin
                        OK := false;
                        writeln(OUP,'System does not have a unique solution.')
                     end
                  else
                     begin
                        if (KK <> I) then
 {                         Row interchange necessary                   }
                           begin
                              IS := NROW[I];
                              NROW[I] := NROW[KK];
                              NROW[KK] := IS;
                              for J := 1 to M do
                                 begin
                                    C := A[I,J];
                                    A[I,J] := A[KK,J];
                                    A[KK,J] := C
                                 end
                           end;
                        for J := I+1 to N do
                           begin
                              A[J,I] := chip(D,A[J,I]/A[I,I]);
                              for L := I+1 to M do
                                 A[J,L] := chip(D,A[J,L]-chip(D,A[J,I]*A[I,L]))
                           end
                     end;
                  I := I + 1
               end;
            if ((abs(A[N,N]) < ZERO) and (OK)) then
               begin
                  OK := false;
                  writeln(OUP,'System has singular matrix')
               end;
            if (OK) then
               begin
                  writeln(OUP,'Reduced system');
                  for I := 1 to N do
                     begin
                        for J := 1 to M do write(OUP,A[I,J]);
                        writeln(OUP)
                     end;
                  X[N] := CHIP(D,A[N,M]/A[N,N]);
                  for I := 1 to N-1 do
                     begin
                        J := N-I;
                        S := 0.0;
                        for L := J+1 to N do
                            S := CHIP(D,S-CHIP(D,A[J,L]*X[L]));
                        S := CHIP(D,A[J,M]+S);
                        X[J] := CHIP(D,S/A[J,J])
                     end
               end;
            writeln(OUP,'Initial solution');
            for I := 1 to N do write(OUP,X[I]);
            writeln(OUP);
{           Refinement begins                                          }
{           Step 1                                                     }
            if (OK) then
               begin
                  K := 1;
                  for I := 1 to N do XX[I] := X[I]
               end;
{           Step 2                                                     }
            while (( OK ) and ( K <= NN )) do
               begin
{              LL is set to 1 if the desired accuracy in any component
               is not achieved.  Thus, LL is initially 0 for each
               iteration.                                              }
                  LL := 0;
{                 Step 3                                               }
                  for I := 1 to N do
                     begin
                        R[I] := 0.0;
                        for J := 1 to N do R[I] :=
                           CHIP(2*D,R[I]-CHIP(2*D,B[I,J]*XX[J]));
                        R[I] := CHIP(2*D,B[I,M]+R[I])
                     end;
                  writeln(OUP,'Residual number  ',K);
                  for I := 1 to N do
                     begin
                          R[I] := chip(D,R[I]);
                          write(OUP,R[I]);
                     end;
                  writeln(OUP);
{                 Step 4                                               }
{                 Solve the linear system in the same order as in
                  step 0.  The solution will be placed in X instead
                  of in Y                                              }
                  for I := 1 to N-1 do
                     begin
                        I1 := NROW[I];
                        for J := I+1 to N do
                           begin
                              J1 := NROW[J];
                              R[J1] := CHIP(D,R[J1]-CHIP(D,A[J,I]*R[I1]))
                           end
                     end;
                  X[N] := CHIP(D,R[NROW[N]]/A[N,N]);
                  for I := 1 to N-1 do
                     begin
                        J := N-I;
                        S := 0.0;
                        for L := J+1 to N do
                           S := CHIP(D,S-CHIP(D,A[J,L]*X[L]));
                        S := CHIP(D,S+R[NROW[J]]);
                        X[J] := CHIP(D,S/A[J,J])
                     end;
                  writeln(OUP,'Vector Y');
                  for I := 1 to N do write(OUP,X[I]);
                  writeln(OUP);
{                 Steps 5 and 6                                        }
                  XXMAX := 0.0;
                  YMAX := 0.0;
                  LL := 0;
                  ERR1 := 0.0;
                  for I := 1 to N do
                     begin
{                       If not accurate set LL to 1                    }
                        if (abs(X[I]) >= TOL ) then LL := 1;
                        If K = 1 then
                           begin
                              if (abs(X[I]) > YMAX) then
                                 YMAX := abs(X[I]);
                              if (abs(XX[I]) > XXMAX) then
                                 XXMAX := abs(XX[I]);
                           end;
                        TEMP := XX[I];
                        XX[I] := chip(D,XX[I]+X[I]);
                        TEMP := abs(TEMP-XX[I]);
                        if TEMP > ERR1 then ERR1 := TEMP;
                     end;
                  if TEMP < TOL then LL := 2;
                  if K = 1 then COND := YMAX/XXMAX*exp(D*ln(10.0));
                  writeln(OUP,'New approximation');
                  for I := 1 to N do write(OUP,XX[I]);
                  writeln(OUP);
{                 Step 7                                               }
                  if (LL = 0) then
                     begin
                        writeln(OUP,'The above vector is the solution.');
                        OK := false
                     end
                  else if LL = 2 then
                     begin
                        writeln(OUP,'The above vector is the best possible');
                        writeln(OUP,'with TOL =',TOL);
                        OK := false
                     end
                  else K := K+1
{                 Step 8 is not used in this implementation            }
               end;
            if (K > NN) then
               writeln('Maximum Number of Iterations Exceeded.');
            writeln(OUP,'Condition number is ',COND);
            close(OUP)
         end
   end.
