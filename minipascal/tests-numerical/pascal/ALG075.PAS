program ALG075;
{  Conjugate Gradient ALGORITHM 7.5

   To solve Ax = b given the preconditioning matrix C inverse
   and an initial approximation x(0):

   INPUT:   the number of equations and unknowns n; the entries
            A(I,J), 1<=I, J<=n, of the matrix A; the entries
            B(I), 1<=I<=n, of the inhomogeneous term b;  the
            entries C(I,J), 1<=I, J<=n, of the preconditioning
            matrix C inverse, the entries XO(I), 1<=I<=n, of x(0);
            tolerance TOL; maximum number of iterations N.

    OUTPUT:  the approximate solution X(1),...,X(N) or a message
             that the number of iterations was exceeded.
}
var
   INP,OUP : text;
   A : array [ 1..10, 1..11 ] of real;
   CI, CT : array [1..10,1..10] of real;
   R,V,W,X1,U,Z : array [ 1..10 ] of real;
   ALPHA,BETA,SS,S,ERR,TOL,ERR1,T,QERR : real;
   FLAG,N,I,J,NN,K : integer;
   OK : boolean;
   AA : char;
   NAME : string [ 30 ];
   procedure INPUT;
   begin
      writeln('This is the Conjugate Gradient Method for Linear Systems.');
      OK := false;
      writeln ('The array will be input from a text file in the order: ');
      writeln('A(1,1), A(1,2), ..., A(1,n+1), A(2,1), A(2,2), ..., A(2,n+1),');
      write ('..., A(n,1), ');
      writeln ('A(n,2), ..., A(n,n+1) '); writeln;
      write ('Place as many entries as desired on each line, but separate ');
      writeln ('entries with ');
      writeln ('at least one blank.');
      write ('The preconditioner, C inverse, should follow in ');
      writeln(' the same way.');
      writeln ('The initial approximation should also follow in same format.' );
      writeln; writeln;
      writeln ('Has the input file been created? - enter Y or N. ');
      readln ( AA );
      if ( AA = 'Y' ) or ( AA = 'y' ) then
         begin
            writeln ('Input the file name in the form - drive:name.ext, ');
            writeln ('for example: A:DATA.DTA ');
            readln ( NAME );
            assign ( INP, NAME );
            reset ( INP );
            OK := false;
            while ( not OK ) do
               begin
                  writeln ('Input the number of equations - an integer. ');
                  readln ( N );
                  if ( N > 0 ) then
                     begin
                        for I := 1 to N do
                           for J := 1 to N + 1 do read ( INP, A[I,J] );
                        for I := 1 to N do
                           for J := 1 to N do read ( INP, CI[I,J] );
                        for I := 1 to N do read ( INP, X1[I]);
                        OK := true;
                        close ( INP )
                     end
                  else writeln ('The number must be a positive integer. ')
               end;
            OK := false;
            while ( not OK) do
               begin
                  writeln ('Input the tolerance.');
                  readln ( TOL );
                  if (TOL > 0) then OK := true
                  else writeln('Tolerance must be a positive number.')
               end;
            OK := false;
            while ( not OK) do
               begin
                  writeln('Input maximum number of iterations.');
                  readln ( NN );
                  if (NN > 0) then OK := true
                  else writeln('Number must be a positive integer.')
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
               assign ( OUP, NAME )
            end
         else  assign ( OUP, 'CON' );
         rewrite ( OUP );
         writeln(OUP,'CONJUGATE GRADIENT METHOD FOR LINEAR SYSTEMS');
         writeln ( OUP );
         writeln ( OUP, 'The solution vector is : ');
         for I := 1 to N do write ( OUP, X1[I]:12:8 );
         writeln ( OUP ); writeln ( OUP, 'using ',K,' iterations with  ');
         writeln ( OUP, 'Tolerance', TOL);
         writeln ( OUP, 'The Residual vector is : ');
         for I := 1 to N do write ( OUP, R[I]:12:8 );
         close ( OUP )
      end;
   begin
      INPUT;
      if ( OK ) then
         begin
         { STEP 1 }
            for I := 1 to N do
                for J := 1 to N do
                    begin
                        CT[I,J] := CI[J,I]
                    end;
            for I := 1 to N do
                begin
                    R[I] := A[I,N+1];
                    for J := 1 to N do
                        R[I] := R[I] - A[I,J] * X1[J];
                end;
            for I := 1 to N do
                begin
                   W[I] := 0;
                   for J := 1 to N do
                      W[I] := W[I] + CI[I,J] * R[J];
                end;
            for I := 1 to N do
                begin
                   V[I] := 0;
                   for J := 1 to N do
                      V[I] := V[I]+CT[I,J]*W[J];
                end;
            ALPHA := 0;
            for I := 1 to N do ALPHA := ALPHA + W[I]*W[I];
            { STEP 2 }
            K := 1;
            OK := false;
            while ( not OK ) and ( K <= NN ) do
               begin
                  ERR := 0.0;
                  for I := 1 to N do ERR := ERR + V[I]*V[I];
                  { STEP 4 }
                  if ( sqrt(ERR) < TOL) then
                     begin
                        OK := true;
                        K := K-1
                     end
                  else
                     begin
                     { STEP 5 }
                        for I := 1 to N do
                           begin
                              U[I] := 0;
                              for J := 1 to N do
                                 U[I] := U[I] + A[I,J]*V[J]
                           end;
                        S := 0;
                        for I := 1 to N do S := S + V[I]*U[I];
                        T := ALPHA/S;
                        for I := 1 to N do
                            begin
                               X1[I] := X1[I] + T*V[I];
                               R[I] := R[I] - T*U[I]
                            end;
                        for I := 1 to N do
                            begin
                               W[I] := 0;
                               for J := 1 to N do
                                   W[I] := W[I] + CI[I,J]*R[J]
                            end;
                        BETA := 0;
                        for I := 1 to N do
                            BETA := BETA + W[I]*W[I];
                        { STEP 6 }
                        if ( sqrt(BETA) <= TOL ) then
                           begin
                               ERR := 0;
                               for I := 1 to N do
                                   ERR := ERR + R[I]*R[I];
                               ERR := sqrt(ERR);
                               if (ERR < TOL) then  OK := true
                           end;
                        if (not OK) then
                           begin
                           { STEP 7 }
                              K := K + 1;
                              S := BETA/ALPHA;
                              for I := 1 to N do
                                 begin
                                    Z[I] := 0;
                                    for J := 1 to N do
                                       Z[I] := Z[I] + CT[I,J]*W[J]
                                 end;
                              for I := 1 to N do
                                 V[I] := Z[I] + S*V[I];
                              ALPHA := BETA ;
                           end
                     end
               end;
            { STEP 8 }
            if ( not OK ) then writeln
                 ('Maximum Number of Iterations Exceeded. ')
            else OUTPUT
         end
   end.
