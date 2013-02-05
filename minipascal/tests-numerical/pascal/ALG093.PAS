program ALG093;
{  INVERSE POWER METHOD ALGORITHM 9.3

   To approximate an eigenvalue and an associated eigenvector of the
   n by n matrix A given a nonzero vector x:

   INPUT:   Dimension n; matrix A; vector x; tolerance TOL;
            maximum number of iterations N.

   OUTPUT:  Approximate eigenvalue MU; approximate eigenvector x
            or a message that the maximum number of iterations was
            exceeded.
}
const
   ZERO = 1.0E-20;
var
   A : array [ 1..10, 1..10 ] of real;
   X,Y,B : array [ 1..10 ] of real;
   NROW : array [1..10] of integer;
   T,AMAX,YMU,ERR,TOL,Q,S : real;
   FLAG,N,I,J,NN,K,LP : integer;
   OK : boolean;
   AA : char;
   NAME : string [ 30 ];
   INP, OUP : text;
procedure INPUT;
   begin
      writeln('This is the Inverse Power Method.');
      OK := false;
      writeln ('The array will be input from a text file in the order: ');
      writeln('A(1,1), A(1,2), ..., A(1,n), A(2,1), A(2,2), ..., A(2,n),');
      write ('..., A(n,1), ');
      writeln ('A(n,2), ..., A(n,n) '); writeln; writeln;
      write ('Place as many entries as desired on each line, but separate ');
      writeln ('entries with ');
      writeln ('at least one blank. ');
      writeln ('The initial approximation should follow in same format.' );
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
                  writeln ('Input the dimension n. ');
                  readln ( N );
                  if ( N > 0 ) then
                     begin
                        for I := 1 to N do
                           for J := 1 to N do read ( INP, A[I,J] );
                        for I := 1 to N do read ( INP, X[I]);
                        close ( INP );
                        while ( not OK ) do
                           begin
                              writeln ('Input the tolerance. ');
                              readln ( TOL );
                              if ( TOL > 0.0 ) then OK := true
                              else
                                 writeln ('Tolerance must be positive number.')
                           end;
                        OK := false;
                        while ( not OK ) do
                           begin
                              write ('Input maximum number of iterations ');
                              writeln ('- integer.');
                              readln ( NN );
{                             use NN for N                             }
                              if ( NN > 0 ) then OK := true
                              else
                                 writeln ('Number must be positive integer. ')
                           end
                     end
                  else writeln ('The dimension must be a positive integer. ')
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
         writeln(OUP,'INVERSE POWER METHOD');
         writeln ( OUP );
      end;
procedure MULTIP;
{  Procedure MULTIP determines the row ordering and multipliers for the
   matrix A - Q*I for use in Gaussian elimination with partial pivoting }
   var
      K,I,M,IMAX,J,IP,L1,L2,JJ,I1,J1,N1 : integer;
   begin
      for I := 1 to N do NROW[I] := I;
      OK := true;
      I := 1;
      M := N - 1;
      while ( ( I <= M ) and OK ) do
         begin
            IMAX := I;
            J := I + 1;
            for IP := J to N do
               begin
                  L1 := NROW[IMAX];
                  L2 := NROW[IP];
                  if ( abs( A[L2,I] ) > abs( A[L1,I] ) ) then IMAX := IP
               end;
            if ( abs( A[NROW[IMAX],I] ) <= ZERO ) then
               begin
                  OK := false;
                  write ('A - Q * I is singular, Q = ', Q, ' is an ');
                  writeln ('eigenvalue ')
               end
            else
               begin
                  if NROW[I] <> NROW[IMAX] then
                     begin
                        JJ := NROW[I];
                        NROW[I] := NROW[IMAX];
                        NROW[IMAX] := JJ;
                     end;
                  I1 := NROW[I];
                  for JJ := J to N do
                     begin
                        J1 := NROW[JJ];
                        A[J1,I] := A[J1,I] / A[I1,I];
                        for K := J to N do
                           A[J1,K] := A[J1,K] - A[J1,I] * A[I1,K]
                     end
               end;
            I := I + 1
         end;
      if ( abs( a[NROW[N],N] ) <= ZERO ) then
         begin
            OK := false;
            writeln ('A - Q * I is singular, Q = ', Q, ' is an eigenvalue ')
         end
   end;
procedure SOLVE;
{  Procedure SOLVE solves the linear system (A - Q*I) Y = X  given
   a vector X and the row ordering and multipliers from MULTIP         }
   var
      M,I,J,I1,JJ,J1,N1,N2,L,K,KK : integer;
   begin
      M := N - 1;
      for I := 1 to M do
         begin
            J := I + 1;
            I1 := NROW[I];
            for JJ := J to N do
               begin
                  J1 := NROW[JJ];
                  B[J1] := B[J1] - A[J1,I] * B[I1]
               end;
         end;
      N1 := NROW[N];
      Y[N] := B[N1] / A[N1,N];
      L := N - 1;
      for K := 1 to L do
         begin
            J := L - K + 1;
            JJ := J + 1;
            N2 := NROW[J];
            Y[J] := B[N2];
            for KK := JJ to N do Y[J] := Y[J] - A[N2,KK] * Y[KK];
            Y[J] := Y[J] / A[N2,J]
         end
   end;
   begin
      INPUT;
      if ( OK ) then
         begin
            OUTPUT;
{           STEP 1                                                             }
{           Q could be input instead of computed by deleting
            the next 7 steps                                           }
            Q := 0.0;
            S := 0.0;
            for I := 1 to N do
               begin
                  S := S + X[I] * X[I];
                  for J := 1 to N do Q := Q + A[I,J] * X[I] * X[J]
               end;
            Q := Q / S;
            writeln('Q is ',Q);
            writeln('input new Q if desired, otherwise just press enter');
            readln(Q);
            writeln(OUP,'Iteration  Eigenvalue  Eigenvector');
{           STEP 2                                                     }
            K := 1;
            for I := 1 to N do A[I,I] := A[I,I] - Q;
{           Call subroutine to compute multipliers M(I,J) and
            upper triangular matrix for matrix A using Gauss
            elimination with partial pivoting.
            NROW holds the ordering of rows for interchanges           }
            Multip;
            if ( OK ) then
               begin
{                 STEP 3                                               }
                  LP := 1;
                  for I := 2 to N do
                     if ( abs( X[I] ) > abs( X[LP] ) ) then LP := I;
{                 STEP 4                                               }
                  AMAX := X[LP];
                  for I := 1 to N do X[I] := X[I] / (AMAX);
{                 STEP 5                                               }
                  while ( ( K <= NN ) and OK ) do
                     begin
{                       STEPS 6 and 7                                  }
                        for I := 1 to N do B[I] := X[I];
{                       Subroutine solve returns the solution of
                        ( A - Q * I )Y = B in Y                        }
                        SOLVE;
{                       STEP 8                                         }
                        YMU := Y[LP];
{                       STEP 9 AND 10                                  }
                        LP := 1;
                        for I := 2 to N do
                           if ( abs( Y[I] ) > abs( Y[LP] ) ) then LP := I;
                        AMAX := Y[LP];
                        ERR := 0.0;
                        for I := 1 to N do
                           begin
                              T := Y[I] / AMAX;
                              if ( abs( X[I] - T ) > ERR ) then
                                 ERR := abs( X[I] - T );
                              X[I] := T
                           end;
                        YMU := 1 / YMU + Q;
{                       STEP 11                                        }
                        writeln(OUP,K:3,' ',YMU:12:8);
                        for I := 1 to N do write(OUP,X[I]:12:8);
                        writeln(OUP);
                        if ( ERR < TOL ) then
                           begin
                              OK := false;
                              write(OUP,'Eigenvalue = ',YMU:12:8);
                              writeln(OUP,' to tolerance = ',TOL);
                              writeln(OUP,'obtained on iteration number ',K);
                              writeln ( OUP );
                              writeln (OUP,'Unit eigenvector is : ');
                              for I := 1 to N do write ( OUP, X[I]:12:8 );
                           end
                        else
{                          STEP 12                                     }
                           K := K + 1
                     end;
                  if K > NN then
                     writeln('No convergence within ',NN,' iterations');
               end;
            close ( OUP )
         end
{  STEP 13                                                             }
   end.

