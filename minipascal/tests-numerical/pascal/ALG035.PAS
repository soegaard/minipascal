program ALG035;
{  CLAMPED CUBIC SPLINE ALGORITHM 3.5

   To construct the cubic spline interpolant S for the function f,
   defined at the numbers x(0) < x(1) < ... < x(n), satisfying
   S'(x(0)) = f'(x(0)) and S'(x(n)) = f'(x(n)):

   INPUT:   n; x(0), x(1), ..., x(n); either generate A(I) = f(x(I))
            for i = 0, 1, ..., n or input A(I) for I = 0, 1, ..., n;
            FPO = f'(x(0)) and FPN = f'(x(n)) are both input.

    OUTPUT:  A(J), B(J), C(J), D(J) for J = 0, 1, ..., n - 1.

    NOTE:    S(x) = A(J) + B(J) * ( x - x(J) ) + C(J) * ( x - x(J) )**2 +
             D(J) * ( x - x(J) )**3 for x(J) <= x < x(J + 1)           }

var
   X,A,B,C,D,H,XA,XL,XU,XZ : array [ 0..25 ] of real;
   XX, S, Y, FP0, FPN : real;
   FLAG,N,I,J,M : integer;
   OK : boolean;
   AA : char;
   INP,OUP : text;
   NAME : string [ 30 ];
{  Change function F for a new problem                                 }
function F ( X : real ) : real;
   begin
      F := exp(2.0*X)
   end;
procedure INPUT;
   begin
      writeln('This is Clamped Cubic Spline Interpolation.');
      OK := false;
      while ( not OK ) do
         begin
            writeln ('Choice of input method: ');
            writeln ('1. Input entry by entry from keyboard ');
            writeln ('2. Input data from a text file ');
            write ('3. Generate data using a function F with nodes entered ');
            writeln ('from keyboard ');
            write ('4. Generate data using a function F with nodes from a ');
            writeln ('text file ');
            writeln ('Choose 1, 2, 3, or 4 please ');
            readln ( FLAG );
            if ( FLAG >= 1 ) and ( FLAG <= 4 ) then OK := true
         end;
      case FLAG of
         1 : begin
                OK := false;
                while ( not OK ) do
                   begin
                      writeln ('Input n ');
                      readln ( N );
                      if ( N > 0 ) then
                         begin
                            OK := true;
                            for I := 0 to N do
                               begin
                                  write ('Input X(',I,') and F(X(',I,')) ');
                                  writeln ('separated by space ');
                                  readln ( X[I] , A[I] )
                               end
                         end
                      else writeln ('Number must be a positive integer ')
                   end
             end;
         2 : begin
                write ('Has a text file been created with data in two ');
                writeln ('columns? ');
                writeln ('Enter Y or N ');
                readln ( AA );
                if ( AA = 'Y' ) or ( AA = 'y' ) then
                   begin
                      write ('Input the file name in the form - ');
                      writeln ('drive:name.ext ');
                      writeln ('for example:   A:DATA.DTA ');
                      readln ( NAME );
                      assign ( INP , NAME );
                      reset ( INP );
                      OK := false;
                      while ( not OK ) do
                         begin
                            writeln ('Input n ');
                            readln ( N );
                            if ( N > 0 ) then
                               begin
                                  OK := true;
                                  for I := 0 to N do
                                     readln ( INP , X[I] , A[I] );
                                  close ( INP )
                               end
                            else writeln ('Number must be a positive integer ')
                         end
                   end
                else
                   begin
                      write ('Please create the input file in two column ');
                      writeln ('form with the ');
                      write ('X values and F(X) values in the ');
                      writeln ('corresponding columns ');
                      write ('The program will end so the input file can ');
                      writeln ('be created. ');
                      OK := false
                   end
            end;
         3 : begin
                writeln ('Has the function F been created in the program ');
                writeln ('immediately proceding the INPUT procedure? ');
                writeln ('Enter Y or N ');
                readln ( AA );
                if ( AA = 'Y' ) or ( AA = 'y' ) then
                   begin
                      OK := false;
                      while ( not OK ) do
                         begin
                            writeln ('Input n ');
                            readln ( N );
                            if ( N > 0 ) then
                               begin
                                  OK := true;
                                  for I := 0 to N do
                                     begin
                                        writeln ('Input X(',I,') ');
                                        readln ( X[I] );
                                        A[I] := F( X[I] )
                                     end
                               end
                            else writeln ('Number must be a positive integer ')
                         end
                   end
                else
                   begin
                      write ('The program will end so that the function F ');
                      writeln ('can be created ');
                      OK := false
                   end
             end;
         4 : begin
                write ('Has the text file with X-values been created and ');
                writeln ('has the function F been created in the program ');
                writeln ('immediately preceding the INPUT procedure? ');
                writeln ('Enter Y or N ');
                readln ( AA );
                if ( AA = 'Y' ) or ( AA = 'y') then
                   begin
                      write ('Input the file name in the form - ');
                      writeln ('drive:name.ext ');
                      writeln ('for example:   A:DATA.DTA ');
                      readln ( NAME );
                      assign ( INP , NAME );
                      reset ( INP );
                      OK := false;
                      while ( not OK ) do
                         begin
                            writeln ('Input n ');
                            readln ( N );
                            if ( N > 0 ) then
                               begin
                                  OK := true;
                                  for I := 0 to N do
                                     begin
                                        read ( INP , X[I] );
                                        A[I] := F( X[I] )
                                     end;
                                  close ( INP )
                               end
                            else writeln ('Number must be a positive integer ')
                         end
                   end
                else
                   begin
                      write ('The program will end so the input file and ');
                      writeln ('F can be created. ');
                      OK := false
                   end
             end
       end;
       if ( OK ) then
          begin
             writeln ('Enter F''(X(0)) and F''(X(N)) separated by blank ');
             readln ( FP0, FPN )
          end
   end;
procedure OUTPUT;
   begin
      writeln ('Select output destination: ');
      writeln ('1. Screen ');
      writeln ('2. Text file ');
      writeln ('Enter 1 or 2 ');
      readln ( FLAG );
      if ( FLAG = 2 ) then
         begin
            writeln ('Input the file name in the form - drive:name.ext ');
            writeln ('for example:   A:OUTPUT.DTA ');
            readln ( NAME );
            assign ( OUP, NAME )
         end
      else assign ( OUP, 'CON' );
      rewrite ( OUP );
      writeln(OUP,'CLAMPED CUBIC SPLINE INTERPOLATION');
      writeln(OUP);
      writeln ( OUP, 'The numbers X(0), ..., X(N) are: ');
      for I := 0 to N do write (OUP, X[I]:12:8 );
      writeln ( OUP ); writeln ( OUP );
      writeln(OUP,'The coefficients of the spline on the subintervals are:');
      writeln ( OUP, 'for I = 0, ..., N-1 ');
      write(OUP,'     A(I)          B(I)           C(I)     ');
      writeln(OUP,'     D(I) ');
      for I := 0 to M do
          writeln ( OUP, A[I]:14:8, B[I]:14:8, C[I]:14:8, D[I]:14:8 );
      close ( OUP )
   end;
   begin
      INPUT;
      if ( OK ) then
         begin
{           STEP 1                                                     }
            M := N - 1;
            for I := 0 to M do H[I] := X[I+1] - X[I];
{           STEP 2                                                     }
{           use XA instead of alpha                                    }
            XA[0] := 3.0 * ( A[1] - A[0] ) / H[0] - 3.0 * FP0;
            XA[N] := 3.0 * FPN - 3.0 * ( A[N] - A[N-1] ) / H[N-1];
{           STEP 3                                                     }
            for I := 1 to M do
               XA[I] := 3.0 * ( A[I+1] * H[I-1] - A[I] *
                        ( X[I+1] - X[I-1] ) + A[I-1] * H[I] )
                       / ( H[I] * H[I-1 ] );
{           STEP 4                                                     }
{           STEPS 4, 5, 6 and part of 7 solve the tridiagonal system using
            Algorithm 6.7                                              }
{                                                                      }
{           use XL, XU, XZ in place of L, MU, Z resp.                  }
            XL[0] := 2.0 * H[0]; XU[0] := 0.5; XZ[0] := XA[0] / XL[0];
{           STEP 5                                                     }
            for I := 1 to M do
               begin
                  XL[I] := 2.0 * ( X[I+1] - X[I-1] ) - H[I-1] *
                           XU[I-1];
                  XU[I] := H[I] / XL[I];
                  XZ[I] := ( XA[I] - H[I-1] * XZ[I-1] ) / XL[I]
               end;
{           STEP 6                                                     }
            XL[N] := H[N-1] * ( 2.0 - XU[N-1]);
            XZ[N] := ( XA[N] - H[N-1] * XZ[N-1] ) / XL[N];
            C[N] := XZ[N];
{           STEP 7                                                     }
            for I := 1 to N do
               begin
                  J := N - I;
                  C[J] := XZ[J] - XU[J] * C[J+1];
                  B[J] := ( A[J+1] - A[J] ) / H[J] - H[J] *
                          ( C[J+1] + 2.0 * C[J] ) / 3.0;
                  D[J] := ( C[J+1] - C[J] ) / (3.0 * H[J] )
               end;
{           STEP 8                                                     }
            OUTPUT
         end
   end.








