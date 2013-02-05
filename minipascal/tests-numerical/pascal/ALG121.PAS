program ALG121;
{
   POISSON EQUATION FINITE-DIFFERENCE ALGORITHM 12.1

   To approximate the solution to the Poisson equation
              DEL(u) = F(x,y), a <= x <= b, c <= y <= d,
   SUBJECT TO BOUNDARY CONDITIONS:
                   u(x,y) = G(x,y),
       if x = a or x = b for c <= y <= d,
       if y = c or y = d for a <= x <= b

   INPUT:   endpoints a, b, c, d; integers m, n; tolerance TOL;
            maximum number of iterations M

   OUTPUT:  approximations W(I,J) to u(X(I),Y(J)) for each
            I = 1,..., n-1 and J=1,..., m-1 or a message that the
            maximum number of iterations was exceeded.
}
var
   W : array [ 0..25, 0..25 ] of real;
   X,Y : array [ 0..25 ] of real;
   TOL,A,B,C,D,H,K,V,VV,Z,E,ALPHA,BETA : real;
   FLAG,M,N,NN,M1,M2,N1,N2,I,J,L,LL : integer;
   OK : boolean;
   AA : char;
   NAME : string [ 30 ];
   OUP : text;
{  Change function F for a new problem                                }
function F( X,Y : real ) : real;
   begin
      F := X*exp(Y)
   end;
{  Change G for a new problem                                          }
function G( X,Y : real ) : real;
   begin
      G := X*exp(Y)
   end;
procedure INPUT;
   begin
      writeln('This is the Finite-Difference Method for Elliptic Equations.');
      OK := false;
      writeln ('Have the functions F(x,y) and G(x,y) been created');
      writeln ('immediately preceding the INPUT procedure? Answer Y or N.');
      readln ( AA );
      if ( AA = 'Y' ) or ( AA = 'y' ) then
         begin
            OK := false;
            while ( not OK ) do
              begin
                 writeln ('Input endpoints of interval [A,B] on X-axis ');
                 writeln ('separated by a blank. ');
                 readln ( A, B );
                 writeln ('Input endpoints of interval [C,D] on Y-axis ');
                 writeln ('separated by a blank . ');
                 readln ( C, D );
                 if ( ( A >= B ) or ( C >= D ) ) then
                   writeln ('Left endpoint must be less than right endpoint.')
                 else Ok := true
              end;
            OK := false;
            while ( not OK ) do
              begin
                 writeln('Input number of intervals n on the X-axis and m ');
                 writeln ('on the Y-axis separated by a blank ');
                 writeln('Note that both n and m should be larger than 2.');
                 readln ( N,M );
                 if ( ( M <= 2 ) or ( N <= 2 ) ) then
                    writeln ('Numbers must exceed 2. ')
                 else OK := true
              end;
            OK := false;
            while ( not OK ) do
              begin
                 writeln ('Input the Tolerance. ');
                 readln ( TOL );
                 if ( TOL <= 0.0 ) then
                   writeln ('Tolerance must be positive. ')
                 else
                    OK := true
              end;
            OK := false;
            while ( not OK ) do
              begin
                 writeln ('Input the maximum number of iterations. ');
                 readln ( NN );
                 if ( NN <= 0 ) then
                   writeln ('Number must be a positive integer. ')
                 else Ok := true
              end
         end
      else
         begin
            write ('The program will end so that the functions ');
            writeln ('F and G can be created. ');
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
      rewrite(OUP);
      writeln(OUP,'POISSON EQUATION FINITE-DIFFERENCE METHOD');
      writeln(OUP);
      writeln ( OUP, 'I':3,'J':3,'X(I)':12,'Y(J)':12,'W(I,J)':14);
      writeln ( OUP );
      for I := 1 to N1 do
         for J := 1 to M1 do
            writeln(OUP,I:3,J:3,X[I]:12:8,Y[J]:12:8,W[I,J]:14:8);
      writeln ( OUP, 'Convergence occurred on iteration number: ', L );
      close ( OUP )
   end;
   begin
      INPUT;
      if ( OK ) then
         begin
            M1 := M - 1;
            M2 := M - 2;
            N1 := N - 1;
            N2 := N - 2;
{           STEP 1                                                     }
            H := ( B - A ) / N;
            K := ( D - C ) / M;
{           STEPS 2 and 3 construct mesh points                        }
{           STEP 2                                                     }
            for I := 0 to N do X[I] := A + I * H;
{           STEP 3                                                     }
            for J := 0 to M DO Y[J] := C + J * K;
{           STEP 4                                                     }
            for I := 1 to N1 do
               begin
                  W[I,0] := g(X[I],Y[0]);
                  W[I,M] := g(X[I],Y[M])
               end;
            for J := 0 to M do
               begin
                  W[0,J] := g(X[0],Y[J]);
                  W[N,J] := g(X[N],Y[J])
               end;
            for I := 1 to N1 do
               for J := 1 to M1 do W[I,J] := 0.0;
{           STEP 5
            use V for lambda, VV for mu                                }
            V := H * H / ( K * K );
            VV := 2.0 * ( 1.0 + V );
            L := 1;
            OK := false;
{           Z is a new value of W(I,J) to be used in computing
            the norm of the error E used in place of NORM
            STEP 6                                                     }
            while ( ( L <= NN ) and ( not OK ) ) do
               begin
{                 STEPS 7 through 20 perform Gauss-Seidel iterations
                  STEP 7                                               }
                  Z := (-H*H*F(X[1],Y[M1])+G(A,Y[M1])+V*
                       G(X[1],D)+V*W[1,M2]+W[2,M1])/VV;
                  E := abs( W[1,M1] - Z );
                  W[1,M1] := Z;
{                 STEP 8                                               }
                  for I := 2 to N2 do
                     begin
                        Z := (-H*H*F(X[I],Y[M1])+V*G(X[I],D)+
                             W[I-1,M1]+W[I+1,M1]+V*W[I,M2])/VV;
                        if ( abs( W[I,M1] - Z ) > E ) then
                           E := abs( W[I,M1] - Z );
                        W[I,M1] := Z
                     end;
{                 STEP 9                                               }
                  Z := (-H*H*F(X[N1],Y[M1])+G(B,Y[M1])+V*
                       G(X[N1],D)+W[N2,M1]+V*W[N1,M2])/VV;
                  if ( abs( W[N1,M1] - Z ) > E ) then
                     E := abs( W[N1,M1] - Z );
                  W[N1,M1] := Z;
{                 STEP 10                                              }
                  for LL := 2 to M2 do
                     begin
                        J := M2 - LL + 2;
{                       STEP 11                                        }
                        Z := (-H*H*F(X[1],Y[J])+G(A,Y[J])+
                             V*W[1,J+1]+V*W[1,J-1]+W[2,J])/VV;
                        if ( abs( W[1,J] - Z ) > E ) then
                           E := abs( W[1,J] - Z );
                        W[1,J] := Z;
{                       STEP 12                                        }
                        for I := 2 to N2 do
                           begin
                              Z := (-H*H*F(X[I],Y[J])+W[I-1,J]+
                                   V*W[I,J+1]+V*W[I,J-1]+W[I+1,J])/VV;
                              if ( abs( W[I,J] - Z ) > E ) then
                                 E := abs( W[I,J] - Z );
                              W[I,J] := Z
                           end;
{                       STEP 13                                        }
                        Z := (-H*H*F(X[N1],Y[J])+G(B,Y[J])+
                             W[N2,J]+V*W[N1,J+1]+V*W[N1,J-1])/VV;
                        if ( abs( W[N1,J] - Z ) > E ) then
                           E := abs( W[N1,J] - Z );
                        W[N1,J] := Z
                     end;
{                 STEP 14                                              }
                  Z := ( -H * H * F( X[1],Y[1] ) + V * G( X[1], C ) +
                       G( A, Y[1] ) + V * W[1,2] + W[2,1] ) / VV;
                  if ( abs( W[1,1] - Z ) > E ) then
                     E := abs( W[1,1] - Z );
                  W[1,1] := Z;
{                 STEP 15                                              }
                  for I := 2 to N2 do
                     begin
                        Z := (-H*H*F(X[I],Y[1])+V*G(X[I],C)+
                             W[I+1,1]+W[I-1,1]+V*W[I,2])/VV;
                        if ( abs( W[I,1] - Z ) > E ) then
                           E := abs( W[I,1] - Z );
                        W[I,1] := Z
                     end;
{                 STEP 16                                              }
                  Z := (-H*H*F(X[N1],Y[1])+V*G(X[N1],C)+
                       G(B,Y[1])+W[N2,1]+V*W[N1,2])/VV;
                  if ( abs( W[N1,1] - Z ) > E ) then
                     E := ABS( W[N1,1] - Z );
                  W[N1,1] := Z;
{                 STEP 17                                              }
                  if ( E <= TOL ) then
                     begin
{                       STEP 18                                        }
                        OUTPUT;
{                       STEP 19                                        }
                        OK := true
                     end
                  else
{                    STEP 20                                           }
                     L := L + 1
               end;
{           STEP 21                                                    }
            if ( L > NN ) then
               writeln ('Method fails after iteration number ', NN )
         end
   end.


