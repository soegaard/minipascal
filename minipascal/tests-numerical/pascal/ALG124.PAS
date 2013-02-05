program ALG124;
{
   WAVE EQUATION FINITE-DIFFERENCE ALGORITHM 12.4

   To approximate the solution to the wave equation:
   subject to the boundary conditions
                u(0,t) = u(l,t) = 0, 0 < t < T = max t
   and the initial conditions
                u(x,0) = F(x) and Du(x,0)/Dt = G(x), 0 <= x <= l:

   INPUT:   endpoint l; maximum time T; constant ALPHA; integers m, N.

   OUTPUT:  approximations W(I,J) to u(x(I),t(J)) for each I = 0, ..., m
            and J=0,...,N.
}
var
   W : array [ 1..21, 1..21 ] of real;
   FT,FX,ALPHA,H,K,V,X : real;
   N,M,M1,M2,N1,N2,FLAG,I,J : integer;
   OK : boolean;
   AA : char;
   NAME : string [ 30 ];
   OUP : text;
{  Change function F for a new problem                                 }
function F( X : real ) : real;
   begin
      F := sin( PI * X )
   end;
{  Change function G for a new problem                                 }
function G( X : real ) : real;
   begin
      G := 0.0
   end;
procedure INPUT;
   begin
      writeln('This is the Finite-Difference for the Wave Equation.');
      writeln ('Have the functions F and G been created immediately');
      writeln ('preceding the INPUT procedure? Answer Y or N. ');
      readln ( AA );
      if ( AA = 'Y' ) or ( AA = 'y' ) then
         begin
            writeln ('The lefthand endpoint on the X-axis is 0. ');
            OK := false;
            while ( not OK ) do
               begin
                  writeln ('Input the righthand endpoint on the X-axis. ');
                  readln ( FX );
                  if ( FX <= 0.0 ) then
                     writeln ('Must be a positive number. ')
                  else
                     OK := true
               end;
            OK := false;
            while ( not OK ) do
               begin
                  writeln ('Input the maximum value of the time variable T. ');
                  readln ( FT );
                  if ( FT <= 0.0 ) then
                     writeln ('Must be a positive number. ')
                  else
                     Ok := true
               end;
            writeln ('Input the constant ALPHA. ');
            readln ( ALPHA );
            OK := false;
            while ( not OK ) do
               begin
                  writeln ('Input integer m = number of intervals on X-axis ');
                  write ('and N = number of time intervals ');
                  writeln ('- separated by a blank. ');
                  writeln('Note that m should be 3 or larger.');
                  readln ( M, N );
                  if ( ( M <= 2 ) or ( N <= 0 ) ) then
                     writeln ('Numbers not in the correct range. ')
                  else
                     OK := true
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
            writeln ('for example:   A:OUTPUT.DTA');
            readln ( NAME );
            assign ( OUP, NAME )
         end
      else assign ( OUP, 'CON' );
      rewrite ( OUP );
      writeln(OUP,'FINITE DIFFERENCE METHOD FOR THE WAVE EQUATION');
      writeln(OUP);
      write ( OUP,'I':3,'X(I)':12,'          W(X(I),',FT:12,')' );
      writeln ( OUP );
      for I := 1 to M1 do
         begin
            X := ( I - 1.0 ) * H;
            writeln(OUP,I:3,X:12:8,W[I,N1]:14:8)
         end;
      close ( OUP )
   end;
   begin
      INPUT;
      if ( OK ) then
         begin
            M1 := M + 1;
            M2 := M - 1;
            N1 := N + 1;
            N2 := N - 1;
{           STEP 1
            V is used for lambda                                       }
            H := FX / M;
            K := FT / N;
            V := ALPHA * K / H;
{           STEP 2
            the subscripts are shifted to avoid zero subscripts        }
            for J := 2 to N1 do
               begin
                  W[1,J] := 0.0;
                  W[M1,J] := 0.0
               end;
{           STEP 3                                                     }
            W[1,1] := F( 0.0 );
            W[M1,1] := F ( FX );
{           STEP 4                                                     }
            for I := 2 to M do
               begin
                  W[I,1] := F( H * ( I - 1.0 ) );
                  W[I,2] := (1.0-V*V)*F(H*(I-1.0))+V*V*(F(I*H)+
                            F(H*(I-2.0)))/2.0+K*G(H*(I-1.0))
               end;
{           STEP 5                                                     }
            for J := 2 to N do
               for I := 2 to M do
                  W[I,J+1] := 2.0*(1.0-V*V)*W[I,J]+V*V*
                              (W[I+1,J]+W[I-1,J])-W[I,J-1];
{           STEP 6                                                     }
            OUTPUT
         end
{  STEP 7                                                              }
   end.


