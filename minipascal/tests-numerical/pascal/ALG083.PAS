program ALG083;
{  FAST FOURIER TRANSFORM ALGORITHM 8.3

   To compute the coefficients in the discrete approximation
   for the data (x(J),y(J)), 0<=J<=2m-1 where m=2**p and
   x(J)=-pi+J*pi/m for 0<=J<=2m-1.

   INPUT:  m; y(0),y(1),...y(2m-1).

   OUTPUT: complex numbers c(0),...,c(2m-1); real numbers
           a(0),...,a(m); b(1),...,b(m-1).

   NOTE:   The multiplication by EXP(-K*PI*I) is done within the
           program.
}
const
   ZERO = 1.0e-20;
var
   CR, CI, WR, WI, Y : array[1..64] of real;
   WWR, WWI, T1R, T1I, T3R, T3I : real;
   Z, XR, XI, YR, YI, TW : real;
   NG,N, N2, NU1, I, K, L, M, NP, J, M1 : integer;
   FLAG : integer;
   OK : boolean ;
   A : char;
   INP,OUP : text;
   NAME : string [ 30 ];
{  Change F if program is to calculate y                               }
function F ( Z : real ) : real;
   var
      x : real;
   begin
      x := 1+Z/pi;
      F := ((x-3)*x+2)*x*x-sin(x*(x-2))/cos(x*(x-2))
   end;
procedure INPUT;
   begin
      writeln('This is the Fast Fourier Transform.');
      writeln(' ');
      writeln('The user must make provisions if the');
      writeln('interval is not [-pi,pi].');
      writeln('The example illustrates the required');
      writeln('provisions under input method 3.');
      OK := false;
      while ( not OK ) do
         begin
            writeln ('Choice of input method: ');
            writeln ('1. Input entry by entry from keyboard ');
            writeln ('2. Input data from a text file ');
            writeln ('3. Generate data using a function F ');
            writeln ('Choose 1, 2, or 3 please. ');
            readln ( FLAG );
            if ( FLAG = 1 ) or ( FLAG = 2 ) or ( FLAG = 3 ) then OK := true
         end;
      case FLAG of
         1 : begin
                OK := false;
                while ( not OK ) do
                   begin
                      writeln ('Input m. ');
                      readln ( M );
                      if ( M > 0 ) then
                         begin
                            OK := true;
                            N := 2*M;
                            for I := 1 to N do
                               begin
                                  J := I - 1;
                                  writeln ('Input y(',J,').');
                                  readln(Y[I])
                               end
                         end
                      else writeln ('Number must be a positive integer. ')
                   end
             end;
         2 : begin
                write ('Has a text file been created with the ');
                writeln ('entries y(0),...,y(2m-1)');
                writeln ('separated by a blank? ');
                writeln ('Enter Y or N. ');
                readln ( A );
                if ( A = 'Y' ) or ( A = 'y' ) then
                   begin
                      write ('Input the file name in the form - ');
                      writeln ('drive:name.ext, ');
                      writeln ('for example:   A:DATA.DTA ');
                      readln ( NAME );
                      assign ( INP, NAME );
                      reset ( INP );
                      OK := false;
                      while ( not OK ) do
                         begin
                            writeln ('Input number m. ');
                            readln ( M );
                            N := 2*M;
                            if ( N > 0 ) then
                               begin
                                  for I := 1 to N do
                                     read ( INP, Y[I]);
                                  close ( INP );
                                  OK := true
                               end
                            else writeln ('Number must be a positive integer.')
                         end
                   end
                else
                   begin
                      write ('The program will end so the input file can ');
                      writeln ('be created. ');
                      OK := false
                   end
             end;
         3 : begin
                write ('Has the function F been created in the program ');
                writeln ('immediately preceding ');
                writeln ('the INPUT procedure? ');
                writeln ('Enter Y or N. ');
                readln ( A );
                if (( A = 'Y' ) or ( A = 'y' )) then
                   begin
                      OK := false;
                      while ( not OK ) do
                         begin
                            writeln ('Input the number m.');
                            readln ( M );
                            N := 2*M;
                            if ( N > 0 ) then
                               begin
                                  for I := 1 to N do
                                     begin
                                        Z := -PI+(I-1)*PI/M;
                                        Y[I] := F(Z)
                                     end;
                                  OK := true
                               end
                            else writeln ('Number must be a positive integer.')
                         end
                   end
                else
                   begin
                      write ('The program will end so that the function F ');
                      writeln ('can be created. ');
                      OK := false
                   end
             end
      end
   end;
procedure OUTPUT;
   begin
      writeln ('Select output destination ');
      writeln ('1. Screen ');
      writeln ('2. Text file ');
      writeln ('Enter 1 or 2. ');
      readln ( FLAG );
      if ( FLAG = 2 ) then
         begin
            write ('Input the file name in the form - ');
            writeln ('drive:name.ext, ');
            writeln ('for example:   A:OUTPUT.DTA ');
            readln ( NAME );
            assign ( OUP, NAME )
         end
      else assign ( OUP, 'CON');
      rewrite ( OUP );
      writeln(OUP,'FAST FOURIER TRANSFORM');
      writeln(OUP)
   end;
function IBR( J, NU : integer) : integer;
var
   K, I, J2, J1 : integer;
begin
   J1 := J;
   K := 0;
   for I := 1 to NU do
      begin
         J2 := J1 div 2;
         K := 2*K+(J1-2*J2);
         J1 := J2
      end;
   IBR := K
end;
procedure CMULT ( A,B,C,D : real; var E,F : real );
{ Performs complex multiplication:
        (A + Bi) * (C + Di) -> E + Fi    }
   var
      A1, B1, C1, D1 : real;
   begin
      if ( abs(A) <= ZERO)  then A1 := 0.0
                            else A1 := A;
      if ( abs(B) <= ZERO)  then B1 := 0.0
                            else B1 := B;
      if ( abs(C) <= ZERO)  then C1 := 0.0
                            else C1 := C;
      if ( abs(D) <= ZERO)  then D1 := 0.0
                            else D1 := D;
      E := ( A1*C1 ) - (B1*D1 );
      F := A1*D1 + B1*C1
   end;
procedure CEXP ( A,B : real; var C,D : real );
{ Performs complex exponentiation:
        exp (A + Bi) -> E + Fi      }
   var
      E : real;
   begin
      E := exp( A );
      C := E * cos( B );
      D := E * sin( B )
   end;
begin
   INPUT;
   if (OK) then
      begin
         OUTPUT;
         TW := ln(2.0);
{        STEP 1                                                        }
{        use N2 for m, NG for p, NU1 for q, WW for zeta                }
         N2 := N div 2;
{        STEP 2                                                        }
         for I := 1 to N do
            begin
               CR[I] := Y[I];
               CI[I] := 0.0
            end;
         Z := N;
         NG := round(ln(Z)/TW);
         NU1 := NG - 1;
         YR := 0.0;
         YI := 2.0*PI/N;
         CEXP(YR,YI,WWR,WWI);
{        STEP 3                                                        }
         for I := 1 to N2 do
            begin
               XR := 1.0;
               XI := 0.0;
               YR := 1.0;
               YI := 0.0;
               for J := 1 to I do
                  begin
                     CMULT(XR,XI,WWR,WWI,YR,YI);
                     XR := YR;
                     XI := YI
                  end;
               WR[I] := XR;
               WI[I] := XI;
               WR[N2+I] := -XR;
               WI[N2+I] := -XI
            end;
{        STEP 4                                                        }
         K := 0;
{        STEP 5                                                        }
         for L := 1 to NG do
            begin
{              STEP 6                                                  }
               while (K < N-1) do
                  begin
{                    STEP 7                                            }
                     for I := 1 to N2 do
                        begin
{                          STEP 8                                      }
                           Z := exp(NU1*TW);
                           M1 := round(Z);
                           M1 := K div M1;
{                          IBR does the bit reversal                   }
                           NP := IBR(M1,NG);
{                          T1 = T1R + T1i is eta                       }
                           T1R := CR[K+N2+1];
                           T1I := CI[K+N2+1];
{                          STEP 9                                      }
                           if (NP <> 0) then
                              begin
                                 XR := T1R;
                                 XI := T1I;
                                 CMULT(XR,XI,WR[NP],WI[NP],T1R,T1I)
                              end;
                           CR[K+N2+1] := CR[K+1] - T1R;
                           CI[K+N2+1] := CI[K+1] - T1I;
                           CR[K+1] := CR[K+1] + T1R;
                           CI[K+1] := CI[K+1] + T1I;
{                          STEP 10                                     }
                           K := K+1
                        end;
{                    STEP 11                                           }
                     K := K+N2
                  end;
{             STEP 12                                                  }
              K := 0;
              N2 := N2 div 2;
              NU1 := NU1 - 1;
            end;
{        STEP 13                                                       }
         while (K < N-1) do
            begin
{              STEP 14                                                 }
               I := IBR(K,NG);
{              STEP 15                                                 }
               if (I > K) then
                  begin
                     T3R := CR[K+1];
                     CR[K+1] := CR[I+1];
                     CR[I+1] := T3R;
                     T3I := CI[K+1];
                     CI[K+1] := CI[I+1];
                     CI[I+1] := T3I
                  end;
{              STEP 16                                                 }
               K := K+1
            end;
{        STEPS 17 and 18                                               }
         writeln(OUP,'Coefficients c(0), ... , c(2m-1)');
         writeln(OUP);
         for I := 1 to N do
            begin
               YR := 0.0;
               YI := -(I-1)*PI;
               CEXP(YR,YI,XR,XI);
               CMULT(XR,XI,CR[I],CI[I],YR,YI);
               CR[I] := YR/(0.5*N);
               CI[I] := YI/(0.5*N);
               K := I - 1;
               writeln(OUP,K:3,' ',CR[I]:14:8,CI[I]:14:8)
            end;
         writeln(OUP);
         writeln(OUP,'Coefficients a(0), ..., a(m)');
         writeln(OUP);
         for I := 1 to M+1 do writeln(OUP, CR[I]);
         writeln(OUP);
         writeln(OUP,'Coefficients b(1), ..., b(m-1)');
         writeln(OUP);
         for I := 2 to M do writeln(OUP, CI[I]);
         close (OUP)
      end
   end.
