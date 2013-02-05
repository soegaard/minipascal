program ALG082;
{  CHEBYSHEV RATIONAL APPROXIMATION ALGORITHM 8.2

   To obtain the rational approximation

   rT(x) = (p0*T0 + p1*T1 +...+ Pn*Tn) / (q0*T0 + q1*T1 +...+ qm*Tm)

   for a given function f(x):

   INPUT  nonnegative integers m and n.

   OUTPUT  coefficients q0, q1, ... , qm, p0, p1, ... , pn.

   The coefficients of the Chebyshev expansion a0, a1,  ... could
   be calculated instead of input as is assumed in this program.

   Note that a0 is to be doubled.                                      }
const
   ZERO = 1.0E-20;
var
   INP,OUP : text;
   A : array [ 0..10, 0..11 ] of real;
   AA : array [0..10] of real;
   NROW : array [ 0..10 ] of integer;
   P,Q : array[0..6] of real;
   AMAX,XM,SUM : real;
   LM,LN,BN : integer;
   PP,FLAG,N,M,I,NN,IMAX,J,JJ,IP,JP,NCOPY,I1,J1,N1,K,N2,LL,KK : integer;
   OK : boolean;
   NAME : string [ 30 ];
   AAA : char;
procedure INPUT;
   begin
      writeln('This is Chebyshev Approximation.');
      writeln;
      OK := false;
      while ( not OK ) do
         begin
            writeln('The Chebyshev coefficients a[0], a[1], ... , a[N+m]');
            writeln('are to be input.');
            writeln ('Choice of input method: ');
            writeln ('1. Input entry by entry from keyboard ');
            writeln ('2. Input data from a text file ');
            writeln ('Choose 1 or 2 please ');
            readln ( FLAG );
            if ( FLAG = 1 ) or ( FLAG = 2 ) then OK := true
         end;
      case FLAG of
         1 : begin
                OK := false;
                while (not OK) do
                   begin
                      writeln('Input m and n ');
                      readln(LM,LN);
                      BN := LM + LN;
                      if ( (LM >= 0) and (LN >= 0) ) then OK := true
                      else
                         writeln('m and n must both be nonnegative.');
                      if (LM = 0) and (LN = 0) then
                         begin
                            OK := false;
                            writeln('Not both m and n can be zero')
                         end
                end;
                writeln ('Input in order a[0] to a[N+m]');
                for I := 0 to BN+LM do
                  begin
                     writeln('Enter A[',I:3,']');
                     readln( AA[I]);
                  end
             end;
         2 : begin
                writeln ('Has a text file been created?');
                writeln ('Enter Y or N ');
                readln ( AAA );
                if ( AAA = 'Y' ) or ( AAA = 'y' ) then
                   begin
                      write ('Input the file name in the form - ');
                      writeln ('drive:name.ext ');
                      writeln ('for example:   A:DATA.DTA ');
                      readln ( NAME );
                      assign ( INP, NAME );
                      reset ( INP );
                      OK := false;
                      while (not OK) do
                         begin
                            writeln('Input m and n ');
                            readln(LM,LN);
                            BN := LM + LN;
                            if ( (LM >= 0) and (LN >= 0) ) then OK := true
                            else
                               writeln('m and n must both be nonnegative.');
                            if (LM = 0) and (LN = 0) then
                               begin
                                  OK := false;
                                  writeln('Not both m and n can be zero')
                               end
                         end;
                      for I := 0 to BN+LM do read(INP,AA[I]);
                      close(INP)
                   end
                else
                   begin
                      writeln ('Please create the input file.');
                      write ('The program will end so the input file can ');
                      writeln ('be created. ');
                      OK := false
                   end
             end;
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
      else  assign ( OUP, 'CON' );
      rewrite ( OUP );
      writeln(OUP,'CHEBYSHEV RATIONAL APPROXIMATION');
      writeln(OUP);
      writeln(OUP,'Denominator Coefficients Q[0], ..., Q[M]');
      for I := 0 to LM do
            write ( OUP, Q[I]:12:8 );
      writeln(OUP);
      writeln(OUP,'Numerator Coefficients P[0], ..., P[N]');
      for J := 0 to LN do
            write(OUP,P[J]:12:8);
      writeln(OUP);
      close ( OUP )
   end;
   begin
      INPUT;
      if ( OK ) then
         begin
{           STEP 1                                                     }
            N := BN;
            M := N + 1;
{           STEP 2 - performed on input                                }
            for I := 0 to N do NROW[I] := I;
{           initialize row pointer                                     }
            NN := N - 1;
{           STEP 3                                                     }
            Q[0] := 1.0;
{           STEP 4
            set up a linear system with matrix A instead of B          }
            for I := 0 to N do
               begin
{                 STEP 5                                               }
                  for J := 0 to I do
                     begin
                        if J <= LN then A[I,J] := 0.0
                     end;
{                 STEP 6                                               }
                  if I <= LN then A[I,I] := 1.0;
{                 STEP 7                                               }
                  for J := I+1 to LN do A[I,J] := 0.0;
{                 STEP 8                                               }
                  for J := LN+1 to N do
                     begin
                        if I <> 0 then
                           begin
                              PP := I-J+LN;
                              if PP < 0 then PP := -PP;
                              A[I,J] := -(AA[I+J-LN]+AA[PP])/2.0
                           end
                        else A[I,J] := -AA[J-LN]/2.0
                     end;
                  A[I,N+1] := AA[I]
               end;
{           STEP 9                                                     }
            A[0,N+1] := A[0,N+1]/2.0;
{           STEPS 10-21 solve the linear system using partial pivoting }
            I := LN+1;
{           STEP 10                                                    }
            while ( OK ) and ( I <= NN ) do
               begin
{                 STEP 11                                              }
                  IMAX := NROW[I];
                  AMAX := abs( A[IMAX,I] );
                  IMAX := I;
                  JJ := I + 1;
                  for IP := JJ to N do
                     begin
                        JP := NROW[IP];
                        if ( abs( A[JP,I] ) > AMAX ) then
                           begin
                              AMAX := abs( A[JP,I] );
                              IMAX := IP
                           end
                     end;
{                    STEP 12                                           }
                     if ( AMAX <= ZERO ) then OK := false
                     else
                        begin
{                          STEP 13                                     }
{                          simulate row interchange                    }
                           if ( NROW[I] <> NROW[IMAX] ) then
                              begin
                                 NCOPY := NROW[I];
                                 NROW[I] := NROW[IMAX];
                                 NROW[IMAX] := NCOPY
                              end;
                           I1 := NROW[I];
{                          STEP 14                                     }
{                          Perform elimination.                        }
                           for J := JJ to N do
                              begin
                                 J1 := NROW[J];
{                                STEP 15                               }
                                 XM := A[J1,I] / A[I1,I];
{                                STEP 16                               }
                                 for K := JJ to M do
                                    A[J1,K] := A[J1,K] - XM * A[I1,K];
{                                STEP 17                               }
                                 A[J1,I] := 0.0
                              end
                        end;
                   I := I + 1
               end;
            if ( OK ) then
               begin
{                 STEP 18                                              }
                  N1 := NROW[N];
                  if ( abs( A[N1,N] ) <= ZERO ) then OK := false
{                 system has no unique solution                        }
                  else
                     begin
{                       STEP 19                                        }
{                       start backward substitution                    }
                        if LM > 0 then
                           begin
                              Q[LM] := A[N1,M] / A[N1,N];
                              A[N1,M] := Q[LM];
                           end;
                        PP := 1;
{                       STEP 20                                        }
                        for K := LN+1 to NN do
                           begin
                              I := NN - K + LN+1;
                              JJ := I + 1;
                              N2 := NROW[I];
                              SUM := A[N2,N+1];
                              for KK := JJ to N do
                                 begin
                                    LL := NROW[KK];
                                    SUM := SUM - A[N2,KK] * A[LL,M]
                                 end;
                              A[N2,M] := SUM / A[N2,I];
                              Q[LM-PP] := A[N2,M];
                              PP := PP + 1
                           end;
{                       STEP 21                                        }
                        for K := 0 to LN do
                           begin
                              I := LN - K;
                              N2 := NROW[I];
                              SUM := A[N2,N+1];
                              for KK := LN+1 to N do
                                 begin
                                    LL := NROW[KK];
                                    SUM := SUM - A[N2,KK] * A[LL,M]
                                 end;
                              A[N2,M] := SUM;
                              P[LN-K] := A[N2,M];
                           end;
{                       STEP 22                                        }
{                       procedure completed successfully               }
                        OUTPUT
                     end
               end;
            if ( not OK ) then writeln ('System has no unique solution ')
         end
   end.

