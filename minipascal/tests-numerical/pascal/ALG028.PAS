program alg028;
{  MULLER'S ALGORITHM 2.8

   To find a solution to f(x) = 0 given three approximations x0, x1
   and x2:

   INPUT:  x0,x1,x2; tolerance TOL; maximum number of iterations NO.

   OUTPUT: approximate solution p or message of failure.

   This implementation allows for a switch to complex arithmetic.
   The coefficients are stored in the vector A, so the dimension
   of A may have to be changed.                                        }
const
   ZERO = 1.0e-20;
var
   A : array[1..50] of real;
   ZR,ZI,GR,GI,F,X : array[1..4] of real;
   CH1R,CH1I,H : array [1..3] of real;
   CDEL1R,CDEL1I,DEL1 : array[1..2] of real;
   CDELR,CDELI,CBR,CBI,CDR,CDI,CER,CEI : real;
   DEL,B,D,E,TOL : real;
   QR,QI,ER,EI,FR,FI,P : real;
   FLAG,N,M,I,J,K,ISW,KK : integer;
   OK : boolean;
   NAME : string [ 30 ];
   INP, OUP : text;
   AA : char;
procedure CADD ( A,B,C,D : real; var E,F : real );
{ procedure to perform complex addition :
   (A + Bi) + (C + Di) -> E + Fi  }
   begin
      E := A + C;
      F := B + D
   end;
procedure CMULT ( A,B,C,D : real; var E,F : real );
{ procedure to perform complex multiplication :
   (A + Bi) * (C + Di) -> E + Fi  }
   begin
      E := ( A * C ) - ( B * D );
      F := A * D + B * C
   end;
procedure CDIV ( A,B,C,D : real; var E,F : real );
{ procedure to perform complex division :
   (A + Bi) / (C + Di) -> E + Fi  }
   var
      G : real;
   begin
      G := C * C + D * D;
      E := ( A * C + B * D ) / G;
      F := ( B * C - A * D ) / G
   end;
function CABS ( A,B : real ) : real;
{ function to compute complex absolute value :
   | A + Bi | = sqrt(A*A + B*B)  }
   var
      C : real;
   begin
      C := sqrt(A * A + B * B);
      CABS := C
   end;
 procedure CSQRT ( A,B : real; var C,D : real );
 {procedure to compute complex square root:
    sqrt( A + Bi) -> C + Di  }
   const
      ZERO = 1.0E-20;
   var
      G,R,T,HP : real;
   begin
      HP := 0.5*pi;
      if ( abs( A ) <= ZERO ) then
         begin
            if ( abs( B ) <= ZERO ) then
               begin
                  R := 0.0;
                  T := 0.0
               end
            else
               begin
                  T := HP;
                  if ( B < 0.0 ) then T := -T;
                  R := abs( B )
               end
         end
      else
         begin
            R :=  sqrt(A * A + B * B) ;
            if (abs(B) < ZERO) then
               begin
                  T := 0.0;
                  if (A < 0.0) then T := pi
               end
            else
               begin
                  T := arctan( B / A );
                  if (A < 0.0) then T := T + pi
               end
         end;
      G := sqrt( R );
      C := G * cos( 0.5 * T );
      D := G * sin( 0.5 * T )
   end;
procedure CSUB ( A,B,C,D : real; var E,F : real );
{ procedure to perform complex subtraction :
   (A + Bi) - (C + Di) -> E + Fi  }
   begin
      E := A - C;
      F := B - D
   end;
procedure INPUT;
   begin
      writeln('This is Mullers Method');
      OK := false;
      while ( not OK ) do
         begin
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
                while ( not OK ) do
                   begin
                      writeln('Input the degree n of the polynomial');
                      readln(N);
                      if ( N > 0 ) then
                         begin
                            OK := true;
                            write('Input the coefficients of the');
                            writeln(' polynomial in ascending order');
                            writeln('of exponent at the prompt');
                            N := N+1;
                            for I := 1 to N do
                               begin
                                  J := I-1;
                                  writeln('Input A( ',J,' )');
                                  readln(A[I])
                               end
                         end
                      else writeln(' n must be a positive integer.');
                   end;
             end;
         2 : begin
                writeln('Is there a text file containing the coefficients');
                writeln('in ascending order of exponent?');
                writeln ('Enter Y or N ');
                readln ( AA );
                if ( AA = 'Y' ) or ( AA = 'y' ) then
                   begin
                      OK := true;
                      write ('Input the file name in the form - ');
                      writeln ('drive:name.ext ');
                      writeln ('for example:   A:DATA.DTA ');
                      readln ( NAME );
                      assign ( INP, NAME );
                      reset ( INP );
                      OK := false;
                      while ( not OK ) do
                         begin
                            writeln ('Input n');
                            readln ( N );
                            if ( N > 0 ) then
                               begin
                                  OK := true;
                                  N := N+1;
                                  for I := 1 to N do
                                     read ( INP,A[I]);
                                  close ( INP )
                               end
                            else writeln ('Number must be a positive integer ')
                         end
                   end
                else
                   begin
                      writeln ('Please create the input file.');
                      writeln ('The program will end so the input file can ');
                      writeln ('be created. ');
                      OK := false
                   end
             end
      end;
      if (A[N] = 0) and OK then
         begin
            writeln('Leading coefficient is 0 - error in input');
            OK := false
         end;
      if (N = 2) and OK then
         begin
            P := -A[1]/A[2];
            writeln('Polynomial is linear:  root is ',P);
            OK := false;
         end;
      if OK then
         begin
            OK := false;
            while ( not OK ) do
               begin
                  writeln ('Input tolerance ');
                  readln ( TOL );
                  if (TOL <= 0.0) then
                     writeln ('Tolerance must be positive ')
                  else OK := true
               end;
            OK := false;
            while ( not OK ) do
               begin
                  write('Input maximum number of iterations ');
                  writeln('- no decimal point ');
                  readln ( M );
                  if ( M <= 0 ) then
                     writeln ('Must be positive integer ')
                  else OK := true
               end;
            writeln('Input the first of three starting values');
            readln(X[1]);
            writeln('Input the second of three starting values');
            readln(X[2]);
            writeln('Input the third starting value');
            readln(X[3]);
         end;
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
      writeln(OUP,'MULLERS METHOD');
      writeln(OUP,'The input polynomial:');
      for I := 1 to N do
         begin
            writeln(OUP,'Coefficient of x^',I-1,' is ',A[I]:12:8);
         end;
      writeln(OUP,'The output is i, approximation x(i), f(x(i))');
      writeln(OUP);
      writeln(OUP,'Three columns means the results are real numbers,');
      writeln(OUP,'Five columns means the results are complex ');
      writeln(OUP,'numbers with real and imaginary parts of x(i)');
      writeln(OUP,'followed by real and imaginary parts f(x(i)).');
      writeln(OUP);
   end;
begin
   INPUT;
   if OK then
     begin
        OUTPUT;
{  Evaluate F using Horner's Method and save in the vector F           }
        for I := 1 to 3 do
           begin
              F[I] := A[N];
              for J := 2 to N do
                 begin
                    K := N-J+1;
                    F[I] := F[I]*X[I]+A[K]
                 end
           end;
{  Variable ISW is used to note a switch to complex arithmetic
    ISW=0 means real arithmetic, and ISW=1 means complex arithmetic    }
        ISW := 0;
{       STEP 1                                                         }
        H[1] := X[2]-X[1];
        H[2] := X[3]-X[2];
        DEL1[1] := (F[2]-F[1])/H[1];
        DEL1[2] := (F[3]-F[2])/H[2];
        DEL := (DEL1[2]-DEL1[1])/(H[2]+H[1]);
        I := 3;
        OK := true;
{       STEP 2                                                         }
        while ((I <= M) and (OK)) do
           begin
{        STEPS 3-7 for real arithmetic                                 }
              if (ISW = 0) then
                 begin
{              STEP 3                                                  }
                    B := DEL1[2]+H[2]*DEL;
                    D := B*B-4.0*F[3]*DEL;
{              test to see if need complex arithmetic                  }
                    if (D >= 0.0) then
                       begin
{                   real arithmetic - test to see if straight line     }
                          if (abs(DEL) <= ZERO) then
                             begin
{                          straight line - test if horizontal line     }
                                   if (abs(DEL1[2]) <= ZERO) then
                                      begin
                                         writeln('Horizontal Line');
                                         OK := false
                                      end
                                   else
                                      begin
{                                straight line but not horizontal      }
                                   writeln('line - not horizontal');
                                         X[4] := (F[3]-DEL1[2]*X[3])
                                              /DEL1[2];
                                         H[3] := X[4]-X[3]
                                      end
                             end
                          else
                             begin
{                          not straight line                           }
                                D := sqrt(D);
{                          STEP 4                                      }
                                E := B+D;
                                if (abs(B-D) > abs(E)) then E := B-D;
{                          STEP 5                                      }
                                H[3] := -2.0*F[3]/E;
                                X[4] := X[3]+H[3]
                             end;
                          if (OK) then
                             begin
{                          evaluate f(x(I))=F[4] by Horner's method    }
                                F[4] := A[N];
                                for J := 2 to N do
                                   begin
                                      K := N-J+1;
                                      F[4] := F[4]*X[4]+A[K]
                                   end;
                                writeln(OUP,I,X[4],F[4]);
{                          STEP 6                                      }
                                if (abs(H[3]) < TOL) then
                                   begin
                                      writeln(OUP);
                                      writeln(OUP,'Method Succeeds');
                                      write(OUP,'Approximation is within ');
                                      writeln(OUP,TOL);
                                      writeln(OUP,'in ',I,' iterations');
                                      OK := false
                                   end
                                else
{                             STEP 7                                   }
                                   begin
                                      for J := 1 to 2 do
                                         begin
                                            H[J] := H[J+1];
                                            X[J] := X[J+1];
                                            F[J] := F[J+1]
                                         end;
                                      X[3] := X[4];
                                      F[3] := F[4];
                                      DEL1[1] := DEL1[2];
                                      DEL1[2] := (F[3]-F[2])/H[2];
                                      DEL := (DEL1[2]-DEL1[1])
                                           /(H[2]+H[1])
                                   end
                             end
                       end
                    else
                       begin
{                    switch to complex arithmetic                      }
                          ISW := 1;
                          for J := 1 to 3 do
                             begin
                                ZR[J] := X[J]; ZI[J] := 0.0;
                                GR[J] := F[J]; GI[J] := 0.0
                             end;
                          for J := 1 to 2 do
                             begin
                                CDEL1R[J] := DEL1[J]; CDEL1I[J] := 0.0;
                                CH1R[J] := H[J]; CH1I[J] := 0.0
                             end;
                          CDELR := DEL; CDELI := 0.0
                       end
                 end;
              if ((ISW = 1) and (OK)) then
{               STEPS 3-7 complex arithmetic                           }
                 begin
{              test if straight line                                   }
                    if (CABS(CDELR,CDELI) <= ZERO) then
                       begin
{                   straight line - test if horizontal line            }
                          if (CABS(CDEL1R[1],CDEL1I[1]) <= ZERO) then
                             begin
                                writeln('horizontal line - complex');
                                OK := false
                             end
                          else
{                       straight line but not horizontal               }
                             begin
                                writeln('line - not horizontal');
                                CMULT(CDEL1R[2],CDEL1I[2],
                                      ZR[3],ZI[3],ER,EI);
                                CSUB(GR[3],GI[3],ER,EI,FR,FI);
                                CDIV(FR,FI,CDEL1R[2],
                                     CDEL1I[2],ZR[4],ZI[4]);
                                CSUB(ZR[4],ZI[4],ZR[3],ZI[3],
                                     CH1R[3],CH1I[3])
                             end
                       end
                    else
{                not straight line                                     }
                       begin
{                   STEP 3                                             }
                          CMULT(CH1R[2],CH1I[2],CDELR,CDELI,ER,EI);
                          CADD(CDEL1R[2],CDEL1I[2],ER,EI,CBR,CBI);
                          CMULT(GR[3],GI[3],CDELR,CDELI,ER,EI);
                          CMULT(ER,EI,4.0,0.0,FR,FI);
                          QR := CBR; QI := CBI;
                          CMULT(CBR,CBI,QR,QI,ER,EI);
                          CSUB(ER,EI,FR,FI,CDR,CDI);
                          CSQRT(CDR,CDI,FR,FI);
{                    STEP 4                                            }
                          CDR := FR; CDI := FI;
                          CADD(CBR,CBI,CDR,CDI,CER,CEI);
                          CSUB(CBR,CBI,CDR,CDI,FR,FI);
                          if (CABS(FR,FI) > CABS(CER,CEI)) then
                              CSUB(CBR,CBI,CDR,CDI,CER,CEI);
{                    STEP 5                                            }
                          CDIV(GR[3],GI[3],CER,CEI,ER,EI);
                          CMULT(ER,EI,-2.0,0.0,CH1R[3],CH1I[3]);
                          CADD(ZR[3],ZI[3],CH1R[3],CH1I[3],ZR[4],ZI[4])
                       end;
                    if (OK) then
                       begin
{                    evaluate f(x(i))=f(4) by Horner's method          }
                          GR[4]:= A[N]; GI[4] := 0.0;
                          for J := 2 to N do
                             begin
                                K := N-J+1;
                                CMULT(GR[4],GI[4],ZR[4],ZI[4],ER,EI);
                                GR[4] := ER+A[K];
                                GI[4] := EI
                             end;
                          writeln(OUP,i:4,' ',ZR[4]:14:8,' ',ZI[4]:14:8,' ',
                                  GR[4]:14:8,' ',GI[4]:14:8);
{                       STEP 6                                         }
                          if (CABS(CH1R[3],CH1I[3]) <= TOL) then
                             begin
                                writeln(OUP);
                                writeln(OUP,'Method Succeeds');
                                writeln(OUP,'Approximation is within ',TOL);
                                writeln(OUP,'in ',I,' iterations');
                                OK := false
                             end
                          else
{                       STEP 7                                         }
                             begin
                                for J := 1 to 2 do
                                   begin
                                      CH1R[J] := CH1R[J+1];
                                      CH1I[J] := CH1I[J+1];
                                      ZR[J] := ZR[J+1];
                                      ZI[J] := ZI[J+1];
                                      GR[J] := GR[J+1];
                                      GI[J] := GI[J+1]
                                   end;
                                ZR[3] := ZR[4];
                                ZI[3] := ZI[4];
                                GR[3] := GR[4];
                                GI[3] := GI[4];
                                CDEL1R[1] := CDEL1R[2];
                                CDEL1I[1] := CDEL1I[2];
                                CSUB(GR[3],GI[3],GR[2],GI[2],ER,EI);
                                CDIV(ER,EI,CH1R[2],CH1I[2],
                                     CDEL1R[2],CDEL1I[2]);
                                CSUB(CDEL1R[2],CDEL1I[2],
                                     CDEL1R[1],CDEL1I[1],ER,EI);
                                CADD(CH1R[2],CH1I[2],CH1R[1],
                                     CH1I[1],FR,FI);
                                CDIV(ER,EI,FR,FI,CDELR,CDELI)
                             end
                       end
                 end;
{         STEP 7 CONTINUED                                             }
              I := I + 1
           end;
{    STEP 8                                                            }
        if ( I > M ) and OK then
          writeln(OUP,'Method failed to give accurate approximation.');
        close(OUP)
     end
end.
