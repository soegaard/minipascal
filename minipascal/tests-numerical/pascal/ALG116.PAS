program alg116;
{
   CUBIC SPLINE RAYLEIGH-RITZ ALGORITHM 11.6

   To approximate the solution to the boundary-value problem

      -D(P(X)Y')/DX + Q(X)Y = F(X), 0 <= X <= 1, Y(0)=Y(1)=0

   With a sum of cubic splines:

   INPUT:  Integer n

   OUTPUT: Coefficients C(0),...,C(n+1) of the basis functions

   To change problems do the following:
       1. Change functions P, Q and F in the subprograms named P,Q,F
       2. Change dimensions in main program to values given by
             dimension A(N+2,N+3),X(N+2),C(N+2),CO(N+2,4,4),
                       DCO(N+2,4,3),AF(N+1),BF(N+1),CF(N+1),DF(N+1),
                       AP(N+1),BP(N+1),CP(N+1),DP(N+1),
                       AQ(N+1),BQ(N+1),CQ(N+1),DQ(N+1)
       3. Change dimensions in subroutine COEF to
             dimension AA(N+2),BB(N+2),CC(N+2),DD(N+2),
                       H(N+2),XA(N+2),XL(N+2),XU(N+2),XZ(N+2)

   GENERAL OUTLINE

       1. Nodes labelled X(I)=(I-1)*H, 1 <= I <= N+2, where
          H=1/(N+1) so that zero subscripts are avoided
       2. The functions PHI(I) and PHI'(I) are shifted so that
          PHI(1) and PHI'(1) are centered at X(1), PHI(2) and PHI'(2)
          are centered at X(2), . . . , PHI(N+2) and
          PHI'(N+2) are centered at (X(N+2)---for example,
                   PHI(3) = S((X-X(3))/H)
                          = S(X/H + 2)
       3. The functions PHI(I) are represented in terms of their
          coefficients in the following way:
          (PHI(I))(X) = CO(I,K,1) + CO(I,K,2)*X + CO(I,K,3)*X**2
                     CO(I,K,4) *X**3
          for X(J) <= X <= X(J+1) where
          K=1 IF J=I-2, K=2 IF J=I-1, K=3 IF J=I, K=4 IF J=I+1
          since PHI(I) is nonzero only between X(I-2) and X(I+2)
          unless I = 1, 2, N+1 or N+2
          (see subroutine PHICO)
       4. The derivative of PHI(I) denoted PHI'(I) is represented
          as in 3. By its coefficients DCO(I,K,L), L = 1, 2, 3
          (See subroutine DPHICO).
       5. The functions P,Q and F are represented by their cubic
          spline interpolants using clamped boundary conditions
          (see Algorithm 3.5).  Thus, for X(I) <= X <= X(I+1) we
          use AF(I)+BF(I)*(X-X[I])+CF(I)*(X-X[I])^2+DF(I)*(X-X[I])^3
          to represent F(X).  Similarly, AP,BP,CP,DP are used for P
          and AQ,BQ,CQ,DQ are used for Q.  (see subroutine COEF).
       6. The integrands in STEPS 6 and 9 are replaced by products
          of cubic polynomial approximations on each subinterval of
          length H and the integrals of the resulting polynomials
          are computed exactly.  (see subroutine XINT).                }
type
   VEC = array [1..20] of real;
var
   A : array [1..21,1..22] of real;
   CO : array [1..21,1..4,1..4] of real;
   DCO : array [1..21,1..4,1..3] of real;
   X, C : array [1..21] of real;
   AF, BF, CF, DF, AP, BP, CP, DP, AQ, BQ, CQ, DQ : VEC;
   T,H, XU, XL, A1, B1, C1, D1, A2, B2, C2, D2 : real;
   A3, B3, C3, D3, A4, B4, C4, D4, CC, S, SS : real;
   FPL,FPR,PPL,PPR,QPL,QPR : real;
   N, N1, N2, N3, I, J, K, II, JJ, J0, J1, J2 : integer;
   FLAG,KK,K2, K3, JJ1, JJ2 : integer;
   OK : boolean;
   NAME : string[30];
   AA : char;
   OUP : text;
function F (X : real) : real;
   begin
      F := 2*pi*pi*sin(pi*X)
   end;
function P (x : real) : real;
   begin
      P := 1
   end;
function Q (x : real) : real;
   begin
      Q := pi*pi
   end;
procedure INPUT;
   begin
      writeln('This is the Cubic Spline Rayleigh-Ritz Method.');
      OK := false;
      writeln ('Have functions P, Q and F been created ');
      writeln ('immediately preceding the INPUT procedure?  ');
      writeln ('Answer Y or N. ');
      readln ( AA );
      if ( AA = 'Y' ) or ( AA = 'y' ) then
         begin
            while ( not OK ) do
               begin
                  write ('Input a positive integer n, where x(0) = 0, ');
                  writeln ('..., x(n+1) = 1. ');
                  readln ( N );
                  if ( N <= 0 ) then
                     writeln ('Number must be a positive integer. ')
                  else OK := true
               end;
            writeln('Input the derivative of f at 0 and at 1');
            readln(FPL,FPR);
            writeln('Input the derivative of p at 0 and at 1');
            readln(PPL,PPR);
            writeln('Input the derivative of q at 0 and at 1');
            readln(QPL,QPR);
         end
      else writeln ('The program will end so that P, Q, F can be created. ')
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
      rewrite ( OUP );
      writeln(OUP,'CUBIC SPLINE RAYLEIGH-RITZ METHOD');
      writeln(OUP)
   end;
function MINO ( I,J : integer) : integer;
   begin
      MINO := I;
      if (J < I) then MINO := J
   end;
function MAXO (I,J : integer) : integer;
   begin
      MAXO := I;
      if (J > I) then MAXO := J
   end;
function INTE (J,JJ : integer) : integer;
   begin
      INTE := JJ - J + 3
   end;
procedure DPHICO ( I,J : integer; var A,B,C : real);
   var
      EE,E, AA, BB, CC : real;
      OK : boolean;
   begin
      A := 0.0;
      B := 0.0;
      C := 0.0;
      E := I - 1.0;
      OK := true;
      if ( (J < I-2) or (J >= I+2) ) then OK := false;
      if ( (I = 1) and (J < I) ) then OK := false;
      if ( (I = 2) and (J < I-1) ) then OK := false;
      if ( (I = N+1) and (J > N+1) ) then OK := false;
      if ( (I = N+2) and (J >= N+2) ) then OK := false;
      if ( OK ) then
         begin
            if (J <= I-2) then
               begin
                  A := ((E-4.0)*E+4.0)/(8.0*H);
                  B := (-E+2.0)/(4.0*H*H);
                  C := 1.0/(8.0*H*H*H);
                  OK := false
               end
            else
               begin
                  if ( J > I) then
                     begin
                        A := ((-E-4.0)*E-4.0)/(8.0*H);
                        B := (E+2.0)/(4.0*H*H);
                        C := -1.0/(8.0*H*H*H);
                        OK := false
                     end
                  else
                     begin
                        if (J > I-1) then
                           begin
                              A := (3.0*E+4.0)*E/(8.0*H);
                              B := (-3.0*E-2.0)/(4.0*H*H);
                              C := 3.0/(8.0*H*H*H);
                              if ( (I <> 1) and (I <> N+1) ) then OK := false
                           end
                        else
                           begin
                              A := (-3.0*E+4.0)*E/(8.0*H);
                              B := (3.0*E-2.0)/(4.0*H*H);
                              C := -3.0/(8.0*H*H*H);
                              if ( (I <> 2) and (I <> N+2) ) then OK := false
                           end
                     end
               end
         end;
      if ( OK ) then
         begin
            if ( I <= 2 ) then
               begin
                  AA := -1.0/(8.0*H);
                  BB := 1.0/(4.0*H*H);
                  CC := -1.0/(8.0*H*H*H);
                  if ( I = 2 ) then
                     begin
                        A := A - AA;
                        B := B - BB;
                        C := C - CC
                     end
                  else
                     begin
                        A := A - 4.0*AA;
                        B := B - 4.0*BB;
                        C := C - 4.0*CC
                     end
               end
            else
               begin
                  EE := N+2.0;
                  AA := ((EE-4.0)*EE+4.0)/(8.0*H);
                  BB := (-EE+2.0)/(4.0*H*H);
                  CC := 1.0/(8.0*H*H*H);
                  if ( I = N+1 ) then
                     begin
                        A := A - AA;
                        B := B - BB;
                        C := C - CC
                     end
                  else
                     begin
                        A := A - 4.0*AA;
                        B := B - 4.0*BB;
                        C := C - 4.0*CC
                     end
               end
         end
   end;
procedure PHICO ( I,J : integer; var A,B,C,D : real);
   var
      EE,E, AA, BB, CC,DD : real;
      OK : boolean;
   begin
      A := 0.0;
      B := 0.0;
      C := 0.0;
      D := 0.0;
      E := I - 1.0;
      OK := true;
      if ( (J < I-2) or (J >= I+2) ) then OK := false;
      if ( (I = 1) and (J < I) ) then OK := false;
      if ( (I = 2) and (J < I-1) ) then OK := false;
      if ( (I = N+1) and (J > N+1) ) then OK := false;
      if ( (I = N+2) and (J >= N+2) ) then OK := false;
      if ( OK ) then
         begin
            if (J <= I-2) then
               begin
                  A := (((-E+6.0)*E-12.0)*E+8.0)/24.0;
                  B := ((E-4.0)*E+4.0)/(8.0*H);
                  C := (-E+2.0)/(8.0*H*H);
                  D := 1.0/(24.0*H*H*H);
                  OK := false
               end
            else
               begin
                  if (J > I) then
                     begin
                        A := (((E+6.0)*E+12.0)*E+8.0)/24.0;
                        B := ((-E-4.0)*E-4.0)/(8.0*H);
                        C := (E+2.0)/(8.0*H*H);
                        D := -1.0/(24.0*H*H*H);
                        OK := false
                     end
                  else
                     begin
                        if ( J > I-1) then
                           begin
                              A := ((-3.0*E-6.0)*E*E+4.0)/24.0;
                              B := (3.0*E+4.0)*E/(8.0*H);
                              C := (-3.0*E-2.0)/(8.0*H*H);
                              D := 1.0/(8.0*H*H*H);
                              if ( (I <> 1) and (I <> N+1) ) then OK := false
                           end
                        else
                           begin
                              A := ((3.0*E-6.0)*E*E+4.0)/24.0;
                              B := (-3.0*E+4.0)*E/(8.0*H);
                              C := (3.0*E-2.0)/(8.0*H*H);
                              D := -1.0/(8.0*H*H*H);
                              if (( I <> 2) and (I <> N+2)) then OK := false
                           end
                     end
               end
         end;
      if ( OK ) then
         begin
            if ( I <= 2 ) then
               begin
                  AA := 1.0/24.0;
                  BB := -1.0/(8.0*H);
                  CC := 1.0/(8.0*H*H);
                  DD := -1.0/(24.0*H*H*H);
                  if ( I = 2 ) then
                     begin
                        A := A - AA;
                        B := B - BB;
                        C := C - CC;
                        D := D - DD
                     end
                  else
                     begin
                        A := A - 4.0*AA;
                        B := B - 4.0*BB;
                        C := C - 4.0*CC;
                        D := D - 4.0*DD
                     end
               end
            else
               begin
                  EE := N+2.0;
                  AA := (((-EE+6.0)*EE-12.0)*EE+8.0)/24.0;
                  BB := ((EE-4.0)*EE+4.0)/(8.0*H);
                  CC := (-EE+2.0)/(8.0*H*H);
                  DD := 1.0/(24.0*H*H*H);
                  if ( I = N+1 ) then
                     begin
                        A := A - AA;
                        B := B - BB;
                        C := C - CC;
                        D := D - DD
                     end
                  else
                     begin
                        A := A - 4.0*AA;
                        B := B - 4.0*BB;
                        C := C - 4.0*CC;
                        D := D - 4.0*DD
                     end
               end
         end
   end;
function XINT(XU,XL,A1,B1,C1,D1,A2,B2,C2,D2,A3,B3,C3,D3 : real) : real;
var
   C : array [1..20] of real;
   AA,BB,CC,DD,EE,FF,GG,XHIGH,XLOW : real;
   I : integer;
begin
   AA := A1*A2;
   BB := A1*B2+A2*B1;
   CC := A1*C2+B1*B2+C1*A2;
   DD := A1*D2+B1*C2+C1*B2+D1*A2;
   EE := B1*D2+C1*C2+D1*B2;
   FF := C1*D2+D1*C2;
   GG := D1*D2;
   C[10] := AA*A3;
   C[9] := (AA*B3+BB*A3)/2.0;
   C[8] := (AA*C3+BB*B3+CC*A3)/3.0;
   C[7] := (AA*D3+BB*C3+CC*B3+DD*A3)/4.0;
   C[6] := (BB*D3+CC*C3+DD*B3+EE*A3)/5.0;
   C[5] := (CC*D3+DD*C3+EE*B3+FF*A3)/6.0;
   C[4] := (DD*D3+EE*C3+FF*B3+GG*A3)/7.0;
   C[3] := (EE*D3+FF*C3+GG*B3)/8.0;
   C[2] := (FF*D3+GG*C3)/9.0;
   C[1] := (GG*D3)/10.0;
   XHIGH := 0.0;
   XLOW := 0.0;
   for I := 1 to 10 do
      begin
         XHIGH := (XHIGH + C[I])*XU;
         XLOW := (XLOW + C[I])*XL
      end;
   XINT := XHIGH - XLOW
end;
procedure COEF(L,N,M : integer; FPO, FPN : real; var A,B,C,D : VEC);
var
   AA,BB,CC,DD,XA,XL,XU,XZ : array[1..25] of real;
   I, J : integer;
begin
   for I := 1 to N do
         case L of
            1 : AA[I] := F(X[I]);
            2 : AA[I] := P(X[I]);
            3 : AA[I] := Q(X[I])
         end;
   XA[1] := 3.0*(AA[2]-AA[1])/H - 3.0*FPO;
   XA[N] := 3.0*FPN - 3.0*(AA[N]-AA[N-1])/H;
   XL[1] := 2.0*H;
   XU[1] := 0.5;
   XZ[1] := XA[1]/XL[1];
   for I := 2 to M do
      begin
         XA[I] := 3.0*(AA[I+1]-2.0*AA[I]+AA[I-1])/H;
         XL[I] := H*(4.0-XU[I-1]);
         XU[I] := H/XL[I];
         XZ[I] := (XA[I]-H*XZ[I-1])/XL[I]
      end;
   XL[N] := H*(2.0-XU[N-1]);
   XZ[N] := (XA[N]-H*XZ[N-1])/XL[N];
   CC[N] := XZ[N];
   for I := 1 to M do
      begin
         J := N-I;
         CC[J] := XZ[J]-XU[J]*CC[J+1];
         BB[J] := (AA[J+1]-AA[J])/H -H*(CC[J+1]+2.0*CC[J])/3.0;
         DD[J] := (CC[J+1]-CC[J])/(3.0*H)
      end;
   for I := 1 to M do
      begin
         A[I] := ((-DD[I]*X[I]+CC[I])*X[I]-BB[I])*X[I]+AA[I];
         B[I] := (3.0*DD[I]*X[I]-2.0*CC[I])*X[I]+BB[I];
         C[I] := CC[I]-3.0*DD[I]*X[I];
         D[I] := DD[I]
      end
end;
begin
   INPUT;
   if (OK) then
      begin
         OUTPUT;
{        STEP 1                                                        }
         H := 1.0/(N+1.0);
         N1 := N+1;
         N2 := N+2;
         N3 := N+3;
{        Initialize matrix A at zero, note that A[I, N+3] = B[I]       }
         for I := 1 to N2 do
            for J := 1 to N3 do
               A[I,J] := 0.0;
{        STEP 2                                                        }
{        X[1]=0,...,X[I] = (I-1)*H,...,X[N+1] = 1 - H, X[N+2] = 1      }
         for I := 1 to N2 do X[I] := (I-1.0)*H;
{        STEPS 3 and 4 are implemented in what follows.
             Initialize coefficients CO[I,J,K], DCO[I,J,K]             }
         for I := 1 to N2 do
            for J := 1 to 4 do
               begin
                  for K := 1 to 4 do
                     begin
                        CO[I,J,K] := 0.0;
                        if (K <> 4) then DCO[I,J,K] := 0.0
                     end;
{                 JJ corresponds the coefficients of phi and phi'
                  to the proper interval involving J                   }
                  JJ := I+J-3;
                  PHICO(I,JJ,CO[I,J,1],CO[I,J,2],CO[I,J,3],CO[I,J,4]);
                  DPHICO(I,JJ,DCO[I,J,1],DCO[I,J,2],DCO[I,J,3])
               end;
{        Output the basis functions.                                   }
         writeln(OUP,'Basis Function: A + B*X + C*X**2 + D*X**3');
         writeln(OUP);
         writeln(OUP,'                          A','            B'
         ,'            C','             D');
         writeln(OUP);
         for I := 1 to N2 do
            begin
               writeln(OUP,'phi( ',I,' )'); writeln(OUP);
               for J := 1 to 4 do
                  if ((I <> 1) or ((J <> 1) and (J <> 2))) then
                     if ((I <> 2) or (J <> 2)) then
                        if ((I <> N1) or (J <> 4)) then
                           if ((I <> N2) or ((J <> 3) and (J <> 4))) then
                              begin
                                 JJ1 := I+J-3;
                                 JJ2 := I+J-2;
                                 write(OUP,'On (X( ',JJ1,' ), X( ',JJ2,' )  ');
                                 for K := 1 to 4 do
                                     write(OUP,' ',CO[I,J,K]:12:8,' ');
                                 writeln(OUP);
                                 writeln(OUP)
                              end
            end;
{        Obtain coefficients for F, P, Q - NOTE _ the fourth and fifth
         arguements in COEF are the derivatives of F, P, or Q evaluated
         at 0 and 1 respectively.                                      }
         COEF(1,N2,N1,FPL,FPR,AF,BF,CF,DF);
         COEF(2,N2,N1,PPL,PPR,AP,BP,CP,DP);
         COEF(3,N2,N1,QPL,QPR,AQ,BQ,CQ,DQ);
{        STEPS 5 - 9 are implemented in what follows                   }
         for I := 1 to N2 do
{           indices for limits of integration for A[I,I] and B[I]      }
            begin
               J1 := MINO(I+2,N+2);
               J0 := MAXO(I-2,1);
               J2 := J1 - 1;
{              integrate over each subinterval where phi(I) nonzero    }
               for JJ := J0 to J2 do
                  begin
{                    limits of integration for each call               }
                     XU := X[JJ+1];
                     XL := X[JJ];
{                    coefficients of bases                             }
                     K := INTE(I,JJ);
                     A1 := DCO[I,K,1];
                     B1 := DCO[I,K,2];
                     C1 := DCO[I,K,3];
                     D1 := 0.0;
                     A2 := CO[I,K,1];
                     B2 := CO[I,K,2];
                     C2 := CO[I,K,3];
                     D2 := CO[I,K,4];
{                    call subprogram for integrations                  }
                     A[I,I] := A[I,I]+
                               XINT(XU,XL,AP[JJ],BP[JJ],CP[JJ],DP[JJ],
                               A1,B1,C1,D1,A1,B1,C1,D1)+
                               XINT(XU,XL,AQ[JJ],BQ[JJ],CQ[JJ],DQ[JJ],
                               A2,B2,C2,D2,A2,B2,C2,D2);
                     A[I,N+3] := A[I,N+3] +
                                 XINT(XU,XL,AF[JJ],BF[JJ],CF[JJ],DF[JJ],
                                 A2,B2,C2,D2,1.0,0.0,0.0,0.0)
                  end;
{              compute A[I,J] for J = I+1, ..., Min(I+3,N+2)           }
               K3 := I+1;
               if (K3 <= N2) then
                  begin
                     K2 := MINO(I+3,N+2);
                     for J := K3 to K2 do
                        begin
                           J0 := MAXO(J-2,1);
                           for JJ := J0 to J2 do
                              begin
                                 XU := X[JJ+1];
                                 XL := X[JJ];
                                 K := INTE(I,JJ);
                                 A1 := DCO[I,K,1];
                                 B1 := DCO[I,K,2];
                                 C1 := DCO[I,K,3];
                                 D1 := 0.0;
                                 A2 := CO[I,K,1];
                                 B2 := CO[I,K,2];
                                 C2 := CO[I,K,3];
                                 D2 := CO[I,K,4];
                                 K := INTE(J,JJ);
                                 A3 := DCO[J,K,1];
                                 B3 := DCO[J,K,2];
                                 C3 := DCO[J,K,3];
                                 D3 := 0.0;
                                 A4 := CO[J,K,1];
                                 B4 := CO[J,K,2];
                                 C4 := CO[J,K,3];
                                 D4 := CO[J,K,4];
                                 A[I,J] := A[I,J] +
                                           XINT(XU,XL,AP[JJ],BP[JJ],
                                           CP[JJ],DP[JJ],A1,B1,C1,D1,
                                           A3,B3,C3,D3)+
                                           XINT(XU,XL,AQ[JJ],BQ[JJ],
                                           CQ[JJ],DQ[JJ],A2,B2,C2,D2,
                                           A4,B4,C4,D4)
                              end;
                           A[J,I] := A[I,J]
                        end
                  end
            end;
{        STEP 10                                                       }
         for I := 1 to N1 do
            begin
               for J := I+1 to N2 do
                  begin
                     CC := A[J,I]/A[I,I];
                     for K := I+1 to N3 do A[J,K] := A[J,K]-CC*A[I,K];
                     A[J,I] := 0.0
                  end
            end;
         C[N2] := A[N2,N3]/A[N2,N2];
         for I := 1 to N1 do
            begin
               J := N1-I+1;
               C[J] := A[J,N3];
               for KK := J+1 to N2 do C[J] := C[J]-A[J,KK]*C[KK];
               C[J] := C[J]/A[J,J]
            end;
{        STEP 11
         output coefficients                                           }
         writeln(OUP);
         writeln(OUP,'Coefficients:  c(1), c(2), ... , c(n)');
         writeln(OUP);
         for I := 1 to N1+1 do writeln(OUP,'  ',C[I]:12,' ');
         writeln(OUP);
{        compute and output value of the approximation at the nodes    }
         writeln(OUP,'The approximation evaluated at the nodes: ');
         writeln(OUP);
         writeln(OUP,'    Node         Value');
         writeln(OUP);
         for I := 1 to N2 do
            begin
               S := 0.0;
               for J := 1 to N2 do
                  begin
                     J0 := MAXO(J-2,1);
                     J1 := MINO(J+2,N+2);
                     SS := 0.0;
                     if ((I < J0) or (I >= J1)) then S := S + C[J]*SS
                     else
                        begin
                           K := INTE(J,I);
                           SS := ((CO[J,K,4]*X[I]+CO[J,K,3])*X[I]+
                                 CO[J,K,2])*X[I]+CO[J,K,1];
                           S := S + C[J]*SS
                        end
                  end;
               writeln(OUP,X[I]:12:8,' ',S:12:8)
            end;
{        STEP 12                                                       }
         close(OUP)
      end
end.