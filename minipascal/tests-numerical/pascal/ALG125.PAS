program alg125;
{  Finite Element Algorithm 12.5

   To approximate the solution to an elliptic partial-differential
   equation subject to Dirichlet, mixed, or Neumann boundary
   conditions:

   Input:   see STEP 0

   Output:  description of triangles, nodes, line integrals, basis
            functions, linear system to be solved, and the
            coefficients of the basis functions


   Step 0
   General outline

      1. Triangles numbered: 1 to K for triangles with no edges on
         Script-S-1 or Script-S-2, K+1 to N for triangles with
         edges on script-S-2, N+1 to M for remaining triangles.
         Note: K=0 implies that no triangle is interior to D.
         Note: M=N implies that all triangles have edges on
         script-S-2.

      2. Nodes numbered: 1 to LN for interior nodes and nodes on
         script-S-2, LN+1 to LM for nodes on script-S-1.
         Note: LM and LN represent lower case m and n resp.
         Note: LN=LM implies that script-S-1 contains no nodes.
         Note: If a node is on both script-S-1 and script-S-2, then
         it should be treated as if it were only on script-S-1.

      3. NL=number of line segments on script-S-2
         line(I,J) is an NL by 2 array where
         line(I,1)= first node on line I and
         line(I,2)= second node on line I taken
         in positive direction along line I

      4. For the node labelled KK,KK=1,...,LM we have:
         A) coordinates XX(KK),YY(KK)
         B) number of triangles in which KK is a vertex= LL(KK)
         C) II(KK,J) labels the triangles KK is in and
         NV(KK,J) labels which vertex node KK is for
         each J=1,...,LL(KK)

      5. NTR is an M by 3 array where
         NTR(I,1)=node number of vertex 1 in triangle I
         NTR(I,2)=node number of vertex 2 in triangle I
         NTR(I,3)=node number of vertex 3 in triangle I

      6. Function subprograms:
         A) P,Q,R,F,G,G1,G2 are the functions specified by
            the particular differential equation
         B) RR is the integrand
            R*N(J)*N(K) on triangle I in step 4
         C) FFF is the integrand F*N(J) on triangle I in step 4
         D) GG1=G1*N(J)*N(K)
            GG2=G2*N(J)
            GG3=G2*N(K)
            GG4=G1*N(J)*N(J)
            GG5=G1*N(K)*N(K)
            integrands in step 5
         E) QQ(FF) computes the double integral of any
            integrand FF over a triangle with vertices given by
            nodes J1,J2,J3 - the method is an O(H**2) approximation
            for triangles
         F) SQ(PP) computes the line integral of any integrand PP
            along the line from (XX(J1),YY(J1)) to (XX(J2),YY(J2))
            by using a parameterization given by:
            X=XX(J1)+(XX(J2)-XX(J1))*T
            Y=YY(J1)+(YY(J2)-YY(J1))*T
            for 0 <= t <= 1
            and applying Simpson's composite method with H=.01

      7. Arrays:
         A) A,B,C are M by 3 arrays where the basis function N
            for the Ith triangle, Jth vertex is
            N(X,Y)=A(I,J)+B(I,J)*X+C(I,J)*Y
            for J=1,2,3 and i=1,2,...,M
         B) XX,YY are LM by 1 arrays to hold coordinates of nodes
         C) line,LL,II,NV,NTR have been explained above
         D) Gamma and Alpha are clear

      8. Note that A,B,C,XX,YY,I,I1,I2,J1,J2,J3,Delta are reserved
         storage so that in any subprogram we know that
         triangle I has vertices (XX(J1),YY(J1)),(XX(J2),YY(J2)),
         (XX(J3),YY(J3)). That is, vertex 1 is node J1, vertex 2 is
         node J2, vertex 3 is node J3 unless a line integral is
         involved in which case the line integral goes from node J1
         to node J2 in triangle I or unless vertex I1 is node J1
         and vertex I2 is node J2 - the basis functions involve
         A(I,I1)+B(I,I1)*X+C(I,I1)**Y for vertex I1 in triangle I
         and A(I,I2)+B(I,I2)*X+C(I,I2)*Y for vertex I2 in triangle I
         delta is 1/2 the area of triangle I

    To change problems:
      1) change function subprograms P,Q,R,F,G,G1,G2
      2) change data input for K,N,M,LN,LM,NL.
      3) change data input for XX,YY,LLL,II,NV.
      4) change data input for line.
      5) change definition statements to read :
         A(M,3),B(M,3),C(M,3),XX(LM),YY(LM)
         definition LINE(NL,2),LL(LM),II(LM,MAX LL(LM)),
              NV(LM,MAX LL(LM))
         definition NTR(M,3),GAMMA(LM),ALPHA(LN,LN+1), use
         the appropriate numbers for the variables M, LM,
         NL.
                                                                       }
var
   A,B,C : array [1..10,1..3] of real;
   XX,YY,GAMMA : array[1..11] of real;
   ALPHA : array [1..5,1..6] of real;
   II, NV : array [1..11,1..5] of integer;
   NTR : array [1..10,1..3] of integer;
   LL : array [1..11] of integer;
   LINE : array [1..5,1..2] of integer;
   XJ,XJ1,CCC,XJ2,XI1,XI2,HH,ZZ,DELTA : real;
   I,I1,I2,J1,J2,J3,K,N,M,LN1,LM,NL,KK,LLL,J,N1,N2,K1,L1,L,JJ1 : integer;
   JI,JJ2,INN : integer;
   AA : char;
   OK : boolean;
   OUP,INP : text;
   NAME : string [30];
function P(X,Y:real) : real;
   begin
      P := 1.0
   end;
function Q(X,Y:real) : real;
   begin
      Q := 1.0
   end;
function R(X,Y:real) : real;
   begin
      R := 0.0
   end;
function F(X,Y:real) : real;
   begin
      F := 0.0
   end;
function G(X,Y:real) : real;
   begin
      G := 4.0
   end;
function G1(X,Y:real) : real;
   begin
      G1 := 0.0
   end;
function G2(X,Y:real) : real;
   var
      T,Z1 : real;
   begin
      T := 1.0e-05;
      Z1 := 0.0;
      if ( ( (0.2-T) <= X) and (X <= (0.4+T)) and (abs(Y-0.2) <= T)) then
         Z1 := X;
      if ( ( (0.5-T) <= X) and (X <= (0.6+T)) and (abs(Y-0.1) <= T)) then
         Z1 := X;
      if ( ( -T <= Y) and (Y <= (0.1+T)) and (abs(X-0.6) <= T)) then
         Z1 := Y;
      if ( ( -T <= X) and (X <= (0.2+T)) and (abs(Y+X-0.4) <= T)) then
         Z1 := (X+Y)/sqrt(2.0);
      if ( (0.4 -T <= X) and (X <= (0.5+T)) and (abs(Y+X-0.6) <= T)) then
         Z1 := (X+Y)/sqrt(2.0);
      G2 := Z1
   end;
function RR(X,Y:real) : real;
   begin
      RR := R(X,Y)*(A[I,I1]+B[I,I1]*X+C[I,I1]*Y)*
            (A[I,I2]+B[I,I2]*X+C[I,I2]*Y)
   end;
function FFF(X,Y:real) : real;
   begin
      FFF := F(X,Y)*(A[I,I1]+B[I,I1]*X+C[I,I1]*Y)
   end;
function GG1(X,Y:real) : real;
   begin
      GG1 := G1(X,Y)*(A[I,I1]+B[I,I1]*X+C[I,I1]*Y)*
             (A[I,I2]+B[I,I2]*X+C[I,I2]*Y)
   end;
function GG2(X,Y:real) : real;
   begin
      GG2 := G2(X,Y)*(A[I,I1]+B[I,I1]*X+C[I,I1]*Y)
   end;
function GG3(X,Y:real) : real;
   begin
      GG3 := G2(X,Y)*(A[I,I2]+B[I,I2]*X+C[I,I2]*Y)
   end;
function GG4(X,Y:real) : real;
   begin
      GG4 := G1(X,Y)*sqr(A[I,I1]+B[I,I1]*X+C[I,I1]*Y)
   end;
function GG5(X,Y:real) : real;
   begin
      GG5 := G1(X,Y)*sqr(A[I,I2]+B[I,I2]*X+C[I,I2]*Y)
   end;
function QQ(L:integer) : real;
var
   X,Y,S : array[1..7] of real;
   QQQ,T1,T2,T3 : real;
   I : integer;
begin
   X[1] := XX[J1]; Y[1] := YY[J1];
   X[2] := XX[J2]; Y[2] := YY[J2];
   X[3] := XX[J3]; Y[3] := YY[J3];
   X[4] := 0.5*(X[1]+X[2]); Y[4] := 0.5*(Y[1]+Y[2]);
   X[5] := 0.5*(X[1]+X[3]); Y[5] := 0.5*(Y[1]+Y[3]);
   X[6] := 0.5*(X[2]+X[3]); Y[6] := 0.5*(Y[2]+Y[3]);
   X[7] := (X[1]+X[2]+X[3])/3.0; Y[7] := (Y[1]+Y[2]+Y[3])/3.0;
   case L of
      1 : for I := 1 to 7 do S[I] := P(X[I],Y[I]);
      2 : for I := 1 to 7 do S[I] := Q(X[I],Y[I]);
      3 : for I := 1 to 7 do S[I] := RR(X[I],Y[I]);
      4 : for I := 1 to 7 do S[I] := FFF(X[I],Y[I])
   end;
   T1 := 0.0;
   for I := 1 to 3 do T1 := T1 + S[I];
   T2 := 0.0;
   for I := 4 to 6 do T2 := T2 + S[I];
   T3 := S[7];
   QQQ := 0.5*(T1/20.0 + 2.0*T2/15.0 + 9.0*T3/20.0)*abs(DELTA);
   QQ := QQQ
end;
function SQ (L:integer) : real;
var
   S : array [1..101] of real;
   SSQ,X,T1,T2,X1,X2,Y1,Y2,H,Q1,Q2,Q3,T3 : real;
   I : integer;
begin
   X1 := XX[J1];
   Y1 := YY[J1];
   X2 := XX[J2];
   Y2 := YY[J2];
   H := 0.01;
   T1 := X2-X1;
   T2 := Y2-Y1;
   T3 := sqrt(T1*T1+T2*T2);
   for I := 1 to 101 do
      begin
         X := (I-1.0)*H;
         case L of
            1 : S[I] := T3*GG1(T1*X+X1,T2*X+Y1);
            2 : S[I] := T3*GG2(T1*X+X1,T2*X+Y1);
            3 : S[I] := T3*GG3(T1*X+X1,T2*X+Y1);
            4 : S[I] := T3*GG4(T1*X+X1,T2*X+Y1);
            5 : S[I] := T3*GG5(T1*X+X1,T2*X+Y1)
         end
      end;
   Q3 := S[1]+S[101];
   Q1 := 0.0;
   Q2 := S[100];
   for I := 1 to 49 do
      begin
         Q1 := Q1 + S[2*I+1];
         Q2 := Q2 + S[2*I]
      end;
   SSQ := (Q3 + 2.0*Q1 + 4.0*Q2)*H/3.0;
   SQ := SSQ
end;
procedure INPUT;
begin
   writeln('This is the Finite Element Method.');
   OK := false;
   writeln('The input will be from a text file in the following form:');
   writeln('K     N     M     n     m     nl'); writeln;
   write('Each of the above is an integer -');
   writeln('separate with at least one blank.'); writeln;
   writeln('Follow with the input for each node in the form:');
   write('x-coor., y-coord., number of triangles in which the');
   writeln(' node is a vertex.'); writeln;
   writeln('Continue with the triangle number and vertex number for');
   writeln('each triangle in which the node is a vertex.');
   writeln('Separate each entry with at least one blank.'); writeln;
   writeln('After all nodes have been entered follow with information');
   writeln('on the lines over which line integrals must be computed.');
   writeln('The format of this data will be the node number of the');
   writeln('starting node, followed by the node number of the ending');
   writeln('node for each line, taken in the positive direction.');
   writeln('There should be 2 * nl such entries, each an integer');
   writeln('separated by a blank.'); writeln;
   writeln('Have the functions P,Q,R,F,G,G1,G2 been created and');
   writeln('has the input file been created?  Answer Y or N.');
   readln(AA);
   if (AA = 'y') or (AA = 'Y') then
      begin
         writeln('Input the file name in the form - drive: name.ext');
         writeln('for example:  A:DATA.DTA');
         readln(NAME);
         assign(INP,NAME);
         reset(INP);
         OK := true;
         readln(INP,K,N,M,LN1,LM,NL);
         for KK := 1 to LM do
            begin
               read(INP,XX[KK],YY[KK],LLL);
               for J := 1 to LLL do read(INP,II[KK,J],NV[KK,J]);
               LL[KK] := LLL;
               for J := 1 to LLL do
                  begin
                     N1 := II[KK,J];
                     N2 := NV[KK,J];
                     NTR[N1,N2] := KK
                  end
            end;
         if (NL > 0) then
            begin
               for I := 1 to NL do
                  for J := 1 to 2 do read(INP,LINE[I,J])
            end;
         close(INP);
      end
   else
      writeln('The program will end so that the input file can be created.')
end;
procedure OUTPUT;
var
   FLAG : integer;
begin
   writeln ('Choice of output method: ');
   writeln ('1. Output to screen ');
   writeln ('2. Output to text file ');
   writeln ('Please enter 1 or 2 ');
   readln ( FLAG );
   if ( FLAG = 2 ) then
      begin
         writeln ('Input the file name in the form - drive:name.ext ');
         writeln ('for example:   A:OUTPUT.DTA');
         readln ( NAME );
         assign ( OUP, NAME )
      end
   else  assign ( OUP, 'CON' );
   rewrite ( OUP );
   writeln(OUP,'FINITE ELEMENT METHOD');
   writeln ( OUP );
   writeln ( OUP )
end;
begin
   INPUT;
   if (OK) then
      begin
         OUTPUT;
         K1 := K+1;
         N1 := LN1+1;
         writeln(OUP,'Vertices and Nodes of Triangles');
         writeln(OUP,'Trinagle-node number for vertex 1 to 3');
         for I := 1 to M do
            begin
               write(OUP,I:6);
               for J := 1 to 3 do write(OUP,NTR[I,J]:5);
               writeln(OUP)
            end;
         writeln(OUP,'x and y coordinates of nodes');
         for KK := 1 to LM do writeln(OUP,KK:5,XX[KK]:12:8,YY[KK]:12:8);
         writeln(OUP,'Lines of the Domain');
         for KK := 1 to NL do writeln(OUP,KK:5,LINE[KK,1]:5,LINE[KK,2]:5);
{        STEP 1                                                        }
         if (LM <> LN1) then
            for L := N1 to LM do GAMMA[L] := G(XX[L],YY[L]);
{        STEP 2 - initialization of ALPHA - note that
                  ALPHA[I,LN1 + 1] = BETA[I]                           }
         for I := 1 to LN1 do
            for J := 1 to N1 do ALPHA[I,J] := 0.0;
{        STEPS 3, 4, and 6 - 12 are within the next loop
         for each triangle I let node J1 be vertex 1, node J2 be
         vertex 2 and node J3 be vertex 3                              }
{        STEP 3                                                        }
         for I := 1 to M do
            begin
               J1 := NTR[I,1];
               J2 := NTR[I,2];
               J3 := NTR[I,3];
               DELTA := XX[J2]*YY[J3]-XX[J3]*YY[J2]-XX[J1]*(YY[J3]-YY[J2])+
                        YY[J1]*(XX[J3]-XX[J2]);
               A[I,1] := (XX[J2]*YY[J3]-YY[J2]*XX[J3])/DELTA;
               B[I,1] := (YY[J2]-YY[J3])/DELTA;
               C[I,1] := (XX[J3]-XX[J2])/DELTA;
               A[I,2] := (XX[J3]*YY[J1]-YY[J3]*XX[J1])/DELTA;
               B[I,2] := (YY[J3]-YY[J1])/DELTA;
               C[I,2] := (XX[J1]-XX[J3])/DELTA;
               A[I,3] := (XX[J1]*YY[J2]-YY[J1]*XX[J2])/DELTA;
               B[I,3] := (YY[J1]-YY[J2])/DELTA;
               C[I,3] := (XX[J2]-XX[J1])/DELTA;
{              STEP 4
               I1 = J for STEP 4 and I1 = K for STEP 7                 }
               for I1 := 1 to 3 do
{                 STEP 8                                               }
                  begin
                     JJ1 := NTR[I,I1];
{                    I2 = K for STEP 4 and I2 = J for STEP 9           }
                     for I2 := 1 to I1 do
{                       STEP 4 and STEP 10                             }
                        begin
                           JJ2 := NTR[I,I2];
                           ZZ := B[I,I1]*B[I,I2]*QQ(1)+
                                 C[I,I1]*C[I,I2]*QQ(2)-QQ(3);
{                          STEPS 11 and 12                             }
                           if (JJ1 <= LN1) then
                              begin
                                 if (JJ2 <= LN1) then
                                    begin
                                       ALPHA[JJ1,JJ2] := ALPHA[JJ1,JJ2]+ZZ;
                                       if (JJ1 <> JJ2) then
                                       ALPHA[JJ2,JJ1] := ALPHA[JJ2,JJ1]+ZZ
                                    end
                                 else ALPHA[JJ1,N1] := ALPHA[JJ1,N1]-
                                                       ZZ*GAMMA[JJ2]
                              end
                           else
                              if (JJ2 <= LN1) then ALPHA[JJ2,N1] :=
                                     ALPHA[JJ2,N1]-ZZ*GAMMA[JJ1]
                        end;
                     HH := -QQ(4);
                     if (JJ1 <= LN1) then ALPHA[JJ1,N1] := ALPHA[JJ1,N1]+HH
                  end
            end;
{        output the basis functions                                    }
         writeln(OUP,'Basis Functions');
         writeln(OUP,'Triangle - Vertex - Node - Function');
         writeln(OUP,' ':12,'A[i]':14,'B[i]':14,'C[i]':14);
         for I := 1 to M do
            for J := 1 to 3 do
               writeln(OUP,I:4,J:4,NTR[I,J]:4,A[I,J]:14:8,
                       B[I,J]:14:8,C[I,J]:14:8);
{        STEP 5
         for each line segment JI = 1,..., NL and for each
         triangle I, I = K1,..., N which may contain line JI
         search all 3 vertices for possible correspondences.
         STEP 5 and STEPS 13 - 19                                      }
         if ((NL <> 0) and (N <> K)) then
            for JI := 1 to NL do
               for I := K1 to N do
                  for I1 := 1 to 3 do
{                    I1 = J in STEP 5 and I1 = K in STEP 14
                     STEP 15                                           }
                     begin
                        J1 := NTR[I,I1];
                        if (LINE[JI,1] = J1) then
                           begin
                              for I2 := 1 to 3 do
{                                I2 = K in STEP 5 and I2 = J in STEP 16
                                 STEP 17                               }
                                 begin
                                    J2 := NTR[I,I2];
                                    if (LINE[JI,2] = J2) then
{                                      There is a correspondence of
                                       vertex I1 in triangle I with
                                       node J1 as the start of line
                                       JI and vertex I2 with node J2
                                       as the end of line JI
                                       STEP 5                          }
                                       begin
                                          XJ := sq(1);
                                          XJ1 := sq(4);
                                          XJ2 := sq(5);
                                          XI1 := sq(2);
                                          XI2 := sq(3);
{                                         STEPS 8 and 19               }
                                          if (J1 <= LN1) then
                                             if (J2 <= LN1) then
                                                begin
                                                   ALPHA[J1,J1] :=
                                                      ALPHA[J1,J1]+XJ1;
                                                   ALPHA[J1,J2] :=
                                                      ALPHA[J1,J2]+XJ;
                                                   ALPHA[J2,J2] :=
                                                      ALPHA[J2,J2]+XJ2;
                                                   ALPHA[J2,J1] :=
                                                      ALPHA[J2,J1]+XJ;
                                                   ALPHA[J1,N1] :=
                                                      ALPHA[J1,N1]+XI1;
                                                   ALPHA[J2,N1] :=
                                                      ALPHA[J2,N1]+XI2
                                                end
                                             else
                                                begin
                                                   ALPHA[J1,N1] :=
                                                      ALPHA[J1,N1]-
                                                      XJ*GAMMA[J2]+XI1;
                                                   ALPHA[J1,J1] :=
                                                      ALPHA[J1,J1]+XJ1
                                                end
                                          else
                                             if (J2 <= LN1) then
                                                begin
                                                   ALPHA[J2,N1] :=
                                                      ALPHA[J2,N1]-
                                                      XJ*GAMMA[J1]+XI2;
                                                   ALPHA[J2,J2] :=
                                                      ALPHA[J2,J2]+XJ2
                                                end
                                       end
                                 end
                           end
                     end;
{        output ALPHA                                                  }
         writeln(OUP,'Matrix ALPHA follows:');
         for I := 1 to LN1 do
            begin
               writeln(OUP,'Row ',I:4);
               for J := 1 to N1 do
                  writeln(OUP,ALPHA[I,J]:14,' ')
            end;
         writeln(OUP);
{        STEP 20                                                       }
         if (LN1 > 1) then
            begin
               INN := LN1-1;
               for I := 1 to INN do
                  begin
                     I1 := I+1;
                     for J := I1 to LN1 do
                        begin
                           CCC := ALPHA[J,I]/ALPHA[I,I];
                           for J1 := I1 to N1 do
                              ALPHA[J,J1] := ALPHA[J,J1]-CCC*ALPHA[I,J1];
                           ALPHA[J,I] := 0.0
                        end
                  end
            end;
         GAMMA[LN1] := ALPHA[LN1,N1]/ALPHA[LN1,LN1];
         if (LN1 > 1) then
            for I := 1 to INN do
               begin
                  J := LN1-I;
                  CCC := ALPHA[J,N1];
                  J1 := J+1;
                  for KK := J1 to LN1 do
                     CCC := CCC-ALPHA[J,KK]*GAMMA[KK];
                  GAMMA[J] := CCC/ALPHA[J,J]
               end;
{        STEP 21
         output gamma                                                  }
         writeln(OUP,'Coefficients of Basis Functions: ');
         for I := 1 to LM do
            begin
               LLL := LL[I];
               write(OUP,I:3,GAMMA[I]:14:8,' ',I);
               for J := 1 to LLL do write(OUP,II[I,J]:5);
               writeln(OUP)
            end;
         close(OUP)
{     STEP 23                                                          }
      end
end.




