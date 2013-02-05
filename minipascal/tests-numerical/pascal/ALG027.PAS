program ALG027;
{  HORNER'S ALGORITHM 2.7

   To evaluate the polynomial
   p(x) = a(n) * x ^ n + a(n-1) * x ^ (n-1) + ... + a(1) * x + a(0)
   and its derivative p'(x) at x = x0;

   INPUT:   degree n; coefficients aa(0),aa(1),...,aa(n);
            value of x0.

   OUTPUT:  y = p(x0), z = p'(x0).
                                                                       }
var
   AA : array [ 0 .. 50 ] of real;
   X0,Y,Z : real;
   N,MM,I,J : integer;
   OK : boolean;
procedure INPUT;
   begin
      writeln('This is Horners Method');
      OK := false;
      while ( not OK ) do
         begin
            writeln ('Input degree n of polynomial - no decimal point ');
            readln ( N );
            if ( N < 0 ) then writeln ('Integer N must be nonnegative')
            else OK := true
         end;
      writeln('Input coefficients of P(X) in ascending order.');
      for I := 0 to N do
         begin
            writeln ('Input coefficient of X^ ',I );
            readln ( AA[I] )
         end;
      writeln ('Input argument X0 at which to evaluate P(X)');
      readln ( X0 )
   end;
   begin
      INPUT;
{     STEP 1                                                           }
{     compute b(n) for p(x)                                            }
      Y := AA[N];
{     compute b(n-1) for q(x) = p'(x)                                  }
      if N = 0 then Z := 0
               else Z := AA[N];
      MM := N - 1;
{     STEP 2                                                           }
      for I := 1 to MM do
         begin
            J := N - I;
{           compute b(j) for p(x)                                      }
            Y := Y * X0 + AA[J];
{           compute b(j-1) for q(x)                                    }
            Z := Z * X0 + Y
         end;
{     STEP 3                                                           }
{     compute b(0) for p(x)                                            }
      if (N <> 0) then Y := Y * X0 + AA[0];
      writeln ('Coefficients of polynomial P : ');
{     STEP 4                                                           }
      for I := 0 to N do
         writeln ('Exponent = ',I:3,'     Coefficient = ',AA[I]:12:8 );
      writeln (' ');
      writeln (' P ( ',X0,' ) = ',Y:12:8 );
      writeln (' P'' ( ',X0,' ) = ',Z:12:8 )
   end.
