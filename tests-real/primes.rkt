#lang minipascal
{(* Eratosthenes Sieve Prime Number Program in PASCAL *)}
program primes;

{This is the original version from Byte, with only the modifications necessary
to allow it to compile under UCSD and a bell at the end.  
If you choose to report your results, 
PLEASE RUN IT AS IS!  It is a known fact that the speed of this version
can be significantly improved by turning off range checking and re-ordering
the declarations of the variables,  but this is the version which has been used
most and we desire consistantcy.  

Start timing by typing <cr> and stop at the bell.  gws}

{The results of this version on several systems have been reported on MUSUS.

System          UCSD version    Time (sec)
------          ------------    ----------

Sage II         IV.1            57     (68000 at 8 MHz)
WD uEngine      III.0           59     (fillchar is so slow on uE)

LSI-11/23       IV.01           92-122 (depends on memory speed)
LSI-11/23       II.0            105    (98 seconds under IV.01)
LSI-11/23       IV.1            107    (non-extended memory)
LSI-11/23       IV.1            128    (extended memory)

NEC APC         IV.1            144     8086 at 4.9 Mhz  extended memory

JONOS           IV.03 ?         162    (pretty good for a 4 MHz Z-80A)
NorthStar       I.5             183    (Z-80 at 4 MHz)
OSI C8P-DF      II.0  ?         197    (6502 at 2 MHz)
H-89            II.0            200    (4 MHz Z-80A)
LSI-11/2        IV.0            202
IBM PC          IV.03           203    (4.77 MHz 8088)
LSI-11/2        II.0            220

Apple ][        II.1            390    (1 MHz 6502)
H-89            II.0            455    (2 MHz Z-80)

}

const
 size = 8190;

var
 flags : array [0..size] of boolean;
 I,PRIME,K,COUNT,ITER : INTEGER;

BEGIN
{WRITE('10 iterations, <cr> to start');}
{readln;}
FOR ITER := 1 TO 10 DO BEGIN
       COUNT := 0;
       FILLCHAR(FLAGS,SIZEOF(FLAGS),CHR(1));
       FOR I := 0 TO SIZE DO
               IF FLAGS[I] THEN BEGIN
                       PRIME := I+I+3;
                       K := I + PRIME;
                       WHILE K <= SIZE DO BEGIN
                               FLAGS[K] := FALSE;
                               K := K + PRIME
                               END;
                       COUNT := COUNT + 1;
{                       (* WRITELN(PRIME) *)}
                       END;
       END;
{WRITELN( chr ( 7 ),COUNT,' primes')}
END.


