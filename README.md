MiniPascal
==========

MiniPascal as a Racket language
-------------------------------

This is the `minipascal' package. It provides MiniPascal
as a new #lang language.

After installation (see later) the following program will run
as is in DrRacket.

    #lang minipascal
    program fact;
    type 
      int=integer;
    function fact(n:int):int;
      begin
        if n=0 then
          fact:=1
        else
          fact:=n*fact(n-1)
      end;
    begin
      writeln(fact(10));
    end.

See `mini-pascal-grammar.rkt` for a complete grammar.

Features
--------

The following features are supported:
  - constant definitions
  - type definitions
  - function and procedure declarations
  - nested functions and procedures
  - base types: integer, boolean, char,
  - arrays 
  - strings
  - integer and char can be used as index ranges

Compilers
---------
  
MiniPascal comes with two compilers. The simple one
in "semantics-simple.rkt" translates directly from
Pascal to Racket without any (compile time) type 
checking. This compiler is written in the same
spirit as the example in the Ragg tutorial. In
other words it "compiles by macro expansion".

The other compiler in "semantics.rkt" demonstrates
how scoping and type checking can by implemented. 
The compiler more traditional, it expands the whole 
Pascal program in one go.

To switch from the full compiler to the simple one,
add 'simple' to the #lang line. That is, the lines:
    #lang minipascal
    #lang minipascal simple
use the full and simple compiler respectively.

Exercises
---------
  - Add mod as an operator
  - Add repeat
  - Add case
  - Add enumerated types  
  - Add real numbers
  - Add records
  - Add files
  
Installation
------------

Install this package from the command line with:

```sh
raco pkg install --deps search-ask minipascal
```

or, by evaluating the following in DrRacket:

```racket
#lang racket

(require pkg)
(install "minipascal" #:deps 'search-ask)
```

Use the Racket package manager to install:

```
raco pkg install minipascal
```

References
----------
Pascal ISO 7185 from 1990:
http://pascal-central.com/docs/iso7185.pdf
