#lang ragg
;;; MiniPascal

program :                "program" IDENTIFIER ";" block "."
block :                  constant-definition-part
                         type-definition-part 
                         variable-declaration-part 
                         procedure-and-function-declaration-part
                         statement-part

constant-definition-part :  ["const" (constant-definition ";")+]
constant-definition :       IDENTIFIER ("," IDENTIFIER)* "=" INTEGER-CONSTANT

type-definition-part :      ["type" (type-definition ";")+]
type-definition :           IDENTIFIER "=" type 

variable-declaration-part : ["var" (variable-declaration ";")+]
variable-declaration :      IDENTIFIER ("," IDENTIFIER)* ":" type

type :                   simple-type | array-type
array-type :             "array" "[" index-type "]" "of" simple-type
index-type :             type-identifier | index-range
index-range :                 ([sign] (constant-name | CHARACTER-CONSTANT | INTEGER-CONSTANT))
                         ".." ([sign] (constant-name | CHARACTER-CONSTANT | INTEGER-CONSTANT))
simple-type :            type-identifier | index-range
constant-name :          IDENTIFIER
type-identifier :        IDENTIFIER


procedure-and-function-declaration-part : 
                         ((procedure-declaration ";") | (function-declaration ";"))*

procedure-declaration :  "procedure" IDENTIFIER 
                         ["(" formal-parameters (";" formal-parameters)* ")"] ";"
                         block
formal-parameters :      ["var"] IDENTIFIER ("," IDENTIFIER)* ":" type 

function-declaration :   "function" IDENTIFIER 
                         ["(" formal-parameters (";" formal-parameters)* ")"] 
                         ":" type-identifier ";" block


statement-part :         compound-statement
compound-statement :     "begin" [statement (";"+ statement)*] [";"+] "end"
statement :	             simple-statement | structured-statement
simple-statement :       assignment-statement | procedure-statement | 
                         application | read-statement | 
                         write-statement | writeln-statement
assignment-statement :   variable ["[" expression "]"] ":=" expression
procedure-statement :    procedure-identifier
application :            IDENTIFIER "(" expression ("," expression)* ")"
read-statement :         ("readln"|"read")    "(" IDENTIFIER ("," IDENTIFIER)* ")"
write-statement :	     "write" "(" output-value ("," output-value)* ")"
writeln-statement :	     "writeln" ["(" output-value ("," output-value)* ")"]

procedure-identifier :   IDENTIFIER
output-value :	     expression

structured-statement :   compound-statement | if-statement |  while-statement
if-statement :           "if" expression "then" statement | 
                         "if" expression "then" statement "else" statement
while-statement :        "while" expression "do" statement

expression :             simple-expression | 
                         simple-expression relational-operator simple-expression
simple-expression :      [sign] term (adding-operator term)*
term :                   factor (multiplying-operator factor)*
factor :	             application | variable | constant | "(" expression ")" | "not" factor 
relational-operator :    "=" | "<>" | "<" | "<=" | ">=" | ">"
sign :	             "+" | "-"
adding-operator :	     "+" | "-"   | "or"
multiplying-operator :   "*" | "div" | "and"

variable :               IDENTIFIER | IDENTIFIER  "[" expression "]"
parameter-identifier :   IDENTIFIER

;; Lexical grammar
constant :	             INTEGER-CONSTANT | CHARACTER-CONSTANT | constant-identifier
constant-identifier :    IDENTIFIER
