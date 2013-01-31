#lang racket
(provide (all-from-out "runtime.rkt"))

(require "runtime.rkt")

(require (for-syntax "compiler-simple.rkt"))

; The macro, program, compiles the syntax-object 
; the parser returned into a Racket program written
; in the language specified in "runtime.rkt".

(provide program)
(define-syntax (program stx)
  (displayln "Input program:")
  (displayln stx)
  (displayln "Compiled program:")
  
  (define compiled (compile-program stx))  
  
  (displayln compiled)
  (displayln "Running program:")
  
  compiled)

; In "compiler-simple.rkt" the output contains
; forms such as (term ...). To get the macro
; expander to expand the term with compile-term,
; we must set up a macro, term, that simply
; uses compile-term to compute the expansion.
; This must be done for each non-terminal.

; As a convenience the
; following macro, generate-macro, is used to
; define and provide a bunch of these macros.

(require (for-syntax syntax/parse 
                     racket/syntax))

(define-syntax (generate-macros stx)
  (syntax-parse stx
    [(_ id ...)
     (with-syntax ([(compile-id ...) 
                    (map (λ (id) (format-id id "compile-~a" id))
                         (syntax->list #'(id ...)))])
       #'(begin
           (define-syntax id compile-id) ...
           (provide id ...)))]))

(generate-macros
 constant-definition-part constant-definition
 type-definition-part type-definition
 variable-declaration-part variable-declaration
 procedure-and-function-declaration-part
 procedure-declaration function-declaration
 block statement-part statement compound-statement
 simple-statement assignment-statement read-statement
 write-statement writeln-statement 
 procedure-statement structured-statement
 if-statement while-statement for-statement
 expression simple-expression term factor constant
 constant-identifier output-value application
 relational-operator multiplying-operator
 variable procedure-identifier)

