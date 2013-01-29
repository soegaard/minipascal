#lang racket
(provide (all-from-out "runtime.rkt")
         program)

(require (for-syntax "compiler.rkt"))
(require "runtime.rkt")

; The macro  program  compiles the syntax-object 
; the parser returned into a Racket program written
; in the language specified in "runtime.rkt".

(define-syntax (program stx)
  (displayln "Input program:")
  (displayln stx)
  (displayln "Compiled program:")
  
  (define compiled (compile-program stx))  
  
  (displayln compiled)
  (displayln "Running program:")
  
  compiled)
