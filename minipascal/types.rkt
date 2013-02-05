#lang racket
(provide
 <boolean> <integer> <char> <true> <false>
 <string> <star> <void>

 index-range index-range?
 array       array?
 procedure   procedure?
 function    function?
 
 array-of array-from array-to
 
 type?
 base?
 ordinal?
 string-type?
 
 subtype? type=?
 match-type)

(require (for-syntax syntax/parse))

;;;
;;; TYPES
;;;

; This module contains structure definitions
; that are used to represent the types
; of Pascal values.

(struct type () #:transparent)
; There are two kind of types. 
; Base types and structured types:
(struct base type () #:transparent)
(struct structured type () #:transparent)

; Some of the base types are ordinal.
(struct ordinal base ())

; The base types are:        
(struct Boolean base ())
(struct False   base ())
(struct True    base ())
(struct Void    base ())
(struct Char    ordinal ())
(struct Integer ordinal ())

; The capitalization is to avoid
; clashes with Racket's boolean?, char? and integer?.

; The structured types are:
(struct index-range base (from to) #:transparent)
; where from and to are ordinal

; array[<range>] of <type>
(struct array type (range of) #:transparent)

; procedure name(arg:input,...);
(struct procedure type (inputs) #:transparent)
; where inputs is a list of types

; function name(arg:input,...):output;
(struct function  type (inputs outputs) #:transparent)
; where inputs is a list of types,
; and output is a type

; Besides the above types, we will need a wild-card:
(struct Star type ())


; Instantations
(define <boolean> (Boolean))
(define <char>    (Char))
(define <integer> (Integer))
(define <false>   (False))
(define <true>    (True))
(define <star>    (Star))
(define <void>    (Void))

; Strings are arrays of chars, 
; whose index-range begin with 1.
(define <string> (array (index-range 1 <star>) <char>))

(define (type=? t1 t2)
  (equal? t1 t2))

(define (subtype? t1 t2)
  ; return true of t1 is a subtype of t2, that 
  ; is, all values of type t1 is also value t2
  (or 
   ; anything is a subtype of <star>
   (equal? t2 <star>)
   ; if t1=t2 then t1 is a subtube
   (type=? t1 t2)
   ; values of the type t2, have the right type
   (and (integer? t1) (Integer? t2))
   (and (char? t1)    (Char? t2))
   (and (boolean? t1) (Boolean? t2))   
   (match (list t1 t2)
     [(list (index-range f1 t1) (index-range f2 t2))
      (and (subtype? f1 t2) (subtype? t1 t2))]
     [(list (array r1 of1) (array r2 of2))
      (and (subtype? r1 r2) (subtype? of1 of2))]
     [(list (procedure ins1) (procedure ins2))
      (and (= (length ins1) (length ins2))
           (andmap subtype? ins1 ins2))]
     [(list (function ins1 out1) (function ins2 out2))
      (and (= (length ins1) (length ins2))
           (andmap subtype? ins1 ins2)
           (subtype? out1 out2))]
     [else #f])))

(define (type->string t)
  (define (types->string ts)
    (apply ~a (map type->string ts)
           #:separator ", "))  
  ; this is needed for nice error messages
  (match t
    [(Boolean) "boolean"]
    [(True)    "true"]
    [(False)   "false"]
    [(Char)    "char"]
    [(Integer) "integer"]
    [(Star)    "<any>"]
    [(array (index-range 1 (Star)) (Char))
     "string"]
    [(index-range from to) 
     (~a from ".." to)]
    [(array range of)
     (~a "array [" (type->string range) "]" 
         " of " (type->string of))]
    [(procedure inputs)
     (~a "procedure(" (types->string inputs) ")")]
    [(function inputs out)
     (~a "function(" (types->string inputs) "):"
         (type->string out))]
    [else (error 'internal-error "forgot a type?")]))

(define (string-type? t)
  (subtype? t <string>))

(define (raise-unless-subtype type expected-type name blame-stx)
  (unless (subtype? type expected-type)
    (raise-type-error name (type->string expected-type) blame-stx)))


(define-syntax (match-type stx)
  (syntax-parse stx
    [(_ type0 
        [type1 answer1 ...+] 
        [type answer ...+] ... 
        [else default ...])
     (syntax/loc stx
       (let ([t0 type0])
         (cond
           [(subtype? t0 type1) answer1 ...]
           [(subtype? t0 type)  answer ...]
           ...
           [else (let () default ...)])))]
    [_
     (displayln stx)     
     (error)]))

(define (array-from a)
  (index-range-from (array-range a)))
(define (array-to a)
  (index-range-to (array-range a)))
  

;(require rackunit)
;(check-true (andmap base? (list <char> <integer> 
;                                <boolean> <true> <false>)))
;(check-false (base? (procedure (list))))
;(check-false (base? (function (list) <char>)))
;
;(check-true (ordinal? <char>))
;(check-true (ordinal? <integer>))
;
;(check-true (type=? <boolean> <boolean>))
;(check-true (type=? <char>    <char>))
;(check-true (type=? <integer> <integer>))
;(check-true (type=? <false>   <false>))
;(check-true (type=? <true>    <true>))
;(check-true (type=? (array (index-range 0 3) <integer>)
;                     (array (index-range 0 3) <integer>)))
;
;(check-true (subtype? <char> <char>))
;(check-true (subtype? <char> <star>))
;(check-true (subtype? (index-range 1 10) (index-range 1 10)))
;(check-true (subtype? (index-range 1 10) (index-range 1 <star>)))
;(check-true (subtype? (procedure (list <char> <integer>))
;                      (procedure (list <char> <star>))))
;(check-true (subtype? (function (list <char> <integer>) <boolean>)
;                      (function (list <char> <star>)    <boolean>)))
;
;(check-true  (string-type? (array (index-range 1 10) <char>)))
;(check-false (string-type? (array (index-range 1 10) <true>)))
;
;(check-exn exn? (λ() (raise-unless-subtype <boolean> <char> 'foo #'here)))
;
;(check-equal? (match-type <char> [<boolean> 'b] [<char> 'c] [else #f]) 'c)
