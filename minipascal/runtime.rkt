#lang racket/base
;;;
;;; RUNTIME
;;;

; This module implements the language
; constructs used in the expansion of a
; Pacal program. In the compiler, this
; modules is required as 
;  (require (for-template "runtime.rkt"))
; the syntax objects constructed will thus
; refer syntax and functions defined here.

; The provided constructs are:

; 1. Standard syntax from racket/bae
;      E.g. let, define-values, #%app

; 2. IO Routines

; 3. The Array data structure
;      Pascal arrays can use any ordinal types 
;      as the index types. An ordinal type
;      has values that can be paired 1-1 with
;      integers.

(provide 
 ; Standard syntax
 (all-from-out racket/base)
 ; IO
 pascal:read-integer
 pascal:read-char
 pascal:read ; for "compiler-simple.rkt"
 pascal:write
 pascal:writeln
 ; Arrays
 makearray ; only for "compiler-simple.rkt"
 (rename-out
  [array-ref       pascal:array-ref]
  [array-set!      pascal:array-set!]
  [construct-array pascal:construct-array])
 ;;; STANDARD LIBRARY
 chr)

(require racket/match)  
(require (for-syntax syntax/parse
                     racket/base))

;;; INPUT AND OUTPUT
; "compiler.rkt" know to choose read-integer or read-char
(define pascal:read-integer read)
(define pascal:read-char read-char)
; "compiler-simple.rkt" must dispatch on type on runtime

(define-syntax (pascal:read stx)
  (syntax-parse stx
    [(_ id)
     (syntax/loc stx 
       (cond
         [(integer? id) (set! id (pascal:read-integer))]
         [(char? id)    (set! id (pascal:read-char))]
         [else 
          (displayln id)
          (error 'read "read only supports integer and char")]))]))

(define pascal:readln read-line) ; for now

(define (pascal:write v)
  (display
   (match v
     [#t "true"]
     [#f "false"]
     [else v])))
(define (pascal:writeln val)
  (pascal:write val)
  (newline))

;;; PASCAL ARRAYS  

; A Pacal array such such as 
;   array foo['a'..'z'] of integer
; is represented as 
;   (array from to vec ->index)
; where 
;    from is the ordinal value of 'a'
;    to   is the ordinal value od 'z'
;    vec  is a vector of length to-from+1
;    ->index is (λ(c) (- (ord c) 
;                        (ord #\a))
             
(struct array (from to vec ->index) #:transparent)

; array-ref : array index -> value
;   return the value whose index is idx
(define (array-ref arr idx)
  (match arr
    [(struct array (from to vec ->index))
     (let ([i (->index idx)])
       (unless (<= (->index from) i (->index to))
         (raise-range-error 'array-ref "array" "" idx vec from to))
       (vector-ref vec i))]))

; array-set! : array index value -> value
;   set the value whose index is idx to v
(define (array-set! arr idx v)
  (match arr
    [(struct array (from to vec ->index))
     (let ([i (->index idx)])
       (unless (<= (->index from) i (->index to))
         (raise-range-error 'array-ref "array" "" idx vec from to))
       (vector-set! vec i v))]))

; construct-array : value value (value -> natural) (-> value)
;   return an array of length (- (ord to) (ord from))
;   (where ord returns the ordinal of a value)
;   whose initial values are filled be calling the thunk 
;   init-elm repeatedly
(define (construct-array from to ->index init-elm)
  (let ([from-i (->index from)]
        [to-i   (->index to)])
    (array from to 
           (build-vector (+ (- (->index to) (->index from)) 1)
                         (λ (_) (init-elm)))
           ->index)))

; NOTE:
;   make-array is only used by "compiler-simple.rkt".
;   In "compiler.rkt" the type is used to generate
;   code to initialize the array automaticcaly.

; (make-array from to expr)
;   create an array with index-range from..to,
;   filled with the initial value expr.
(define (makearray from to val)
  (define ->index
    (cond
      [(and (char? from) (char? to))
       (λ (c)
         (- (char->integer c)
            (char->integer from)))]
      [(and (integer? from) (integer? to))
       (λ (x) (- x from))]
      [else
       (define msg 
         "'compiler-simple.rkt' only supports char and integer.")
       (error 'makearray msg)]))     
  (construct-array from to ->index (λ() from)))

;;;CHARACTERS 

; chr : byte -> char
;  convert byte to character
(define (chr b)
  (unless (<= 0 b 256)
    (raise-argument-error 'chr "byte" b))
  (integer->char b))

  




