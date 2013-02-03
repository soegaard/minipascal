#lang racket

;;; Experiments in order to find a way to
;;; implement call by reference in minipascal.

(require (for-syntax syntax/parse))
(begin-for-syntax
  (require (for-syntax syntax/parse)
           (for-syntax racket/base)))

(define-syntax (wrap stx)
  (syntax-parse stx
    [(_ () body ...)
     #'(let () body ...)]
    ; call by reference
    [(_ (("ref" arg act) (type argn actn) ...) body ...)
     #'(let-syntax
           ([arg (make-set!-transformer
                  (lambda (s)
                    (syntax-case s (set!)
                      [(set! _ v)          #'(set! act v)]
                      [i (identifier? #'i) #'act]
                      [_ (error)])))])
         (wrap ((type argn actn) ...)
               body ...))]
    ; call by value
    [(_ (("val" arg act) (type argn actn) ...) body ...)
     #'(let ([arg act])
         (wrap ((type argn actn) ...)
               body ...))]))

(define-syntax (define-function stx)
  (syntax-parse stx
    [(_ (id (type arg) ...) body ...)
     #'(define-syntax (id so)
         (syntax-parse so
           [(_ act (... ...))
            (with-syntax ([(ar (... ...)) #'(arg ...)]
                          [(ty (... ...)) #'(type ...)])
              #'(wrap ((ty ar act) (... ...))
                      body ...))]))]))

(define-function (triple ("ref" a) ("ref" b))
  (set! a (* 3 a))
  (set! b (* 3 b))
  (list a b))

(define s 3)
(define t 4)
(list s t)
(triple s t)
