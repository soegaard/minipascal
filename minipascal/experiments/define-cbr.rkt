#lang racket
(require (for-syntax syntax/parse))
(begin-for-syntax
  (require (for-syntax syntax/parse)
           (for-syntax racket/base)))

(define-syntax (define-cbr stx)
  (syntax-parse stx
    [(_ (id argument ...) body ...)
     #'(define-syntax (id so)
         (syntax-parse so
           [(_ actual (... ...))
            (with-syntax
                ([(arg (... ...)) #'(argument ...)])
              #'(let-syntax
                    ([arg 
                      (make-set!-transformer
                       (lambda (s)
                         (syntax-case s (set!)
                           [(set! _ v)          #'(set! actual v)]
                           [i (identifier? #'i) #'actual]
                           [_ (error)])))]
                     (... ...))
                  body ...))]))]))

(define-cbr (double a)
  (set! a (* 2 a)))

(define x 3)
(double x)
x

              
    