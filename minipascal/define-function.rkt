#lang racket
; program varparam;
; var
;   y:integer;
;
; procedure double(var x:integer);
; begin
;   x:=2*x;
; end;
;
; begin
;   y:=3;
;   writeln(y);
;   double(y);
;   writeln(y);  
; end.

(require (for-syntax syntax/parse))

(define-syntax (define-function stx)
  (syntax-parse stx
    [(_ (id (~and arg ((~or "var" "ref") a)) ...) body ...)
     (syntax/loc stx
       (define-syntax (id so)
         (syntax-parse so
           [(_ a ...)
            #'(wrap-var-parameters 
               (arg a ...)
               (begin body ...))])))]))

(define-syntax (wrap-var-parameters stx)
  (syntax-parse stx
    [(_ () body ...) 
     #'(begin body ...)]
    [(_ (arg0 arg ...) body ...)
     (syntax-parse #'arg0
       [("ref" id)
        #'id]
       [("var" id)
        #'(let-syntax 
              ([id (make-set!-transformer
                    (lambda (so)
                      (syntax-case so (set!)
                        [(set! _ v)           #'(set! id v)]
                        [i (identifier? #'i)  #'id]
                        [_ (error)])))])
            (wrap-var-parameters (arg ...)
                body ...))])]))
 
(define x 3)
(define y 4)

(define-function (double ("var" a))
  (set! a (* 2 a)))

(displayln (list x y))
(double x)
(displayln (list x y))

