#lang racket
(provide compile/fpc)
;;;
;;; COMPILE USING FREE PASCAL
;;;

; This file contains a "compiler" that uses the 
; Free Pascal Compiler (fpc) to compile and 
; evaluate the Pascal program. Having such
; a "compiler" is practical for testing purposes.

; Compiling a file "foo.rkt" containing a MiniPascal
; is a done as follows:

;  1. The contents of "foo.rkt" minus the #lang line 
;     is written to "foo.pas".
;  2. The Free Pascal Compiler is invoked:
;        fpc foo.pas
;     This produces "foo".
;  3. The program "foo" is run with standard
;     input and output wired to Racket's
;     standard in- and outputs. 

(require racket/file)

(define (generate-pas-file ip)
  (define pas-file 
    (make-temporary-file "~a.pas" #f (current-directory)))
  (with-output-to-file pas-file
    (λ () (copy-port ip (current-output-port)))
    #:exists 'truncate)
  pas-file)

(define (fpc-compile pas-file)
  (define out-file 
    (make-temporary-file "~a.out" #f (current-directory)))
  (with-output-to-string
   (λ () (system* "/usr/local/bin/fpc" 
                  (~a "-o" out-file)
                  pas-file)))
  out-file)

(define (run out-file)
  (system* out-file))

(define (compile/fpc src ip)
  ; At this point the #lang line is gone
  ;(displayln (current-directory))
  ;(displayln src)
  ;(displayln ip)
  (define pas-file (generate-pas-file ip))
  ;(displayln pas-file)
  (define out-file (fpc-compile pas-file))
  (run out-file)
  (delete-file pas-file)
  (quasisyntax/loc #'here
    (module fpc racket/base
      (void))))
