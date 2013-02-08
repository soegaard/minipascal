#lang racket
(provide compile/fpc)
;;;
;;; COMPILE USING FREE PASCAL
;;;

; This file contains a "compiler" that uses the 
; Free Pascal Compiler (fpc) to compile the Pascal 
; program. After compilation the resulting 
; executable is run. This "compiler" makes it
; easy to make conformance tests.

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
  ; Note: The reader has at this point already 
  ;       removed the #lang line
  (define pas-file 
    (make-temporary-file "~a.pas" #f (current-directory)))
  (with-output-to-file pas-file
    (λ () (copy-port ip (current-output-port)))
    #:exists 'truncate)
  pas-file)

(define (fpc-compile pas-file)
  (define out-file 
    (make-temporary-file "~a.out" #f (current-directory)))
  (define (compile)
    (system* "/usr/local/bin/fpc" 
              "-k-macosx_version_min 10.6"
              (~a "-o" out-file)
              pas-file))
  (define exit-code 0)
  (define output (with-output-to-string 
                  (λ () (set! exit-code (compile)))))
  (cond
    [exit-code
     (define permissions
       (bitwise-ior user-execute-bit user-read-bit user-write-bit))
     (file-or-directory-permissions out-file permissions)
     out-file]
    [else
     (displayln output)
     #f]))

(define (run out-file input)
  (define in
    (cond
      [(string? input) (open-input-string input)]
      [(port? input)   input]
      [(eq? input #f)  (current-input-port)]))
  (parameterize ([current-input-port in])
    (system* out-file)))

(define (compile/fpc src ip input)
  (define pas-file (generate-pas-file ip))
  (define out-file (fpc-compile pas-file))
  (when out-file
    (run out-file input)
    (delete-file out-file))
  (delete-file pas-file)
  (quasisyntax/loc #'here
    (module fpc racket/base
      (void))))
