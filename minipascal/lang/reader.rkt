#lang racket
(provide 
 (rename-out [my-read read]
             [my-read-syntax read-syntax]
             [my-get-info get-info]))

(require "../mini-pascal-lexer.rkt"
         "../mini-pascal-grammar.rkt")

(require "../compiler.rkt"
         "../compiler-fpc.rkt")

; In order to specify paths relative to the
; path of "reader.rkt" (rather than the file 
; containing the file to be read) we use 
; runtime-paths.
(require racket/runtime-path)
(define-runtime-path sem-simple-path  "../semantics-simple.rkt")
(define-runtime-path color-lexer-path "../mini-pascal-lexer.rkt")

; read-syntax runs the lexer and parser on the 
; input port containing the pascal program.
; The parser returns a syntax object of the form:
;   (program (block ...))
; This syntax object is wrapped in a module,
; whose language is specified in "semantics.rkt".
; In "semantics.rkt"  program  is a macro that
; compiles (block ...) into the language
; specified in the "runtime.rkt".

(define mode 'full)

(define (my-read-syntax src ip)  
  (define after-minipascal (read-line ip))
  ; determine which compiler to use
  ;   #lang minipascal simple  => semantics-simple.rkt
  ;   #lang minipascal         => compiler.rkt
  ;   #lang fpc                => compile-fpc.rkt
  (define test-input #f)
  (define after (open-input-string after-minipascal))
  ; look for options
  (let loop ()
    (define option (read after))
    (unless (eof-object? option)
      (match option 
        ['simple           (set! mode 'simple)]
        ['fpc              (set! mode 'fpc)]
        [(list 'input str) (set! test-input str)]
        [_ (displayln (~a "unrecognized option: " option))])
      (loop)))
  ; parse and compile
  (case mode
    [(simple)
     (define parse-tree (parse src (lex ip)))
     (quasisyntax/loc #'here
       (module minipascal #,sem-simple-path
         #,parse-tree))]
    [(full)
     (define parse-tree (parse src (lex ip)))
     (define prg (compile-program parse-tree))
     ; (displayln prg)
     prg]
    [(fpc)
     (define prg (compile/fpc src ip test-input))
     prg]))

; read returns the same as read-syntax,
; except as a datum.
(define (my-read in)
  (syntax->datum (my-read-syntax #f in)))

; get-info is in general used by external
; tools to retriece information about a
; program. Here we use get-info to tell
; DrRacket which lexer it should use to
; syntax color Pascal programs.
(define (my-get-info in mod line col pos)
  (lambda (key default)
    (case key
      [(color-lexer)
       (dynamic-require color-lexer-path 'color-lex)]
      [else default])))
