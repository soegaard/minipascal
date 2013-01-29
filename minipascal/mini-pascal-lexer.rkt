#lang racket
;;;
;;; LEXER
;;;

; This module defines two lexers for MiniPascal.

(provide lex color-lex)

; The main one, lex, returns tokens to
; be used by the ragg parser.

; The other one, color-lex, returns tokens
; to be used by DrRacket to syntax color 
; Pascal Programs.

(require ragg/support parser-tools/lex)

; Since we want to define two lexers, it is
; convenient to define lexer abbreviations
; that can be used in both lexers.

(define-lex-abbrevs
  [letter     
   (union (char-range #\a #\z) (char-range #\A #\Z))]
  [digit      
   (char-range #\0 #\9)]
  [identifier 
   (concatenation letter 
                  (repetition 0 +inf.0 (union letter digit)))]
  [integer ; non-negative
   (repetition 1 +inf.0 digit)]
  [char-constant 
   (union (concatenation #\' (char-complement #\') #\') "''''")]
  [reserved
   (union "div" "or" "and" "not" "if"
          "then" "else" "of" "while" "do" "begin" "end" 
          "read" "readln" "write" "writeln"
          "var" "const" "array" "type" "bindable"
          "procedure" "function" "program")]
  [slash-comment
   (concatenation "//" (repetition 0 +inf.0 (char-complement #\newline)))]
  [curly-comment 
   (concatenation #\{  (repetition 0 +inf.0 (char-complement #\})) #\})]
  [comment 
   (union slash-comment curly-comment)]
  [operators 
   (union "+" "-" "*" "=" "<>" "<" ">" "<=" ">=" ":=")]
  [brackets
   (union "(" ")" "[" "]")]
  [other-delimiters
   (union "." "," ";" ":" "..")]
  [delimiters
   (union operators brackets other-delimiters)])

;; Lexer for MiniPascal
(define (lex ip)
  (port-count-lines! ip)
  (define my-lexer
    (lexer-src-pos
     [(union "integer" "char" "boolean" "true" "false")
      (token 'IDENTIFIER (string->symbol lexeme))]
     [(union reserved delimiters) ; includes operators
      lexeme]
     [identifier
      (token 'IDENTIFIER (string->symbol (string-downcase lexeme)))]
     [integer
      (token 'INTEGER-CONSTANT (string->number lexeme))]
     [char-constant
      (if (equal? lexeme "''''")
          (token 'CHARACTER-CONSTANT #\')
          (token 'CHARACTER-CONSTANT (string-ref lexeme 1)))]
     [whitespace
      (token 'WHITESPACE lexeme #:skip? #t)]
     [comment
      (token 'COMMENT lexeme #:skip? #t)]     
     [(eof)
      (void)]))
  (define (next-token) 
    (my-lexer ip))
  next-token)

;;;
;;; COLOR LEXER
;;;

; This lexer is used by DrRacket to color the Pacal program.

; The color lexer returns 5 values:
; - Either a string containing the matching text or the eof object. 
;   Block comments and specials currently return an empty string. 
;   This may change in the future to other string or non-string data.
; - A symbol in '(error comment sexp-comment white-space constant 
;                 string no-color parenthesis other symbol eof).
; - A symbol in '(|(| |)| |[| |]| |{| |}|) or #f.
; - A number representing the starting position of the match (or #f if eof).
; - A number representing the ending position of the match (or #f if eof).

(define (syn-val a b c d e)
  (values a ; string with matching text
          b ; symbol in '(comment white-space no-color eof)
          c ; symbol in '(|(| |)| |[| |]| |{| |}|) or #f.
          (position-offset d)    ; start pos
          (max                   ; end pos
           (position-offset e)
           (+ (position-offset d) 1))))

(define color-lex
  ; REMEMBER to restart DrRacket to test any changes in the 
  ; color-lexer. The lexer is only imported into DrRacket 
  ; at startup.
  (lexer
   [(eof)
    (syn-val lexeme 'eof          #f start-pos end-pos)]
   [reserved
    (syn-val lexeme 'keyword      #f start-pos end-pos)]   
   [brackets
    (syn-val lexeme 'parenthesis  (string->symbol lexeme) start-pos end-pos)]   
   [whitespace
    (syn-val lexeme 'white-space  #f start-pos end-pos)]   
   [slash-comment
    (syn-val lexeme 'comment      #f start-pos end-pos)]
   [curly-comment
    (syn-val lexeme 'sexp-comment #f start-pos end-pos)]
   [(union "{" "}")
    (syn-val lexeme 'sexp-comment (string->symbol lexeme) start-pos end-pos)]
   #;[operators 
      (syn-val lexeme 'symbol       #f start-pos end-pos)]
   #;[string 
      (syn-val lexeme 'string     #f start-pos end-pos)]   
   [identifier
    (syn-val lexeme 'identifier       #f start-pos end-pos)]      
   [(union integer char-constant) 
    (syn-val lexeme 'constant     #f start-pos end-pos)]
   [delimiters
    (syn-val lexeme 'no-color     #f start-pos end-pos)]
   [any-char ; anything else is an error (red)
    (syn-val lexeme 'error        #f start-pos end-pos)]))
