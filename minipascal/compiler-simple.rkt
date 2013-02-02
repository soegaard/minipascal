#lang racket
;;;
;;; COMPILER
;;;

(provide compile-program
         (all-defined-out))

;; This is a simple compiler for MiniPascal.
;; No type checking at compile time.
;; See "compiler.rkt" for a compiler that
;; checks the types.

; Known Bugs
;   - Index ranges with signs are not supported

; The syntax objects created by the compiler
; refers to bindings from the runtime.
(require (for-template "runtime.rkt"))

; The main destructuring is done by syntax-parse. 
(require syntax/parse)

; A few other helpers are used:
(require unstable/list ; for map2
         racket/match
         racket/syntax)

;;; Syntax used in the compiler

; Convenient syntax for local definitions.
; (This is mostly to safe screen space when using define-values.)
; (def id expr)            => (define id expr)
; (def (id ...) expr ...)  => (define-values (id ...) expr ...)

(require (for-syntax syntax/parse))
(define-syntax (def stx)
  ; def is an alias for define-values
  (syntax-parse stx
    [(_ (id ...) . more) 
     (syntax/loc stx (define-values (id ...) . more))]
    [(_ id expr)
     (syntax/loc stx (define id expr))]))

;;;
;;; Compilation
;;;

(define (compile-program stx)  
  (syntax-parse stx
    [({~datum program} "program" program-name ";" block ".")
     (compile-block #'block)]))

(define (compile-block stx)
  (syntax-parse stx
    [(_ constant-definition-part
        type-definition-part
        variable-declaration-part
        procedure-and-function-declaration-part
        statement-part)
     (syntax/loc stx
       (let () ; new scope
         constant-definition-part
         type-definition-part
         variable-declaration-part
         procedure-and-function-declaration-part
         statement-part))]
    [_ (error 'compile-block "internal error")]))

(define (compile-constant-definition-part stx)
  ; constant-definition-part : ["const" (constant-definition ";")+]
  (syntax-parse stx
    [(_) 
     (syntax/loc stx
       (begin 'constant-definition-part))]
    [(_ "const" (~seq const-def ";") ...)
     (syntax/loc stx
       (begin 
         'constant-definition-part
         const-def ...))]))

(define (compile-constant-definition stx)
  ; constant-definition : IDENTIFIER ("," IDENTIFIER)* "=" INTEGER-CONSTANT
  (syntax-parse stx
    [(_ id0 (~seq "," id) ... "=" x)
     (syntax/loc stx
       (begin
         (define id0 x)
         (define id x)
         ...))]))

(define (compile-type-definition-part stx)
  ; type-definition-part : ["type" (type-definition ";")+]
  (syntax-parse stx
    [(_) 
     (syntax/loc stx
       (begin 'type-definition-part))]
    [(_ "type" (~seq type-def ";") ...)
     (syntax/loc stx
       (begin 
         'type-definitiopn-part
         type-def ...))]))

(define (compile-type-definition stx)
  ; type-definition : IDENTIFIER "=" type
  (syntax-parse stx
    [(_ id "=" type)
     (syntax/loc stx       
       (define id 'type))]))

(define (compile-variable-declaration-part stx)
  ; variable-declaration-part : ["var" (variable-declaration ";")+]
  (syntax-parse stx
    [(_) 
     (syntax/loc stx 
       (begin 'variable-declaration-part))]
    [(_ "var" (~seq var-decl ";") ...+)
     (syntax/loc stx
       (begin 
         'variable-declaration-part
         var-decl ...))]))

(define (compile-variable-declaration stx)
  ; variable-declaration : IDENTIFIER ("," IDENTIFIER)* ":" type
  (syntax-parse stx
    [(_ id0 (~seq "," id) ... ":" type)
     (syntax/loc stx
       (begin
         (define id0 'undefined)
         (define id 'undefined)
         ...))]))

(define (compile-procedure-and-function-declaration-part stx)
  ; procedure-and-function-declaration-part : 
  ;   ((procedure-declaration ";") | (function-declaration ";"))*
  (syntax-parse stx
    [(_ (~seq decl ";") ...)
     (syntax/loc stx
       (begin
         'procedure-and-function-declaration-part
         decl ...))]
    [_ (error)]))

(define (formals->ids stx)
  ; formal-parameters : ["var"] IDENTIFIER ("," IDENTIFIER)* ":" type 
  (syntax-parse stx
    [(_ (~optional "var") id0 (~seq "," id) ... ":" type)
     (syntax->list #'(id0 id ...))]))

(define (compile-procedure-declaration stx)
  ; procedure-declaration 
  ;   : "procedure" IDENTIFIER 
  ;     ["(" formal-parameters (";" formal-parameters)* ")"] ";"
  ;     block  
  (syntax-parse stx
    [(_ "procedure" id ";" block)     
     (with-syntax ([thunk (generate-temporary 
                           (~a (syntax->datum #'id) "-thunk"))])
       (syntax/loc stx 
         (begin  
           (define-syntax id
             (make-set!-transformer
              (lambda (so)
                (syntax-case so (set!)                  
                  ; Stand alone use of id really calls thunk
                  [i (identifier? #'i) #'(thunk)]
                  ; And applications (maybe raise error here?)
                  [_ (error)]))))
           (define (thunk) block))))]
    [(_ "procedure" id 
        (~seq "(" formals0 (~seq ";" formals) ... ")") 
        ";" block)
     (with-syntax ([(formal0 ...) (formals->ids #'formals0)]
                   [((formal ...) ...)
                    (map formals->ids 
                         (syntax->list #'(formals ...)))])
       (syntax/loc stx
         (define (id formal0 ... formal ... ...)
           block)))]))

(define (compile-function-declaration stx)
  ; function-declaration 
  ;   : "function" IDENTIFIER 
  ;     ["(" formal-parameters (";" formal-parameters)* ")"] 
  ;     ":" type-identifier ";" block
  (syntax-parse stx    
    [(_ "function" id ":" type-id ";" block)
     (with-syntax ([result (generate-temporary 
                            (~a (syntax->datum #'id) "-result"))]
                   [thunk (generate-temporary 
                           (~a (syntax->datum #'id) "-thunk"))])
       (syntax/loc stx
         (begin
           ; Pascal uses a variable to store the result value
           (define result 'uninitialized)           
           (define-syntax id
             (make-set!-transformer
              (lambda (so)
                (syntax-case so (set!)
                  ; Redirect mutation of id to result
                  [(set! _ v) #'(set! result v)]
                  ; Stand alone use of id really calls thunk
                  [i (identifier? #'i) #'(thunk)]
                  ; And applications (maybe raise error here?)
                  [(i . more) #'(thunk . more)]))))
           (define (thunk) block result))))]
    [(_ "function" id 
        (~seq "(" formals0 (~seq ";" formals) ... ")")
        ":" type-id ";" block)
     (with-syntax ([(formal0 ...) 
                    (formals->ids #'formals0)]
                   [((formal ...) ...)  
                    (map formals->ids
                         (syntax->list #'(formals ...)))]                   
                   [result (generate-temporary 
                            (~a (syntax->datum #'id) "-result"))])       
       (syntax/loc stx
         (begin
           (define result 'uninitialized)
           (define (id formal0 ... formal ... ...)
             (let-syntax 
                 ([id (make-set!-transformer
                       (lambda (so)
                         (syntax-case so (set!)
                           ; Redirect mutation of id to result
                           [(set! _ v) #'(set! result v)]
                           ; Normal use of id really gets id
                           [i (identifier? #'i) #'id]
                           ; And applications
                           [(i . more) #'(id . more)])))])
               block
               result)))))]
    [_ (error 'internal-error)]))

(define (compile-statement-part stx)
  (syntax-parse stx
    [(_ compound-statement)
     (compile-compound-statement #'compound-statement)]
    [_ (error)]))

(define (compile-compound-statement stx)
  ; compound-statement : 
  ;    "begin" [statement (";"+ statement)*] [";"+] "end"
  (syntax-parse stx
    [(_ "begin" "end")
     (syntax/loc stx 
       (begin))]
    [(_ (~seq "begin" 
              (~optional (~seq statement0 (~seq ";" statement) ...))
              (~optional ";")
              "end"))
     (syntax/loc stx 
       (begin statement0 statement ...))]
    [_ (error)]))

(define (compile-statement stx)
  ; statement : simple-statement | structured-statement
  (syntax-parse stx
    [(_ sub) #'sub]
    [_ (error)]))

(define (compile-simple-statement stx)
  ;simple-statement :   assignment-statement | procedure-statement 
  ;                   | read-statement | write-statement 
  ;                   | writeln-statement | application
  (syntax-parse stx
    [(_ sub) #'sub]    
    [_ (error)]))

(define (compile-assignment-statement stx)
  ; assignment-statement :   
  ;    variable ["[" expression "]"] ":=" expression
  ; variable :  IDENTIFIER | IDENTIFIER  "[" expression "]"
  (syntax-parse stx
    [(_ ((~datum variable) id) ":=" expr)
     (syntax/loc stx (set! id expr))]
    [(_ ((~datum variable) id) "[" expr1 "]" ":=" expr2)     
     (syntax/loc stx 
       (pascal:array-set! id expr1 expr2))]
    [_ (error)]))

(define (compile-read-statement stx)
  ; read-statement : ("readln"|"read")  "(" IDENTIFIER ("," IDENTIFIER)* ")"
  ; Note: The usage of IDENTIFIER rather than  variable  in the grammar,
  ;       disallows read(a[3]). Remember a[3] is a legal variable/lvalue.   
  (syntax-parse stx 
    [(_ "read" "(" id0 (~seq "," id) ... ")")
     (syntax/loc stx 
       (begin
         (pascal:read id0)
         (pascal:read id) ...))]
    [(_ "readln" "(" id0 (~seq "," id) ... ")")
     ; TODO: Figure out what the Pascal readln actually does
     (error 'TODO)]))

(define (compile-write-statement stx)
  ; write-statement :
  ;  "write"    "(" output-value ("," output-value)* ")"
  (syntax-parse stx 
    [(_ "write" "(" out0 (~seq "," out) ... ")")
     (syntax/loc stx
       (begin
         (pascal:write out0)
         (pascal:write out) ...))]))

(define (compile-writeln-statement stx)
  ; writeln-statement :
  ;  "writeln" ["(" output-value ("," output-value)* ")"]
  (syntax-parse stx     
    [(_ "writeln")
     (syntax/loc stx
       (newline))]
    [(_ "writeln" "(" out0 (~seq "," out) ... ")")
     (syntax/loc stx
       (begin
         (pascal:writeln out0)
         (pascal:writeln out) ...))]))

(define (compile-output-value stx)
  ; output-value : expression
  (syntax-parse stx
    [(_ expr)
     #'expr]
    [_ (error)]))

(define (compile-procedure-statement stx)
  ; procedure-statement : procedure-identifier
  (syntax-parse stx
    [(_ proc-id)
     (syntax/loc stx
       proc-id)]
    [_ (error)]))

(define (compile-procedure-identifier stx)
  ; procedure-identifier :	 IDENTIFIER
  (syntax-parse stx
    [(_ id) #'id]))

(define (compile-variable stx)
  ; variable : IDENTIFIER | IDENTIFIER  "[" expression "]"
  (syntax-parse stx
    [(_ id)
     #'id]
    [(_ id "[" expr "]")     
     (syntax/loc stx 
       (pascal:array-ref id expr))]
    [_ (error)]))

(define (compile-application stx)
  ; application : IDENTIFIER "(" expression ("," expression)* ")"
  (syntax-parse stx
    [(_ id "(" expr0 (~seq "," expr) ... ")")
     (syntax/loc stx 
       (id expr0 expr ...))]
    [_ (error)]))

(define (compile-expression stx)
  ; expression : 
  ;    simple-expression | 
  ;    simple-expression relational-operator simple-expression
  (syntax-parse stx
    [(_ simple-expr)
     #'simple-expr]
    [(_ simple-expr1 rel-op simple-expr2)
     (quasisyntax/loc stx 
       (rel-op simple-expr1 simple-expr2))]
    [_ (error)]))

(define-syntax (raise-unless-equal-types-error stx)
  (syntax-parse stx
    [(_ type1 type2 msg blame-stx)
     (syntax/loc stx
       (unless (equal? type1 type2)
         (def msg1 (~a msg1 "\nThe offending types are  "
                       type1 "  and  " type2))
         (raise-syntax-error 'type-error msg blame-stx)))]))

(define (compile-relational-operator stx)
  ; relational-operator : "=" | "<>" | "<" | "<=" | ">=" | ">"
  (syntax-parse stx
    [(_ "=")   #'equal? ]
    [(_ "<>")  #'(λ (a b) (not (equal? a b)))]
    [(_ "<")   #'(λ (a b) (cond [(integer? a) (< a b)]
                                [(char? a)    (char<? a b)]))]
    [(_ ">")   #'(λ (a b) (cond [(integer? a) (> a b)]
                                [(char? a)    (char>? a b)]))]
    [(_ "<=")  #'(λ (a b) (cond [(integer? a) (<= a b)]
                                [(char? a)    (char<=? a b)]))]
    [(_ ">=")  #'(λ (a b) (cond [(integer? a) (>= a b)]
                                [(char? a)    (char>=? a b)]))]))

(define (compile-simple-expression stx)
  ; simple-expression : sign term (adding-operator term)*
  ; sign : ["+" | "-"]
  (syntax-parse stx 
    [(_ term)
     #'term]
    [(_ (~and sub ((~datum sign) s)) term)
     (syntax/loc stx (* sub term))]
    [(_ term0 add-op term1)
     ; The adding operators + and - must expand to applications,
     ; and or must expand to a macro application. Therefore
     ; the operator needs to be expanded now.     
     (def op (compile-adding-operator #'add-op))
     (op #'term0 #'term1)]
    [(_ term0 add-op0 term1 add-op1 (~seq  term add-op) ... termN)     
     (def op0 (compile-adding-operator #'add-op0))
     (def op1 (compile-adding-operator #'add-op1))
     (with-syntax ([(_ t0 a0 t1 a1 . more) stx])
       (op1 (op0 #'term0 #'term1) 
            (compile-simple-expression 
             #'(simple-expression . more))))]
    [(_ sign term0 . more)       
     (compile-simple-expression #'((* sign term0) . more))]
    [_ (error)]))

(define (compile-adding-operator stx)
  ; adding-operator : "+" | "-" | "or"
  (syntax-parse stx
    [(_ "+")
     (λ (t1 t2) (quasisyntax/loc stx (+ #,t1 #,t2)))]
    [(_ "-")
     (λ (t1 t2) (quasisyntax/loc stx (- #,t1 #,t2)))]
    [(_ "or")
     (λ (t1 t2) (quasisyntax/loc stx (or #,t1 #,t2)))]
    [else (error)]))

(define (compile-sign stx)
  ; sign :	["+" | "-"]
  (syntax-parse stx
    [(_ "-") #'-1]
    [_       #'1]))

(define (compile-term stx)
  ; term : factor (multiplying-operator factor)*; 
  (syntax-parse stx
    [(_ factor) 
     #'factor]
    [(_ factor0 mul-op . sub-factors)
     (def op (compile-multiplying-operator #'mul-op))
     (op #'factor0 
         (compile-term #'(term . sub-factors)))]
    [_ (error)]))

(define (compile-multiplying-operator stx)
  ; multiplying-operator : "*" | "div" | "and"  
  (syntax-parse stx
    [(_ "*")
     (λ (f1 f2) (quasisyntax/loc stx (* #,f1 #,f2)))]
    [(_ "div")
     (λ (f1 f2) (quasisyntax/loc stx (quotient #,f1 #,f2)))]
    [(_ "and")
     (λ (f1 f2) (quasisyntax/loc stx (and #,f1 #,f2)))]
    [else (error)]))

(define (compile-factor stx)
  ; factor : application | variable | constant | "(" expression ")" | "not" factor 
  (syntax-parse stx
    [(_ sub) 
     #'sub]
    [(_ "(" expr ")") 
     #'expr]
    [(_ "not" factor) 
     (syntax/loc stx (not factor))]))

(define (compile-constant stx)
  ; constant : 
  ;   INTEGER-CONSTANT | CHARACTER-CONSTANT | STRING-CONSTANT | constant-identifier
  (syntax-parse stx
    [(_ sub)
     (define datum (syntax->datum #'sub))
     (cond
       [(string? datum) (syntax/loc stx (string->array sub))]
       [else            #'sub])]
    [_ (error)]))

(define (compile-constant-identifier stx)
  (syntax-parse stx
    [(_ (~datum true))  #'#t]
    [(_ (~datum false)) #'#f]
    [(_ id) #'id]
    [_ (error)]))

(define (compile-structured-statement stx)
  ; structured-statement : 
  ;   compound-statement | if-statement |  while-statement
  (syntax-parse stx 
    [(_ sub) #'sub]
    [_ (error)]))

(define (compile-if-statement stx)
  ; if-statement : "if" expression "then" statement | 
  ;                "if" expression "then" statement "else" statement
  (syntax-parse stx 
    [(_ "if" expr "then" stat)
     (syntax/loc stx 
       (when expr stat))]
    [(_ "if" expr "then" stat1 "else" stat2)
     (syntax/loc stx 
       (if expr stat1 stat2))]))

(define (compile-while-statement stx)
  ; while-statement : "while" expression "do" statement
  (syntax-parse stx
    [(_ "while" expr "do" stat)
     (syntax/loc stx
       (let while ()
         (when expr stat (while))))]))

(define (compile-for-statement stx)
  ; for-statement : "for" IDENTIFIER ":=" expression 
  ;                 ("to"|"downto") expression "do" statement
  (syntax-parse stx
    [(_ "for" id ":=" expr1 "to" expr2 "do" stat)
     (syntax/loc stx
       (let ([initial expr1] [final expr2])
         (let for ([id initial])
           stat
           (unless (eqv? id final)
             (for (succ id))))))]
    [(_ "for" id ":=" expr1 "downto" expr2 "do" stat)
     (syntax/loc stx
       (let ([initial expr1] [final expr2])
         (let for ([id initial])
           stat
           (unless (eqv? id final)
             (for (prev id))))))]))
