#lang racket
;;;
;;; COMPILER
;;;

(provide compile-program)

;; This is a compiler for MiniPascal that checks 
;; types at compile time. 

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
;;; TYPES
;;;

; The following table shows how the Pacal types are represented.
; The representation is called a (type) description.

;;; Pascal                    Description

;   boolean               ->  'boolean
;   char                  ->  'char
;   integer               ->  'integer
;   false                 ->  'false
;   true                  ->  'true
;   from..to              ->  (list from-desc to-desc)
;   array[idx] of T       ->  (list 'array (list from to) T-desc)
;   procedure             ->  (list 'procedure (list))
;   procedure(T1,...)     ->  (list 'procedure (list T1-desc ...))
;   function (T1,...) : T ->  (list 'function  (list T1-desc ...) T-desc)

(define type:boolean 'boolean)
(define type:char    'char)
(define type:integer 'integer)
(define (type:index-range from to) 
  (list from to))
(define (type:array idx-type elm-type) 
  (list 'array idx-type elm-type))
(define (type:procedure input-types) 
  (list 'procedure input-types))
(define (type:function input-types return-type) 
  (list 'function input-types return-type))

(define (compile-type stx)
  ; type : simple-type | array-type
  (syntax-parse stx 
    [(_ sub)
     (syntax-parse #'sub
       [((~datum simple-type) . more) (compile-simple-type #'sub)]
       [((~datum array-type) . more)  (compile-array-type #'sub)]
       [_ (displayln stx) (error 'ct:internal-error)])]))

(define (compile-array-type stx)
  ; array-type : "array" "[" index-type "]" "of" simple-type
  (syntax-parse stx
    [(_ "array" "[" index "]" "of" simple)
     (type:array (compile-index-type #'index)
                 (compile-simple-type #'simple))]))

(define (compile-index-range stx)
  ;index-range :      
  ; ([sign] (constant-name | CHARACTER-CONSTANT | INTEGER-CONSTANT))
  ; ".." 
  ; ([sign] (constant-name | CHARACTER-CONSTANT | INTEGER-CONSTANT))
  (syntax-parse stx
    ; TODO: Handle signs
    [(_ ((~datum constant-name) from-const) ".." to)
     (unless (bound-to-constant? #'from-const) 
       (error 'index-range "unknown constant" from stx))
     (def from (lookup #'from-const))
     (with-syntax ([val (constant-info-value from)])
       (compile-index-range #'(_ val ".." to)))]
    [(_ from ".." ((~datum constant-name) to-const))
     (unless (bound-to-constant? #'to-const) 
       (error 'index-range "unknown constant" #'to-const stx))
     (def to (lookup #'to-const))
     (with-syntax ([val (constant-info-value to)])         
       (compile-index-range #'(_ from ".." val)))]
    [(_ from ".." to)
     (type:index-range (syntax->datum #'from)
                       (syntax->datum #'to))]))

(define (compile-index-type stx)
  ;index-type : type-identifier | index-range
  (syntax-parse stx 
    [(_ sub)
     (syntax-parse #'sub 
       [((~datum type-identifier) id)
        (syntax->datum #'id)]
       [((~datum index-range) . more)
        (compile-index-range #'sub)])]))

(define (compile-simple-type stx)
  ; simple-type : type-identifier | index-range
  (syntax-parse stx
    [(_ (~and sub ((~datum type-identifier) . more)))
     (compile-type-identifier #'sub)]
    [(_ (~and sub ((~datum index-type) . more)))
     (compile-index-type #'sub)]
    [_ (error 'internal-error)]))

(define (compile-type-identifier stx)
  (syntax-parse stx
    [(_ id)
     (match (lookup #'id)
       [(type-info desc) desc]
       [_ 
        (def msg (~a "The type identifier " #'id " is unbound."))
        (raise-syntax-error 'compile-type-identifier msg stx)])]))

(define (type:initial-value-constructor type)
  (def desc (lookup type))
  (match (if desc desc type)
    [(struct type-info ('integer)) #'0]
    [(struct type-info ('boolean)) #'#t]
    [(struct type-info ('char))    #'#\a]
    [(struct type-info ('true))    #'#t]
    [(struct type-info ('false))   #'#f]
    [(list 'array (list from to) of-desc)
     (define ->index-stx
       (cond
         [(and (integer? from) (integer? to))
          (with-syntax ([from from] [to to])
            #'(λ (x) (- x from)))]
         [(and (char? from) (char? to))
          (with-syntax ([from from] [to to])
            #'(λ (x) (- (char->integer x) (char->integer from))))]
         [else (error 'non-ordinal-types)]))
     (with-syntax 
         ([of-construction-stx
           (type:initial-value-constructor of-desc)]
          [->index ->index-stx])         
       #`(pascal:construct-array 
          #,from #,to ->index (λ () of-construction-stx)))]
    [#f (error "type not defined" type)]))

;;; END OF TYPES


;;;
;;; SCOPE
;;;

; Scope is represented as list of frames
(define current-scope (make-parameter '()))
; Each frame is an association list from 
; symbols to info structures.
(define-struct frame (alist) #:mutable #:transparent)

; There are 3 different info structures,
; name for constants, types and variables.
(define-struct info ())
(define-struct (constant-info info) (value)       #:transparent)
(define-struct (type-info info)     (description) #:transparent)
(define-struct (variable-info info) (description) #:transparent)

(define (make-empty-frame) (make-frame '()))
(define (make-empty-scope) (list (make-empty-frame)))

(define (lookup-in-frame sym frame)
  (cond [(assoc sym (frame-alist frame)) => cdr]
        [else #f]))

(define (lookup sym)
  ; return what sym is associated to in the current scope,
  ; return #f when sym is not in the current scope
  (set! sym (if (identifier? sym) (syntax->datum sym) sym))
  (for/or ([frame (in-list (current-scope))])
    (lookup-in-frame sym frame)))

(define (free? sym)
  (not (lookup sym)))

(define (bound? sym)
  (not (not (lookup sym))))

(define (bound-to-constant? sym)
  (cond [(lookup sym) => constant-info?]
        [else #f]))

(define (bound-to-variable? sym)
  (cond [(lookup sym) => variable-info?]
        [else #f]))

(define (bound-to-type? sym)
  (cond [(lookup sym) => type-info?]
        [else #f]))

(define (push-frame! frame)
  (current-scope (cons frame (current-scope))))

(define (push-empty-frame!)
  (push-frame! (make-empty-frame)))

(define (pop-frame!)
  (def frames (current-scope))
  (if (empty? frames)
      (raise-argument-error 'pop-frame "non-empty scope")
      (current-scope (cdr (current-scope)))))

(define (top-frame)
  (def scope (current-scope))
  (cond [(empty? scope) => (λ(_) (error 'internal-error))]
        [else (car scope)]))

(define (add-to-scope! sym info)
  ; sym can be either a symbol or an identifier
  (def frame (top-frame))
  (def alist (frame-alist frame))
  (when (identifier? sym)
    (set! sym (syntax->datum sym)))
  (set-frame-alist! frame (cons (cons sym info) alist)))

(define-syntax (with-extended-scope stx)
  (syntax-parse stx
    [(_ frame body ...)
     #'(let ()
         (push-frame! frame)
         (define result (let () body ...))
         (pop-frame!)
         result)]))

;;;
;;; Compilation
;;;

(define (compile-program stx)  
  (syntax-parse stx
    [({~datum program} "program" program-name ";" block ".")
     (parameterize ([current-scope (make-empty-scope)])
       ;add base types
       (add-to-scope! 'integer (make-type-info 'integer))
       (add-to-scope! 'boolean (make-type-info 'boolean))
       (add-to-scope! 'char    (make-type-info 'char))
       (add-to-scope! 'true    (make-type-info 'boolean))
       (add-to-scope! 'false   (make-type-info 'boolean))
       ; Any library functions can be added here.
       (push-empty-frame!)         
       (compile-block #'block))]))

(define (compile-block stx)
  (syntax-parse stx
    [(_ constant-definition-part
        type-definition-part
        variable-declaration-part
        procedure-and-function-declaration-part
        statement-part)
     ; The compilation of the constant and type definitions 
     ; adds the defined constants and types to the current
     ; scope. 
     (compile-constant-definition-part #'constant-definition-part)
     (compile-type-definition-part #'type-definition-part)
     ; The expansion of the variable declaration part
     ; is a list of syntax objects of the form (define-values ...)
     (def def-vars (compile-variable-declaration-part
                    #'variable-declaration-part))
     (def decls (compile-procedure-and-function-declaration-part
                 #'procedure-and-function-declaration-part))
     (def stat-part (compile-statement-part #'statement-part))
     (quasisyntax/loc stx
       (let () ; new scope
         #,@def-vars
         #,@decls
         #,stat-part))]
    [_ (error 'compile-block "internal error")]))

(define (compile-constant-definition-part stx)
  ; constant-definition-part : ["const" (constant-definition ";")+]
  (syntax-parse stx
    [(_) (void)]
    [(_ "const" (~seq const-def ";") ...)
     (push-empty-frame!) ; new scope for constants
     (for-each compile-constant-definition 
               (syntax->list #'(const-def ...)))]))

(define (compile-constant-definition stx)
  ; constant-definition : IDENTIFIER ("," IDENTIFIER)* "=" INTEGER-CONSTANT
  (syntax-parse stx
    [(_ id0 (~seq "," id) ... "=" x)
     (def val (syntax->datum #'x))
     (def info (make-constant-info val))
     (for ([c (in-list (cons #'id0 (syntax->list #'(id ...))))])
       (add-to-scope! c info))]))

(define (compile-type-definition-part stx)
  ; type-definition-part : ["type" (type-definition ";")+]
  (syntax-parse stx
    [(_) (void)]
    [(_ "type" (~seq type-def ";") ...)
     (push-empty-frame!) ; new scope for type definitions 
     (for-each compile-type-definition
               (syntax->list #'(type-def ...)))]))

(define (compile-type-definition stx)
  ; type-definition : IDENTIFIER "=" type
  (syntax-parse stx
    [(_ id "=" type)
     (add-to-scope! #'id (make-type-info (compile-type #'type)))]))

(define (compile-variable-declaration-part stx)
  ; variable-declaration-part : ["var" (variable-declaration ";")+]
  (syntax-parse stx
    [(_) '()]
    [(_ "var" (~seq var-decl ";") ...+)
     (push-empty-frame!) ; new scope
     (append-map compile-variable-declaration
                 (syntax->list #'(var-decl ...)))]))

(define (compile-variable-declaration stx)
  ; variable-declaration : IDENTIFIER ("," IDENTIFIER)* ":" type
  (syntax-parse stx
    [(_ id0 (~seq "," id) ... ":" type)
     (def desc (compile-type #'type))
     (def c (type:initial-value-constructor desc))
     (for/list ([id (in-list (syntax->list #'(id0 id ...)))])
       (add-to-scope! id (make-variable-info desc))
       (quasisyntax/loc stx 
         (define #,id #,c)))]))

(define (compile-procedure-and-function-declaration-part stx)
  ; procedure-and-function-declaration-part : 
  ;   ((procedure-declaration ";") | (function-declaration ";"))*
  (syntax-parse stx
    [(_ (~seq decl ";") ...)
     (define (compile-decl d)
       (syntax-parse d 
         [((~datum procedure-declaration) . more)            
          (compile-procedure-declaration d)]
         [((~datum function-declaration) . more)
          (compile-function-declaration d)]))
     (push-empty-frame!) ; new scope ? (TODO)
     (map compile-decl
          (syntax->list #'(decl ...)))]
    [_ (error)]))

(define (formals->ids stx)
  ; formal-parameters : ["var"] IDENTIFIER ("," IDENTIFIER)* ":" type 
  (syntax-parse stx
    [(_ (~optional "var") id0 (~seq "," id) ... ":" type)
     (syntax->list #'(id0 id ...))]))

(define (formals->description stx)
  ; formal-parameters : ["var"] IDENTIFIER ("," IDENTIFIER)* ":" type 
  (syntax-parse stx
    [(_ (~optional "var") id0 (~seq "," id) ... ":" type)
     (compile-type #'type)]))

(define (formals->descriptions stx)
  ; formal-parameters : ["var"] IDENTIFIER ("," IDENTIFIER)* ":" type 
  (syntax-parse stx
    [(_ (~optional "var") id0 (~seq "," id) ... ":" type)
     (def desc (compile-type #'type))
     (map (λ(_) desc) (formals->ids stx))]))

(define (compile-procedure-declaration stx)
  ; procedure-declaration 
  ;   : "procedure" IDENTIFIER 
  ;     ["(" formal-parameters (";" formal-parameters)* ")"] ";"
  ;     block  
  (syntax-parse stx
    [(_ "procedure" id ";" block)
     (add-to-scope! (syntax->datum #'id)
                    (make-variable-info (type:procedure '())))
     (def compiled-block (compile-block #'block))
     (with-syntax ([thunk (generate-temporary 
                           (~a (syntax->datum #'id) "-thunk"))])
       (quasisyntax/loc stx 
         (begin  
           (define-syntax id
             (make-set!-transformer
              (lambda (so)
                (syntax-case so (set!)                  
                  ; Stand alone use of id really calls thunk
                  [i (identifier? #'i) #'(thunk)]
                  [_ (error)]))))
           (define (thunk) #,compiled-block))))]
    [(_ "procedure" id 
        (~seq "(" formals0 (~seq ";" formals) ... ")") 
        ";" block)
     (with-syntax ([(formal0 ...) (formals->ids #'formals0)]
                   [((formal ...) ...)
                    (map formals->ids 
                         (syntax->list #'(formals ...)))])
       (def formals-desc 
         (append* (map formals->descriptions
                       (syntax->list #'(formals0 formals ...)))))
       (def proc-desc (type:procedure formals-desc))
       (def alist 
         (map cons 
              (syntax->datum #'(formal0 ... formal ... ...))
              (map make-variable-info formals-desc)))  
       (def sym (syntax->datum #'id))                      
       (add-to-scope! sym info)
       (def info (make-variable-info proc-desc))
       (def compiled-block 
         (with-extended-scope (make-frame alist)
           (add-to-scope! sym info)
           (compile-block #'block)))       
       (quasisyntax/loc stx
         (define (id formal0 ... formal ... ...)
           #,compiled-block)))]))

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
       ; register type
       (def sym (syntax->datum #'id))
       (def desc 
         (type:function '() (compile-type-identifier #'type-id)))
       (def info (make-variable-info desc))
       (def compiled-block 
         (with-extended-scope (make-empty-frame)
           (add-to-scope! sym info)
           (compile-block #'block)))
       (quasisyntax/loc stx
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
           (define (thunk) #,compiled-block result))))]
    [(_ "function" id 
        (~seq "(" formals0 (~seq ";" formals) ... ")")
        ":" type-id ";" block)
     ;;; TODO XXX check lookup succeeds.
     (with-syntax ([(formal0 ...) 
                    (formals->ids #'formals0)]
                   [((formal ...) ...)  
                    (map formals->ids
                         (syntax->list #'(formals ...)))]
                   [result (generate-temporary 
                            (~a (syntax->datum #'id) "-result"))])
       (define input-types
         (map formals->descriptions
              (syntax->list #'(formals0 formals ...))))
       (def sym (syntax->datum #'id))
       (def desc (type:function 
                  input-types
                  (compile-type-identifier #'type-id)))
       (def info (make-variable-info desc))
       (def formals-desc 
         (append*
          (map formals->descriptions
               (syntax->list #'(formals0 formals ...)))))
       (def alist 
         (map cons 
              (syntax->datum #'(formal0 ... formal ... ...))
              (map make-variable-info formals-desc)))
       (def compiled-block 
         (with-extended-scope (make-frame alist)
           (add-to-scope! sym info)
           (compile-block #'block)))
       (quasisyntax/loc stx
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
               #,compiled-block
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
    [(_ (~seq "begin" 
              (~optional (~seq statement0 (~seq ";" statement) ...))
              (~optional ";")
              "end"))
     (def statements 
       (map compile-statement              
            (syntax->list #'(statement0 statement ...))))     
     (quasisyntax/loc stx 
       (begin #,@statements))]
    [_ (error)]))

(define (compile-statement stx)
  ; statement : simple-statement | structured-statement
  (syntax-parse stx
    ; a structured statement begins with begin, if, or, while.
    [(_ (~and sub ((~datum structured-statement) . more)))
     (compile-structured-statement #'sub)]
    [(_ sub)
     (compile-simple-statement #'sub)]
    [_ (error)]))

(define (compile-simple-statement stx)
  ;simple-statement : assignment-statement | procedure-statement 
  ;             | read-statement | write-statement | application
  (syntax-parse stx
    [(_ (~and sub ((~datum assignment-statement) . more)))
     (compile-assignment-statement #'sub)]
    [(_ (~and sub ((~datum procedure-statement) . more)))
     (compile-procedure-statement #'sub)]
    [(_ (~and sub ((~datum read-statement) . more)))
     (compile-read-statement #'sub)]
    [(_ (~and sub ((~datum write-statement) . more)))
     (compile-write-statement #'sub)]
    [(_ (~and sub ((~datum writeln-statement) . more)))
     (compile-write-statement #'sub)]
    [(_ (~and sub ((~datum application) . more)))
     (def (s st) (compile-application #'sub))
     s]
    [_ (error 'compile-simple-statement "internal error")]))

(define (compile-assignment-statement stx)
  ; assignment-statement :   
  ;    variable ["[" expression "]"] ":=" expression
  ; variable :  IDENTIFIER | IDENTIFIER  "[" expression "]"
  (syntax-parse stx
    [(_ ((~datum variable) id) ":=" expr)
     ; TODO: Check that types are compatible
     (def sym (syntax->datum #'id))
     (unless (bound-to-variable? sym)
       (raise-syntax-error 'compile-assignment-statement
                           "unbound variable" stx #'id))
     (def (e _) (compile-expression #'expr))
     (quasisyntax/loc stx (set! id #,e))]
    [(_ ((~datum variable) id) "[" expr1 "]" ":=" expr2)
     (def sym (syntax->datum #'id))
     (unless (bound-to-variable? sym)
       (raise-syntax-error 'compile-assignment-statement
                           "unbound variable" stx #'id))
     (def (e1 _)  (compile-expression #'expr1))
     (def (e2 __) (compile-expression #'expr2))
     (quasisyntax/loc stx 
       (pascal:array-set! id #,e1 #,e2))]
    [_ (error)]))

(define (compile-read-statement stx)
  ; read-statement : ("readln"|"read")  "(" IDENTIFIER ("," IDENTIFIER)* ")"
  ; Note: The usage of IDENTIFIER rather than  variable  in the grammar,
  ;       disallows read(a[3]). Remember a[3] is a legal variable/lvalue.   
  (syntax-parse stx 
    [(_ "read" "(" id0 (~seq "," id) ... ")")
     (def var-info (lookup #'id0))
     (def reader
       (match var-info
         [(struct variable-info ('integer)) #'pascal:read-integer]
         [(struct variable-info ('char))    #'pascal:read-char]
         [else     
          (error "read supports only integer and char")]))
     (quasisyntax/loc stx 
       (begin
         (set! id0 (#,reader))
         (set! id  (#,reader)) ...))]
    [(_ "readln" "(" id0 (~seq "," id) ... ")")
     ; TODO: Figure out what the Pascal readln actually does
     (error 'TODO)]))

(define (compile-write-statement stx)
  ; write-statement :
  ;   "write"    "(" output-value ("," output-value)* ")"
  ; | "writeln" ["(" output-value ("," output-value)* ")"]
  (syntax-parse stx 
    [(_ write-op "(" out0 (~seq "," out) ... ")")     
     (def writer 
       (match (syntax->datum #'write-op)
         ["writeln" #'pascal:writeln]
         ["write"   #'pascal:write]))
     (def outs (syntax->list #'(out0 out ...)))
     (def writes
       (for/list ([out (in-list outs)])
         (def (expr type) (compile-output-value out))
         (quasisyntax/loc stx
           (#,writer #,expr))))
     (quasisyntax/loc stx
       (begin #,@writes))]
    [(_ "writeln")
     (syntax/loc stx
       (newline))]))

(define (compile-output-value stx)
  ; output-value : expression
  (syntax-parse stx
    [(_ expr)
     (compile-expression #'expr)]
    [_ (error)]))

(define (compile-procedure-statement stx)
  ; procedure-statement : procedure-identifier
  (syntax-parse stx
    [(_ proc-id)
     (def id (compile-procedure-identifier #'proc-id))
     ; TODO: check id is bound to correct procedure type
     (quasisyntax/loc stx
       #,id)]
    [_ (error)]))

(define (compile-procedure-identifier stx)
  ; procedure-identifier :	 IDENTIFIER
  (syntax-parse stx
    [(_ id) #'id]))

(define (compile-variable stx)
  ; variable : IDENTIFIER | IDENTIFIER  "[" expression "]"
  (syntax-parse stx
    [(_ id)
     (values #'id (lookup #'id))]
    [(_ id "[" expr "]")
     (def (e et) (compile-expression #'expr))
     (match (lookup #'id)
       [(variable-info (list 'array index-type of-type))
        ; TODO: check that et matches index-type
        (values (quasisyntax/loc stx 
                  (pascal:array-ref id #,e))
                of-type)]
       [else (raise-syntax-error 'compile-variable
                                 "variable has wrong type"
                                 #'id)])]))

(define (compile-application stx)
  ; application : IDENTIFIER "(" expression ("," expression)* ")"
  (syntax-parse stx
    [(_ id "(" expr0 (~seq "," expr) ... ")")
     (def exprs (syntax->list #'(expr0 expr ...)))
     (def (es types) (map2 compile-expression exprs))
     (def desc 
       (match (lookup #'id)
         [(variable-info (list 'function inputs return-desc))
          return-desc]
         [(variable-info (list 'procedure inputs))
          'pascal:'void]
         [else 
          (raise-syntax-error
                'application "name unbound" #'id)]))
     (with-syntax ([(e0 e ...) es])
       (values (syntax/loc stx (id e0 e ...))
               desc))]
    [_ (error)]))

(define (compile-expression stx)
  ; expression : 
  ;    simple-expression | 
  ;    simple-expression relational-operator simple-expression
  (syntax-parse stx
    [(_ simple-expr)
     (compile-simple-expression #'simple-expr)]
    [(_ simple-expr1 rel-op simple-expr2)
     (def (se1 t1) (compile-simple-expression #'simple-expr1))
     (def (se2 t2) (compile-simple-expression #'simple-expr2))
     (def op (compile-relational-operator #'rel-op t1 t2))
     (values (quasisyntax/loc stx (#,op #,se1 #,se2))
             'boolean)]
    [_ (error)]))

(define-syntax (raise-unless-equal-types-error stx)
  (syntax-parse stx
    [(_ type1 type2 msg blame-stx)
     (syntax/loc stx
       (unless (equal? type1 type2)
         (def msg1 (~a msg1 "\nThe offending types are  "
                       type1 "  and  " type2))
         (raise-syntax-error 'type-error msg blame-stx)))]))

(define (compile-relational-operator stx type1 type2)
  ; relational-operator : "=" | "<>" | "<" | "<=" | ">=" | ">"
  (def msg "Comparison of values with different types.")
  (raise-unless-equal-types-error type1 type2 msg stx)
  (match type1
    ['integer
     (syntax-parse stx
       [(_ "=")   #'=]
       [(_ "<>")  #'(λ (a b) (not (= a b)))]
       [(_ "<")   #'<]
       [(_ ">")   #'>]
       [(_ "<=")  #'<=]
       [(_ ">=")  #'>=])]
    ['char
     (syntax-parse stx
       [(_ "=")   #'char=?]
       [(_ "<>")  #'(λ (a b) (not (char=? a b)))]
       [(_ "<")   #'char<?]
       [(_ ">")   #'char>?]
       [(_ "<=")  #'char<=?]
       [(_ ">=")  #'char>=?])]
    [else 
     (def msg "comparison of values with non-ordinal type")
     (raise-syntax-error 'rel-op msg stx)]))

(define (compile-simple-expression stx)
  ; simple-expression : sign term (adding-operator term)*
  ; sign : ["+" | "-"]
  (syntax-parse stx 
    [(_ term)
     (compile-term #'term)]
    [(_ ((~datum sign) s) term)
     (def (t type) (compile-term #'term))       
     (values (quasisyntax/loc stx (* (sign s) #,t)) type)]
    [(_ term0 add-op term1)
     ; The adding operators + and - must expand to applications,
     ; and or must expand to a macro application. Therefore
     ; the operator needs to be expanded now.
     (def (t0 type0) (compile-term #'term0))
     (def (t1 type1) (compile-term #'term1))
     (def op (compile-adding-operator #'add-op type0 type1))
     (values (quasisyntax/loc stx (#,op #,t0 #,t1)) type0)]
    [(_ term0 add-op0 term1 add-op1 (~seq  term add-op) ... termN)
     (def (et0 type0) (compile-term #'term0))
     (def (et1 type1) (compile-term #'term1))
     (def op0 (compile-adding-operator #'add-op0 type0 type1))
     (def op1 (compile-adding-operator #'add-op1 type1 type1))
     (with-syntax ([(_ t0 a0 t1 a1 . more) stx])
       (def (es es-type)
         (compile-simple-expression
          (syntax/loc stx
            (simple-expression . more))))
       (values (quasisyntax/loc stx 
                 (#,op1 (#,op0 #,et0 #,et1) #,es))
               type0))]
    [(_ sign term0 . more)       
     (compile-simple-expression
      (syntax/loc stx 
        (simple-expression (* sign term0) . more)))]
    [_ (error)]))

(define (compile-adding-operator stx type1 type2)
  ; adding-operator : "+" | "-" | "or"
  (def msg "Adding operators must have terms with same types.")
  (raise-unless-equal-types-error type1 type2 msg stx)    
  (match type1
    ['integer (syntax-parse stx
                [(_ "+")  #'+]
                [(_ "-")  #'-])]
    ['boolean (syntax-parse stx
                [(_ "or") #'or])]
    [else
     (def msg 
       (~a "The operator is not defined for this type" type1))
     (raise-syntax-error msg 'add-op stx)]))

(define (compile-sign stx)
  ; sign :	["+" | "-"]
  (syntax-parse stx
    [(_ "-") #'-1]
    [_       #'1]))

(define (compile-term stx)
  ; term : factor (multiplying-operator factor)*; 
  (syntax-parse stx
    [(_ factor) 
     (compile-factor #'factor)]
    [(_ factor0 op . sub-factors)
     (def (cf0 type0) (compile-factor #'factor0))
     (def (cf1 type1) (compile-term #'(term . sub-factors)))
     (def cop (compile-multiplying-operator #'op type0 type1))
     (values (quasisyntax/loc stx 
               (#,cop #,cf0 #,cf1))
             type0)]
    [_ (error)]))

(define (compile-multiplying-operator stx type1 type2)  
  ; multiplying-operator : "*" | "div" | "and"
  (def msg "Multiplying operators must have terms with same types.")
  (raise-unless-equal-types-error type1 type2 msg stx)
  (match type1
    ['integer (syntax-parse stx
                [(_ "*")    #'*]
                [(_ "div")  #'quotient])]
    ['boolean (syntax-parse stx
                [(_ "and")  #'and])]
    [else 
     (raise-syntax-error 
      'mul-op "operator not defined for this type" stx)]))

(define (compile-factor stx)
  ; factor : application | variable | constant | "(" expression ")" | "not" factor 
  (syntax-parse stx
    [(_ sub)
     (syntax-parse #'sub
       [((~datum application) . more) (compile-application #'sub)]
       [((~datum variable) . more)    (compile-variable #'sub)]
       [((~datum constant) . more)    (compile-constant #'sub)])]
    [(_ "(" expr ")") 
     (compile-expression #'expr)]
    [(_ "not" factor) 
     (def (ef type) (compile-factor #'factor))
     (unless (equal? type 'boolean)
       (raise-syntax-error 
        '<not-factor> "expected (not <boolean-expr>)" stx))
     (values (quasisyntax/loc stx (not #,ef))
             'boolean)]))

(define (compile-constant stx)
  ; constant : 
  ;   INTEGER-CONSTANT | CHARACTER-CONSTANT | constant-identifier
  (define (type-of val)
    (cond [(integer? val) 'integer]
          [(char? val) 'char]
          [(boolean? val) 'boolean]))
  (syntax-parse stx
    [(_ (~and sub ((~datum constant-identifier) . more)))
     (compile-constant-identifier #'sub)]
    [(_ val) 
     (values #'val (type-of (syntax->datum #'val)))]
    [_ (error 'compile-constant "internal error")]))

(define (compile-constant-identifier stx)
  ; constant-identifier : IDENTIFIER
  (define (type-of val)
    (cond [(integer? val) type:integer]  ; make-type-info XXXX
          [(char? val)    type:char]
          [(boolean? val) type:boolean]))
  (syntax-parse stx
    [(_ (~datum true))
     (values #'#t type:boolean)]
    [(_ (~datum false))
     (values #'#f type:boolean)]
    [(_ id)
     ; XXX TODO catch calls to functions with no arguments
     (match (lookup #'id)
       [(constant-info val)
        (values (with-syntax ([val val])
                  (syntax/loc stx val))
                (type-of val))]
       ; The lexer might return (constant-identifier _) 
       ; for non-constants (the lexer doesn't know which
       ; identifiers are constants and which are variables.
       [(variable-info desc)
        (match desc
          [(list 'function input output)
           (values #'id output)]
          [else (values #'id desc)])] ; error?
       [else
        (def msg (~a "unbound identifier"))
        (raise-syntax-error 'identifier msg #'id)])]
    [_ (error)]))

(define (compile-structured-statement stx)
  ; structured-statement : 
  ;   compound-statement | if-statement |  while-statement
  (syntax-parse stx 
    [(_ sub)
     (syntax-parse #'sub
       [((~datum compound-statement) . more)
        (compile-compound-statement #'sub)]
       [(~and sub ((~datum if-statement) . more))
        (compile-if-statement #'sub)]
       [(~and sub ((~datum while-statement) . more))
        (compile-while-statement #'sub)]
       [_ (error)])]
    [_ (error)]))

(define (compile-if-statement stx)
  ; if-statement : "if" expression "then" statement | 
  ;                "if" expression "then" statement "else" statement
  (syntax-parse stx 
    [(_ "if" expr "then" stat)
     (def (e et) (compile-expression #'expr))
     (unless (equal? et type:boolean)
       (def msg "boolean expression expected")
       (raise-syntax-error 'if msg stx #'expr))
     (def s (compile-statement #'stat))
     (quasisyntax/loc stx (when #,e #,s))]
    [(_ "if" expr "then" stat1 "else" stat2)
     (def (e et) (compile-expression #'expr))
     (unless (equal? et type:boolean)
       (def msg "boolean expression expected")
       (raise-syntax-error 'if msg stx #'expr))
     (def s1 (compile-statement #'stat1))
     (def s2 (compile-statement #'stat2))
     (quasisyntax/loc stx (if #,e #,s1 #,s2))]))

(define (compile-while-statement stx)
  ; while-statement : "while" expression "do" statement
  (syntax-parse stx
    [(_ "while" expr "do" stat)
     (def (e et) (compile-expression #'expr))
     (unless (equal? et type:boolean)
       (def msg "boolean expression expected")
       (raise-syntax-error 'while msg stx #'expr))
     (def s (compile-statement #'stat))
     (quasisyntax/loc stx
       (let while ()
         (when #,e #,s (while))))]))
