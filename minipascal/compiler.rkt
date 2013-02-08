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

; Type constructors and predicates are defined 
; in "types.rkt".

(require "types.rkt")

(define (compile-type stx)
  ; type : simple-type | array-type
  (syntax-parse stx 
    [(_ sub)
     (syntax-parse #'sub
       [((~datum simple-type) . _) (compile-simple-type #'sub)]
       [((~datum array-type)  . _) (compile-array-type  #'sub)]
       [_ (displayln stx) (error 'ct:internal-error)])]))

(define (compile-array-type stx)
  ; array-type : "array" "[" index-type "]" "of" simple-type
  (syntax-parse stx
    [(_ "array" "[" index "]" "of" simple)
     (array (compile-index-type #'index)
            (compile-simple-type #'simple))]))

(define (compile-index-constant stx)
  ; index-const : [sign] INTEGER-CONSTANT | CHARACTER-CONSTANT 
  ;             | [sign] constant-name 
  (syntax-parse stx
    [(_ (~optional sign #:defaults ([sign #'(sign "+")]))
        const:integer)
     (define s (compile-sign #'sign))
     (define val (syntax->datum #'const))
     (if (= s 1) val (- val))]
    [(_ (~optional sign #:defaults ([sign #'(sign "+")]))
        (~and sub ((~datum constant-name) . _)))
     (define s (compile-sign #'sign))
     (define val (compile-constant-name #'sub))
     (if (= s 1) val (- val))]
    [(_ c:char)
     (syntax->datum #'c)]
    [_ (error)]))

(define (compile-constant-name stx)
  (syntax-parse stx
    [(_ id)
     (def sym (syntax->datum #'id))
     (unless (bound-to-constant? sym)
       (raise-syntax-error 'compile-constant-name
                           "unbound variable" stx #'id))
     (match (lookup #'id)
       [(constant-info val) val]
       [_ (error)])]
    [_ (error)]))

(define (compile-index-range stx)
  ; index-range : index-constant ".." index-constant
  (syntax-parse stx
    [(_ from ".." to)
     (index-range 
      (compile-index-constant #'from)
      (compile-index-constant #'to))]))

(define (compile-index-type stx)
  ;index-type : type-identifier | index-range
  (syntax-parse stx 
    [(_ sub)
     (syntax-parse #'sub 
       [((~datum type-identifier) id) (syntax->datum #'id)]
       [((~datum index-range) . _)    (compile-index-range #'sub)])]))

(define (compile-simple-type stx)
  ; simple-type : type-identifier | index-range
  (syntax-parse stx
    [(_ (~and sub ((~datum type-identifier) . _)))
     (compile-type-identifier #'sub)]
    [(_ (~and sub ((~datum index-range) . _)))
     (compile-index-range #'sub)]
    [_ (displayln (list 'cst: stx))
       (error 'internal-error)]))

(define (compile-type-identifier stx)
  (syntax-parse stx
    [(_ id)
     (match (lookup #'id)
       [(type-info desc) desc]
       [_ 
        (def msg (~a "The type identifier " #'id " is unbound."))
        (raise-syntax-error 'compile-type-identifier msg stx)])]))

(define (initial-value-constructor type)
  (def maybe-desc (lookup type))
  (def desc (if maybe-desc maybe-desc type))
  (match-type desc
    [<true>    #'#t]
    [<false>   #'#f]
    [<integer> #'0]
    [<boolean> #'#t]
    [<char>    #'#\a]
    [else
     (cond
       [(array? desc)
        (def of   (array-of desc))
        (def from (array-from desc))
        (def to   (array-to desc))
        (match-type desc
          [(array (index-range 1 <integer>) <char>)
           ; this will generate a mutable string
           #`(make-string #,(+ to 1) #\space)]
          [(array (index-range 1 <star>) <char>)
           #`(make-string #,(+ 255 1) #\space)]
          [else
           (def ->index
             (match-type desc             
               [(array (index-range <integer> <integer>) <star>)
                #`(λ (x) (- x #,from))]
               [(array (index-range <char> <char>) <star>)
                (let ([from-index (char->integer from)])
                  #`(λ (x) (- (char->integer x) from-index)))]
               [else 
                (error 'non-ordinal-types)]))
           (def of-construction (initial-value-constructor of))
           #`(pascal:construct-array
              #,from #,to #,->index
              (λ () #,of-construction) '#,of)])]
       [else (error "type not defined" type)])]))

(define (string->type str)
  (def len (string-length str))
  (array (index-range 0 (+ len 1)) <char>))

;;; END OF TYPES

;;;
;;; SCOPE
;;;

; Scope is represented as list of frames
(define current-scope (make-parameter '()))
; Each frame is an association list from 
; symbols to info structures.
(define-struct frame (alist) #:mutable #:transparent)

(define (display-scope)
  (displayln
   (map (λ (al) (map car al))
        (map frame-alist (current-scope)))))

; There are 3 different info structures,
; namely for constants, types and variables.
(define-struct info ()                            #:transparent)
(define-struct (constant-info info) (value)       #:transparent)
(define-struct (type-info info)     (description) #:transparent)
(define-struct (var-info info)      (description) #:transparent)

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

(define (lookup-var sym)
  (def info (lookup sym))
  (if (var-info? info)
      info
      #f))

(define (free? sym)
  (not (lookup sym)))

(define (bound? sym)
  (not (not (lookup sym))))

(define (bound-to-constant? sym)
  (cond [(lookup sym) => constant-info?]
        [else #f]))

(define (bound-to-variable? sym)
  (cond [(lookup sym) => var-info?]
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

(define (add-to-scope! id info)
  ; id can be either a symbol or an identifier
  (def frame (top-frame))
  (def alist (frame-alist frame))
  (def sym id)
  (when (identifier? sym)
    (set! sym (syntax->datum sym)))
  (when (member sym (map car alist))
    (display (list id info alist))
    (raise-syntax-error 'duplicate-bindingX "duplicate binding" id))
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
;;; Provide
;;; 

; Top level functions and procedures are 
; exported from the resulting modeule.
; We must keep track of, whether we are
; on top level or not.

(define current-level (make-parameter 0))
(define (on-top-level?)
  (equal? (current-level) 0))
(define-syntax (with-increased-level stx)
  (syntax-parse stx
    [(_ expr ...)
     (syntax/loc stx
       (parameterize ([current-level (add1 (current-level))])
         expr ...))]))

(define current-provides (make-parameter '()))

(define (add-provide! id)
  (when (on-top-level?)
    (current-provides
     (cons id (current-provides)))))

;;;
;;; Compilation
;;;

(define (compile-program stx)
  (syntax-parse stx
    [((~datum program) "program" program-name ";" block ".")
     (parameterize ([current-scope (make-empty-scope)])
       ; Add base types
       (define base-env
         (list (list 'integer <integer>)
               (list 'boolean <boolean>)
               (list 'char    <char>)
               (list 'true    <boolean>)
               (list 'false   <boolean>)
               (list 'string  <string>)))
       (for ([b (in-list base-env)])
         (match b 
           [(list sym type) 
            (add-to-scope! sym (make-type-info type))]))
       ; Add standard lib
       (define stdlib 
         (list 
          (list 'chr  (function (list <integer>) <char>))
          (list 'succ (function (list <star>) <star>))
          (list 'prev (function (list <star>) <star>))
          (list 'ord  (function (list <star>) <integer>))
          (list 'low  (function (list (array <star> <star>)) <integer>))
          (list 'high (function (list (array <star> <star>)) <integer>))))
       (for ([b (in-list stdlib)])
         (match b 
           [(list sym type) 
            (add-to-scope! sym (var-info type))]))
       ; new scope - thus user functions can shadow stdlib
       (push-empty-frame!)
       (def compiled-block (compile-block #'block))
       (def provides
         (for/list ([id (in-list (current-provides))])
           (quasisyntax/loc stx
             (provide #,id))))
       (quasisyntax/loc stx
         (module program-name (lib "minipascal/runtime.rkt")
           (require (lib "minipascal/runtime.rkt"))
           #,@provides
           #,compiled-block)))]))

(define (compile-block stx)
  (syntax-parse stx
    [(_ constant-definition-part
        type-definition-part
        variable-declaration-part
        procedure-and-function-declaration-part
        statement-part)
     ; The compilation of the constant and type definitions 
     ; adds the defined constants and types to the current scope. 
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
       (begin
         #,@def-vars
         #,@decls
         #,stat-part))]
    [_ (error 'compile-block "internal error")]))

(define (compile-constant-definition-part stx)
  ; constant-definition-part : ["const" (constant-definition ";")+]
  (syntax-parse stx
    [(_) (void)]
    [(_ "const" (~seq const-def ";") ...)
     ; (push-empty-frame!) ; new scope for constants
     (for-each compile-constant-definition 
               (syntax->list #'(const-def ...)))]))

(define (compile-constant-definition stx)
  ; constant-definition : 
  ;    IDENTIFIER ("," IDENTIFIER)* "=" INTEGER-CONSTANT
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
     ; (push-empty-frame!) ; new scope for type definitions 
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
     (append-map compile-variable-declaration
                 (syntax->list #'(var-decl ...)))]))

(define (compile-variable-declaration stx)
  ; variable-declaration : IDENTIFIER ("," IDENTIFIER)* ":" type
  (syntax-parse stx
    [(_ id0 (~seq "," id) ... ":" type)
     (def desc (compile-type #'type))
     (for/list ([id (in-list (syntax->list #'(id0 id ...)))])
       (cond
         [(index-range? desc)
          (quasisyntax/loc stx 
            (define #,id '#,desc))]
         [else
          (def c (initial-value-constructor desc))
          (add-to-scope! id (var-info desc))
          (quasisyntax/loc stx 
            (define #,id #,c))]))]))

(define (compile-procedure-and-function-declaration-part stx)
  ; procedure-and-function-declaration-part : 
  ;   ((procedure-declaration ";") | (function-declaration ";"))*
  (syntax-parse stx
    [(_ (~seq decl ";") ...)
     (define (compile-decl d)
       (syntax-parse d 
         [((~datum procedure-declaration) . _)
          (compile-procedure-declaration d)]
         [((~datum function-declaration) . _)
          (compile-function-declaration d)]))
     (map compile-decl
          (syntax->list #'(decl ...)))]
    [_ (error)]))

(define (formals->ids stx)
  ; formal-parameters : IDENTIFIER ("," IDENTIFIER)* ":" type 
  (syntax-parse stx
    [(_ id0 (~seq "," id) ... ":" type)
     (syntax->list #'(id0 id ...))]))

(define (formals->description stx)
  ; formal-parameters : IDENTIFIER ("," IDENTIFIER)* ":" type 
  (syntax-parse stx
    [(_ id0 (~seq "," id) ... ":" type)
     (compile-type #'type)]))

(define (formals->descriptions stx)
  ; formal-parameters : IDENTIFIER ("," IDENTIFIER)* ":" type 
  (syntax-parse stx
    [(_ id0 (~seq "," id) ... ":" type)
     (def desc (compile-type #'type))
     (map (λ(_) desc) (formals->ids stx))]))

(define (compile-procedure-declaration stx)
  ; procedure-declaration 
  ;   : "procedure" IDENTIFIER 
  ;     ["(" formal-parameters (";" formal-parameters)* ")"] ";"
  ;     block  
  (syntax-parse stx
    [(_ "procedure" id ";" block)
     (def sym (syntax->datum #'id))
     (def info (var-info (procedure '())))
     (add-to-scope! sym info)
     (def compiled-block 
       (with-increased-level
        (compile-block #'block)))
     (with-syntax ([thunk (generate-temporary 
                           (~a (syntax->datum #'id) "-thunk"))])
       (add-provide! #'id)
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
       (def proc-desc (procedure formals-desc))
       (def alist 
         (map cons 
              (syntax->datum #'(formal0 ... formal ... ...))
              (map var-info formals-desc)))  
       (def sym (syntax->datum #'id))                      
       (def info (var-info proc-desc))
       (add-to-scope! sym info)
       (def compiled-block 
         (with-increased-level
          (with-extended-scope (make-frame alist)
            (compile-block #'block))))
       (add-provide! #'id)
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
       (def output-type (compile-type-identifier #'type-id))
       (def desc (function '() output-type))
       (def info (var-info desc))
       (add-to-scope! sym info)
       (def compiled-block 
         (with-increased-level
          (with-extended-scope (make-empty-frame)
            (compile-block #'block))))
       (add-provide! #'id)
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
     (with-syntax 
         ([(formal0 ...)      (formals->ids #'formals0)]
          [((formal ...) ...) (map formals->ids
                                   (syntax->list #'(formals ...)))]
          [result           (generate-temporary 
                             (~a (syntax->datum #'id) "-result"))])
       (def input-types
         (map formals->descriptions
              (syntax->list #'(formals0 formals ...))))
       (def output-type (compile-type-identifier #'type-id))
       (def sym (syntax->datum #'id))
       (def desc (function input-types output-type))
       (def info (var-info desc))
       (def formals-desc 
         (append*
          (map formals->descriptions
               (syntax->list #'(formals0 formals ...)))))
       (def alist 
         (map cons 
              (syntax->datum #'(formal0 ... formal ... ...))
              (map var-info formals-desc)))
       (add-to-scope! sym info)
       (def compiled-block 
         (with-increased-level
          (with-extended-scope (make-frame alist)           
            (compile-block #'block))))
       (add-provide! #'id)       
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
    [(_ (~seq "begin" "end"))
     (syntax/loc stx (begin))]
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
    [(_ (~and sub ((~datum structured-statement) . _)))
     (compile-structured-statement #'sub)]
    [(_ sub)
     (compile-simple-statement #'sub)]
    [_ (error)]))

(define (compile-simple-statement stx)
  ;simple-statement : assignment-statement | procedure-statement 
  ;       | read-statement | write-statement | writeln-statement
  ;       | application
  (syntax-parse stx
    [(_ (~and sub ((~datum assignment-statement) . _)))
     (compile-assignment-statement #'sub)]
    [(_ (~and sub ((~datum procedure-statement) . _)))
     (compile-procedure-statement #'sub)]
    [(_ (~and sub ((~datum read-statement) . _)))
     (compile-read-statement #'sub)]
    [(_ (~and sub ((~datum write-statement) . _)))
     (compile-write-statement #'sub)]
    [(_ (~and sub ((~datum writeln-statement) . _)))
     (compile-writeln-statement #'sub)]
    [(_ (~and sub ((~datum application) . _)))
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
     (def desc (var-info-description (lookup-var sym)))
     (match-type desc
       [<string> 
        (quasisyntax/loc stx
          (string-set! id #,e1 #,e2))]
       [else
        (quasisyntax/loc stx 
          (pascal:array-set! id #,e1 #,e2))])]
    [_ (error)]))

(define (compile-read-statement stx)
  ; read-statement : ("readln"|"read")  "(" IDENTIFIER ("," IDENTIFIER)* ")"
  ; Note: The usage of IDENTIFIER rather than  variable  in the grammar,
  ;       disallows read(a[3]). Remember a[3] is a legal variable/lvalue.   
  (syntax-parse stx 
    [(_ "read" "(" id0 (~seq "," id) ... ")")
     (def reader
       (match-type (var-info (lookup-var #'id0))
         [<integer> #'pascal:read-integer]
         [<char>    #'pascal:read-char]
         [else (error "read supports only integer and char")]))
     (quasisyntax/loc stx 
       (begin
         (set! id0 (#,reader))
         (set! id  (#,reader)) ...))]
    [(_ "readln" "(" id0 (~seq "," id) ... ")")
     (def ids (syntax->list #'(id0 id ...)))
     (for ([id (in-list ids)])
       (match-type (var-info-description (lookup-var id))
         [<string> 'ok]
         [else (error "readln support <string> only.")]))
     (quasisyntax/loc stx 
       (begin
         (set! id0 (read-line))
         (set! id  (read-line)) ...))]))

(define (compile-write-statement stx)
  ; write-statement :
  ;   "write"    "(" output-value ("," output-value)* ")"
  (syntax-parse stx 
    [(_ "write" "(" out0 (~seq "," out) ... ")")          
     (def outs (syntax->list #'(out0 out ...)))
     (def writes
       (for/list ([out (in-list outs)])
         (def (expr type) (compile-output-value out))
         (quasisyntax/loc stx
           (pascal:write #,expr))))
     (quasisyntax/loc stx
       (begin #,@writes))]))

(define (compile-writeln-statement stx)
  ; writeln-statement :
  ;   "writeln" ["(" output-value ("," output-value)* ")"]
  (syntax-parse stx 
    [(_ "writeln")
     (syntax/loc stx
       (newline))]
    [(_ "writeln" "(" out0 (~seq "," out) ... ")")     
     (def outs (syntax->list #'(out0 out ...)))
     (def writes
       (for/list ([out (in-list outs)])
         (def (expr type) (compile-output-value out))
         (quasisyntax/loc stx
           (pascal:writeln #,expr))))
     (quasisyntax/loc stx
       (begin #,@writes))]))

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
     (def info (lookup #'id))
     (unless (var-info? info)
       (raise-syntax-error 
        'compile-variable "expected an array, in" #'id))
     ; TODO: check that et matches index-type
     (def ae (var-info-description info))
     (def of (array-of ae))
     (match-type ae
       [<string> (values (quasisyntax/loc stx 
                           (string-ref id (sub1 #,e)))
                         of)]
       [(array (index-range <star> <star>) <star>)
        (values (quasisyntax/loc stx 
                  (pascal:array-ref id #,e))
                of)]
       [else (def msg "variable has wrong type")
             (raise-syntax-error 'compile-variable msg #'id)])]))

(define (compile-application stx)
  ; application : IDENTIFIER "(" expression ("," expression)* ")"
  (syntax-parse stx
    [(_ id "(" expr0 (~seq "," expr) ... ")")
     (def exprs (syntax->list #'(expr0 expr ...)))
     (def (es types) (map2 compile-expression exprs))
     (define info (lookup-var #'id))
     (unless info
       (raise-syntax-error 'application "name unbound" #'id))
     (def (inputs output)
       (match (var-info-description info) 
         [(function inputs output) (values inputs output)]
         [(procedure inputs)       (values inputs <void>)]
         [else (def msg "expected function or procedure")
               (raise-syntax-error 'application msg #'id)]))
     (unless (= (length es) (length inputs))
       (def msg (~a "expected " (length inputs) "arguments"))
       (raise-syntax-error 'application msg) #'stx)
     (for ([t types] [i inputs] [e es])
       (unless (subtype? t i)
         (def msg (~a "expected " (type->string i) 
                      ", got " (type->string t)))
         (raise-syntax-error (syntax->datum #'id) msg e)))
     (with-syntax ([(e0 e ...) es])
       (values (syntax/loc stx (id e0 e ...))
               output))]
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
             <boolean>)]
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
  (match-type type1
    [<integer>
     (syntax-parse stx
       [(_ "=")   #'=]
       [(_ "<>")  #'(λ (a b) (not (= a b)))]
       [(_ "<")   #'<]
       [(_ ">")   #'>]
       [(_ "<=")  #'<=]
       [(_ ">=")  #'>=])]
    [<char>
     (syntax-parse stx
       [(_ "=")   #'char=?]
       [(_ "<>")  #'(λ (a b) (not (char=? a b)))]
       [(_ "<")   #'char<?]
       [(_ ">")   #'char>?]
       [(_ "<=")  #'char<=?]
       [(_ ">=")  #'char>=?])]
    [<string>
     (syntax-parse stx
       [(_ "=")   #'pascal:string=]
       [(_ "<>")  #'pascal:string<>]
       [(_ "<")   #'pascal:string<]
       [(_ ">")   #'pascal:string>]
       [(_ "<=")  #'pascal:string<=]
       [(_ ">=")  #'pascal:string>=])]
    [else 
     (def msg "comparison of values with non-ordinal type")
     (raise-syntax-error 'rel-op msg stx)]))

(define (compile-simple-expression stx)
  ; simple-expression : [sign] term (adding-operator term)*
  ; sign : ["+" | "-"]
  (syntax-parse stx 
    [(_ term)
     (compile-term #'term)]
    [(_ (~and sub ((~datum sign) _)) term)
     (define s (compile-sign #'sub))
     (def (t type) (compile-term #'term))       
     (values (quasisyntax/loc stx (* #,s #,t)) type)]
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
  (match-type type1
    [<integer> (syntax-parse stx
                 [(_ "+")  #'+]
                 [(_ "-")  #'-])]
    [<boolean> (syntax-parse stx
                 [(_ "or") #'or])]
    [else
     (def msg 
       (~a "The operator is not defined for this type" type1))
     (raise-syntax-error msg 'add-op stx)]))

(define (compile-sign stx)
  ; sign :	["+" | "-"]
  (syntax-parse stx
    [(_ "-") -1]
    [_       +1]))

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
  ; (raise-unless-equal-types-error type1 type2 msg stx)
  (match-type type1
    [<integer> (syntax-parse stx
                 [(_ "*")    #'*]
                 [(_ "div")  #'quotient])]
    [<boolean> (syntax-parse stx
                 [(_ "and")  #'and])]
    [else 
     (raise-syntax-error 
      'mul-op "operator not defined for this type" stx)]))

(define (compile-factor stx)
  ; factor : application | variable | 
  ;          constant | "(" expression ")" | "not" factor 
  (syntax-parse stx
    [(_ sub)
     (syntax-parse #'sub
       [((~datum application) . _) (compile-application #'sub)]
       [((~datum variable) . _)    (compile-variable #'sub)]
       [((~datum constant) . _)    (compile-constant #'sub)])]
    [(_ "(" expr ")") 
     (compile-expression #'expr)]
    [(_ "not" factor) 
     (def (ef type) (compile-factor #'factor))
     (unless (type=? type <boolean>)
       (raise-syntax-error 
        '<not-factor> "expected (not <boolean-expr>)" stx))
     (values (quasisyntax/loc stx (not #,ef))
             <boolean>)]))

(define (compile-constant stx)
  ; constant : [sign] (INTEGER-CONSTANT | constant-identifier) 
  ;          | CHARACTER-CONSTANT | STRING-CONSTANT 
  (define (type-of val)
    (cond [(integer? val) <integer>]
          [(char? val)    <char>]
          [(boolean? val) <boolean>]))
  (syntax-parse stx
    [(_  (~optional sign) 
         (~and sub ((~datum constant-identifier) . _)))
     (def (id idt) (compile-constant-identifier #'sub))
     (cond
       [(attribute sign)
        (def s (compile-sign #'sign))
        (values (quasisyntax/loc stx
                  (* #,s #,id))
                idt)]       
       [else (values id idt)])]
    [(_  (~optional sign) int-const:integer)
     (def c (syntax->datum #'int-const))
     (cond
       [(attribute sign)
        (def s (compile-sign #'sign))
        (values (quasisyntax/loc stx
                  (* #,s #,c))
                (type-of c))]
       [else   (values #'int-const <integer>)])]
    [(_ val)
     (def datum (syntax->datum #'val))
     (cond
       [(string? datum) 
        (values (syntax/loc stx (string->array val))
                (array (index-range 1 (+ (string-length datum) 1)) 
                       <char>))]
       [else (values #'val (type-of (syntax->datum #'val)))])]
    [_ (error 'compile-constant "internal error")]))

(define (compile-constant-identifier stx)
  ; constant-identifier : IDENTIFIER
  (define (type-of val)
    (cond [(integer? val) <integer>]  ; make-type-info XXXX
          [(char? val)    <char>]
          [(boolean? val) <boolean>]))
  (syntax-parse stx
    [(_ (~datum true))
     (values #'#t <boolean>)]
    [(_ (~datum false))
     (values #'#f <boolean>)]
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
       [(var-info desc)
        (match desc
          [(function input output)
           (values #'id output)]
          [else (values #'id desc)])] ; error?
       [else
        (def msg (~a "unbound identifierXXX" (syntax->datum #'id)))
        (raise-syntax-error 'identifier msg #'id)])]
    [_ (error)]))

(define (compile-structured-statement stx)
  ; structured-statement : 
  ;   compound-statement | if-statement |  while-statement
  (syntax-parse stx 
    [(_ sub)
     (syntax-parse #'sub
       [((~datum compound-statement) . _)
        (compile-compound-statement #'sub)]
       [(~and sub ((~datum if-statement) . _))
        (compile-if-statement #'sub)]
       [(~and sub ((~datum while-statement) . _))
        (compile-while-statement #'sub)]
       [(~and sub ((~datum for-statement) . _))
        (compile-for-statement #'sub)]
       [_ (error)])]
    [_ (error)]))

(define (compile-if-statement stx)
  ; if-statement : "if" expression "then" statement | 
  ;                "if" expression "then" statement "else" statement
  (syntax-parse stx 
    [(_ "if" expr "then" stat)
     (def (e et) (compile-expression #'expr))
     (unless (equal? et <boolean>)
       (def msg "boolean expression expected")
       (raise-syntax-error 'if msg stx #'expr))
     (def s (compile-statement #'stat))
     (quasisyntax/loc stx (when #,e #,s))]
    [(_ "if" expr "then" stat1 "else" stat2)
     (def (e et) (compile-expression #'expr))
     (unless (equal? et <boolean>)
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
     (unless (equal? et <boolean>)
       (def msg "boolean expression expected")
       (raise-syntax-error 'while msg stx #'expr))
     (def s (compile-statement #'stat))
     (quasisyntax/loc stx
       (let while ()
         (when #,e #,s (while))))]))

(define (compile-for-statement stx)
  ; for-statement : "for" IDENTIFIER ":=" expression 
  ;                 ("to"|"downto") expression "do" statement
  (define (compile-for id expr1 expr2 stat next)
    (def (e1 e1t) (compile-expression expr1))
    (def (e2 e2t) (compile-expression expr2))
    (def s (compile-statement stat))     
    (quasisyntax/loc stx
      (let ([initial #,e1] [final #,e2])
        (let for ([#,id initial])
          #,s
          (unless (eqv? #,id final)
            (for (#,next #,id)))))))
  (syntax-parse stx
    [(_ "for" id ":=" expr1 "to" expr2 "do" stat)
     (compile-for #'id #'expr1 #'expr2 #'stat #'succ)]
    [(_ "for" id ":=" expr1 "downto" expr2 "do" stat)
     (compile-for #'id #'expr1 #'expr2 #'stat #'prev)]))
