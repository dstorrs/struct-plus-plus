#lang racket

(require handy/try handy/utils)

;;    syntax->keyword and struct/kw were lifted from:
;;
;; http://www.greghendershott.com/2015/07/keyword-structs-revisited.html

;;    (struct++ id maybe-super (field ...) struct-option ...)
;;
;; All as per standard 'struct', except that field is as follows:
;;
;;    field =   field-id
;;            | [(field-id default-value)]
;;            | [field-id field-contract]
;;            | [field-id field-contract wrapper-func]
;;            | [(field-id default-value) field-contract]
;;            | [(field-id default-value) field-contract wrapper-func]
;;
;; (struct food (name flavor-type))
;;
;; (struct++ pie (filling cook-temp))
;; (struct++ pie (filling [(cook-temp 450)]))
;; (struct++ pie (filling [cook-temp       exact-positive-integer?      ]))
;; (struct++ pie (filling [cook-temp       exact-positive-integer? F->C ]))
;; (struct++ pie (filling [(cook-temp 450) exact-positive-integer?      ]))
;; (struct++ pie (filling [(cook-temp 450) exact-positive-integer? F->C ]))
;; (struct++ pie food (filling flavor-type))
;; (struct++ pie
;;           food
;;           ([filling (or/c 'berry "berry" 'chocolate "chocolate" 'cheese "cheese")
;;                     symbol-string->string]
;;            [(cook-temp 450) exact-positive-integer?)])
;;           #:transparent
;;           #:guard (lambda (name flavor-type filling cook-temp type)
;;                     (values name flavor-type filling cook-temp type)))

;(struct field-info (name getter setter mutable? contract default parent) #:transparent)

(require (for-syntax syntax/parse/experimental/template
                     syntax/parse
                     racket/syntax
                     (only-in racket/list partition flatten)
                     )
         syntax/parse/experimental/template
         racket/syntax
         syntax/parse)

(begin-for-syntax
  (define syntax->keyword (compose1 string->keyword symbol->string syntax->datum)))

(define-syntax (struct++ stx)
  (define-template-metafunction (make-ctor-contract stx)
    (define-syntax-class contract-spec
      (pattern (required?:boolean  (kw:keyword contr:expr))))
    ;;
    (syntax-parse stx
      #:datum-literals (make-ctor-contract)
      [(make-ctor-contract (item:contract-spec ...+ predicate))
       (let-values
           ([(mandatory optional)
             (partition car
                        (syntax->datum #'(item ...)))])
         (define flat-mand (flatten (map cdr mandatory)))
         (define flat-opt  (flatten (map cdr optional)))
         (cond [(null? flat-opt) #`(-> #,@flat-mand predicate)]
               [else #`(->* (#,@flat-mand) (#,@flat-opt) predicate)]))]))
  ;;
  (define-syntax-class contract-spec
    (pattern (required?:boolean  (kw:keyword contr:expr))))
  ;;
  (define-syntax-class field
    (pattern id:id
             #:with kw (syntax->keyword #'id)
             #:with ctor-arg #`(#,(syntax->keyword #'id) id)
             #:with field-contract #'any/c
             #:with required? #'#t
             #:with wrapper-func #'identity)
    (pattern [id:id field-contract:expr]
             #:with kw (syntax->keyword #'id)
             #:with ctor-arg #`(#,(syntax->keyword #'id) id)
             #:with required? #'#t
             #:with wrapper-func #'identity)
    (pattern [id:id field-contract:expr wrapper-func:expr]
             #:with kw (syntax->keyword #'id)
             #:with required? #'#t
             #:with ctor-arg #`(#,(syntax->keyword #'id) id))
    (pattern [(id:id default-value:expr)]
             #:with kw (syntax->keyword #'id)
             #:with required? #'#f
             #:with ctor-arg #`(#,(syntax->keyword #'id) [id default-value])
             #:with field-contract #'any/c
             #:with wrapper-func #'identity)
    (pattern [(id:id default-value:expr) field-contract:expr]
             #:with kw (syntax->keyword #'id)
             #:with required? #'#f
             #:with ctor-arg #`(#,(syntax->keyword #'id) [id default-value])
             #:with wrapper-func #'identity)
    (pattern [(id:id default-value:expr) field-contract:expr wrapper-func:expr]
             #:with kw (syntax->keyword #'id)
             #:with required? #'#f
             #:with ctor-arg #`(#,(syntax->keyword #'id) [id default-value])))
  ;;
  (syntax-parse stx
    ((struct++ struct-id:id (~optional super-type:id) (field:field ...) opt ...)
     ; A double ... (used repeatedly below) flattens one level
     (with-syntax* ([ctor-id (format-id #'struct-id "~a++" #'struct-id)]
                    [((ctor-arg ...) ...) #'(field.ctor-arg ...)]
                    [predicate (format-id #'predicate "~a?" #'struct-id)]
                    )
       (template
        (begin
          (struct struct-id (field.id ...) opt ...)

          (define/contract (ctor-id ctor-arg ... ...)
            (make-ctor-contract
             ((field.required? (field.kw field.field-contract)) ... predicate))
            (struct-id (field.wrapper-func field.id) ...))))))))

;; (struct++ ball (type
;;                 ; field  default   contract           wrapper func
;;                 [(owner 'bob)]
;;                 [purchase-epoch   natural-number/c]
;;                 ;                [maker            symbol?            symbol->string]
;;                 [(color 9)        natural-number/c   add1]
;;                 ) #:transparent)

(print-syntax-width 100000)
;; (say "fail: " (exn:fail:contract?
;;                (defatalize (ball++ #:type 'soccer #:purchase-epoch 'a ))))
;; (ball++ #:type 'soccer #:purchase-epoch 193939 )


(struct++ thing (name) #:transparent)
(thing 'ball)
(thing++ #:name 'ball)

(struct++ sandwich thing (break filler) #:transparent)
