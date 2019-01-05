#lang racket/base

(require (for-syntax racket/base
                     (only-in racket/list
                              partition
                              flatten
                              append-map)
                     racket/syntax
                     syntax/parse
                     syntax/parse/class/struct-id
                     syntax/parse/experimental/template)
         racket/contract/base
         racket/contract/region
         racket/function
         "make_functional_setter.rkt")

(provide struct++)

;; See scribblings/struct-plus-plus.scrbl for full details.  Cheat sheet:
;;
;; (struct++ type:id (field ...) struct-option ...)
;;
;;          field =  field-id
;;                | [field-id                   field-contract               ]
;;                | [field-id                   field-contract   wrapper]
;;                | [(field-id  default-value)                               ]
;;                | [(field-id  default-value)  field-contract               ]
;;                | [(field-id  default-value)  field-contract   wrapper]
;;
;; field-contract = contract?
;;
;; struct-option = as per the 'struct' builtin  (e.g. #:transparent, #:guard, etc)
;;
;;
;; Example of declaration with all the bells and whistles:
;;
;;   ; Declare a struct
;;   (struct++ pie (filling [(cook-temp 450) exact-positive-integer? F->C ]))
;;
;;   ; Create an instance, set an element, update an element.  Both
;;   ; set and update are functional changes, not mutators.
;;   (define p (pie++ #:filling 'berry))   ; the #:cook-temp keyword will default
;;   (set-pie-filling p 'cherry)                  
;;   (update-pie-filling p (lambda (x) 'unknown)) 
;;   (set-pie-cook-temp p 'invalid) ; EXCEPTION!  Field requires an exact-positive-integer?
;;

;;    syntax->keyword was lifted from:
;; http://www.greghendershott.com/2015/07/keyword-structs-revisited.html
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
             (partition (syntax-parser [(flag _) (syntax-e #'flag)])
                        (syntax->list #'(item ...)))])
         (with-syntax ((((_ (mand-kw mand-contract)) ...) mandatory)
                       (((_ (opt-kw  opt-contract)) ...)  optional))
           (template (->* ((?@ mand-kw mand-contract) ...)
                          ((?@ opt-kw opt-contract) ...)
                          predicate))))]))
  ;;
  (define-syntax-class field
    (pattern id:id
             #:with kw (syntax->keyword #'id) ; Ugly to repeat in each case, but clearer
             #:with ctor-arg #`(#,(syntax->keyword #'id) id)
             #:with field-contract #'any/c
             #:with required? #'#t
             #:with wrapper #'identity)
    (pattern [id:id field-contract:expr]
             #:with kw (syntax->keyword #'id)
             #:with ctor-arg #`(#,(syntax->keyword #'id) id)
             #:with required? #'#t
             #:with wrapper #'identity)
    (pattern [id:id field-contract:expr wrapper:expr]
             #:with kw (syntax->keyword #'id)
             #:with required? #'#t
             #:with ctor-arg #`(#,(syntax->keyword #'id) id))
    (pattern [(id:id default-value:expr)]
             #:with kw (syntax->keyword #'id)
             #:with required? #'#f
             #:with ctor-arg #`(#,(syntax->keyword #'id) [id default-value])
             #:with field-contract #'any/c
             #:with wrapper #'identity)
    (pattern [(id:id default-value:expr) field-contract:expr]
             #:with kw (syntax->keyword #'id)
             #:with required? #'#f
             #:with ctor-arg #`(#,(syntax->keyword #'id) [id default-value])
             #:with wrapper #'identity)
    (pattern [(id:id default-value:expr) field-contract:expr wrapper:expr]
             #:with kw (syntax->keyword #'id)
             #:with required? #'#f
             #:with ctor-arg #`(#,(syntax->keyword #'id) [id default-value])))
  ;;
  (syntax-parse stx
    ((struct++ struct-id:id (field:field ...) opt ...)
     ; A double ... (used below) flattens one level
     (with-syntax* ([ctor-id (format-id #'struct-id "~a++" #'struct-id)]
                    [((ctor-arg ...) ...) #'(field.ctor-arg ...)]
                    [predicate (format-id #'struct-id "~a?" #'struct-id)]
                    )
       (template
        (begin
          ;
          (struct struct-id (field.id ...) opt ...)
          ;
          (define/contract (ctor-id ctor-arg ... ...)
            (make-ctor-contract
             ((field.required? (field.kw field.field-contract)) ... predicate))
            (struct-id (field.wrapper field.id) ...))
          ;
          (make-functional-setter struct-id field.id field.field-contract field.wrapper) ...
          (make-functional-updater struct-id field.id field.field-contract field.wrapper) ...
          ))))))

(struct++ object (name) #:mutable)
(struct++ thing (color))

(object++ #:name 'ball)
(thing++ #:color 'blue)
