#lang racket

(require (for-syntax syntax/parse/experimental/template
                     syntax/parse
                     racket/syntax
                     (only-in racket/list partition flatten append-map)
                     syntax/parse/class/struct-id  ; package: syntax-classes-lib
                     )
         "make_functional_setter.rkt"
         )

(provide struct++)

;; See scribblings/struct-plus-plus.scrbl for full details.  Cheat sheet:
;;
;; (struct++ type:id (field ...) struct-option ...)
;;
;;          field =  field-id
;;                | [field-id                   field-contract               ]
;;                | [field-id                   field-contract   wrapper-func]
;;                | [(field-id  default-value)                               ]
;;                | [(field-id  default-value)  field-contract               ]
;;                | [(field-id  default-value)  field-contract   wrapper-func]
;;
;; field-contract = contract?
;;
;; struct-option = as per the 'struct' builtin  (e.g. #:transparent, #:guard, etc)
;;
;;
;; Example of declaration with all the bells and whistles:
;;   (struct++ pie (filling [(cook-temp 450) exact-positive-integer? F->C ]))
;;
;; Automatically creates the "pie++" keyword constructor, but also the
;; set-pie-filling and set-pie-cook-temp functional setters.




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
            (struct-id (field.wrapper-func field.id) ...))
          ;
          (make-functional-setter struct-id field.id field.field-contract field.wrapper-func) ...
          (make-functional-updater struct-id field.id field.field-contract field.wrapper-func) ...
          ))))))
