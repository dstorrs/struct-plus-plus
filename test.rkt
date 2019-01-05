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
      (pattern (required?:boolean  (name:id contr:expr))))
    ;;
    (syntax-parse stx
      #:datum-literals (make-ctor-contract)
      [(make-ctor-contract (item:contract-spec ...+ predicate))
       (let-values
           ([(mandatory optional)
             (partition (syntax-parser [(flag _) (syntax-e #'flag)])
                        (map (syntax-parser [(flag (name contr))
                                             (quasitemplate (flag (#,(syntax->keyword #'name)
                                                                   contr)))])
                             (syntax->list #'(item ...))))])
         (with-syntax ((((_ (mand-kw mand-contract)) ...) mandatory)
                       (((_ (opt-kw  opt-contract)) ...)  optional))
           (template (->* ((?@ mand-kw mand-contract) ...)
                          ((?@ opt-kw opt-contract) ...)
                          predicate))))]))
  ;;
  (define-syntax-class field
    (pattern (~or id:id
                  [id:id (~optional (~seq field-contract:expr (~optional wrapper:expr)))])
             #:with required? #'#t
             #:with ctor-arg #`(#,(syntax->keyword #'id) id))
    (pattern [(id:id default-value:expr)
              (~optional (~seq field-contract:expr (~optional wrapper:expr)))]
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
             ((field.required? (field.id (?? field.field-contract any/c))) ... predicate))
            (struct-id ((?? field.wrapper identity) field.id) ...))
          ;
          (begin
            (make-functional-setter  struct-id field.id
                                     (?? field.field-contract any/c)
                                     (?? field.wrapper identity))
            ...)
          (begin (make-functional-updater struct-id field.id
                                          (?? field.field-contract any/c)
                                          (?? field.wrapper identity))
                 ...)))))))

(require          racket/string
)
(struct++ object (name) #:mutable)
(struct++ thing ([color thing/c]) #:transparent)
(struct++ building (type
                    [street-num]
                    [size integer?]
                    [street string? string-trim]
                    [(color 'white)]
                    [(floors 2) integer?]
                    [(windows 8) integer? add1]
                    ))
(object++   #:name 'ball)
(thing++    #:color "blue")
(building++ #:type 'private #:street-num 27 #:size 8 #:street "elm")