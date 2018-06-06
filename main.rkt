#lang racket

;;    syntax->keyword and struct/kw were lifted from:
;;
;; http://www.greghendershott.com/2015/07/keyword-structs-revisited.html

(require (for-syntax racket/syntax syntax/parse syntax/stx))

(provide struct++
         )

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

(begin-for-syntax
  (define syntax->keyword (compose1 string->keyword symbol->string syntax->datum)))

(define-syntax (struct++ stx)
  (define-syntax-class field
    (pattern id:id
             #:with ctor-arg #`(#,(syntax->keyword #'id) id)
             #:with field-contract #'any/c
             #:with has-optional #'#f
             #:with wrapper-func #'identity)
    (pattern [id:id field-contract:expr]
             #:with ctor-arg #`(#,(syntax->keyword #'id) id)
             #:with has-optional #'#f
             #:with wrapper-func #'identity)
    (pattern [id:id field-contract:expr wrapper-func:expr]
             #:with has-optional #'#f
             #:with ctor-arg #`(#,(syntax->keyword #'id) id))
    (pattern [(id:id default-value:expr)]
             #:with has-optional #'#t
             #:with ctor-arg #`(#,(syntax->keyword #'id) [id default-value])
             #:with field-contract #'any/c
             #:with wrapper-func #'identity)
    (pattern [(id:id default-value:expr) field-contract:expr]
             #:with has-optional #'#t
             #:with ctor-arg #`(#,(syntax->keyword #'id) [id default-value])
             #:with wrapper-func #'identity)
    (pattern [(id:id default-value:expr) field-contract:expr wrapper-func:expr]
             #:with has-optional #'#t
             #:with ctor-arg #`(#,(syntax->keyword #'id) [id default-value])))

  (syntax-parse stx
    [(struct++ struct-id:id (field:field ...))
     (with-syntax* ([ctor-id (format-id #'struct-id "~a++" #'struct-id)]
                    [((ctor-arg ...) ...) #'(field.ctor-arg ...)]) ; The double ... flattens one level

       ;; (with-syntax* ([predicate (format-id #'predicate "~a?" #'struct-id)]
       ;;                [arrow (if (foldl (lambda (x acc) (or x acc))
       ;;                                  #f
       ;;                                  (syntax->datum #'(list field.has-optional ...)))
       ;;                           #'->  ; at least one field has a default value
       ;;                           #'->)] ; no field has a default value
       ;;                [ctor-contract #'(arrow field.field-contract ... predicate)]
       ;;                )
       #'(begin
           (struct struct-id (field.id ...) #:transparent)
           (define (ctor-id ctor-arg ... ...) ; The double ... flattens one level
             (struct-id field.id ...))
           (println ctor-id)
           )
       )]))


(struct++ foo ([(x 17) integer?] [(y 9)] [(z 10)]))

foo++

(foo++ #:x 99)
