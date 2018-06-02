#lang racket

;;    syntax->keyword and struct/kw were lifted from:
;;
;; http://www.greghendershott.com/2015/07/keyword-structs-revisited.html

(require (for-syntax racket/syntax syntax/parse))

(provide struct/kw
         struct++
         )

;(struct field-info (name getter setter mutable? contract default parent) #:transparent)

(begin-for-syntax
  (define syntax->keyword (compose1 string->keyword symbol->string syntax->datum)))

(define-syntax (struct/kw stx)
  (define-syntax-class field
    (pattern id:id
             #:with ctor-arg #`(#,(syntax->keyword #'id) id))
    (pattern [id:id default:expr]
             #:with ctor-arg #`(#,(syntax->keyword #'id) [id default])))
  (syntax-parse stx
    [(_ struct-id:id (field:field ...) opt ...)
     (with-syntax ([ctor-id (format-id #'struct-id "~a/kw" #'struct-id)]
                   [((ctor-arg ...) ...) #'(field.ctor-arg ...)]) ; The double ... flattens one level
       #'(begin
           (struct struct-id (field.id ...) opt ...)
           (define (ctor-id ctor-arg ... ...) ; The double ... flattens one level
             (struct-id field.id ...))))]
    ;
    [(_ struct-id:id super-id:id (field:field ...) opt ...)
     (with-syntax ([ctor-id (format-id #'struct-id "~a/kw" #'struct-id)]
                   [((ctor-arg ...) ...) #'(field.ctor-arg ...)]) ; The double ... flattens one level
       #'(begin
           (struct struct-id super-id (field.id ...) opt ...)
           (define (ctor-id ctor-arg ... ...) ; The double ... flattens one level
             (struct-id field.id ...))))]
    ))

;;----------------------------------------------------------------------

(define-syntax (struct++ stx)
  (define-syntax-class field
    (pattern id:id)
    (pattern [id:id field-contract:expr])
    (pattern [id:id field-contract:expr wrapper-func:expr])
    (pattern [(id:id default-value:expr)])
    (pattern [(id:id default-value:expr) field-contract:expr])
    (pattern [(id:id default-value:expr) field-contract:expr wrapper-func:expr]))
  ;; (define-syntax-class declaration
  ;;   (pattern id:id (field:field ...) opt ...)
  ;;   (pattern id:id super-id:id (field:field ...) opt ...))
  (syntax-parse stx
    [(_ struct-id:id ignored ...)
     (with-syntax ([ctor-id (format-id #'struct-id "~a++" #'struct-id)]
                   [pred    (format-id #'struct-id "~a?" #'struct-id)])
       (syntax-parse stx
         [(_ struct-id:id (field:field ...) opt ...)
          #'(begin
              (struct struct-id (field.id ...) opt ...)
              (define (ctor-id x y) (struct-id 'x 'y)))]))]))


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
