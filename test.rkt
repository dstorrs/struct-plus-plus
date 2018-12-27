#!/usr/bin/env racket
#lang racket

(require handy/utils
         (for-syntax syntax/parse/experimental/template
                     syntax/parse
                     racket/syntax
                     (only-in racket/list partition flatten append-map)
                     syntax/parse/class/struct-id  ; package: syntax-classes-lib
                     )
         "make_functional_setter.rkt"
         )

(provide struct++)

(define-syntax (struct++ stx)
  (define syntax->keyword (compose string->keyword symbol->string syntax->datum))

  (define-template-metafunction (make-ctor-contract stx)
    (define-syntax-class contract-spec
      (pattern (required?  (kw:keyword contr:expr))))
    ;;
    (syntax-parse stx
      #:datum-literals (make-ctor-contract)
      [(make-ctor-contract (item:contract-spec ...+ predicate))
       (let-values
           ([(optional mandatory)
             (partition (syntax-parser [(flag _) (syntax-e #'flag)])
                        (syntax->list #'(item ...)))])
         (with-syntax ((((_ (mand-kw mand-contract)) ...) mandatory)
                       (((_ (opt-kw  opt-contract)) ...)  optional))
           (template (->* ((?@ mand-kw mand-contract) ...)
                          ((?@ opt-kw opt-contract) ...)
                          predicate))))]))

  (define-syntax-class field
    (pattern (~or id:id
                  [id:id contract:expr (~optional wrapper:expr)]
                  [(id:id (~optional default:expr))]
                  [(id:id default:expr) contract:expr (~optional wrapper:expr)])
             #:with kw (syntax->keyword #'id)
             #:with ctor-arg (if (syntax->datum (template (?? default #f)))
                                 (quasitemplate  (#,(syntax->keyword #'id) [id default]))
                                 (quasitemplate  (#,(syntax->keyword #'id) id)))
             )
    )
  (syntax-parse stx
    #:datum-literals (struct++)
    [(struct++ struct-name (item:field ...) opt ...)
     (with-syntax* (
                    [ctor-name (format-id #'struct-name "~a++" #'struct-name)]
                    [predicate  (format-id #'struct-name "~a?" #'struct-name)]
                    [((ctor-arg ...) ...) #'(item.ctor-arg ...)]
                    )

       (quasitemplate (begin
                        (struct struct-name (item.id ...) opt ...)
                        (define/contract (ctor-name ctor-arg ... ...)
                          (make-ctor-contract
                           (((?? item.default #f) (item.kw (?? item.contract any/c))) ...
                            predicate))

                          (struct-name item.id ...)
                          )
                        )))
     ]))

(struct++ thing (name
                 [kingdom symbol?]
                 [order symbol? symbol-string->symbol]
                 [(color 'brown)]
                 [(furry? #t) boolean?]
                 [(species "unknown") string?]
                 )
          #:transparent)
(thing 'name 'king 'order 'color 7 'species)
(thing++ #:name 'bob
         #:kingdom 'king
         #:order 'order
         #:color 'red
         )




;; ;;    syntax->keyword was lifted from:
;; ;; http://www.greghendershott.com/2015/07/keyword-structs-revisited.html
;; (begin-for-syntax
;;   (define syntax->keyword (compose1 string->keyword symbol->string syntax->datum)))

;; (define-syntax (struct++ stx)
;;   (define-template-metafunction (make-ctor-contract stx)
;;     (define-syntax-class contract-spec
;;       (pattern (required?:boolean  (kw:keyword contr:expr))))
;;     ;;
;;     (syntax-parse stx
;;       #:datum-literals (make-ctor-contract)
;;       [(make-ctor-contract (item:contract-spec ...+ predicate))
;;        (let-values
;;            ([(mandatory optional)
;;              (partition (syntax-parser [(flag _) (syntax-e #'flag)])
;;                         (syntax->list #'(item ...)))])
;;          (with-syntax ((((_ (mand-kw mand-contract)) ...) mandatory)
;;                        (((_ (opt-kw  opt-contract)) ...)  optional))
;;            (template (->* ((?@ mand-kw mand-contract) ...)
;;                           ((?@ opt-kw opt-contract) ...)
;;                           predicate))))]))


;;   ;;
;;   (define-syntax-class field
;;     (pattern id:id
;;              #:with kw (syntax->keyword #'id) ; Ugly to repeat in each case, but clearer
;;              #:with ctor-arg #`(#,(syntax->keyword #'id) id)
;;              #:with field-contract #'any/c
;;              #:with required? #'#t
;;              #:with wrapper-func #'identity)
;;     (pattern [id:id field-contract:expr]
;;              #:with kw (syntax->keyword #'id)
;;              #:with ctor-arg #`(#,(syntax->keyword #'id) id)
;;              #:with required? #'#t
;;              #:with wrapper-func #'identity)
;;     (pattern [id:id field-contract:expr wrapper-func:expr]
;;              #:with kw (syntax->keyword #'id)
;;              #:with required? #'#t
;;              #:with ctor-arg #`(#,(syntax->keyword #'id) id))
;;     (pattern [(id:id default-value:expr)]
;;              #:with kw (syntax->keyword #'id)
;;              #:with required? #'#f
;;              #:with ctor-arg #`(#,(syntax->keyword #'id) [id default-value])
;;              #:with field-contract #'any/c
;;              #:with wrapper-func #'identity)
;;     (pattern [(id:id default-value:expr) field-contract:expr]
;;              #:with kw (syntax->keyword #'id)
;;              #:with required? #'#f
;;              #:with ctor-arg #`(#,(syntax->keyword #'id) [id default-value])
;;              #:with wrapper-func #'identity)
;;     (pattern [(id:id default-value:expr) field-contract:expr wrapper-func:expr]
;;              #:with kw (syntax->keyword #'id)
;;              #:with required? #'#f
;;              #:with ctor-arg #`(#,(syntax->keyword #'id) [id default-value])))
;;   ;;
;;   (syntax-parse stx
;;     [(struct++ struct-id:id
;;                (~or (field-name:id ...)

;;                     )
;;                opt ...)
;;      ; A double ... (used below) flattens one level
;;      (with-syntax* ([ctor-id (format-id #'struct-id "~a++" #'struct-id)]
;;                     [((ctor-arg ...) ...) #'(field.ctor-arg ...)]
;;                     [predicate (format-id #'struct-id "~a?" #'struct-id)]
;;                     )
;;        (template
;;         (begin
;;           ;
;;           (struct struct-id (field.id ...) opt ...)
;;           ;
;;           (define/contract (ctor-id ctor-arg ... ...)
;;             (make-ctor-contract
;;              ((field.required? (field.kw field.field-contract)) ... predicate))
;;             (struct-id (field.wrapper-func field.id) ...))
;;           ;
;;           (make-functional-setter struct-id field.id field.field-contract field.wrapper-func) ...
;;           (make-functional-updater struct-id field.id field.field-contract field.wrapper-func) ...
;;           )))]))
