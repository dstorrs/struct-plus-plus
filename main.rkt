#lang racket

(require (for-syntax syntax/parse/experimental/template
                     syntax/parse
                     racket/syntax
                     (only-in racket/list partition flatten)
                     syntax/parse/class/struct-id  ; package: syntax/classes-lib
                     )
         syntax/parse/experimental/template
         racket/syntax
         syntax/parse)

(provide struct++)

;; This is an extended version of Racket's <struct>.
;;
;; (struct++ type:id maybe-super-type:id (field ...) struct-option ...)
;;
;;    maybe-super = 
;;                | super-id    
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
;;  struct-option = as per the 'struct' builtin 
;;
;;         IMPORTANT:
;;       Field options (#:mutable and #:auto) are not supported.
;;       Note the extra set of parens when setting a default!
;;
;; @@TODO:  
;;    - field options
;;    - functional setters
;;    - supertype fields appear in the kw signature
;;    - reflection
;;
;;   (struct food (name flavor-type))
;;  
;;      ;; Various possibilities.  Obviously you can't use them all in one file.
;;   (struct++ pie (filling cook-temp))           
;;   (struct++ pie (filling [(cook-temp 450)]))
;;   (struct++ pie (filling [cook-temp       exact-positive-integer?      ])) 
;;   (struct++ pie (filling [cook-temp       exact-positive-integer? F->C ]))
;;   (struct++ pie (filling [(cook-temp 450) exact-positive-integer?      ]))
;;   (struct++ pie (filling [(cook-temp 450) exact-positive-integer? F->C ]))
;;   (struct++ pie food (filling flavor-type))
;;   (struct++ pie
;;             food
;;             ([filling (or/c 'berry "berry" 'chocolate "chocolate" 'cheese "cheese")
;;                       symbol-string->string]
;;              [(cook-temp 450) exact-positive-integer?)])
;;             #:transparent
;;             #:guard
;;               (lambda (name flavor-type filling cook-temp type)    ; this is here just to
;;                 (values name flavor-type filling cook-temp type))) ; prove you can do it


;;    syntax->keyword was lifted from:
;; http://www.greghendershott.com/2015/07/keyword-structs-revisited.html
(begin-for-syntax
  (define syntax->keyword (compose1 string->keyword symbol->string syntax->datum)))

(define-syntax (struct++ stx)
  (define-template-metafunction (make-ctor-contract stx)
    (define-syntax-class contract-spec
      (pattern (required?:boolean  (kw:keyword contr:expr))))
    (syntax-parse stx
      #:datum-literals (make-ctor-contract)
      [(make-ctor-contract (item:contract-spec ...+ predicate))
       (let-values
           ([(mandatory optional)
             (partition car
                        (syntax->datum #'(item ...)))])

         (define flat-mand (if (null? mandatory) '() (foldl append '() (map cadr mandatory))))
         (define flat-opt  (if (null? optional)  '() (foldl append '() (map cadr optional ))))
         
         (cond [(null? flat-opt) #`(-> #,@flat-mand  predicate)]
               [else #`(->* (#,@flat-mand) (#,@flat-opt) predicate)]))]))
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
    ((struct++ struct-id:id (~optional super-type:id) (field:field ...) opt ...)
     ; A double ... (used below) flattens one level
     (with-syntax* ([ctor-id (format-id #'struct-id "~a++" #'struct-id)]
                    [((ctor-arg ...) ...) #'(field.ctor-arg ...)]
                    [predicate (format-id #'struct-id "~a?" #'struct-id)]
                    )
       (template
        (begin
          (struct struct-id (field.id ...) opt ...)

          (define/contract (ctor-id ctor-arg ... ...)
            (make-ctor-contract
             ((field.required? (field.kw field.field-contract)) ... predicate))
            (struct-id (field.wrapper-func field.id) ...))))))))

