#lang racket/base

(require handy/hash
         handy/struct
         (for-syntax racket/base
                     (only-in racket/list
                              append-map
                              count
                              flatten
                              partition)
                     racket/syntax
                     syntax/parse
                     syntax/parse/class/struct-id
                     syntax/parse/experimental/template)
         racket/bool
         racket/contract/base
         racket/contract/region
         racket/function
         (only-in racket/list count flatten)
         )

(provide struct++ struct->hash)

(define-syntax struct->hash
  (syntax-parser
    [(_ s:struct-id instance:expr)
     (template
      (let* ([name-str (symbol->string  (object-name s.constructor-id))]
             [field-name (lambda (f)
                           (string->symbol
                            (regexp-replace (pregexp (string-append  name-str "-"))
                                            (symbol->string (object-name f))
                                            "")))]
             )
        (make-immutable-hash (list  (cons  (field-name s.accessor-id)
                                           (s.accessor-id instance)
                                           ) ...))))]))


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
  (define-template-metafunction (make-functional-setter stx)
    (syntax-parse stx
      [(make-functional-setter #f
                               struct-id ctor-id predicate
                               field-name field-contract wrapper)
       #''()]
      [(make-functional-setter #t
                               struct-id ctor-id predicate
                               field-name field-contract wrapper)
       (with-syntax ([setter-name (format-id #'struct-id
                                             "set-~a-~a"
                                             #'struct-id
                                             #'field-name)])
         (template
          (define/contract (setter-name instance val)
            (-> predicate field-contract predicate)
            (hash->struct/kw ctor-id
                             (safe-hash-set (struct->hash struct-id instance)
                                            'field-name
                                            (wrapper val))))))]))

  (define-template-metafunction (make-functional-updater stx )
    (syntax-parse stx
      [(make-functional-updater #f
                                struct-id ctor-id predicate
                                field-name field-contract wrapper)
       #''()
       ]
      [(make-functional-updater #t
                                struct-id ctor-id predicate
                                field-name field-contract wrapper)
       (with-syntax ([updater-name (format-id #'struct-id
                                              "update-~a-~a"
                                              #'struct-id
                                              #'field-name)]
                     [getter (format-id  #'struct-id
                                         "~a-~a"
                                         #'struct-id
                                         #'field-name)]
                     )
         (template
          (define/contract (updater-name instance updater)
            (-> predicate field-contract predicate)
            (hash->struct/kw ctor-id
                             (safe-hash-set (struct->hash struct-id instance)
                                            'field-name
                                            (wrapper (updater (getter instance))))))))]))

  (define-template-metafunction (make-converter-function stx)
    (syntax-parse stx
      [(make-converter-function  struct-id purpose predicate arg ...)
       (with-syntax ([funcname (format-id #'struct-id "~a/convert->~a" #'struct-id #'purpose)])
         (template
          (define/contract (funcname instance)
            (-> predicate any)
            (hash-remap (struct->hash struct-id instance) (~@ arg ...)))))]))

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
  (define-splicing-syntax-class rule
    (pattern
     (~seq #:rule (rule-name:str (~seq #:transform target (var:id ...) [code:expr ...+])))
     #:with result (template (set! target ((lambda (var ...) code ...) var ...))))

    (pattern
     (~seq #:rule (rule-name:str (~seq #:check (var:id ...) [code:expr])))
     #:with result (template
                    ((lambda (var ...)
                       (when (not code)
                         (let ([args (flatten (map list
                                                   (map symbol->string '(var ...))
                                                   (list var ...)))])
                           (apply raise-arguments-error
                                  (string->symbol rule-name)
                                  "check failed"
                                  args))))
                     var ...)))
    (pattern
     (~seq #:rule
           (rule-name:str (~seq #:at-least
                                min-ok:exact-positive-integer
                                (~optional predicate:expr)
                                (var:id ...))))
     #:with result (template (let ([num-valid (count (?? predicate (procedure-rename
                                                                    (negate false?)
                                                                    'true?))
                                                     (list var ...))])
                               (when (< num-valid min-ok )
                                 (let ([args (flatten (map list
                                                           (map symbol->string '(var ...))
                                                           (list var ...)))])
                                   (apply raise-arguments-error
                                          (string->symbol rule-name)
                                          "too many invalid fields"
                                          "minimum allowed" min-ok
                                          "predicate" predicate
                                          args)))))))
  (define-splicing-syntax-class converter
    (pattern (~seq #:convert-for (name (opt ...+)))))

  (define-splicing-syntax-class make-setters-clause
    (pattern (~seq #:make-setters? yes?:boolean)))

  (syntax-parse stx
    ((struct++ struct-id:id
               (field:field ...)
               (~optional ((~alt (~optional make-setters:make-setters-clause)
                                 c:converter
                                 r:rule)
                           ...))
               opt ...
               )
     ; A double ... (used below) flattens one level
     (with-syntax* ([ctor-id (format-id #'struct-id "~a++" #'struct-id)]
                    [((ctor-arg ...) ...) #'(field.ctor-arg ...)]
                    [predicate (format-id #'struct-id "~a?" #'struct-id)])
       (template
        (begin
          (struct struct-id (field.id ...) opt ...)
          ;
          (define/contract (ctor-id ctor-arg ... ...)
            (make-ctor-contract
             ((field.required? (field.id (?? field.field-contract any/c))) ... predicate))

            (?? (?@ r.result ...))

            (struct-id ((?? field.wrapper identity) field.id) ...))
          ;
          (?? (?@ (make-converter-function struct-id c.name predicate c.opt ...) ...))
          ;
          (begin
            (make-functional-setter (?? make-setters.yes? #t)
                                    struct-id ctor-id predicate
                                    field.id
                                    (?? field.field-contract any/c)
                                    (?? field.wrapper identity)
                                    )
            ...)
          (begin
            (make-functional-updater (?? make-setters.yes? #t)
                                     struct-id ctor-id predicate
                                     field.id
                                     (?? field.field-contract any/c)
                                     (?? field.wrapper identity)
                                     )
            ...
            )
          ))))))

;;-----------------------------------------------------------------------
