#lang racket/base

(require racket/require
         handy/hash
         (multi-in racket (bool contract/base contract/region function match promise))
         (only-in racket/list count flatten)
         "reflection.rkt"

         (for-syntax racket/base
                     (only-in racket/list partition)
                     racket/syntax
                     syntax/parse
                     syntax/parse/class/struct-id
                     syntax/parse/experimental/template)
         )

(provide struct++
         struct->hash
         hash->struct++
         wrap-accessor
         (except-out (all-from-out "reflection.rkt")
                     struct++-info++
                     struct++-field++
                     struct++-rule++))

(define/contract (hash->struct++ struct-ctor h)
  (-> procedure? (hash/c symbol? any/c) any/c)
  (define sorted-keys (sort (hash-keys h) symbol<?))
  (keyword-apply struct-ctor
                 (map (compose string->keyword symbol->string) sorted-keys)
                 (map (curry hash-ref h) sorted-keys)
                 '()))

;;======================================================================


(begin-for-syntax

  ; Set up various syntax classes and metafunctions.  struct++ itself
  ; is defined below this begin-for-syntax


  ;;    syntax->keyword was lifted from:
  ;; http://www.greghendershott.com/2015/07/keyword-structs-revisited.html
  (define syntax->keyword (compose1 string->keyword symbol->string syntax->datum))

  ;;--------------------------------------------------

  (define-template-metafunction (make-dotted-accessor stx)
    (syntax-parse stx
      [(make-dotted-accessor #f _ _ _ _ _ _ _)
       #''()]
      [(make-dotted-accessor #t
                             struct-id ctor-id predicate
                             field-name field-contract wrapper
                             accessor-wrapper)
       (with-syntax ([accessor-name (format-id #'struct-id
                                               "~a-~a"
                                               #'struct-id
                                               #'field-name)]
                     [dotted-accessor-name (format-id #'struct-id
                                                      "~a.~a"
                                                      #'struct-id
                                                      #'field-name)])
         (template (define/contract (dotted-accessor-name the-struct)
                     (-> predicate field-contract)
                     (accessor-wrapper the-struct (accessor-name the-struct)))))]))

  ;;--------------------------------------------------

  (define-template-metafunction (make-functional-setter stx)
    (syntax-parse stx
      [(make-functional-setter #f _ _ _ _ _ _)
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
            (hash->struct++ ctor-id
                            (safe-hash-set (struct->hash struct-id instance)
                                           'field-name
                                           (wrapper val))))))]))

  ;;--------------------------------------------------

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
            (-> predicate (-> field-contract field-contract) predicate)
            (hash->struct++ ctor-id
                            (safe-hash-set (struct->hash struct-id instance)
                                           'field-name
                                           (wrapper (updater (getter instance))))))))]))

  ;;--------------------------------------------------

  (define-template-metafunction (make-deprecated-convert-for-function-name stx)
    (syntax-parse stx
      [(make-convert-for-function-name struct-id purpose)
       (format-id #'struct-id "~a/convert->~a" #'struct-id #'purpose)]))

  (define-template-metafunction (make-convert-for-function-name stx)
    (syntax-parse stx
      [(make-convert-for-function-name struct-id purpose)
       (format-id #'struct-id "~a->~a" #'struct-id #'purpose)]))

  ;;--------------------------------------------------

  (define-template-metafunction (make-convert-for-function stx)
    (syntax-parse stx
      [(make-convert-for-function  struct-id purpose predicate arg ...)
       (template
        (begin
          (define/contract ((make-convert-for-function-name struct-id purpose) instance)
            (-> predicate any)
            (hash-remap (struct->hash struct-id instance) (~@ arg ...)))
          (define (make-deprecated-convert-for-function-name struct-id purpose)
            (procedure-rename (make-convert-for-function-name struct-id purpose)
                              '(make-deprecated-convert-for-function-name struct-id purpose))
            )
          ))]))

  ;;--------------------------------------------------

  (define-template-metafunction (make-accessor-name stx)
    (syntax-parse stx
      [(make-accessor-name struct-name field-name)
       (format-id #'struct-name "~a-~a" #'struct-name #'field-name)]))

  ;;--------------------------------------------------

  (define-template-metafunction (make-field-struct stx)
    (syntax-parse stx
      [(make-field-struct struct-name field-name contract wrapper default)
       #'(struct++-field ('field-name
                          (make-accessor-name struct-name field-name)
                          contract
                          wrapper
                          default))]))

  ;;--------------------------------------------------

  (define-template-metafunction (make-convert-from-function stx)
    (syntax-parse stx
      [(make-convert-from-function struct-id:id name:id source-predicate:expr
                                   match-clause:expr (f:field ...))
       (with-syntax ([func-name (format-id #'struct-id "~a->~a++" #'name #'struct-id)]
                     [struct-predicate (format-id #'struct-id "~a?" #'struct-id)]
                     [ctor (format-id #'struct-id "~a++" #'struct-id)]
                     [((ctor-arg ...) ...) #'(f.ctor-arg ...)])
         (template
          (define/contract (func-name val)
            (-> source-predicate struct-predicate)
            (match val
              [match-clause (ctor ctor-arg ... ...)]))))]))

  ;;----------------------------------------------------------------------

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

  ;;--------------------------------------------------

  (define-syntax-class field
    (pattern (~or id:id
                  [id:id  (~optional (~seq (~seq #:wrap-accessor access-wrap:expr)
                                           (~optional contract:expr)
                                           (~optional wrap:expr)
                                           ))]
                  [id:id  (~optional (~seq contract:expr
                                           (~optional (~seq #:wrap-accessor access-wrap:expr))
                                           (~optional wrap:expr)))]
                  [id:id  (~optional (~seq contract:expr
                                           (~optional wrap:expr)
                                           (~optional (~seq #:wrap-accessor access-wrap:expr))))])
             #:with required? #'#t
             #:with field-contract (template (?? contract any/c))
             #:with wrapper (template (?? wrap identity))
             #:with accessor-wrapper (template (?? access-wrap
                                                   (λ (the-struct field-val) field-val)))
             #:with ctor-arg #`(#,(syntax->keyword #'id) id)
             #:with def #''no-default-given)

    (pattern (~or [(id:id default-value)
                   (~optional (~seq contract:expr
                                    (~optional wrap:expr)
                                    (~optional (~seq #:wrap-accessor access-wrap:expr))))]
                  [(id:id default-value)
                   (~optional (~seq (~seq #:wrap-accessor access-wrap:expr)
                                    (~seq contract:expr
                                          (~optional wrap:expr)))
                              )]
                  [(id:id default-value)
                   (~optional (~seq contract:expr
                                    (~optional (~seq #:wrap-accessor access-wrap:expr))
                                    (~optional wrap:expr))
                              )]

                  )
             #:with required? #'#f
             #:with field-contract (template (?? contract any/c))
             #:with wrapper (template (?? wrap identity))
             #:with accessor-wrapper (template (?? access-wrap
                                                   (λ (the-struct field-val) field-val)))
             #:with ctor-arg #`(#,(syntax->keyword #'id) [id default-value])
             #:with def (template  default-value)
             )
    )

  ;;--------------------------------------------------

  (define-splicing-syntax-class rule-clause
    (pattern
     (~seq (rule-name:str (~seq #:transform (target:id ...) (var:id ...) [code:expr ...+])))
     #:with type #''transform
     #:with result (template
                    (set!-values (target ...)
                                 ((lambda (var ...) code ...) var ...))))

    (pattern
     (~seq (rule-name:str (~seq #:transform target (var:id ...) [code:expr ...+])))
     #:with type #''transform
     #:with result (template (set! target ((lambda (var ...) code ...) var ...))))

    (pattern
     (~seq (rule-name:str (~seq #:check (var:id ...) [code:expr])))
     #:with type #''check
     #:with result (template
                    ((lambda (var ...)
                       (when (not code)
                         (let ([args (apply append
                                            (map list
                                                 (map symbol->string '(var ...))
                                                 (list var ...)))])
                           (apply raise-arguments-error
                                  (string->symbol (format "failed in struct++ rule named '~a' (type: check)" rule-name))
                                  "check failed"
                                  args))))
                     var ...)))
    (pattern
     (~seq (rule-name:str (~seq #:at-least
                                min-ok:exact-positive-integer
                                (~optional predicate:expr)
                                (var:id ...))))
     #:with type #''at-least
     #:with result (template
                    (let* ([pred (?? predicate (procedure-rename
                                                (negate false?)
                                                'true?))]
                           [num-valid (count pred (list var ...))])
                      (when (< num-valid min-ok )
                        (let ([args (apply append
                                           (map list
                                                (map symbol->string '(var ...))
                                                (list var ...)))])
                          (apply raise-arguments-error
                                 (string->symbol (format "failed in struct++ rule named '~a' (type: at-least)" rule-name))
                                 "too many invalid fields"
                                 "minimum allowed" min-ok
                                 "predicate" pred
                                 args)))))))

  ;;--------------------------------------------------

  (define-splicing-syntax-class converter
    (pattern (~seq #:convert-for (name (opt ...)))))

  ; e.g. #:convert-from (db-row (vector? (vector a b c) (a b c)))
  (define-splicing-syntax-class convert-from-clause
    (pattern (~seq #:convert-from (name:id (source-predicate:expr
                                            match-clause:expr
                                            (f:field ...+))))))

  ;;--------------------------------------------------

  (define-splicing-syntax-class make-setters-clause
    (pattern (~seq #:make-setters? yes?:boolean)))

  ;;--------------------------------------------------

  (define-splicing-syntax-class make-dotted-accessors-clause
    (pattern (~seq #:make-dotted-accessors? yes?:boolean)))

  )

;;--------------------------------------------------

(define-syntax (wrap-accessor stx)
  (syntax-parse stx
    [(_ func) #'(λ args (apply func (cdr args)))]))

;;--------------------------------------------------

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

;;======================================================================

(define-syntax (struct++ stx)
  (syntax-parse stx
    ([struct++ struct-id:id
               (field:field ...)
               (~optional ((~alt (~optional make-setters:make-setters-clause)
                                 (~optional make-dotted-accessors:make-dotted-accessors-clause)
                                 (~optional (~and  #:omit-reflection omit-reflection))
                                 c:converter
                                 cfrom:convert-from-clause
                                 (~seq #:rule r:rule-clause)
                                 (~seq #:rules (rule:rule-clause ...)))
                           ...))
               opt ...]
     #:with ctor-id   (format-id #'struct-id "~a++" #'struct-id)
     #:with predicate (format-id #'struct-id "~a?" #'struct-id)
     #:with reflectance-data (if (attribute omit-reflection)
                                 #'()
                                 #'(#:property prop:struct++
                                    (delay
                                      (struct++-info++
                                       #:base-constructor struct-id ; base struct constructor
                                       #:constructor ctor-id   ; struct-plus-plus constructor
                                       #:predicate predicate
                                       #:fields (list (struct++-field++
                                                       #:name     'field.id
                                                       #:accessor (make-accessor-name
                                                                   struct-id
                                                                   field.id)
                                                       #:contract field.field-contract
                                                       #:wrapper  field.wrapper
                                                       #:default  field.def)
                                                      ...)
                                       #:rules
                                       (append (list (~? (~@ (struct++-rule++
                                                              #:name r.rule-name
                                                              #:type r.type)
                                                             ...)))
                                               (list (~? (~@ (struct++-rule++
                                                              #:name rule.rule-name
                                                              #:type rule.type)
                                                             ... ...))))
                                       #:converters
                                       (list
                                        (~? (~@ (make-convert-for-function-name
                                                 struct-id
                                                 c.name)
                                                ...)))))))
     ; A double ... (used below) flattens one level
     (with-syntax* ([((ctor-arg ...) ...) #'(field.ctor-arg ...)])
       (quasitemplate
        (begin
          (struct struct-id (field.id ...) opt ... (~@ . reflectance-data))
          ;
          (define/contract (ctor-id ctor-arg ... ...)
            (make-ctor-contract
             ((field.required? (field.id field.field-contract)) ... predicate))

            (?? (?@ r.result ...))

            (?? (?@ rule.result ... ...))

            (struct-id (field.wrapper field.id) ...)
            )
          ;
          (?? (?@ (make-convert-for-function struct-id c.name predicate c.opt ...) ...))
          ;
          (?? (?@ (make-convert-from-function struct-id
                                              cfrom.name
                                              cfrom.source-predicate
                                              cfrom.match-clause
                                              (cfrom.f ...)) ...))

          ;
          (begin
            (make-dotted-accessor (?? make-dotted-accessors.yes? #t)
                                  struct-id ctor-id predicate
                                  field.id
                                  field.field-contract
                                  field.wrapper
                                  field.accessor-wrapper
                                  )
            ...)
          (begin
            (make-functional-setter (?? make-setters.yes? #t)
                                    struct-id ctor-id predicate
                                    field.id
                                    field.field-contract
                                    field.wrapper
                                    )
            ...)
          (begin
            (make-functional-updater (?? make-setters.yes? #t)
                                     struct-id ctor-id predicate
                                     field.id
                                     field.field-contract
                                     field.wrapper
                                     )
            ...)))))))

;;-----------------------------------------------------------------------
