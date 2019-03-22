#lang racket

(provide (struct-out struct++-rule)
         (struct-out struct++-field)
         (struct-out struct++-info)
         struct++-info++
         struct++-field++
         struct++-rule++
         prop:struct++ struct++? struct++-ref)

(struct struct++-rule  (name type))
(struct struct++-field (name accessor contract wrapper default))
(struct struct++-info
  (base-constructor constructor predicate fields rules converters))

(define-values (prop:struct++ struct++? struct++-ref)
  (make-struct-type-property 'struct++ 'can-impersonate))

;;----------------------------------------------------------------------

(define/contract (struct++-rule++  #:name name #:type type)
  (-> #:name string? #:type (or/c 'at-least 'transform 'check)
      struct++-rule?)
  (struct++-rule name type))

;;----------------------------------------------------------------------

(define/contract (struct++-field++
                  #:name           name
                  #:accessor       accessor
                  #:contract       [field-contract any/c]
                  #:wrapper        [wrapper        identity]
                  #:default        [default        'no-default-given])
  (->* (#:name        symbol?
        #:accessor    (-> any/c any/c))
       (#:contract    contract?
        #:wrapper     procedure?
        #:default     any/c)
       struct++-field?)
  (struct++-field name accessor field-contract wrapper default))

;;----------------------------------------------------------------------

(define/contract (struct++-info++
                  #:base-constructor base-constructor
                  #:constructor constructor
                  #:predicate predicate
                  #:fields fields
                  #:rules rules
                  #:converters converters)
  (-> #:base-constructor    procedure?
      #:constructor         procedure?
      #:predicate           predicate/c
      #:fields              (listof struct++-field?)
      #:rules               (listof struct++-rule?)
      #:converters          (listof procedure?)
      struct++-info?)

  (struct++-info base-constructor constructor predicate fields rules converters))

;;----------------------------------------------------------------------
