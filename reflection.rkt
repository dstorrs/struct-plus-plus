#lang racket

(provide (struct-out struct++-rule)
         (struct-out struct++-field)
         (struct-out struct++-info)
         prop:struct++ struct++? struct++-ref)

(struct struct++-rule  (name type) #:transparent)
(struct struct++-field (name accessor contract wrapper default) #:transparent)
(struct struct++-info
  (base-constructor constructor predicate fields rules converters) #:transparent)

(define-values (prop:struct++ struct++? struct++-ref)
  (make-struct-type-property 'struct++ 'can-impersonate))

