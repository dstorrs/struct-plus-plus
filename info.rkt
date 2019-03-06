#lang info

(define collection "struct-plus-plus")
(define version "3.3")
(define deps '("base"
               "handy"
               "syntax-classes-lib"))
(define scribblings '(("scribblings/struct-plus-plus.scrbl" ())))

(define test-omit-paths '("test_main.rkt"
                          "test_make_functional_setter.rkt"))
(define build-deps '("at-exp-lib"
                     "racket-doc"
                     "scribble-lib"))
