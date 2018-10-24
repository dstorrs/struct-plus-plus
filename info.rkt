#lang info

(define collection "struct-plus-plus")
(define version "1.0")
(define deps '("syntax-classes-lib"))
(define scribblings '(("scribblings/struct-plus-plus.scrbl")))

(define test-omit-paths '("test_main.rkt"
                          "test_make_functional_setter.rkt"))
