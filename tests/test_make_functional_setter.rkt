#!/usr/bin/env racket
#lang at-exp racket

(require handy/utils)
(require handy/test-more
         "../make_functional_setter.rkt")


(when #t
  (test-suite
   "basics"

   (struct book (title num-pages filepath) #:transparent)

   (define orig-args (list "Foundation" 297 (build-path "/foo/bar")))
   (define b (apply book  orig-args))

   (say "b: " b)
   
   (make-functional-setter book title)
   (make-functional-setter book num-pages)
   (make-functional-setter book filepath)
   (say "result: " (set-book-title b 'tom))

   (define (verify-setter getter setter old-val setter-arg correct-result)
     (is (getter b)
         old-val
         (~a (object-name getter) " started off with the expected value"))
     
     (is (getter (setter b setter-arg))
         correct-result
         (~a "after setter " (object-name setter) ", value is correct")))

   (for ([getter      (list book-title book-num-pages book-filepath)]
         [setter      (list set-book-title set-book-num-pages set-book-filepath)]
         [old-val     orig-args]
         [setter-arg  (list "Tom Sawyer" 18 (build-path "/tmp/foobar"))]
         [correct     (list "Tom Sawyer" 18 (build-path "/tmp/foobar"))])
     
     (verify-setter getter setter old-val setter-arg correct))

   (throws (thunk (set-book-num-pages 'invalid))
           exn:fail:contract?
           "(set-book-num-pages 'invalid) violates the contract (i.e. exact-positive-integer?)")

   ))
