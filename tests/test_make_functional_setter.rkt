#!/usr/bin/env racket
#lang at-exp racket

(require handy/test-more
         "../make_functional_setter.rkt")


(when #t
  (test-suite
   "basics"

   (struct book (title num-pages filepath) #:transparent)

   (define orig-args (list "Foundation" 297 (build-path "/foo/bar")))
   (define b (apply book  orig-args))

   (make-functional-setter book title)
   (make-functional-setter book num-pages natural-number/c)
   (make-functional-setter book filepath path-string? ~a)

   (define (verify-setter getter setter old-val setter-arg correct-result)
     (is (getter b)
         old-val
         (~a (object-name getter) " started off with the expected value"))

     (define extra-msg (if (equal? 'set-book-filepath (object-name setter))
                           " NOTE:  It was correctly transformed by the wrapper function"
                           ""))
     (is (getter (setter b setter-arg))
         correct-result
         (~a "after setter " (object-name setter) ", value is correct." extra-msg)))

   (for ([getter      (list book-title book-num-pages book-filepath)]
         [setter      (list set-book-title set-book-num-pages set-book-filepath)]
         [old-val     orig-args]
         [setter-arg  (list "Tom Sawyer" 18 (build-path "/tmp/foobar"))]
         [correct     (list "Tom Sawyer" 18 "/tmp/foobar")])
     
     (verify-setter getter setter old-val setter-arg correct))

   (throws (thunk (set-book-num-pages b 'invalid))
           #px"expected: natural-number/c"
           "(set-book-num-pages b 'invalid) violates the contract (i.e. exact-positive-integer?)")

   ))
