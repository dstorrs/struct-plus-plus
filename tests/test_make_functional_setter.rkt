#!/usr/bin/env racket
#lang at-exp racket

(require handy/test-more
         "../make_functional_setter.rkt")

(expect-n-tests 8)

(when #t
  (test-suite
   "basics"

   (struct book (title num-pages filepath) #:transparent)

   (define orig-args (list "Foundation" 297 (build-path "/foo/bar")))
   (define b (apply book  orig-args))

   (make-functional-setter book title)
   (make-functional-setter book num-pages natural-number/c)
   (make-functional-setter book filepath path-string? ~a)

   (make-functional-updater book title)
   (make-functional-updater book num-pages natural-number/c)
   (make-functional-updater book filepath path-string? ~a)

   (define (verify-setter getter setter old-val setter-arg correct-result)
     (is (getter b)
         old-val
         (~a (object-name getter) " started off with the expected value"))

     (define extra-msg (if (equal? 'set-book-filepath (object-name setter))
                           " NOTE:  It was correctly transformed by the wrapper function"
                           ""))
     (is (getter (setter b setter-arg))
         correct-result
         (~a "after " (object-name setter) ", value is correct." extra-msg)))

   (define (verify-updater getter updater old-val updater-arg correct-result)
     (is (getter b)
         old-val
         (~a (object-name getter) " started off with the expected value"))
     (is (getter (updater b updater-arg))
         correct-result
         (~a "after " (object-name updater) ", value is correct.")))   

   (for ([getter      (list book-title book-num-pages book-filepath)]
         [setter      (list set-book-title set-book-num-pages set-book-filepath)]
         [updater     (list update-book-title update-book-num-pages update-book-filepath)]
         [old-val     orig-args]
         [setter-arg  (list "Tom Sawyer" 18 (build-path "/tmp/foobar"))]
         [set-correct (list "Tom Sawyer" 18 "/tmp/foobar")]
         [updater-arg (list string-upcase add1 (compose car explode-path))]
         [update-correct     (list "FOUNDATION" 298 "/")]
         )
     
     (verify-setter  getter setter  old-val setter-arg set-correct)
     (verify-updater getter updater old-val updater-arg update-correct))

   (throws (thunk (set-book-num-pages b 'invalid))
           #px"expected: natural-number/c"
           "(set-book-num-pages b 'invalid) violates the contract (i.e. exact-positive-integer?)")

   ))
