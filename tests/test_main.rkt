#lang racket

(require handy/test-more
         "../main.rkt")

(when #t
  (test-suite
   "struct++"



   (struct thing (name)         #:transparent)
   ; none
   ; default
   ; contract
   ; contract + wrapper
   ; default + contract
   ; default + contract + wrapper
   (struct++ ball  (owner
                    [(maker 'adidas)]
                    ; error: 7 8 are spliced into contract as arguments instead of or/c
                    [color (or/c 'red 'white 'black)]

                    ; works
                    [texture  (or/c 'rough 'smooth)]
                    [(weight 100) exact-positive-integer?]
                    [(shear-force 20) exact-positive-integer? number->string]
                    )
             #:transparent)

   (thing   'wilson)
   (thing++ #:name 'volleyball-friend)
   (ball++ #:owner 'bob
           #:color 'red
           #:texture 'rough
           #:weight 200
           #:shear-force 19)
   ))
