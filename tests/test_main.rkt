#lang at-exp racket

(require handy/test-more
         "../main.rkt")

(expect-n-tests 8)

(when #t
  (test-suite
   "struct++ definitions"

   (struct thing (name)         #:transparent)
   (struct++ ball  (owner
                    [(maker 'adidas)]
                    [color (or/c 'red 'white 'black)]
                    [texture  (or/c 'rough 'smooth) symbol->string]
                    [(weight 100) exact-positive-integer?]
                    [(shear-force 20) exact-positive-integer? number->string]
                    )
             #:transparent)

   (dies (thunk (ball++)) "(ball++) dies")
   
   ; none
   (is (ball++ #:owner 'tom #:maker 'toms #:color 'red
               #:texture 'rough #:weight 77 #:shear-force 17)
       (ball 'tom 'toms 'red "rough" 77 "17")
       "ball++ with all params specified works")
   
   ; default
   (is (ball++ #:owner 'tom #:color 'red #:texture 'rough)
       (ball 'tom 'adidas 'red "rough" 100 "20")
       "ball++ can default")
   
   ; contract
   (throws (thunk (ball++ #:owner 'tom #:texture 'rough #:color 'notarealcolor))
           @pregexp{expected: \(or/c \(quote red\) \(quote white\) \(quote black\)\)}
           "ball++ contracts work")

   ; contract + wrapper
   (is (ball++ #:owner 'tom #:color 'red #:texture 'rough)
       (ball 'tom 'adidas 'red "rough" 100 "20")
       "wrapper around field 'shear-force' worked when field defaulted")

   (is (ball++ #:owner 'tom #:color 'red #:texture 'rough #:shear-force 99)
       (ball 'tom 'adidas 'red "rough" 100 "99")
       "wrapper around field 'shear-force' worked when field was set")


   ; wrapper that sorts the data, since that's probably a common use case
   (struct++ game ([(player-names '()) (listof non-empty-string?) (curryr sort string<?)])
             #:transparent)
   (is (game++ #:player-names '("fred" "bob" "zack"))
       (game++ #:player-names '("bob" "fred" "zack"))
       @~a{(game++ '("fred" "bob" "zack")) sorted its player names upon creation})
   
   ))

