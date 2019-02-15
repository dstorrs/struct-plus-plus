#lang at-exp racket

(require handy/test-more
         "../main.rkt")

(expect-n-tests 18)

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

(when #t
  (test-suite
   "functional setters"

   (struct++ book ([title string?][pages exact-positive-integer?]) #:transparent)
   
   (define b (book++ #:title "title" #:pages 188))
   (is b
       (book "title" 188)
       "created book successfully")

   (is (set-book-title b "newtitle")
       (book "newtitle" 188)
       "successfully set the title")       

   (is b
       (book "title" 188)
       "it was a functional update, not a mutation")

   (is (set-book-pages b 200)
       (book "title" 200)
       "successfully set number of pages")

   (throws (thunk (set-book-pages b 'invalid))
           exn:fail:contract?
           "set-book-pages respects datatype")

   (throws (thunk (set-book-title b 'invalid))
           exn:fail:contract?
           "set-book-title respects datatype")
   ))

(when #t
  (test-suite
   "struct->hash"

   (struct++ person (name [(age 18) integer?]) #:transparent)
   (define bob (person++ #:name 'bob #:age 20))
   (is bob
       (person 'bob 20)
       "created bob"
       )
   (is (struct->hash person bob)
       (hash 'name 'bob
             'age 20)
       "converted to hash"
       )
   ))
