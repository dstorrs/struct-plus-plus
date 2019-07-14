#lang at-exp racket

(require handy/test-more
         handy/utils
         handy/struct
         "../main.rkt")

(expect-n-tests 51)

(test-suite
 "arity-2 wrappers"

 (struct++ dumb
           ([db-id exact-positive-integer?]
            [age exact-positive-integer?
                 (procedure-rename (λ (val self) (+ val (dumb-db-id self)))
                                   'add-id-to-age)])
           (#:omit-reflection)
           #:transparent)

 (define bob (dumb++ #:db-id 7 #:age 12))
 (is bob (dumb 7 19) "creation went okay; wrapper ran")
 (is (set-dumb-db-id bob 111)
     (dumb 111 130)
     "setting the id field (normal wrapper) works, and age updates itself correctly")
 (is (set-dumb-age bob 33)
     (dumb 7 40)
     "setting age (arity-2 wrapper) works")
 )
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

(when #t
  (test-suite
   "struct++ definitions"

   (struct thing (name)         #:transparent)
   (struct++ ball  (owner
                    [(maker 'adidas)]
                    [color (or/c 'red 'white 'black)]
                    [texture  (or/c 'rough 'smooth) symbol->string]
                    [(weight-kg 100) exact-positive-integer?]
                    [(shear-force 20) exact-positive-integer? number->string]
                    )
             ()
             #:transparent)

   (dies (thunk (ball++)) "(ball++) dies")

   ; none
   (is (ball++ #:owner 'tom #:maker 'toms #:color 'red
               #:texture 'rough #:weight-kg 77 #:shear-force 17)
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

   (is (update-book-title b string-upcase)
       (book "TITLE" 188)
       "(update-book-title b string-upcase) works")

   (is (update-book-pages b add1)
       (book "title" 189)
       "(update-book-pages b add1) works")

   (throws (thunk (update-book-pages b (lambda (x) 'invalid)))
           exn:fail:contract?
           "(update-book-pages b (lambda (x) 'invalid)) threw due to violating field contract")
   ))

(when #t
  (test-suite
   "rules"

   (define eye-color/c  (apply or/c '(brown hazel blue green other)))
   (struct++ person ([name           (or/c symbol? non-empty-string?) symbol-string->string]
                     [age            positive?]
                     [eyes           eye-color/c]
                     [(height-m #f)  positive?]
                     [(weight-kg #f) positive?]
                     [(bmi #f)       positive?]
                     [(felonies 0)   positive-integer?]
                     [(notes "")]
                     )
             (
              #:rule ("bmi can be found"       #:at-least 2 (height-m weight-kg bmi))
              #:rule ("ensure height-m"        #:transform height-m (height-m weight-kg bmi) [(or height-m (sqrt (/ weight-kg bmi)))])
              #:rule ("ensure weight-kg"       #:transform weight-kg (height-m weight-kg bmi) [(or weight-kg (* (expt height-m 2) bmi))])
              #:rule ("ensure bmi"             #:transform bmi    (height-m weight-kg bmi) [(or bmi (/ 100 (expt height-m 2)))])
              #:rule ("lie about age"          #:transform age (age) [(define lie 18.0)
                                                                      (cond [(>= age 18) age]
                                                                            [else lie])])
              #:rule ("eligible-for-military?" #:check (age felonies) [(and (>= age 18)
                                                                            (= 0 felonies))])

              )
             #:transparent
             )
   (throws (thunk (person++ #:name 'bob
                            #:age 18
                            #:eyes 'brown
                            #:felonies 1
                            #:bmi 20
                            ))
           #px"bmi can be found"
           "need to supply at least two of bmi/height-m/weight-kg")

   (let ([correct (person "bob" 18 'brown 2 100 25 0 "")]
         [fmt    "bmi/height-m/weight-kg get populated if ~a is missing"]
         [base   (hash 'name "bob" 'age 18 'eyes 'brown 'height-m 2 'weight-kg 100 'bmi 25)]
         )
     (for ([key '(height-m weight-kg bmi)])
       (is (hash->struct/kw person++ (safe-hash-remove base key))
           correct
           (format fmt key))))

   (throws (thunk (person++ #:name 'bob
                            #:age 18
                            #:eyes 'brown
                            #:felonies 1
                            #:height-m 2
                            #:weight-kg 100
                            ))
           #px"eligible-for-military"
           "can't join the army if you've committed a felony"
           )
   (is (person++ #:name 'bob
                 #:age 16
                 #:eyes 'brown
                 #:height-m 2
                 #:weight-kg 100
                 )
       (person "bob" 18.0 'brown 2 100 25 0 "")
       "bob lies about his age to join the military"
       )
   ))

(when #t
  (test-suite
   "convert-for"

   (struct++ person
             (name age height eyes)
             (#:convert-for (db (#:remove '(height eyes) #:add (hash 'race 'unknown)))
              #:convert-for (json (#:remove '(name height) #:add (hash 'location "unknown")))
              #:convert-for (hash ())))

   (let ([sample (person 'bob 19 7 'brown)])
     (is (person/convert->db sample)
         (hash 'name 'bob 'age 19 'race 'unknown)
         "successfully converted to hash for DB")

     (is (person/convert->json sample)
         (hash 'age 19 'eyes 'brown 'location "unknown")
         "successfully converted to hash for JSON")

     (is (person/convert->hash sample)
         (hash 'name 'bob 'age 19 'height 7 'eyes 'brown)
         "It's okay to have a converter with an empty set of transforms; it will return a hash")
     )))

(when #t
  (test-suite
   "full deal"

   (define (get-min-age) 18.0)
   (struct++ recruit
             ([name (or/c symbol? non-empty-string?) ~a]
              [age positive?]
              [(eyes 'brown) (or/c 'brown 'black 'green 'blue 'hazel)]
              [(height-m #f) (between/c 0 3)]
              [(weight-kg #f) positive?]
	      [(bmi #f) positive?]
              [(felonies 0) exact-positive-integer?]
              )
             (#:rule ("bmi can be found" #:at-least  2           (height-m weight-kg bmi))
              #:rule ("ensure height-m"  #:transform   height-m  (height-m weight-kg bmi) [(or height-m (sqrt (/ weight-kg bmi)))])
              #:rule ("ensure weight-kg" #:transform   weight-kg (height-m weight-kg bmi) [(or weight-kg (* (expt height-m 2) bmi))])
              #:rule ("ensure bmi"       #:transform   bmi       (height-m weight-kg bmi) [(or bmi (/ 100 (expt height-m 2)))])
              #:rule ("lie about age"    #:transform   age       (age) [(define min-age (get-min-age))
                                                                        (cond [(>= age 18) age]
                                                                              [else min-age])])
              #:rule ("eligible-for-military?" #:check           (age felonies bmi) [(and (>= age 18)
                                                                                          (= 0 felonies)
                                                                                          (<= 25 bmi))])
              #:convert-for (db (#:remove '(bmi eyes)
                                 #:rename (hash 'height-m 'height 'weight-kg 'weight)))
              #:convert-for (alist (#:remove '(bmi eyes)
                                    #:rename (hash 'height-m 'height 'weight-kg 'weight)
                                    #:post hash->list))
              #:convert-for (json (#:action-order '(rename remove add overwrite)
                                   #:rename (hash 'height-m 'height 'weight-kg 'weight)
                                   #:remove '(felonies)
                                   #:add (hash 'vision "20/20")
                                   #:overwrite (hash 'hair "brown"
                                                     'eyes symbol->string
                                                     'shirt (thunk "t-shirt")
                                                     'age (lambda (age) (* 365 age))
                                                     'vision (lambda (h key val)
                                                               (if (> (hash-ref h 'age) 30)
                                                                   "20/15"
                                                                   val)))))
              )
             #:transparent)

   (define bob (recruit++ #:name      'bob
                          #:age       16
                          #:height-m  2
                          #:weight-kg 100))
   (is (recruit/convert->db bob)
       '#hash((name . "bob")
              (age . 18.0)
              (height . 2)
              (weight . 100)
              (felonies . 0))
       "(recruit/convert->db bob) works")

   (is (recruit/convert->alist bob)
       '((age . 18.0) (felonies . 0) (name . "bob") (weight . 100) (height . 2))
       "(recruit/convert->alist bob) works")

   (is (recruit/convert->json bob)
       '#hash((name . "bob")
              (age . 6570.0)
              (height . 2)
              (weight . 100)
              (bmi . 25)
              (eyes . "brown")
              (hair . "brown")
              (shirt . "t-shirt")
              (vision . "20/20"))
       "(recruit/convert->json bob) works"
       )))

(when #t
  (test-suite
   "reflection"

   ; need to create name/c and then reuse it because
   ; (equal? (or/c symbol? non-empty-string?) (or/c symbol? non-empty-string?)) => #f
   ; meaning that when we check the name field it won't compare equal
   (define name/c (or/c symbol? non-empty-string?))

   (struct++ person ([name           name/c symbol-string->string]
                     [age            positive?]
                     [(height-m #f)  positive?]
                     [(weight-kg #f) positive?]
                     [(bmi #f)       positive?]
                     [(felonies 0)   positive-integer?]
                     [(notes "")]
                     )
             (
              #:rule ("bmi can be found"       #:at-least 2 (height-m weight-kg bmi))
              #:rule ("ensure height-m"        #:transform height-m (height-m weight-kg bmi) [(or height-m (sqrt (/ weight-kg bmi)))])
              #:rule ("eligible-for-military?" #:check (age felonies) [(and (>= age 18)
                                                                            (= 0 felonies))])
              #:convert-for (db (#:add (hash 'baz "jaz")))
              #:convert-for (json (#:add (hash 'foo "bar")))
              )
             #:transparent
             )

   (define s (person++ #:name 'tom
                       #:age 19
                       #:weight-kg 100
                       #:bmi 24))
   (define ref (force (struct++-ref s)))
   (is-type ref  struct++-info?  "got a struct++-info")

   (let ()
     (match-define
       (struct* struct++-info
                ([base-constructor base-constructor]
                 [constructor constructor]
                 [predicate predicate]
                 [fields (list (struct* struct++-field ([name fld-names]
                                                        [accessor accessors]
                                                        [contract field-contracts]
                                                        [wrapper wrappers]
                                                        [default defaults]))
                               ...)]
                 [rules (list (struct* struct++-rule ([name rule-names]
                                                      [type type]))
                              ...)]
                 [converters converters]))
       ref)

     (is (thunk  base-constructor) person "base ctor correct")
     (is (thunk  constructor) person++ "ctor correct")
     (is (thunk  predicate) person? "predicate correct")
     (is fld-names '(name age height-m weight-kg bmi felonies notes) "field names correct")
     (is accessors (list person-name
                         person-age
                         person-height-m
                         person-weight-kg
                         person-bmi
                         person-felonies
                         person-notes)
         "accessors are correct")
     (is field-contracts
         (list name/c positive? positive? positive? positive? positive-integer? any/c)
         "contracts are correct")

     ; #<identity> and #<identity> are not equal?, so compare by name
     (is (map object-name wrappers)
         (map object-name (list symbol-string->string identity identity
                                identity identity identity identity))
         "wrappers are correct")

     (is defaults
         (list 'no-default-given 'no-default-given #f #f #f 0 "")
         "defaults are correct"))
   ))

(test-suite
 "omit-reflection"

 (struct++ zazzle (x) (#:omit-reflection) #:transparent)

 (throws (thunk  (struct++-ref (zazzle 9)))
         #px"expected: struct\\+\\+"
         "#:omit-reflection works"))


(test-suite
 "convert-from"

 (struct++ public-key ([data bytes?]) #:transparent)

 (struct++ person
           ([id exact-positive-integer?]
            [name non-empty-string?]
            [(keys '()) list?]
            )
           (#:convert-from (vector (vector?
                                    (vector id
                                            (app vector->list keys)
                                            name)
                                    (id keys name)))
            #:convert-from (list (list?
                                  (list id
                                        (app (compose
                                              (curry map public-key)
                                              (curryr apply '()))
                                             keys)
                                        (app ~a name)
                                        )
                                  (id keys name)))
            )
           #:transparent)

 (is (vector->person++ (vector 7 (vector #"foo" #"bar") "bob"))
     (person 7 "bob" (list  #"foo" #"bar"))
     @~a{(vector->person++ (vector 7 (vector #"foo" #"bar") "bob")) worked}
     )

 (is (list->person++ (list 7 (thunk (list #"foo" #"bar")) 'fred))
     (person 7 "fred" (list (public-key #"foo") (public-key #"bar")))
     @~a{(list->person++ (list 7 (thunk (list #"foo" #"bar")) 'fred)) works}))
