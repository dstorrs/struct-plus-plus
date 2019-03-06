#lang scribble/manual

@(require (for-label racket))

@title{struct++}
@author{David K. Storrs}

@defmodule[struct-plus-plus]

@section{Introduction}

@racketmodname{struct-plus-plus} provides extended syntax for creating structs.  It does not support field options (#:auto and #:mutable for individual fields), although those will be added.  Aside from that, it's a drop-in replacement for the normal @racket{struct} form. So long as your struct does not use field options, you can literally just change @racket{struct} to @racket{struct++} and your code will continue to work as before but you will now have a keyword constructor and functional setters for all the fields.

@racketmodname{struct-plus-plus} offers the following benefits over normal @racket{struct}:

@itemlist[
          @item{keyword constructor}
          @item{functional setter for each field}
          @item{(optional) distinct defaults for individual fields}
          @item{(optional) contracts for each field}
          @item{(optional) wrapper functions for each field}
          @item{(optional) dependency checking between fields}
          @item{(optional) declarative syntax for business logic rules}
          @item{(optional) declarative syntax for converting the structures to arbitrary other values}
          ]

@section{Design Goal}

The intent is to move structs from being dumb data repositories into being data models in the sense of MVC programming.  They should contain data that is internally consistent and valid according to business rules.  This centralizes the checks that would otherwise need to be done at the point of use.

@section{Synopsis}

Let's make a struct that describes a person who wants to join the military.

@racketblock[

   (define (get-min-age) 18.0)
   (struct++ lying-recruit
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
              #:convert-for (db (#:remove '(eyes bmi)
                                 #:rename (hash 'height-m 'height 'weight-kg 'weight)))
              #:convert-for (alist (#:remove '(bmi eyes)
                                    #:rename (hash 'height-m 'height 'weight-kg 'weight)
                                    #:post hash->list))
              #:convert-for (json (#:action-order '(rename remove add overwrite)
                                   #:rename (hash 'height-m 'height 'weight-kg 'weight)
                                   #:remove '(felonies)
                                   #:add (hash 'vision "20/20")
                                   #:overwrite (hash 'hair "brown"
                                                     'shirt (thunk "t-shirt")
                                                     'age (lambda (age) (* 365 age))
                                                     'vision (lambda (h key val)
                                                               (if (> (hash-ref h 'age) 30)
                                                                   "20/15"
                                                                   val)))))
              )
            #:transparent)
]


@verbatim{
 > (define bob (recruit++ #:name      'bob
                          #:age       16
                          #:height-m  2
                          #:weight-kg 100))
 > bob
 (recruit "bob" 16 2 100 #f 0 "")

 > (set-recruit-age bob 18)
 (recruit "bob" 18 2 100 #f 0 "")

 > (recruit++ #:name 'tom)
 ; application: required keyword argument not supplied
 ; procedure: recruit++
 ; required keyword: #:age

 > (recruit/convert->db bob)
 '#hash((name . "bob")
        (age . 18.0)
        (height . 2)
        (weight . 100)
        (felonies . 0))

 > (recruit/convert->alist bob)
 '((age . 18.0) (name . "bob") (felonies . 0) (weight . 100) (height . 2))
 
 > (recruit/convert->json bob)
 '#hash((name . "bob")
        (age . 6570.0)
        (height . 2)
        (weight . 100)
        (bmi . 25)
        (eyes . "brown")
        (hair . "brown")
        (shirt . "t-shirt")
        (vision . "20/20"))
}

Note about constructors:

There are two constructors for the @racket{recruit} datatype: @racket{recruit} and @racket{recruit++}.  @racket{struct++} will generate both of these while Racket's builtin @racket{struct} generates only @racket{recruit}. Only @racket{recruit++} has keywords, contracts, etc.  Using the default constructor will allow you to create structures that are invalid under the field contracts. See below:

@verbatim{
 > (recruit 'tom -3 99 10000 0.2 -27 'note)
 (recruit 'tom -3 99 10000 0.2 -27 'note)

 > (recruit++ #:name 'tom #:age -3 #:height-m 99 #:weight-kg 10000 #:bmi 0.2 #:felonies -27 #:notes 'note)
 recruit++: contract violation
   expected: positive?
   given: -3
   in: the #:age argument of
       (->*
        (#:age
         positive?
         #:name
         (or/c symbol? non-empty-string?))
        (#:bmi
         positive?
         #:felonies
         natural?
         #:height-m
         positive?
         #:notes
         any/c
         #:weight-kg
         positive?)
        recruit?)
   contract from: (function recruit++)
   blaming: top-level
    (assuming the contract is correct)
}

@section{Syntax}

@verbatim{
  (struct++ type:id (field ...) spp-options struct-option ...)

   field :    field-id
            | (field-id                   field-contract          )
            | (field-id                   field-contract   wrapper)
            | ([field-id  default-value]                          )
            | ([field-id  default-value]  field-contract          )
            | ([field-id  default-value]  field-contract   wrapper)
   
   field-contract : contract? = any/c

   spp-options :
                 | (spp-option ...+)

   spp-option  :   #:make-setters? boolean? = #t
                 | rule
                 | converter
   
   rule :   #:rule (rule-name #:at-least N maybe-pred (field-id ...+))
          | #:rule (rule-name #:check (field-id ...+) [code])
          | #:rule (rule-name #:transform field-id (field-id ...+) (code ...+))

   rule-name :  string?

   N  : exact-positive-integer?

   maybe-pred :
                | (-> any/c boolean?) = (negate false?)

   code      : <expression> 

   converter :    #:to-hash (convert-name (hash-option ...+))

   convert-name : id
   
   hash-option :   #:include      (list key ...+)
                 | #:remove       (list key ...+)
                 | #:overwrite    (hash [key value-generator] ...)
                 | #:add          (hash [key value-generator] ...)
                 | #:rename       (hash [key value-generator] ...)
                 | #:default      (hash [key value-generator] ...)
                 | #:post         (-> hash? any) = identity
                 | #:action-order (list (or/c 'include 'remove 'overwrite
                                         'add 'rename 'default) ...+)
                        = '(include remove overwrite add rename default)

   key             : any/c
   
   value-generator :   (not/c procedure?)            ; use as-is
                     | <procedure of arity != 0,1,3> ; use as-is
                     | (-> any/c)                    ; call w/no args
                     | (-> any/c any/c)              ; w/current value
                     | (-> hash/c any/c any/c any/c) ; w/hash,key,current value
    
   struct-option : As per the 'struct' builtin. (#:transparent, #:guard, etc)
}

Note that supertypes are not supported as of this writing.  The setter functions generated by @racketmodname{struct-plus-plus} make use of struct-copy, which doesn't work reliably when dealing with supertypes.  See Alexis King's module @racketmodname{struct-update} for more details.

@section{Setters and Updaters}
      
When @racket{#:make-setters?} is missing or has the value #t, it will generate a functional setter and updater for each field. When it is defined and has the value #f the setters and updaters will not be generated.

Given a struct of type @racket{recruit} with a field @racket{age}, the name of the setter will be @racket{set-recruit-age} and the updater will be @racket{update-recruit-age}.  Setters receive a value, updaters receive a one-argument function that receives the current value and returns the new value.

The setters and updaters are not exported.  You will need to put them in the @racket{provide} line manually.

@verbatim{
          (struct++ person (name))
          ; set-person-name and update-person-name ARE defined
	  
          (struct++ person (name) (#:make-setters? #t))
          ; set-person-name and update-person-name ARE defined
	  
          (struct++ person (name) (#:make-setters? #f))
          ; set-person-name and update-person-name are NOT defined

	  > (set-person-name (person 'bob) 'tom)
	  (person 'tom)
	  
	  > (update-person-name (person 'bob) (lambda (current) (~a current "'s son")))
	  (person "bob's son")
	  
          }
	  
@section{Rules}

Structs always have business logic associated with them -- that's the entire point.  Much of that can be embodied as contracts or wrapper functions, but if you want to enforce requirements between fields then you need rules. No one wants to code all that stuff manually, so let's have some delicious syntactic sugar that lets us create them declaratively.

Let's go back to our example of the recruit.  In order to be accepted into the military, you must be at least 18 years of age, have no felonies on your record, and be reasonably fit (BMI no more than 25).

Bob @italic{really} wants to join the military, and he's willing to lie about his age to do that.

@racketblock[

   (define (get-min-age) 18.0)
   (struct++ lying-recruit
             ([name (or/c symbol? non-empty-string?) ~a]
              [age positive?]
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
                                                                                           (<= 25 bmi))]))
            #:transparent)
]

Note: In the "ensure height-m" rule it is not necessary to check that you have both weight-kg and bmi because the "bmi can be found" rule has already established that.  The same applies to the "ensure weight-kg" and "ensure bmi" rules.

@verbatim{

  > (define bob (lying-recruit++ #:name      'bob
                                 #:age       16
                                 #:height-m  2
                                 #:weight-kg 100))

  > bob
  (lying-recruit "bob" 18.0 2 100 25 0)
}

Note that Bob's name has been changed from a symbol to a string as per Army regulation 162.11a, his age has magically changed from 16 to 18.0, and his BMI has been calculated.  Suppose we try to invalidate these constraints?

@verbatim{
  > (set-lying-recruit-age bob 3)
  (lying-recruit "bob" 3 2 100 25 0)
}

Oops, the setters don't respect the rules!  That's still TODO and will be coming out in the next release.


@section{Converters}

When marshalling a struct for writing to a database, a file, etc, it is useful to turn it into different data structure, usually but not always a hash.  Converters will change the struct into a hash, then pass the hash to the @racket{hash-remap} function in @racketmodname{handy}, allowing you to return anything you want.  See the handy/hash docs for details, but a quick summary:

@itemlist[
          @item{#:remove <list> : delete the keys in the list from the hash}
          @item{#:overwrite <hash> : change the values of existing keys or add missing ones}
          @item{#:add <hash> : add one or more keys to the hash, die if they were already there}
          @item{#:rename <hash> : change the names of one or more keys}
          @item{#:default <hash> : if a key is there, leave it alone.  If not, add it}
          @item{value-is-default? : change the behavior of #:default so that it sets the value of missing keys or keys that match a specified predicate}
          @item{action-order : specify in what order to apply the above options}
          @item{post : run the resulting hash through a function that returns anything you want}
          ]

Note that @racket{#:overwrite} provides special behavior for values that are procedures with arity 0, 1, or 3.  The values used are the result of calling the procedure with no args (arity 0); the current value (arity 1); or hash, key, current value (arity 3).
               

@section{Warnings, Notes, and TODOs}

Some of these were already mentioned above:

@itemlist[
  @item{WARNING, TODO.  As of this writing, the functional setters do not respect the declarative business rules.}
  @item{@racket{recruit++} checks contracts and rules etc.  @racket{recruit} does not}
  @item{@racket{#:transform} rules take 1+ expressions in their code segment.  The return value becomes the new value of the target}
  @item{@racket{#:check} rules take exactly one expression in their code segment.  If the returned value is true then the rule passed, and if it's @racket{#f} then the rule calls @racket{raise-arguments-error}}
  @item{Rules are processed in order. Changes made by a @racket{#:transform} rule will be seen by later rules}
    @item{None of the generated functions (@racket{struct-name++}, @racket{set-struct-name-field-name}, etc) are exported.  You'll need to list them in your @racket{provide} line manually}
      @item{Note:  As with any function in Racket, default values are not sent through the contract.  Therefore, if you declare a field such as (e.g.) @racket{[(userid #f) integer?]} but you don't pass a value to it during construction then you will have an invalid value (@racket{#f} in a slot that requires an integer).  Default values ARE sent through wrapper functions, so be sure to take that into account -- if you have a default value of @racket{#f} and a wrapper function of @racket{add1} then you are setting yourself up for failure.}
      @item{See the @racket{hash-remap} function in the @racketmodname{handy} module for details on what the @racket{#:to-hash} converter options mean}
    @item{TODO:  Add more complex variations of @racket{#:at-least}, such as:  @racket{#:at-least 1 (person-id (person-name department-id))}}
    @item{TODO:  Add an option to enable easy runtime reflection by storing the elements of the transformer binding into a property at creation time}
  @item{TODO: add a keyword that will control generation of mutation setters that respect contracts and rules. (Obviously, only if you've made your struct @racket{#:mutable})}
    
]

@section{Thanks}

The words 'shoulders of giants' apply here.  I would like to offer great thanks to:

@itemlist[
          @item{Greg Hendershott, for his "Fear of Macros" essay}
               @item{Alexis King (aka lexi-lambda), for teaching me a lot about macros over email and providing the struct-update module which gave me a lot of inspiration}
               @item{Ryan Culpepper, who was generous enough to sit with me at RacketCon8 and walk me through proper use of syntax classes}
               @item{The members of the community for being so helpful on the racket-users list}]


And, as always, to the dev team who produced and maintain Racket.  You guys rule and we wouldn't be here without you.
