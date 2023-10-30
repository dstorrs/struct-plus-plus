#lang scribble/manual

@(require (for-label racket)
          racket/sandbox
          scribble/example)

@title{struct++}
@author{David K. Storrs}

@defmodule[struct-plus-plus]

@section{Introduction}

@racketmodname[struct-plus-plus] provides extended syntax for creating structs.  It does not support supertypes or field options (@racket[#:auto] and @racket[#:mutable]).  Aside from that, it's a drop-in replacement for the normal @racket[struct] form. So long as your struct does not have a supertype or a field marked @racket[#:auto] or @racket[#:mutable], you can literally just change @racket[struct] to @racket[struct++] and your code will continue to work as before but you will now have a keyword constructor, functional setters and updaters for all fields, and reflection data.  (NOTE: See the `Reflection' section below for how to handle structs with @racket[#:prefab].)

@racketmodname[struct-plus-plus] offers the following benefits over normal @racket[struct]:

@itemlist[
 @item{keyword constructor}
 @item{(optional) functional setters and updaters for each field}
 @item{(optional) distinct defaults for individual fields}
 @item{(optional) contracts for each field}
 @item{(optional) dotted accessors to clarify struct/field relationship}
 @item{(optional) wrapper functions for each field}
 @item{(optional) wrappers around field accessors}
 @item{(optional) dependency checking between fields}
 @item{(optional) declarative syntax for business logic rules}
 @item{(optional) declarative syntax for converting the structures to arbitrary other values}
 @item{(optional) declarative syntax for generating the struct type from other values}
 @item{(optional) easy run-time introspection and reflection}
 ]

@section{Design Goal}

The intent is to move structs from being dumb data repositories into being data models in the sense of MVC programming.  They should contain data that is internally consistent and valid according to business rules.  This centralizes the checks that would otherwise need to be done at the point of use.

@section{Synopsis}

@(define eval
   (call-with-trusted-sandbox-configuration
    (lambda ()
      (parameterize ([sandbox-output 'string]
                     [sandbox-error-output 'string]
                     [sandbox-memory-limit 50])
        (make-evaluator 'racket)))))

@examples[
 #:eval eval
 #:label #f
 (require json struct-plus-plus)

 (code:comment "Declare a new struct type")
 (struct++ plant1 (genus) #:transparent)

 (code:comment "\n; create an instance in various ways")
 (code:line (plant1 'Rosa) (code:comment "normal Racket struct usage"))
 (code:line (plant1++ #:genus 'Rosa)  (code:comment "struct-plus-plus keyword constructor"))
 (code:line (hash->struct++ plant1++ (hash 'genus 'Rosa)) (code:comment "create from hash"))

 (code:comment "\n; cf dotted accessors, functional setters and updaters")
 (define p1 (plant1++ #:genus 'Rosa))
 (code:line (plant1.genus p1) (code:comment "plant1.genus is equivalent to plant1-genus"))
 (code:line (set-plant1-genus p1 "Helianthus") (code:comment "functional setter"))
 (code:line (update-plant1-genus p1 (lambda (type) (~a type ", subtype 10"))) (code:comment "functional updater"))

 (code:comment "\n; Let's enforce data types.  Genus names must be strings")
 (struct++ plant2 ([genus string?]) #:transparent)
 (code:line (plant2 'Rosa) (code:comment "basic Racket constructor will generate a non-compliant instance"))
 (code:comment "The keyword constructor raises an error on non-compliant data")
 (eval:error (plant2++ #:genus 'Rosa))
 (plant2++ #:genus "Rosa")
 (hash->struct++ plant2++ (hash 'genus "Rosa"))

 (code:comment "\n; Additionally, let's force scientifically-accurate case onto the genus.")
 (struct++ plant3 ([genus string?])
   (#:rule ("genus names are required to be lowercase with initial capital" #:transform genus (genus) [(string-titlecase genus)]))
   #:transparent)
 (plant3++ #:genus "rosa")

 (code:comment "\n; Same as the above but using a wrapper function instead of a transform rule")
 (struct++ plant3 ([genus string? string-titlecase]) #:transparent)
 (plant3++ #:genus "rosa")

 (code:comment "\n; Time to go hard.  Let's make a struct that describes a person who wants to join the military, even if that requires lying.")

 (define (get-min-age) 18.0)

 (struct++ recruit
           ([name (or/c symbol? non-empty-string?) ~a] (code:comment "Accepts either, forces to string")
            [age positive?]
            [(eyes 'brown) (or/c 'brown 'black 'green 'blue 'hazel)] (code:comment "Defaults to 'brown if not specified")
            [(height-m #f) (between/c 0 3)]  (code:comment "Defaults to #f which is not a valid value but if it wasn't provided then the transform rules below will auto-calculate it")
            [(weight-kg #f) positive?]
            [(bmi #f) positive?]
            [(felonies 0) natural-number/c])

           (#:rule ("bmi can be found" #:at-least    2         (height-m weight-kg bmi))
            #:rule ("ensure height-m"  #:transform   height-m  (height-m weight-kg bmi) [(or height-m (sqrt (/ weight-kg bmi)))])
            #:rule ("ensure weight-kg" #:transform   weight-kg (height-m weight-kg bmi) [(or weight-kg (* (expt height-m 2) bmi))])
            #:rule ("ensure bmi"       #:transform   bmi       (height-m weight-kg bmi) [(or bmi (/ 100 (expt height-m 2)))])
            #:rule ("lie about age"    #:transform   age       (age) [(define min-age (get-min-age))
                                                                      (cond [(>= age 18) age]
                                                                            [else min-age])])
            #:rule ("eligible-for-military?" #:check (age felonies bmi) [(and (>= age 18)
                                                                              (= 0 felonies)
                                                                              (<= 25 bmi))])
            #:make-dotted-accessors? #t (code:comment "This is the default. Use #f to not generate dotted accessors") 
            #:make-setters? #t (code:comment "This is the default. Use #f to not generate functional setters and updaters") 
	    (code:comment "create several functions that marshal the struct for different purposes")
            #:convert-for (db (#:remove '(eyes bmi) (code:comment "Prep the struct for writing to a DB")
                               #:rename (hash 'height-m 'height 'weight-kg 'weight)))
            #:convert-for (alist (#:remove '(bmi eyes)
                                  #:rename (hash 'height-m 'height 'weight-kg 'weight)
                                  #:post hash->list))
            #:convert-for (json (#:action-order '(rename remove add overwrite)
                                 #:post  write-json
                                 #:rename (hash 'height-m 'height 'weight-kg 'weight)
                                 #:remove '(felonies)
                                 #:add (hash 'vision "20/20")
                                 #:overwrite (hash 'hair "brown"  (code:comment "overwrite value")
                                                   'shirt (thunk "t-shirt") (code:comment "use the result of the thunk as the value for 'shirt")
                                                   'eyes symbol->string (code:comment "one-argument function so run the current value through and use the result")
                                                   'age (lambda (age) (* 365 age)) (code:comment "same, except the function is custom")
                                                   'vision (lambda (h key val) (code:comment "three-argument function so pass the hash, the key, and the current value and use the result")
                                                             (if (> (hash-ref h 'age) 30)
                                                                 "20/15"
                                                                 val))))))

           #:transparent)

 (code:comment "\n; keyword constructor exists and requires several fields")
 (eval:error (recruit++ #:name 'tom))

 (define bob
   (recruit++ #:name 'bob
              #:age 16 (code:comment "Bob isn't old enough for the military so he will lie and say he's 18")
              #:height-m 2
              #:weight-kg 100))

 (code:comment "Note that Bob's name is now a string, his age was changed, his BMI was calculated, and his felonies defaulted to 0")
 bob
 (code:comment "\n; side-by-side of the dotted accessors and standard accessors to show equivalence")
 (list (recruit.name bob) (recruit-name bob))
 (list (recruit.age bob) (recruit-age bob))
 
 (code:comment "\n; various conversion functions that marshal to different forms") 
  (recruit->db bob)
  (code:comment "\t deprecated alias for the above")
 (recruit/convert->db bob)    

  (recruit->alist bob)
  (code:comment "\t deprecated alias for the above")
 (recruit/convert->alist bob) 

; (recruit->json bob)
; (recruit/convert->json bob)  (code:comment "\t deprecated alias for the above")
 ]


@section[#:tag "Syntax"]{Syntax}

@verbatim{
 (struct++ type:id (field ...+) spp-options struct-option ...)

 field :   field-id
         | [field-id maybe-accessor-wrapper maybe-field-contract maybe-wrapper]
         | [field-id maybe-field-contract  maybe-accessor-wrapper maybe-wrapper]
         | [field-id maybe-field-contract  maybe-wrapper maybe-accessor-wrapper]

         | [(field-id default-value) maybe-accessor-wrapper maybe-field-contract maybe-wrapper]
         | [(field-id default-value) maybe-field-contract  maybe-accessor-wrapper maybe-wrapper]
         | [(field-id default-value) maybe-field-contract  maybe-wrapper maybe-accessor-wrapper]

 maybe-field-contract : 
                        | contract? = any/c

 maybe-wrapper : 
                  | (and/c procedure? (procedure-arity-includes? 1)) = identity

 maybe-accessor-wrapper : 
                           | #:wrap-accessor (and/c procedure? (procedure-arity-includes? 2))

 spp-options :
               | (spp-option ...+)

 spp-option :   #:make-setters?          boolean? = #t
              | #:make-dotted-accessors? boolean? = #t
              | #:omit-reflection
              | rule
              | rules
              | convert-for
              | convert-from

 rule  :  #:rule rule-clause

 rules :  #:rules (rule-clause ...+)

 rule-clause :   (rule-name #:at-least N maybe-pred (field-id ...+))
               | (rule-name #:check (field-id ...+) [expr])
               | (rule-name #:transform field-id (field-id ...+) (expr  ...+))
               | (rule-name #:transform (field-id ...+) (field-id ...+) (expr  ...+))

 rule-name :  string?

 N  : exact-positive-integer?

 maybe-pred :
              | (-> any/c boolean?) = (negate false?)

 convert-for :    #:convert-for (convert-name (hash-option ...))

 convert-from :   #:convert-from (convert-name (source-predicate match-clause (field-id ...)))

 convert-name : id

 source-predicate : predicate/c

 match-clause : <expression>
		  
 hash-option :  #:include      (list key ...+)
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

Note: Supertypes are not supported as of this writing, nor are field-specific keywords (#:mutable and #:auto).  See below for why.

Note: Rules can be specified using either multiple @racket[#:rule] clauses or a single @racket[#:rules] clause that will be unpacked to be the equivalent set of @racket[#:rule] clauses.  In both cases the syntax of the clauses is exactly the same, so it's mostly an aesthetic preference.  Regardless, it's better to choose one method and use it consistently.  See @secref{Warnings} for details.

@section{Constructors}

After you have used @racket[(struct++ recruit ...)] to generate a @racket[recruit] type, there are three ways to create an instance of that type:

@itemlist[
@item{@racket[recruit++]}
@item{@racket[(hash->struct++ recruit++ h)]}
@item{@racket[recruit]}
]

In most cases the one you want will be @racket[recruit++].  It uses keywords, checks the field contracts, executes business logic as defined in rules, etc.

The @racket[hash->struct++] function will accept a function and a hash where the keys are symbols matching the name of the fields in a @racket[struct++] declaration.  A new instance of the struct type will be created using the specified function with the hash keys used as keyword arguments.  Assuming you are passing one of the @racketmodname[struct-plus-plus] keyword constructor functions this means that field contracts will be checked, wrappers applied, etc.  

The @racket[recruit] constructor is the standard one that Racket generates from the @racket[(struct ...)] declaration.  Using it will allow you to create structures that are invalid under the field contracts. See below:

@examples[
 #:eval eval
 #:label #f
 (require struct-plus-plus)

 (struct++ flower ([genus string?][color symbol?]) #:transparent)
 (code:comment "\n; struct-plus-plus keyword constructor")
 (flower++ #:genus "Rosa" #:color 'red)
 (code:comment "\n; convert from a hash via the struct-plus-plus keyword constructor")
 (hash->struct++  flower++ (hash 'genus "Heliantus" 'color 'yellow))
 
 (code:comment "\n; keyword constructor chokes on data that violates field contracts")
 (eval:error (flower++ #:genus 184 #:color #f))
 (code:comment "\n; ditto when used through hash->struct++")
 (eval:error (hash->struct++  flower++ (hash 'genus 998 'color 17)))

 (code:comment "\n; constructor function given to `hash->struct++` is assumed to be a keyword function, so don't do this:")
 (eval:error (hash->struct++  flower (hash 'genus 998 'color 17)))

 (code:comment "\n; default Racket constructor does not check field constraints")
 (flower 184 #f)  
]

@section{Wrappers}

All fields have wrappers; either you set one or the wrapper is @racket[identity].  Values go through the wrapper whenever the struct is created or when a setter/updater is called.  The return value of the wrapper is what is actually stored in the struct.


@section{Dotted Accessors}

@subsection{Basics}

Racket's default accessor construction can be confusing. For example:

  @racket[(remote-server-send-ch foo)] 

Is that retrieving the value of the @racket[server-send-ch] field in the @racket[remote] struct, or is it retrieving the value of the @racket[send-ch] field in the @racket[remote-server] struct?

Compare the less ambiguous version:

  @racket[(remote-server.send-ch foo)]

When @racket[#:make-dotted-accessors?] is missing or has the value #t, @racket[struct++] will generate a dotted accessor for each field. When @racket[#:make-dotted-accessors?] is defined and has the value #f the dotted accessors will not be generated.

@subsection{Access Wrappers}

Accessor wrappers allow you to make the struct do something extra when retrieving a field's value, such as retrieve the data from a database, log the act of accessing, etc.

@bold{Access wrappers only work through the dotted accessors, not through the default accessors created by} @racket[struct]@bold{.  This is a feature, not a bug.}

Accessor wrappers take two arguments: the struct itself and the current value of the field.  

@examples[
 #:eval eval
 #:label #f
 (struct++ dog ([name (or/c string? promise?) #:wrap-accessor (λ (the-struct current-val) (force current-val))]))
 (define fido (dog++ #:name (lazy "fido")))
 (displayln
 (~a " default accessor returns: " (dog-name fido)
     "\n before dotted accessor, promise was forced?: " (promise-forced? (dog-name fido))
     "\n dotted accessor returns: " (dog.name fido)
     "\n after dotted accessor, promise was forced?: " (promise-forced? (dog-name fido))))
]

That lambda is ugly and a lot of the time we don't want to use the struct argument, only the current value.  How about something prettier?  The @racket[wrap-accessor] macro consumes a function and returns another function that takes any number of arguments and passes all but the first argument to the wrapped function. (i.e., it will create a function that accepts both the struct and the value as arguments and then ignore the struct and passes the value to the wrapped function)

@examples[
 #:eval eval
 #:label #f
 (struct++ horse ([name (or/c string? promise?) #:wrap-accessor (wrap-accessor force)]))
 (define secretariat (horse++ #:name (lazy "secretariat")))
 (displayln
 (~a " default accessor returns: " (horse-name secretariat)
     "\n before dotted accessor, promise was forced?: " (promise-forced? (horse-name secretariat))
     "\n dotted accessor returns: " (horse.name secretariat)
     "\n after dotted accessor, promise was forced?: " (promise-forced? (horse-name secretariat))))
]

Note:  One drawback to accessor wrappers is that they do not actually alter the value stored in the struct, they only return something different.  That means that using something like @racket[match] will pull out the value actually stored there, not the value that would have come from the accessor wrapper.

As an example of why you might want to use accessor wrappers, they would be an excellent way to implement an ORM, where a struct represents a database and performs queries when you access various fields.  Using setters and updaters could then be used to write to the database.


@section{Setters and Updaters}

When @racket[#:make-setters?] is missing or has the value #t, @racket[struct++] will generate a functional setter and updater for each field. When @racket[#:make-setters?]  is defined and has the value #f the setters and updaters will not be generated.

Given a struct of type @racket[recruit] with a field @racket[age], the name of the setter will be @racket[set-recruit-age] and the updater will be @racket[update-recruit-age].  Setters receive a value, updaters receive a one-argument function that receives the current value and returns the new value.

The setters and updaters are not exported.  You will need to put them in the @racket[provide] line manually.

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

@racketmodname[struct-plus-plus] supports three types of rules, all of which are run when a struct instance is created:

@itemlist[
@item{@racket[#:check], which verifies that a condition holds and uses @racket[raise-arguments-exception] if it does not.}
@item{@racket[#:at-least], which verifies that at least N of a particular set of arguments meet a particular requirement (by default `are not @racket[#f]'), thereby enabling the rest to be calculated.}
@item{@racket[#:transform], which alters one or more arguments before the instance is constructed.  This might be used to calculate an argument value or to compel certain constraints to be maintained.  Rules are evaluated in order and later rules will see the transformed values of earlier rules.}
]

Let's go back to our example of the recruit.  In order to be accepted into the military, you must be at least 18 years of age, have no felonies on your record, and be reasonably fit (BMI no more than 25).

Bob @italic{really} wants to join the military, and he's willing to lie about his age to do that.  The following struct checks that enough information is available to calculate Bob's BMI (body mass index) and then populates his height, weight, and BMI.


@examples[
 #:eval eval
 #:label #f
 (define (get-min-age) 18.0)
 (struct++ lying-recruit
           ([name (or/c symbol? non-empty-string?) ~a]
            [age positive?]
            [(height-m #f) (between/c 0 3)]
            [(weight-kg #f) positive?]
            [(bmi #f) positive?]
            [(felonies 0) natural-number/c])
      (#:rule ("bmi can be found"     #:at-least    2       (height-m weight-kg bmi))
       #:rule ("lie about age if necessary" #:transform age (age) [(if (>= age (get-min-age)) age (get-min-age))])
       #:rule ("ensure height/weight/BMI"     #:transform
                                                 (height-m weight-kg bmi)
                                                 (bmi height-m weight-kg name)
                                                 ["fields to transform and fields to use can differ in number and order"
                                                  "values returned must match values to transform in number and order"
                                                  (values
                                                   (or height-m  (sqrt (/ weight-kg bmi)))
                                                   (or weight-kg (* (expt height-m 2) bmi))
                                                   (or bmi       (/ 100 (expt height-m 2))))])
       )
      #:transparent)
          ]

In the "ensure height/weight/BMI" rule it is not necessary to check that you have enough information to do the calculations because the "bmi can be found" rule has already established that either two or three out of the three values (@racketid[height-m], @racketid[weight-kg], @racketid[bmi]) were specified.
 
@examples[
 #:eval eval
 #:label #f
 (define bob
   (lying-recruit++ #:name 'bob
                    #:age 16
                    #:height-m 2
                    #:weight-kg 100))

 bob
 ]

Note that Bob's name has been changed from a symbol to a string as per Army regulation 162.11a, his age has magically changed from 16 to 18.0, and his BMI has been calculated.  Suppose we try to invalidate these constraints?

@examples[
 #:eval eval
 #:label #f
 (eval:error (set-lying-recruit-felonies bob #f))
 ]

Nope!  You cannot invalidate the structure by way of the functional setters/updaters, although you could do it if you marked your struct as @racket[#:mutable] and then used the standard Racket mutators. (e.g. @racket[set-recruit-felonies!])

 There are two separate but equivalent formats for declaring rules:  the @racket[#:rule] keyword followed by a rule clause or the @racket[#:rules] keyword following by a parenthesized sequence of rule clauses.  The @racket[#:rules] parenthesized form is more Racketish, whereas the @racket[#:rule] syntax exists for historical reasons and is maintained for backwards compatibility.  Still, it's mostly an aesthetic choice and they can be intermixed.

@examples[
 #:eval eval
 #:label #f
 
 (struct++ animal
           ([name (or/c symbol? non-empty-string?) ~a]
            [age integer?])
           (
            #:rules (["pointlessly check name" #:check (name)        [#t]]
	    	     ["have a birthday"        #:transform age (age) [(add1 age)]])
            #:rule ("name is >= 2 characters " #:check (name) [(>= (string-length (~a name)) 2)])
	    #:rules (["pointlessly check age"  #:check (age) [#t]])
            )
           #:transparent)

 (animal++ #:name 'fido #:age 0) 
 ]

(NOTE:  Although you *can* intermix @racket[#:rule] and @racket[#:rules], you probably shouldn't, as changes caused by transform rules in a @racket[#:rules] clause are not visible in a later @racket[#:rule] clause.  This is a bug and will be fixed eventually.)

@section{Converters}

@subsection{convert-for}

@subsubsection{Summary}
      
When marshalling a struct for writing to a database, a file, etc, it is useful to turn it into a different data structure, usually but not always a hash.  Converters will change the struct into a hash, then pass the hash to the @racket[hash-remap] function in @racketmodname[handy], allowing you to return anything you want.  @racket[hash-remap] takes a series of keyword arguments specifying how the hash should be mangled.  Specifically:

@itemlist[
 @item{@racket[#:remove] <list> : delete the keys in the list from the hash}
 @item{@racket[#:overwrite] <hash> : change the values of existing keys or add missing ones}
 @item{@racket[#:add] <hash> : add one or more keys to the hash, die if they were already there}
 @item{@racket[#:rename] <hash> : change the names of one or more keys}
 @item{@racket[#:value-is-default?] <function> : a predicate that specifies what values should be defaulted} 
 @item{@racket[#:default] <hash> : if a key is not there, add it.  If it is there it will be set if and only if it matches the value-is-default? predicate.}
 @item{@racket[#:action-order] <list> : specify in what order to apply the above options. Default is '(include remove overwrite add rename default)}
 @item{@racket[#:post] <function> : run the resulting hash through a function that returns anything you want}
 ]

Note that @racket[#:overwrite] provides special behavior for values that are procedures with arity 0, 1, or 3.  The values used are the result of calling the procedure with no args (arity 0); the current value (arity 1); or hash, key, current value (arity 3).

Note that @racket[#:default] provides special behavior for values that are procedures.  If the procedure has arity of exactly 2 then it will be called as (func key hash).  Procedures with other arity will be called as (func key).  The value used for the field is comes back from the procedure call.

@subsubsection{convert-for Examples}

convert-for functions are named @racket[<struct-name>-><purpose>], where `purpose' is the name given to the conversion specification.  (DEPRECATED: There are also aliases named @racket[<struct-name>/convert-><purpose>]) For example:

@examples[
 #:eval eval
 #:label #f
 (struct++ person
           (name age)
           (#:convert-for (db (#:remove '(age)))
            #:convert-for (json (#:add (hash 'created-at (current-seconds))
                                 #:post write-json)))
           #:transparent)
 (define bob (person "bob" 18))
 (person->db   bob)
 (person->json bob)

 (code:comment "\n; deprecated aliases for the above")
 (person/convert->db   bob)
 (person/convert->json bob)

 (code:comment "\n; examples that demonstrate hash-mangling options")
 (struct++ fruit
     (name color price)
     (
     #:convert-for (include (#:include '(name))) (code:comment "include only the name field")
     #:convert-for (remove (#:remove '(name price))) (code:comment "remove everything but the color field")
     #:convert-for (add-good (#:add (hash 'type "added the 'type' key")))
     #:convert-for (add-bad (#:add (hash 'name "Boom! can't 'add' existing keys and 'name' key is already there")))
     #:convert-for (rename (#:rename (hash 'price 'price-in-pennies)))
     #:convert-for (overwrite-name-via-value (#:overwrite (hash 'name "yummy fruit!")))
     #:convert-for (overwrite-name-via-func-arity0 (#:overwrite (hash 'name (thunk "value from thunk"))))
     #:convert-for (overwrite-name-via-func-arity1 (#:overwrite (hash 'name (lambda (v) (~a "the original value was: '" v "'")))))
     #:convert-for (overwrite-name-via-func-arity3 (#:overwrite (hash 'name (lambda (h k v) (list h k v)))))
     #:convert-for (convert-using-all-options
        (#:action-order '(add default rename remove overwrite) (code:comment "specify the order in which to apply the options") 
         #:add (hash 'subtype "honeycrisp"  (code:comment "add new keys")
  	             'source "Vermont"
  	             'organic? 'unspecified
  	             'leaves 2) 
         #:value-is-default? 'unspecified (code:comment "keys with a value of 'unspecified will be defaulted, along with keys that are not present")
         #:default (hash 'source-farm "McDonald's"  (code:comment "key isn't there so it will be added")
		         'organic? #t (code:comment "will replace existing value because existing value is considered default")
		         'leaves (lambda (k h) (add1 (hash-ref h k))))
	 #:rename (hash 'price "price-in-pennies" (code:comment "rename the pre-existing 'price key")
		        'subtype 'breed) (code:comment "rename the 'subtype' key that was created in the 'add' step above")
	 #:remove '(price-in-pennies no-such-key) (code:comment "ensure key is not present. doesn't care if it wasn't")
	 #:overwrite (hash 'name "a new name"
	                   'color (thunk "a value made inside a thunk") (code:comment "function of 0 args is called")
			   'breed string-titlecase (code:comment "function of 1 argument receives the value")
			   'multi-leaved? (lambda (h k v) (code:comment "add new key. function of 3 arguments gets hash, key, value")
			                    (>= (hash-ref h 'leaves) 2)))
			                    ))))

     (define apple (fruit "apple" "red" 199))
     (fruit->include apple)
     (fruit->remove apple)
     (fruit->add-good apple)
     (eval:error (fruit->add-bad apple))     
     (fruit->rename apple)
     (fruit->overwrite-name-via-value apple)
     (fruit->overwrite-name-via-func-arity0 apple)
     (fruit->overwrite-name-via-func-arity1 apple)
     (pretty-print (fruit->overwrite-name-via-func-arity3 apple))
     (pretty-print (fruit->convert-using-all-options apple))
     ]

@subsection{convert-from}

convert-from functions go the opposite direction from convert-for -- they accept an arbitrary value and they turn it into a struct.

convert-from functions are named @racket[<source>-><struct-name>++], where `source' is the name given to the conversion specification.  For example:

@examples[
 #:eval eval
 #:label #f
 (require struct-plus-plus)

 (struct++ key ([data bytes?]) #:transparent)
 
 (struct++ person
           ([id exact-positive-integer?]
            [name non-empty-string?]
            [(keys '()) list?]
            )
	   (#:convert-from (vector (vector?
                                    (vector id
                                            (app (compose (curry map key) vector->list) keys)
                                            name)
                                    (id keys name)))
            )
           #:transparent)

  (vector->person++ (vector 9 (vector #"foo" #"bar") "fred"))
 ]

There are four parts to the convert-from clause:

@itemlist[#:style 'ordered
@item{The name of what is being converted.  In the example this is @racketid[vector]. It is used to generate the name of the converter function, e.g. @racketid[vector->person++]}
@item{A predicate that will recognize that thing (e.g. @racket[vector?]), which is used to build the contract for the converter function}
@item{A @racket[match] clause that will parse the thing and assign its elements to identifiers that correspond to the field names of the struct}
@item{A series of the identifiers from the @racket[match] clause that should be used to construct the struct.  Again, the identifiers must have the same names as the fields; this is so that the macro can use the identifiers to generate the correct keywords for the constructor}]

In this example we created a function named @racketid[vector->person++] which takes a vector of three elements and returns a @racket[person] struct.  The first element of the vector must be an @racket[exact-positive-integer?], the second must be a vector containing zero or more byte strings, and the third must be a @racket[non-empty-string?].  The vector of bytes is converted to a list, the elements of which are converted to @racket[key] structs.

Behind the scenes, the @racket[#:convert-from] specification above is equivalent to the following:

@examples[
 #:eval eval
 #:label #f

 (require struct-plus-plus)

 (struct++ key ([data bytes?]) #:transparent)
 
 (struct++ person
           ([id         exact-positive-integer?]
            [name       non-empty-string?]
            [(keys '()) list?])
	    #:transparent)

 (define/contract (vector->person++ val)
   (-> vector? person?)
   (match val
     [(vector id
              (app (compose (curry map key) vector->list) keys)
              name)
      (person++ #:id id #:keys keys #:name name)]))

 (vector->person++ (vector 9 (vector #"foo" #"bar") "fred"))
]


@section{Reflection}

By default, all struct++ types support reflection by way of a structure property, `prop:struct++', which contains a promise (via @racket[delay]) which contains a struct++-info struct containing relevant metadata.  

Use the @racket[#:omit-reflection] keyword to disable this behavior.  You will need to do so if you are including the @racket[#:prefab] struct option.

Relevant struct definitions:
		
@examples[
 #:eval eval
 #:label #f
 (struct++ person ([name (or/c symbol? string?) ~a]
                   [(age 18) number?]
                   [eyes])
           (#:rule ("name ok" #:check (name) [(> (string-length name) 3)])
            #:rule ("is >= teen" #:check (age) [(>= age 13)])
            #:convert-for (db (#:add (hash 'STRUCT-TYPE 'person))))
           #:transparent)


 (define bob (person 'bob 18 'brown))
 (struct++-ref bob)
 (force (struct++-ref bob))
 ]

Declarations for the various types used in reflection:

@racketblock[
 (struct struct++-rule (name type))
 (code:comment "  contracts: string?  (or/c 'transform 'check 'at-least)")
 (code:comment "  e.g.: \"name ok\" 'check")

 (struct struct++-field (name accessor contract wrapper default))
 (code:comment "  e.g.: 'name (or/c symbol? string?) ~a 'no-default-given")
 (code:comment "  e.g.: 'age number? identity 18")

 (struct struct++-info
   (base-constructor constructor predicate fields rules converters))
 (code:comment "  base-constructor will be the ctor defined by struct, e.g. 'person'")
 (code:comment "  constructor will be the ctor defined by struct++, e.g. 'person++'")
 (code:comment "  predicate will be, e.g., 'person?'")
 (code:comment "  converters will be a list of the procedures defined by the #:convert-for items")
 ]

@examples[
 #:eval eval
 #:label #f

 (match (force (struct++-ref bob))
   [(struct* struct++-info
             ([base-constructor base-constructor] ; person
              [constructor constructor]           ; person++
              [predicate predicate]               ; person?
              [fields (and fields
                           (list (struct* struct++-field
                                          ([name     field-names    ]
                                           [accessor field-accessors]
                                           [contract field-contracts]
                                           [wrapper  field-wrappers ]
                                           [default  field-defaults ]))
                                 ...))]
              [rules (and rules
                          (list (struct* struct++-rule
                                         ([name rule-names]
                                          [type rule-types]))
                                ...))]
              [converters converters]))

    (pretty-print
     (hash 'field-names     field-names
           'field-accessors field-accessors
           'field-contracts field-contracts
           'field-wrappers  field-wrappers
           'field-defaults  field-defaults
           'rule-names      rule-names
           'rule-types      rule-types
           'converters      converters
           'fields          fields
           'rules           rules))])


 ]


@section{API}

  @defform[(struct++)]{See @secref{Syntax}.}

  @subsection{Utilities}

@defproc[(struct->hash [struct-desc struct-id]  [s struct?]) (hash/c symbol? any/c)]{
  Accepts a structure-type transformer binding (cf @racket[struct-id] from the @racketid[syntax/parse/class/struct-id] module) and an instance of that struct type.  Returns an immutable hash where the keys are symbols of the same name as the field names and the values are the contents of those fields.

Helpful tip:  Under normal circumstances, a `structure-type transformer binding' has the same name as the struct type.
  
@examples[
 #:eval eval
 #:label #f

 (struct person (name age hair) #:transparent)
 (define bob (person 'bob 18 "brown"))
 bob
 (struct->hash person bob)
]
}

@defproc[(hash->struct++ [struct-ctor procedure?] [h (hash/c symbol? any/c)]) any/c]{
Takes a struct constructor function and a hash, returns a struct of the appropriate type.  The ctor must be one that accepts keywords (e.g. created by struct++).  The keys of the hash must be symbols that case-sensitiviely match the keywords used by the struct constructor.
@examples[
 #:eval eval
 #:label #f

 (struct++ canine (name age) #:transparent)
 (define fido (canine++ #:name "fido" #:age 17))
 fido
 (hash->struct++ canine++ (hash 'name "fido" 'age 17))
]
  }

  @defform[(wrap-accessor func) #:contracts ([func (and/c procedure? (procedure-arity-includes/c 1))])]{
Consumes a function that can accept one or more non-keyword arguments and has no mandatory keyword arguments.   Produces a function that accepts an arbitrary number of arguments, drops the first one, and passes the rest to the wrapped function.

@examples[
 #:eval eval
 #:label #f

(define acc-wrap (wrap-accessor force))
acc-wrap
(code:comment "`acc-wrap' is equivalent to (lambda args (apply force (cdr args)))")
]
  }

@subsection[#:tag "Reflection Implementation"]{Reflection Implementation}

  See @secref{Reflection} for full details on how to use reflection.

@defthing[prop:struct++ struct-type-property?]{The struct property in which is stored a @racket[promise] which, when @racket[force]d, yields a @racket[struct++-info] in which reflection data is stored.  Retrieve the property using @racket[struct++-ref].}


  @defstruct*[struct++-field ([name symbol?]
  			     [accessor (-> any/c any/c)]
			     [contract contract?]
			     [wrapper procedure?]
			     [default any/c]
			     )]{A struct type for describing the fields of a struct.}

@defstruct*[struct++-info ([base-constructor procedure?]
			  [constructor procedure?]
			  [predicate predicate/c]
			  [fields              (listof struct++-field?)]
			  [rules               (listof struct++-rule?)]
			  [converters          (listof procedure?)])]{A struct type that collects all reflection data on a struct created via @racket[struct++]}

@defproc[(struct++-ref [instance struct?]) struct-type-property?]{Retrieve the @racket[prop:struct++] instance for a particular struct.  For examples, see @secref{Reflection}. }

@defstruct*[struct++-rule ([name string?][type (or/c 'at-least 'transform 'check)])]{A struct type to describe one rule from a @racket[struct++] struct type.}



@section{Warnings, Notes, and TODOs}

Some of these were already mentioned above:

@subsection[#:tag "Warnings"]{Warnings}

@itemlist[
 @item{One drawback to accessor wrappers is that they do not actually alter the value stored in the struct, they only return something different.  That means that using something like @racket[match] will pull out the value actually stored there, not the value that would have come from the accessor wrapper.}
 @item{TO FIX:  Changes caused by transform rules in a @racket[#:rules] clause are not visible in a later @racket[#:rule] clause.}
 @item{If you include the @racket[#:prefab] option then you must also include @racket[#:omit-reflection]} 
 @item{As with any function in Racket, default values are not sent through the contract.  Therefore, if you declare a field such as (e.g.) @racket[[(userid #f) integer?]] but you don't pass a value to it during construction then you will have an invalid value (@racket[#f] in a slot that requires an integer).  Default values ARE sent through wrapper functions, so be sure to take that into account -- if you have a default value of @racket[#f] and a wrapper function of @racket[add1] then you are setting yourself up for failure.}
@item{In a @racketid[convert-from] clause, the match clause must bind components to identifiers that will be used to construct the struct.  Those identifiers must have the same names as the fields so that the code can correctly generate the keywords for those fields.} 
]

@subsection{Notes}

@itemlist[
 @item{@racket[recruit++] checks contracts and rules etc.  @racket[recruit] does not}
 @item{@racket[person.name] respects the accessor wrapper, @racket[person-name] does not}
 @item{@racket[#:transform] rules take 1+ expressions in their code segment.  The return value becomes the new value of the target}
 @item{@racket[#:check] rules take exactly one expression in their code segment.  If the returned value is true then the rule passed, and if it's @racket[#f] then the rule calls @racket[raise-arguments-error]}
 @item{Rules are processed in order. Changes made by a @racket[#:transform] rule will be seen by later rules}
 @item{None of the generated functions (@racket[struct-name++], @racket[set-struct-name-field-name], etc) are exported.  You'll need to list them in your @racket[provide] line manually}
 @item{See the @racket[hash-remap] function in the @racketmodname[handy] module for details on what the @racket[#:convert-for] converter options mean}
 ]

@subsection{TODOs}

@itemlist[
 @item{TODO:  Add more complex variations of @racket[#:at-least], such as:  @racket[#:at-least 1 (person-id (person-name department-id))]}
 @item{TODO: add a keyword that will control generation of mutation setters that respect contracts and rules. (Obviously, only if you've made your struct @racket[#:mutable])}
]

@subsection{Field options and why they aren't available}

Field options (@racket[#:auto] and @racket[#:mutable]) are not supported and there are no plans to support them in the future.

Regarding @racket[#:auto]:  The per-field default syntax that @racket[struct++] provides is strictly superior to @racket[#:auto], so there is no need to provide it.  Furthermore, auto fields come with the restriction that they cannot have values provided at construction time -- it's not a default, it's a "here's this field that is automagically generated and you can't do anything but read it".  This would substantially complicate generating the keyword constructor, since the macro would need to locate all fields that were auto and then exclude them from the constructor.  Furthermore, it wouldn't be sensible for an auto field to have a default value, contract, wrapper, or functional setter, so there would need to be an entirely separate field syntax and then many additional checks.  The costs of supporting @racket[#:auto] far outweigh the marginal value.

Regarding @racket[#:mutable]: Supporting this one would be straightforward, so not supporting it is a deliberate choice.  The functional setters that @racket[struct++] provides should satisfy nearly all the same use cases as the @racket[#:mutable] field option, and it's still possible to use the struct-level #:mutable option if you really want to mutate.  Mutation should be avoided in general, so leaving out the @racket[#:mutable] field option seems like a good decision.

@subsection{Supertypes and why they aren't available}

tl;dr:  It's probably doable but it was more complex than I wanted to put the work in for.  Pull requests welcome.

Supertypes in Racket come with some tricky issues, such as this:

@examples[
 #:eval eval
 #:label #f


(struct animal (name age) #:transparent)
(struct person animal (name job)  #:transparent)
(code:comment "  Note that both animal and person have a name field")
(code:comment "  ")
(define bob (person 'human 27 'bob 'teacher))
bob
(person? bob)
(animal? bob)
(person-name bob)
(animal-name bob)
(struct->hash person bob)
]

The supertype and the subtype have fields with the same name.  When you access fields from the @racketid[animal] part of the struct, you do it using @racketid[animal-] functions, thereby losing the association to the @racketid[person] type.  What should the setter and updater look like?  How do you ensure that the wrapper function and field contracts from the supertype are respected by the subtype? 

This would be possible to do using reflection data, but that data isn't available when @racket[#:omit-reflection] is set, which is necessary if the struct is @racket[#:prefab].  I would prefer to not need to have a note saying "this works, unless you have @racket[#:omit-reflection] set".

@section{Thanks}

The words `shoulders of giants' apply here.  I would like to offer great thanks to:

@itemlist[
 @item{Greg Hendershott, for his @link["http://www.greghendershott.com/fear-of-macros/"]{"Fear of Macros"} essay}
 @item{Alexis King (aka lexi-lambda), for teaching me a lot about macros on the @link["https://www.mail-archive.com/racket-users@googlegroups.com/"]{racket-users} list (especially @link["https://www.mail-archive.com/racket-users@googlegroups.com/msg40201.html"]{this post}) and providing the struct-update module (@link["https://docs.racket-lang.org/struct-update/index.html"]{docs}, @link["https://github.com/lexi-lambda/struct-update/blob/master/struct-update-lib/struct-update/main.rkt"]{code}) which gave me a lot of inspiration}
 @item{Ryan Culpepper, who was generous enough to sit with me at RacketCon8 and walk me through proper use of syntax classes}
 @item{The members of the community for being so helpful on the @link["https://www.mail-archive.com/racket-users@googlegroups.com/"]{racket-users} list}]


And, as always, to the dev team who produced and maintain Racket.  You guys rule and we wouldn't be here without you.

