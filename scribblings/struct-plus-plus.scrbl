#lang scribble/manual

@(require (for-label racket)
          racket/sandbox
          scribble/example)

@title{struct++}
@author{David K. Storrs}

@defmodule[struct-plus-plus]

@section{Introduction}

@racketmodname[struct-plus-plus] provides extended syntax for creating structs.  It does not support supertypes or field options (@racket[#:auto] and @racket[#:mutable]).  Aside from that, it's a drop-in replacement for the normal @racket[struct] form. So long as your struct does not have a supertype or a field marked @racket[#:auto] or @racket[#:mutable], you can literally just change @racket[struct] to @racket[struct++] and your code will continue to work as before but you will now have a keyword constructor, functional setters for all fields, and reflection data.

@racketmodname[struct-plus-plus] offers the following benefits over normal @racket[struct]:

@itemlist[
 @item{keyword constructor}
 @item{(optional) functional setter for each field}
 @item{(optional) distinct defaults for individual fields}
 @item{(optional) contracts for each field}
 @item{(optional) wrapper functions for each field}
 @item{(optional) dependency checking between fields}
 @item{(optional) declarative syntax for business logic rules}
 @item{(optional) declarative syntax for converting the structures to arbitrary other values}
 @item{(optional) declarative syntax for generating the struct type from other values}
 @item{(optional) easy run-time introspection and reflection}
 ]

@section{Design Goal}

The intent is to move structs from being dumb data repositories into being data models in the sense of MVC programming.  They should contain data that is internally consistent and valid according to business rules.  This centralizes the checks that would otherwise need to be done at the point of use.

@section{Synopsis}

Let's make a struct that describes a person who wants to join the military.

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

 (define (get-min-age) 18.0)

 (struct++ recruit
           ([name (or/c symbol? non-empty-string?) ~a]
            [age positive?]
            [(eyes 'brown) (or/c 'brown 'black 'green 'blue 'hazel)]
            [(height-m #f) (between/c 0 3)]
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
            #:convert-for (db (#:remove '(eyes bmi)
                               #:rename (hash 'height-m 'height 'weight-kg 'weight)))
            #:convert-for (alist (#:remove '(bmi eyes)
                                  #:rename (hash 'height-m 'height 'weight-kg 'weight)
                                  #:post hash->list))
            #:convert-for (json (#:action-order '(rename remove add overwrite)
                                 #:post  write-json
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
                                                                 val))))))

           #:transparent)

 (eval:error (recruit++ #:name 'tom))

 (define bob
   (recruit++ #:name 'bob
              #:age 16
              #:height-m 2
              #:weight-kg 100))

 bob
 (set-recruit-age bob 20)
 (recruit/convert->db bob)
 (recruit/convert->alist bob)
 (recruit/convert->json bob)
 ]


@subsection{Constructors: @racket[recruit] vs @racket[recruit++]}

There are two constructors for the @racket[recruit] datatype: @racket[recruit] and @racket[recruit++].  @racket[struct++] will generate both of these while Racket's builtin @racket[struct] generates only @racket[recruit]. Only @racket[recruit++] has keywords, contracts, etc.  Using the default constructor will allow you to create structures that are invalid under the field contracts. See below:

@examples[
 #:eval eval
 (code:line (recruit 'tom -3 'red 99 10000 0.2 -27)
            (code:comment "VIOLATES FIELD CONTRACTS!"))
  (eval:error   (recruit++ #:name 'tom #:age -3 #:eyes 'red #:height-m 99 #:weight-kg 10000 #:bmi 0.2 #:felonies -27))
 ]


@section{Syntax}

@verbatim{
 (struct++ type:id (field ...) spp-options struct-option ...)

 field :   field-id
         | (field-id                   field-contract          )
         | (field-id                   field-contract   wrapper)
         | ([field-id  default-value]                          )
         | ([field-id  default-value]  field-contract          )
         | ([field-id  default-value]  field-contract   wrapper)

 field-contract : contract? = any/c

 spp-options :
               | (spp-option ...+)

 spp-option :   #:make-setters? boolean? = #t
              | #:omit-reflection
              | rule
              | convert-for
              | convert-from

 rule :  #:rule (rule-name #:at-least N maybe-pred (field-id ...+))
       | #:rule (rule-name #:check (field-id ...+) [code])
       | #:rule (rule-name #:transform field-id (field-id ...+) (code ...+))

 rule-name :  string?

 N  : exact-positive-integer?

 maybe-pred :
              | (-> any/c boolean?) = (negate false?)

 code      : <expression>

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

Note that supertypes are not supported as of this writing, nor are field-specific keywords (#:mutable and #:auto).

@section{Field contracts}

Every field has a contract.  It's either specified in the @racket[struct++] field declaration or it defaults to @racket[any/c].  When a struct is created, each non-defaulted field will have its value checked against the contract and will raise an exception if it fails.

NOTE:  As with any Racket function, parameters that default will not be checked against their contract.


@section{Wrappers}

Every field has a wrapper function; it's either specified in the @racket[struct++] field declaration or it defaults to @racket[identity]. Wrappers transform the value of a field when the struct is created and when the field is set via the functional setters.  

NOTE: There is a little bit of magic associated with wrappers.  If your wrapper function is arity-2 then it will be called as @racket[(wrapper val self)], where @racket[self] is the current struct.  If your wrapper is any other arity then it will be called as @racket[(wrapper val)].

You can prevent wrappers from being called inside the constructor and the setters by setting the @racket[(struct-plus-plus-use-wrappers)] parameter to #f.  This is useful if (e.g.) you're doing metaprogramming off the reflection data and you are going to use the wrapper manually before calling the setter.  

@examples[
 #:eval eval
 #:label #f
 (require struct-plus-plus)

 (struct++ person (name
                   [age natural-number/c]
		   [(is-adult? #f) boolean? (lambda (val self) (>= (person-age self) 18))])
		  #:transparent)
 (code:line (code:comment "is-adult? #t is not allowed when age is 10. force is-adult? to #f"))
            (define bob (person++ #:name 'bob #:age 10 #:is-adult? #t))
 bob

 (code:line (code:comment "age 18+ is an adult, so is-adult? will change with the age")
             (set-person-age bob 18))	    
 bob


 (code:line (code:comment "wrappers can be disabled")
   (parameterize ([struct-plus-plus-use-wrappers #f])
       (set-person-age bob 18)))

]

Everything that can be done with an arity-2 wrapper can be done with a transform rule, but arity2 wrappers allow the transform definition to be kept close to the field definition.

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

Let's go back to our example of the recruit.  In order to be accepted into the military, you must be at least 18 years of age, have no felonies on your record, and be reasonably fit (BMI no more than 25).

Bob @italic{really} wants to join the military, and he's willing to lie about his age to do that.

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
           (#:rule ("bmi can be found" #:at-least    2         (height-m weight-kg bmi))
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
 (eval:error (set-lying-recruit-felonies bob 3))
 ]

Nope!  You cannot invalidate the structure by way of the functional setters/updaters, although you could do it if you marked your struct as #:mutable and then used the standard Racket mutators. (e.g. @racket[set-recruit-felonies!])



@section{Converters}

@subsection{convert-for}

When marshalling a struct for writing to a database, a file, etc, it is useful to turn it into a different data structure, usually but not always a hash.  Converters will change the struct into a hash, then pass the hash to the @racket[hash-remap] function in @racketmodname[handy], allowing you to return anything you want.  See the handy/hash docs for details, but a quick summary:

@itemlist[
 @item{@racket[#:remove] <list> : delete the keys in the list from the hash}
 @item{@racket[#:overwrite] <hash> : change the values of existing keys or add missing ones}
 @item{@racket[#:add] <hash> : add one or more keys to the hash, die if they were already there}
 @item{@racket[#:rename] <hash> : change the names of one or more keys}
 @item{@racket[#:default] <hash> : if a key is there, leave it alone.  If not, add it}
 @item{@racket[#:value-is-default?] : change the behavior of #:default so that it sets the value of missing keys or keys that match a specified predicate}
 @item{@racket[#:action-order] : specify in what order to apply the above options}
 @item{@racket[#:post] : run the resulting hash through a function that returns anything you want}
 ]

Note that @racket[#:overwrite] provides special behavior for values that are procedures with arity 0, 1, or 3.  The values used are the result of calling the procedure with no args (arity 0); the current value (arity 1); or hash, key, current value (arity 3).

convert-for functions are named @racket[<struct-name>/convert-><purpose>], where 'purpose' is the name given to the conversion specification.  For example:

@examples[
 #:eval eval
 #:label #f
 (struct++ person
           (name age)
           (#:convert-for (db (#:remove '(age)))
            #:convert-for (json (#:add (hash 'created-at (current-seconds))
                                 #:post write-json)))
           #:transparent)
 (person/convert->db (person 'bob 18))
 (person/convert->json (person "bob" 18))
 ]

@subsection{convert-from}

convert-from functions go the opposite direction from convert-for -- they accept an arbitrary value and they turn it into a struct.

convert-from functions are named @racket[<source>-><struct-name>++], where 'source' is the name given to the conversion specification.  For example:

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

Behind the scenes, the #:convert-from specification above is equivalent to the following:

@examples[
 #:eval eval
 #:label #f

 (require struct-plus-plus)

 (struct++ key ([data bytes?]) #:transparent)
 
 (struct++ person
           ([id exact-positive-integer?]
            [name non-empty-string?]
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

By default, all struct++ types support reflection by way of a structure property, 'prop:struct++', which contains a promise (via @racket[delay]) which contains a struct++-info struct containing relevant metadata.  

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
 (code:comment "  base-constructor will be the ctor defined by @racket[struct], e.g. 'person'")
 (code:comment "  constructor will be the ctor defined by @racket[struct++], e.g. 'person++'")
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



@section{Warnings, Notes, and TODOs}

Some of these were already mentioned above:

@itemlist[
 @item{@racket[recruit++] checks contracts and rules etc.  @racket[recruit] does not}
 @item{@racket[#:transform] rules take 1+ expressions in their code segment.  The return value becomes the new value of the target}
 @item{@racket[#:check] rules take exactly one expression in their code segment.  If the returned value is true then the rule passed, and if it's @racket[#f] then the rule calls @racket[raise-arguments-error]}
 @item{Rules are processed in order. Changes made by a @racket[#:transform] rule will be seen by later rules}
 @item{None of the generated functions (@racket[struct-name++], @racket[set-struct-name-field-name], etc) are exported.  You'll need to list them in your @racket[provide] line manually}
 @item{Note:  As with any function in Racket, default values are not sent through the contract.  Therefore, if you declare a field such as (e.g.) @racket[[(userid #f) integer?]] but you don't pass a value to it during construction then you will have an invalid value (@racket[#f] in a slot that requires an integer).  Default values ARE sent through wrapper functions, so be sure to take that into account -- if you have a default value of @racket[#f] and a wrapper function of @racket[add1] then you are setting yourself up for failure.}
 @item{See the @racket[hash-remap] function in the @racketmodname[handy] module for details on what the @racket[#:convert-for] converter options mean}
 @item{If you include the @racket[#:prefab] option then you must also include @racket[#:omit-reflection]} 
 @item{TODO:  Add more complex variations of @racket[#:at-least], such as:  @racket[#:at-least 1 (person-id (person-name department-id))]}
 @item{TODO:  Add more complex variations of @racket[#:transform] that can handle multiple values at once, such as:  @racket[#:transform (height weight bmi) (height weight bmi) [(values (calc-bmi #f weight bmi) (calc-bmi height #f bmi) (calc-bmi height weight #f))]]}
 @item{TODO: add a keyword that will control generation of mutation setters that respect contracts and rules. (Obviously, only if you've made your struct @racket[#:mutable])}
 ]

@subsection{Field options and why they aren't available}

Field options (@racket[#:auto] and @racket[#:mutable]) are not supported and there are no plans to support them in the future.

Regarding @racket[#:auto]:  The per-field default syntax that @racket[struct++] provides is strictly superior to @racket[#:auto], so there is no need to provide it.  Furthermore, auto fields come with the restriction that they cannot have values provided at construction time -- it's not a default, it's a "here's this field that is automagically generated and you can't do anything but read it".  This would substantially complicate generating the keyword constructor, since the macro would need to locate all fields that were auto and then exclude them from the constructor.  Furthermore, it wouldn't be sensible for an auto field to have a default value, contract, wrapper, or functional setter, so there would need to be an entirely separate field syntax and then many additional checks.  The costs of supporting @racket[#:auto] far outweigh the marginal value.

Regarding @racket[#:mutable]: Supporting this one would be straightforward, so not supporting it is a deliberate choice.  The functional setters that @racket[struct++] provides should satisfy nearly all the same use cases as the @racket[#:mutable] field option, and it's still possible to use the struct-level #:mutable option if you really want to mutate.  Mutation should be avoided in general, so leaving out the @racket[#:mutable] field option seems like a good decision.


@section{Thanks}

The words 'shoulders of giants' apply here.  I would like to offer great thanks to:

@itemlist[
 @item{Greg Hendershott, for his @link["http://www.greghendershott.com/fear-of-macros/"]{"Fear of Macros"} essay}
 @item{Alexis King (aka lexi-lambda), for teaching me a lot about macros on the @link["https://www.mail-archive.com/racket-users@googlegroups.com/"]{racket-users} list (especially @link["https://www.mail-archive.com/racket-users@googlegroups.com/msg40201.html"]{this post}) and providing the struct-update module (@link["https://docs.racket-lang.org/struct-update/index.html"]{docs}, @link["https://github.com/lexi-lambda/struct-update/blob/master/struct-update-lib/struct-update/main.rkt"]{code}) which gave me a lot of inspiration}
 @item{Ryan Culpepper, who was generous enough to sit with me at RacketCon8 and walk me through proper use of syntax classes}
 @item{The members of the community for being so helpful on the @link["https://www.mail-archive.com/racket-users@googlegroups.com/"]{racket-users} list}]


And, as always, to the dev team who produced and maintain Racket.  You guys rule and we wouldn't be here without you.
