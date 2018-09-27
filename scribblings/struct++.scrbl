#lang scribble/manual

@(require (for-label racket))

@title{struct++}
@author{David K. Storrs}

@defmodule[struct++]

@section{Introduction}

This module extends Racket's @racket[struct] keyword in order to support field defaults, field contracts, and field wrapper functions.

It does not currently support supertypes or mutable fields.

It currently supports the #:mutable struct option, but that may be removed in a future version.

@section{Synopsis}

@verbatim{
 (struct++ type:id (field ...) struct-option ...)

   field =  field-id
         | [field-id                   field-contract               ]
         | [field-id                   field-contract   wrapper-func]
         | [(field-id  default-value)                               ]
         | [(field-id  default-value)  field-contract               ]
         | [(field-id  default-value)  field-contract   wrapper

   field-contract = contract?
   
   struct-option = As per the 'struct' builtin. (#:transparent, #:guard, etc)
}

WARNING: The #:mutable struct option is currently supported but may be disallowed in a future version.


@section{Examples}

Various possible ways to declare a @verbatim{pie} struct:

@racketblock[   (struct++ pie (filling [(cook-temp 450) exact-positive-integer? F->C ])) ]


The above line declares a struct named @racketidfont{pie}.

pie has two fields: filling and cook-temp
 
filling accepts any value
 
cook-temp defaults to 450, must be an exact-positive-integer?,
and the value will be run through the F->C function when the
struct is created and whenever the field's functional setter
(i.e. set-pie-cook-temp) is called

It also creates the following functions:

@racketblock[
   (define/contract (pie++ #:filling filling #:cook-temp [cook-temp 450])
     (->* (#:filling any/c) (#:cook-temp exact-positive-integer?) pie?)
     (pie filling (F->C cook-temp)))

   (define/contract (set-pie-filling p new-filling)
     (-> pie? any/c pie?)
     (struct-copy pie [filling new-filling]))
     
   (define/contract (set-pie-cook-temp p new-temp)
     (-> pie? exact-positive-integer? pie?)
     (struct-copy pie [cook-temp (F->C new-temp)]))
]

You can leave out some or all of the field options:

@racketblock[
   (struct++ pie (filling cook-temp))
   (struct++ pie (filling [(cook-temp 450)])) 
   (struct++ pie (filling [cook-temp       exact-positive-integer?      ]))
   (struct++ pie (filling [cook-temp       exact-positive-integer? F->C ]))
   (struct++ pie (filling [(cook-temp 450) exact-positive-integer?      ]))
]

Also, struct options are supported:

@racketblock[
   (struct++ pie
             ([filling (or/c 'berry "berry" 'chocolate "chocolate" 'cheese "cheese")
                       ~a]
              [(cook-temp 450) exact-positive-integer? add1])
             #:transparent
             #:guard
             (with-contract pie
               #:result (-> (or/c 'berry "berry"
                                  'chocolate "chocolate"
                                  'cheese "cheese")
                            exact-positive-integer?
                            symbol? ; the type, autosupplied by Racket
                            any)
               (lambda (filling cook-temp type)
                 (values filling cook-temp))))
]

@section{TODO}

@itemlist[
	@item{functional updaters in addition to the setters}
        @item{support supertypes; means we can't use struct-copy.  See the @racketmodname[struct-update] module for why, and for solutions}
        @item{reflection; simplify introspection by making the values from struct-id available at runtime} 
]

@section{Ramblings}

It might be worth it to wrap a contract around the non-keyword constructor and forbid the #:mutable struct option.  This would make it impossible to have a struct++ item that did not match its contracts.  On the other hand, trusting the programmer to know what they're doing is a good thing.  Maybe add a struct option that would enable this behavior?
