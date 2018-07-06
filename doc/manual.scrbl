#lang scribble/manual

@title{struct++}

@section{Synopsis}

Below are some possible alternatives for a struct++ definition.

@racketblock[
  (struct food (name flavor-type)) ; create normal struct

  (struct++ pie (filling cook-temp)) @(display "; foo")
  (struct++ pie (filling [(cook-temp 450)]))
  (struct++ pie (filling [cook-temp       exact-positive-integer?      ]))
  (struct++ pie (filling [cook-temp       exact-positive-integer? F->C ]))
  (struct++ pie (filling [(cook-temp 450) exact-positive-integer?      ]))
  (struct++ pie (filling [(cook-temp 450) exact-positive-integer? F->C ]))
  (struct++ pie food (filling flavor-type))
  (struct++ pie
            food
            ([filling (or/c 'berry "berry" 'chocolate "chocolate" 'cheese "cheese")
                      symbol-string->string]
             [(cook-temp 450) exact-positive-integer?])
            #:transparent
            #:guard
              (lambda (name flavor-type filling cook-temp type)    ; this is here just to
                (values name flavor-type filling cook-temp type))) ; prove you can do it
]

@section{Description}

`struct++` adds functionality to Racket's built-in `struct`.  Specifically:

@itemlist[ #:style 'ordered
@item{Add contracts to each field}
@item{Add separate wrapper functions around each field}
@item{TODO Generate functional setters and updaters for each field, each of which respects the contract for that field.}
@item{TODO Add good reflection capabilities}
]

@section{Declaration Format}

@verbatim|{
  (struct++ type:id maybe-super:id (field ...) struct-option ...)

    type        = type name
    
    maybe-super = 
                | name of super type
      
    field =  field-id
          | [field-id                   field-contract               ]
          | [field-id                   field-contract   wrapper-func]
          | [(field-id  default-value)                               ]
          | [(field-id  default-value)  field-contract               ]
          | [(field-id  default-value)  field-contract   wrapper-func]
  
    field-contract = contract?
  
    struct-option = as per the 'struct' builtin 
  
           IMPORTANT:
         Field options (#:mutable and #:auto) are not supported.
         Note the extra set of parens when setting a default!
}|
