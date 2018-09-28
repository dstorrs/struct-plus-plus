# struct++

NB:  See the Scribble documentation for more details.

This is an extended version of Racket's [struct](https://docs.racket-lang.org/reference/define-struct.html "structure declaration") form.  struct++ will allow:

* field-level default values
* field-level contracts
* field-level wrapper functions

And will:

* Auto-generate a keyword constructor with appropriate contract
* Auto-generate functional setters for each field

## Limitations 

struct++ does not currently support:

* Supertypes
* The #:auto field-level option.  This isn't necessary since field-level defaults are supported.
* The #:mutable field-level option, although the struct-level #:mutable keyword is accepted. 
* auto-generating functional updaters along with the functional setters

NOTE:  The #:mutable struct option might be disallowed in a future release.

## Synopsis

```
(struct++ pie (filling [(cook-temp 450) exact-positive-integer? add1]) #:transparent)
```

The 'pie' structure has two fields, filling and cook-temp.  filling accepts any value.  cook-temp must be an exact positive integer, has a default value of 450 degrees, and its value (default or otherwise) will be passed through the F->C function at struct creation and when the functional setter is used.  

The above declaration auto-generates the following functions:

~~~~
(define/contract (pie++ #:filling filling #:cook-temp [cook-temp 450])
  (->* (#:filling any/c) (#:cook-temp exact-positive-integer?) pie?)
  (pie filling (F->C cook-temp)))
  
(define/contract (set-pie-filling p new-val)
  (-> pie? any/c pie?)
  (struct-copy pie p [filling new-val]))

(define/contract (set-pie-cook-temp p new-val)
  (-> pie? exact-positive-integer? pie?)
  (struct-copy pie p [cook-temp (F->C new-val)]))
~~~~

Note that struct-copy cannot properly deal with supertypes, meaning that neither can struct++.  See the [struct-update](https://docs.racket-lang.org/struct-update/index.html "lexi-lambda functional updater module") module for details and an alternative to struct++ that handles supertypes but does not support the other functionality of struct++.

This limitation will be fixed in a future version, but I wanted to release something useful now.

## License

See the 'LICENSE' file for full details. The plain-English summary (this summary is non-binding and the information in the LICENSE file controls) is that you can use the module for personal or commerical projects and it's okay to sell things that you make using the module.
