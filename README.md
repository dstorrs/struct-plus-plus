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

NOTE:  The #:mutable struct option might be disallowed in a future release.

## Synopsis

```
(struct++ pie (filling [(cook-temp 450) exact-positive-integer? add1]) #:transparent)
```

The 'pie' structure has two fields, filling and cook-temp.  filling accepts any value.  cook-temp must be an exact positive integer, has a default value of 450 degrees, and its value (default or otherwise) will be passed through the F->C function at struct creation and when the functional setter / updater are used.  

The above declaration auto-generates the following functions:

~~~~
pie++                 :: keyword constructor
set-pie-filling       :: functional setter
update-pie-filling    :: functional update

set-pie-cook-temp     :: functional setter
update-pie-cook-temp  :: functional update
~~~~

Note that struct-copy cannot properly deal with supertypes, meaning that neither can struct++.  See the [struct-update](https://docs.racket-lang.org/struct-update/index.html "lexi-lambda functional updater module") module for details and an alternative to struct++ that handles supertypes but does not support the other functionality of struct++.

This limitation will be fixed in a future version, but I wanted to release something useful now.

## License

See the 'LICENSE' file for full details. The plain-English summary (this summary is non-binding and the information in the LICENSE file controls) is that you can use the module for personal or commerical projects and it's okay to sell things that you make using the module.
