#lang racket

(require (for-syntax racket/syntax syntax/parse))

(provide make-functional-setter make-functional-updater)

;;     make-functional-setter: macro for generating non-mutating field
;;     setter functions for a struct
;;
;; Define a struct:  (struct book (title current-page filepath) #:transparent)
;;
;; Generate 'set-book-title', 'set-book-current-page', and 'set-book-filepath'.
;; All of these take two arguments: the 'book' struct to update and the new value.
;;    (make-functional-setter book title) 
;;    (make-functional-setter book current-page  exact-positive-integer?)
;;    (make-functional-setter book filepath      path-string?            path->string)
;;
;; Details:
;;    set-book-title           accepts any value, regardless of sensibility
;;    set-book-current-page    accepts only exact-positive-integer?s, else contract violation
;;    set-book-filepath        accepts only path-string?s, converts to string before storing
;;
;; Examples:
;;    (define b (book "Foundation" 297 (build-path "/foo/bar")))
;;    b                                                ; (book "Foundation" 297 "/foo/bar")
;;    (set-book-title b (hash))                        ; (book (hash) 297 "/foo/bar")
;;    (set-book-current-page b 99)                     ; (book "Foundation" 99 "/foo/bar") 
;;    (set-book-current-page b 'x)                     ; ERROR!  Contract violation
;;    (set-book-filepath b (build-path "/foo"))        ; (book "Foundation" 297 "/foo")
;;
(define-syntax (make-functional-setter stx)
  (syntax-parse stx
    #:literals (make-functional-setter)
    
    ; First, grab the name of the struct and the field we're making
    ; this for.  We'll build some stuff here then re-parse instead of
    ; copy/pasting for every pattern match
    [(make-functional-setter type-name field-name ignored ...)
     (with-syntax* ([func-name   (format-id #'type-name "set-~a-~a" #'type-name #'field-name)]
                    [func-header #'(func-name the-struct val)]
                    [definer     #'define]
                    [type-pred   (format-id #'type-name "~a?" #'type-name)]
                    [func-body   #'(struct-copy type-name the-struct [field-name val])]
                    )
       (syntax-parse stx
         [(make-functional-setter type-name field-name) #'(definer func-header func-body)]
         [(make-functional-setter type-name field-name field-contract:expr ignored ...)
          (with-syntax ([definer #'define/contract]
                        [func-contract #'(-> type-pred field-contract type-pred)])
            (syntax-parse stx
              [(_ _ _ _) #'(definer func-header func-contract func-body)]
              [(_ _ _ _ wrapper:expr)
               #'(definer func-header
                   func-contract
                   (struct-copy type-name the-struct [field-name (wrapper val)]))]))]))]))

;;----------------------------------------------------------------------

(define-syntax (make-functional-updater stx)
  (syntax-parse stx
    #:literals (make-functional-updater)
    
    ; First, grab the name of the struct and the field we're making
    ; this for.  We'll build some stuff here then re-parse instead of
    ; copy/pasting for every pattern match
;    (make-function-updater thing name symbol-string? symbol-string->string)
    
    [(make-functional-updater type-name field-name ignored ...)
     (with-syntax* ([func-name   (format-id #'type-name "update-~a-~a" #'type-name #'field-name)]
                    [getter      (format-id #'type-name "~a-~a" #'type-name #'field-name)]
                    [func-header #'(func-name the-struct proc)]
                    [definer     #'define]
                    [type-pred   (format-id #'type-name "~a?" #'type-name)]
                    [func-body   #'(struct-copy type-name the-struct [field-name (proc (getter the-struct))])]
                    )
       (syntax-parse stx
         [(make-functional-updater type-name field-name) #'(definer func-header func-body)]
         [(make-functional-updater type-name field-name field-contract:expr ignored ...)
          (with-syntax ([definer #'define/contract]
                        [func-contract #'(-> type-pred (-> any/c field-contract) type-pred)])
            (syntax-parse stx
              [(_ _ _ _) #'(definer func-header func-contract func-body)]
              [(_ _ _ _ wrapper:expr)
               #'(definer func-header
                   func-contract
                   (struct-copy type-name the-struct [field-name (wrapper (proc (getter the-struct)))]))]))]))]))
