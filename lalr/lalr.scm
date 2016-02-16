;;;
;;;  lang.lalr - Umbrella module for lalr versions.
;;;
;;;  Usage
;;;
;;;  (use lang.lalr.lalr) ; load default v2.5.0 version
;;;  (selct-lalr-version 'v2.1.0)
;;;
;;;
;;;  It is lang.lalr.lalr for now
;;;
(define-module lang.lalr.lalr (extend lang.core)
  (export lalr-parser
          make-lexical-token
          make-source-location
          select-lalr-version
          lalr-version
          lr-driver
          glr-driver
          lexical-token?
          lexical-token-value
          lexical-token-source
          ))
(select-module lang.lalr.lalr)

(define *lalr-version* 'v2.5.0)

(define (lalr-version) *lalr-version*)

(define (select-lalr-version v)
  (set! *lalr-version* v)
  (reload-lalr))

(define make-lexical-token #f)
(define make-source-location #f)
(define lexical-token? #f)
(define lexical-token-value #f)
(define lexical-token-source #f)
(define lr-driver #f)

(define (%make-lexical-token category source value)
  (error "make-lexical-token is not availabe for this version of lalr: "
         (lalr-version)))

(define (%make-source-location category source value)
  (error "make-source-location is not availabe for this version of lalr: "
         (lalr-version)))

(define (reload-lalr)
  ;;(set! make-lexical-token   %make-lexical-token)
  ;;(set! make-source-location %make-source-location)
  (set! lr-driver #f)
  (case (lalr-version)
    ((2.1.0 v2.1.0) (load "lang/lalr/lalr-2.1.0.scm" :environment (find-module 'lang.lalr.lalr)))
    ((2.5.0 v2.5.0) (load "lang/lalr/lalr-2.5.0.scm" :environment (find-module 'lang.lalr.lalr)))
    (else
     (error "lang.lalr: Unknown version: " (select-lalr-version)))))
(reload-lalr)

(provide "lang/lalr/lalr")
