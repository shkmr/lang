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
          select-lalr-version
          lalr-version
          lr-driver
          glr-driver
          ))
(select-module lang.lalr.lalr)

(define *lalr-version* 'v2.5.0)

(define (lalr-version) *lalr-version*)

(define (select-lalr-version v)
  (set! *lalr-version* v)
  (reload-lalr))

(define lr-driver  #f)
(define glr-driver #f)

(define (reload-lalr)
  (set! lr-driver  #f)
  (set! glr-driver #f)
  (case (lalr-version)
    ((2.1.0 v2.1.0) (load "lang/lalr/lalr-2.1.0.scm" :environment (find-module 'lang.lalr.lalr)))
    ((2.5.0 v2.5.0) (load "lang/lalr/lalr-2.5.0.scm" :environment (find-module 'lang.lalr.lalr)))
    (else
     (error "lang.lalr: Unknown version: " (select-lalr-version)))))
(reload-lalr)

(provide "lang/lalr/lalr")
