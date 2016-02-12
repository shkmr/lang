;;;
;;;  lang.lalr - Umbrella module for lalr versions.
;;;
;;;  Usage
;;;
;;;  (use lang.core)
;;;  (selelct-lalr-version 'v2.5.0)
;;;  (use lang.lalr.lalr) ; or (use c89-gram)
;;;
;;;  It is lang.lalr.lalr for now
;;;
(define-module lang.lalr.lalr (extend lang.core)
  (export lalr-parser)
  (define (reload-lalr)
    (case (select-lalr-version)
      ((2.1.0 v2.1.0) (load "lang/lalr/lalr-2.1.0.scm" :environment (find-module 'lang.lalr)))
      ((2.5.5 v2.5.0) (load "lang/lalr/lalr-2.5.0.scm" :environment (find-module 'lang.lalr)))
      (else
       (error "lang.lalr: Unknown version: " (lalr-version)))))
  (reload-lalr)
)
(provide "lang/lalr/lalr")
