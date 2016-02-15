;;;
;;;  lang.lalr - Umbrella module for lalr versions.
;;;
;;;
(define-module lang.lalr.lalr (extend lang.core)
  (export lalr-parser)
  (include "lang/lalr/lalr-2.1.0.scm")
  ;(load "lang/lalr/lalr-20080413.scm")
  ;(load "lang/lalr/lalr-2.4.1.scm")
  ;(load "lang/lalr/lalr-2.5.0.scm")
)
(provide "lang/lalr/lalr")
