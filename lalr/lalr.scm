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
;;
;;
;;   The default version is lalr-scm version 2.1.0.
;;   Version 2.5.0, which has GLR, is also available.
;;
;;   However the generated parser of 2.5.0 calls global funtion
;;   [g]lr-driver, which is statefull i.e., not multithread
;;   friendy. On the other hand in 2.1.0, it looks like
;;   everything is enclosed, which is more multithread friendly.
;;   In the end, when we want to make parser multithread safe,
;;   I think 2.1.0 is closer to make it happen.
;;   Thereore, we use 2.1.0 as default, and 2.5.0 can be used
;;   when we want GLR.
;;
;;   I also made changes in 2.5.0, which makes it incompatible
;;   to the original lalr-scm and makes it compatible with 2.1.0.
;;   The change is the data type of tokens the parser expects
;;   (i.e., tokens the lexerp generates).
;;   We could adopt 2.5.0 style lexical-token record type for 2.1.0,
;;   but I think 2.1.0's (cons category value) is more flexible.
;;   We can always include source location information into `value',
;;   so that it can also be included in the synxtax tree the parser
;;   generates.  With 2.5.0, source location information is
;;   removed by the parser and will not be available, unless
;;   you put such infomation into lexical-token-value.
;;   Doing so is redundant.
;;
;;   Anyway, we will keep 2.5.0 and catchup any fixes or improvements.
;;   (to backport them to 2.1.0....)
;;
(define *lalr-version* 'v2.1.0)

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
