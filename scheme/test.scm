(use gauche.test)
(use text.tree)
(test-start "lang.scheme.gauche")
(add-load-path "../..")
(use lang.scheme.gauche)
(test-module 'lang.scheme.gauche)

#;(with-input-from-file "gauche.scm"
    (cut port-for-each print-token gauche-scan))

;;;
;;;
;;;
(test-section "token-string")

(define (test-copy file)
  (let ((x (with-input-from-file file (cut port-map token-string gauche-scan))))
    (with-output-to-file "fo.scm" (cut write-tree x)))
  (sys-system #"diff ~|file| fo.scm"))

(test* "gauche.scm" 0 (test-copy "gauche.scm"))

;;;
;;;
;;;
(test-section "bool")

(define (scan-bool str)
  (let ((x (with-input-from-string str gauche-scan))
        (b (with-input-from-string str read)))
    (and (eq? (token-type x) 'bool)
         (eq? b (with-input-from-string (token-string x) read)))))

(define (test-bool str) (test* str #t (scan-bool str)))

(test-bool "#t")
(test-bool "#true")
(test-bool "#T")
(test-bool "#TRUE")
(test-bool "#True")
(test-bool "#tRUe")
(test-bool "#f")
(test-bool "#false")
(test-bool "#FALSE")
(test-bool "#fALsE")

;;;
;;;
;;;
(test-section "number")

(define (scan-number str)
  (let ((x (with-input-from-string str gauche-scan))
        (n (with-input-from-string str read)))
    (if (not (number? n))
      (error #"~|str| can not be read as number"))
    ;(print str " -> " n)
    (and (eq? (token-type x) 'number)
         (eqv? n (string->number (token-string x))))))

(define (test-number str) (test* str #t (scan-number str)))

(test-number "#b1010101001010101")
(test-number "#o1234567")
(test-number "#o12345677654321")
(test-number "#d123456789")
(test-number "#d123456789987654321")
(test-number "#x123456")
(test-number "#xdeadbeef")
(test-number "#xDeadBeef")
(test-number "#36rdeadbeef")
(test-number "#18RDeadBeef")
(test-number "#36r#edeadbeef")
(test-number "#e#36rdeadbeef")
(test-number "#8r-1234")
(test-number "#8r#e+1234")
(test-number "#e#8r-1234")

(test-number "#e10.0")
(test-number "#e10e10")
(test-number "#e12.34")
(test-number "#i10")
(test-number "#i10.0")
(test-number "#i12.34")

(test-end :exit-on-failure #t)
