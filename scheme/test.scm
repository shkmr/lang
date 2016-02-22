(use gauche.test)
(use gauche.parameter)
(use text.tree)
(test-start "lang.scheme.gauche")
(add-load-path "../..")
(use lang.scheme.gauche)
(test-module 'lang.scheme.gauche)

(define p? (make-parameter #f))

(use file.util)
(use gauche.process)
(define (Gauche/*.scm)
  (with-input-from-process "find ../../Gauche -name '*.scm' -print" 
    (lambda ()
      (port-map (lambda (x) x) read-line))))

(define (test-file-set n)
  (case n
    ((0) '( "gauche.scm"
            "../core.scm"
            "../c/c89-gram.scm"
            "../c/c89-scan.scm"
            ))
    ((1) (Gauche/*.scm))))

;;;
;;;
;;;
(test-section "gauche-read")

(define (compare-read str)
  (let ((x (with-input-from-string str gauche-read))
        (y (with-input-from-string str read)))
    (if (p?) (begin (newline)
                    (write x) (newline)
                    (write y) (newline)))
    (equal? x y)))
(define (test-read str) (test* str #t (compare-read str)))

;;(p? #t)
(test-read "a"     )
(test-read ".a"    )
(test-read ".1"    )
(test-read ".1."   )
(test-read ".."    )
(test-read "..."   )
(test-read "123"   )
(test-read "()"    )
(test-read "'()"   )
(test-read "'a"    )
(test-read "'(a)"  )
(test-read "`a"    )
(test-read "`(a)"           )
(test-read "`(a b c)"       )
(test-read "`(a . (b . c))" )
(test-read "`(a b c)"    )
(test-read "`(a ,b c)"   )
(test-read "`(a ,(b) c)" )
(test-read "`(a ,(b) c)" )
(test-read "`((a . ,b))" )
(test-read "`((a . ,b) (c . ,d))"    )
(test-read "`((\"a(\" . ,b)))"       )
(test-read "(foo `((\"a(\" . ,b))))" )
(test-read "(a)"         )
(test-read "(a b c)"     )
(test-read "(a b . c)"   )
(test-read "(a b . (c))" )
(test-read "(a (b c))"   )
(test-read "#(a b c)"    )
(test-read "#[a b c]"    )
(test-read "#[abc]"      )
(test-read "#[[:alnum:]]" )
(test-read "#[[:xdigit:]]")
(test-read "#/abc/"      )
(test-read "#u8(1 2 3)"  )
(test-read "#f16(1.0 2.0 3.0)")
(test-read "#f64(1.0 2.0 3.0)")
(test-read "#|##|###|#|||#### ||#|||||#|#1")

(test-read "\
  (define uvector-alist
    `((\"#s8(\"  . ,s8vector )
      (\"#u8(\"  . ,u8vector )))))")

;;;
;;;
;;;
(test-section "read-file")

(define (compare-read-file file)
  (let ((x (open-coding-aware-port (open-input-file file)))
        (y (open-coding-aware-port (open-input-file file))))
    (define (xread) (with-input-from-port x gauche-read))
    (define (yread) (with-input-from-port y read))
    (unwind-protect
        (let lp ((x1 (xread)) (y1 (yread)))
          (cond ((and (eof-object? x1)
                      (eof-object? y1)) #t)
                ((eof-object? x1) #f)
                ((eof-object? y1) #f)
                ((equal? x1 y1)
                 (lp (xread) (yread)))
                ((p?)
                 (write x1) (newline)
                 (write y1) (newline)
                 #f)
                (else #f)))
      (close-port x)
      (close-port y))))

(for-each (lambda (f)
            (test* f #t (compare-read-file f)))
          (test-file-set 1))
;;;
;;;
;;;
(test-section "token-string")

(define (test-copy file)
  (let ((x   (with-input-from-file file (cut port-map token-string gauche-scan)))
        (tmp (sys-tmpnam)))
    (unwind-protect
        (begin
          (with-output-to-file tmp (cut write-tree x))
          (sys-system #"diff ~|file| ~|tmp|"))
      (sys-unlink tmp))))

(for-each (lambda (f)
            (test* f 0 (test-copy f)))
          (test-file-set 0))

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
