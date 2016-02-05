(use gauche.test)
(use gauche.parameter)
(use text.tree)
(test-start "lang.c.c89-scan")
(add-load-path "../..")
(use lang.c.c89-scan)
(test-module 'lang.c.c89-scan)

(define (scan str)
  (with-input-from-string str
    (lambda ()
      (port-map (lambda (x)
                  (cons (token-type x)
                        (token-string x)))
                c89-scan))))

(define (test-scan str expect)
  (test* str expect (scan str)))

(test-section "CARM 2.3 tokens")
(test-scan "forwhile;" '((IDENTIFIER . "forwhile") (SEMICOLON . ";")))
(test-scan "b>x;"      '((IDENTIFIER . "b") (> . ">") (IDENTIFIER . "x") (SEMICOLON . ";")))
(test-scan "b->x;"     '((IDENTIFIER . "b") (PTR_OP . "->") (IDENTIFIER . "x") (SEMICOLON . ";")))
(test-scan "b--x;"     '((IDENTIFIER . "b") (DEC_OP . "--") (IDENTIFIER . "x") (SEMICOLON . ";")))
(test-scan "b---x;"    '((IDENTIFIER . "b") (DEC_OP . "--") (- . "-") (IDENTIFIER . "x") (SEMICOLON . ";")))

(test-section "integer constants")
(test-scan "1234;"    '((INTEGER-CONSTANT . "1234"  ) (SEMICOLON . ";")))
(test-scan "012;"     '((INTEGER-CONSTANT . "012"   ) (SEMICOLON . ";")))
(test-scan "0x12;"    '((INTEGER-CONSTANT . "0x12"  ) (SEMICOLON . ";")))

(test-section "character constants")
(test-scan "'a';"     '((CHARACTER-CONSTANT . "'a'"    ) (SEMICOLON . ";")))
(test-scan "'A';"     '((CHARACTER-CONSTANT . "'A'"    ) (SEMICOLON . ";")))
(test-scan "' ';"     '((CHARACTER-CONSTANT . "' '"    ) (SEMICOLON . ";")))
(test-scan "'?';"     '((CHARACTER-CONSTANT . "'?'"    ) (SEMICOLON . ";")))
(test-scan "'\\r';"   '((CHARACTER-CONSTANT . "'\\r'"  ) (SEMICOLON . ";")))
(test-scan "'\\0';"   '((CHARACTER-CONSTANT . "'\\0'"  ) (SEMICOLON . ";")))
(test-scan "'\"';"    '((CHARACTER-CONSTANT . "'\"'"   ) (SEMICOLON . ";")))
(test-scan "'\\377';" '((CHARACTER-CONSTANT . "'\\377'") (SEMICOLON . ";")))
(test-scan "'%';"     '((CHARACTER-CONSTANT . "'%'"    ) (SEMICOLON . ";")))
(test-scan "'\\23';"  '((CHARACTER-CONSTANT . "'\\23'" ) (SEMICOLON . ";")))
(test-scan "'8';"     '((CHARACTER-CONSTANT . "'8'"    ) (SEMICOLON . ";")))
(test-scan "'\\\\';"  '((CHARACTER-CONSTANT . "'\\\\'" ) (SEMICOLON . ";")))
(test-scan "'ABCD';"  '((CHARACTER-CONSTANT . "'ABCD'" ) (SEMICOLON . ";")))

(test-section "floating point constants")
(test-scan "0.;"       '((DOUBLE-CONSTANT . "0."     )       (SEMICOLON . ";")))
(test-scan "3e1;"      '((DOUBLE-CONSTANT . "3e1"    )       (SEMICOLON . ";")))
(test-scan "3.14159;"  '((DOUBLE-CONSTANT . "3.14159")       (SEMICOLON . ";")))
(test-scan ".0;"       '((DOUBLE-CONSTANT . ".0"     )       (SEMICOLON . ";")))
(test-scan "1.0E-3;"   '((DOUBLE-CONSTANT . "1.0E-3" )       (SEMICOLON . ";")))
(test-scan "1e-3;"     '((DOUBLE-CONSTANT . "1e-3"   )       (SEMICOLON . ";")))
(test-scan "1.0;"      '((DOUBLE-CONSTANT . "1.0"    )       (SEMICOLON . ";")))
(test-scan "0.00034;"  '((DOUBLE-CONSTANT . "0.00034")       (SEMICOLON . ";")))
(test-scan "2e+9;"     '((DOUBLE-CONSTANT . "2e+9"   )       (SEMICOLON . ";")))
(test-scan "1.0f;"     '((FLOAT-CONSTANT  . "1.0f"   )       (SEMICOLON . ";")))
(test-scan "1.0e67L;"  '((LONG-DOUBLE-CONSTANT . "1.0e67L")  (SEMICOLON . ";")))
(test-scan "1.37E+6L;" '((LONG-DOUBLE-CONSTANT . "1.37E+6L") (SEMICOLON . ";")))
(test-scan "0E1L;"     '((LONG-DOUBLE-CONSTANT . "0E1L"   )  (SEMICOLON . ";")))
(test-scan "0x1.0p1;"  '((DOUBLE-CONSTANT . "0x1.0p1")       (SEMICOLON . ";")))
(test-scan "0x1.0;"    (test-error <scan-error>)) ; "hexadecimal floating constant requires an exponent"

(test-section "string constants")
(test-scan "\"\";"          '((STRING . "\"\"")       (SEMICOLON . ";")))
(test-scan "\"\\\"\";"      '((STRING . "\"\\\"\"")   (SEMICOLON . ";")))
(test-scan "\"Copyright 2000 \\\nTexas Instruments. \""
           '((STRING . "\"Copyright 2000 \\\nTexas Instruments. \"")))

(test-section "other")
(test-scan "X++Y;"     '((IDENTIFIER . "X") (INC_OP . "++") (IDENTIFIER . "Y")  (SEMICOLON . ";")))
(test-scan "-12ul;"    '((-  . "-") (INTEGER-CONSTANT . "12ul") (SEMICOLON . ";")))
(test-scan "x**2;"     '((IDENTIFIER . "x") (* . "*") (* . "*") (INTEGER-CONSTANT . "2") (SEMICOLON . ";")))
(test-scan "A*=B;"     '((IDENTIFIER . "A") (MUL_ASSIGN . "*=") (IDENTIFIER . "B")  (SEMICOLON . ";")))

;; ## operator is processed by cpp, lexer is not expected to see it.
;;(test-scan "while##DO;" '())

(test-section "token-string")

(define p? (make-parameter #f))

;;(p? #t)
(define (test-copy file)
  (let ((x (with-input-from-file file
             (lambda ()
               (port-map (lambda (x)
                           (if (p?) (print-token x))
                           (token-string x))
                         c89-scan)))))
    (with-output-to-file "fo.scm" (cut write-tree x))
    (sys-system #"diff ~|file| fo.scm")))

(use file.util)
(use srfi-13)
(use gauche.process)

(define (test-c-files n)
  (case n
    ((0) '("c/hello.c"))
    ((1) (directory-list "gcc-torture/src/"
                         :add-path? #t
                         :filter (lambda (e) (string-suffix? ".c" e))))))

(for-each (lambda (f)
            (test* f 0 (test-copy f)))
          (test-c-files 0))

(test-section "lang.c.c89-gram")
(use lang.c.c89-gram)
(test-module 'lang.c.c89-gram)

(define (cscan)
  (let ((x (c89-scan)))
    (if (eof-object? x)
      '*eoi*
      (case (token-type x)
        ((sharp-command comment whitespaces) (cscan))
        (else
         (list (token-type x)
               (token-type x)
               x
               #;(token-string x)
               ))))))

;;
(define (test-gram str expect)
  (define (conv lis)
    (map (lambda (x)
           (cond ((is-a? x token) (token-string x))
                 ((pair? x) (conv x))
                 (else x)))
         lis))
  (test* str expect (with-input-from-string str
                  (lambda ()
                    (conv (c89-gram cscan error))))))

;;
(test-gram "char a;"                             '( (declaration ( (("a" non-pointer) :init #f) )
                                                                 (w/o-storage-class-spefifier (CHAR))) ))
(test-gram "char *a;"                            '( (declaration ( (("a" *) :init #f) )
                                                                 (w/o-storage-class-spefifier (CHAR))) ))
(test-gram "static char *a;"                     '( (declaration ( (("a" *) :init #f) )
                                                                 (STATIC (CHAR))) ))
(test-gram "extern char a, b;"                   '( (declaration ( (("a" non-pointer) :init #f)
                                                                   (("b" non-pointer) :init #f) )
                                                                 (EXTERN (CHAR))) ))
(test-gram "extern char a, *b;"                  '( (declaration ( (("a" non-pointer) :init #f)
                                                                   (("b" *)           :init #f) )
                                                                 (EXTERN (CHAR))) ))
(test-gram "extern char a, **b;"                 '( (declaration ( (("a" non-pointer) :init #f)
                                                                   (("b" * *)         :init #f) )
                                                                 (EXTERN (CHAR))) ))
(test-gram "extern char a, ***b;"                '( (declaration ( (("a" non-pointer) :init #f)
                                                                   (("b" * * *)       :init #f) )
                                                                 (EXTERN (CHAR))) ))

(test-gram "extern char a[];"                    '( (declaration ( (("a" #f array non-pointer)      :init #f) )
                                                                 (EXTERN (CHAR))) ))
(test-gram "extern char *a[];"                   '( (declaration ( (("a" #f array *)                :init #f) )
                                                                 (EXTERN (CHAR))) ))
(test-gram "extern char (*a)[];"                 '( (declaration ( (("a" * #f array non-pointer)    :init #f) )
                                                                 (EXTERN (CHAR))) ))
(test-gram "extern char *(*a)[];"                '( (declaration ( (("a" * #f array *)              :init #f) )
                                                                 (EXTERN (CHAR))) ))
(test-gram "static char a[10];"                  '( (declaration ( (("a" (INTEGER-CONSTANT "10") array non-pointer)              :init #f) )
                                                                 (STATIC (CHAR))) ))
(test-gram "extern char a();"                    '( (declaration ( (("a" #f function non-pointer)   :init #f) )
                                                                 (EXTERN (CHAR))) ))
(test-gram "extern char (*a)();"                 '( (declaration ( (("a" * #f function non-pointer) :init #f) )
                                                                 (EXTERN (CHAR))) ))
(test-gram "extern char *(*a)();"                '( (declaration ( (("a" * #f function *) :init #f) )
                                                                 (EXTERN (CHAR))) ))

(use ggc.port.mirroring)
(define (syntax-check file)
  (reset-typedef-for-c89-scan)
  (with-input-from-process #"cc -D'__attribute__(x)=' -U__BLOCKS__ -D'__restrict=' -E ~|file|"
    (lambda ()
      (with-input-from-port/mirroring-to-port
       (current-input-port)
       (current-output-port)
       (lambda ()
         (c89-gram cscan error)
         0)))))

(for-each (lambda (f)
            (test* f 0 (syntax-check f)))
          #;(take (test-c-files 1) 10)
          (list "c/stdh.c" "c/str.c" "c/foo.c" "c/hello.c")
          )

(test-end :exit-on-failure #t)
