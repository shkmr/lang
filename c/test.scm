(use gauche.test)
(use gauche.parameter)
(use text.tree)
(test-start "lang.c.c89-scan")
(add-load-path "../..")
(use lang.c.c89-scan)
(test-module 'lang.c.c89-scan)

;;;
;;;
;;;
(define use-column-port (make-parameter #t))
(use ggc.port.mirroring)
(use ggc.port.column)
(with-module lang.c.c89-scan (use ggc.port.column))
(define (with-input-from-string/column str thunk)
  (call-with-input-string str
    (lambda (p)
      (with-input-from-port/column p thunk))))

;;;
;;;
;;;
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
(test-scan "0L;"      '((INTEGER-CONSTANT . "0L"    ) (SEMICOLON . ";")))
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
  (let ((tmp (sys-tmpnam))
        (x (with-input-from-file file
             (lambda ()
               (port-map (lambda (x)
                           (if (p?) (print-token x))
                           (token-string x))
                         c89-scan)))))
    (unwind-protect
        (begin
          (with-output-to-file tmp (cut write-tree x))
          (sys-system #"diff ~|file| ~|tmp|"))
      (sys-unlink tmp))))

(use file.util)
(use srfi-13)

(define (test-c-files)
  (remove (lambda (x)
            (member x '(
                        "gcc-torture/src/20010604-1.c" ;      _Bool
                        "gcc-torture/src/20010605-2.c" ;      _Complex
                        "gcc-torture/src/20020227-1.c" ;     __complex__
                        "gcc-torture/src/20020404-1.c" ;     __complex__
                        "gcc-torture/src/20020411-1.c" ;     __complex__
                        "gcc-torture/src/20030408-1.c" ;       Colon initializer
                        "gcc-torture/src/20030714-1.c" ;      _Bool
                        "gcc-torture/src/20030910-1.c" ;     __complex__
                        "gcc-torture/src/20031010-1.c" ;      _Bool
                        "gcc-torture/src/20041124-1.c" ;      _Complex
                        "gcc-torture/src/20041201-1.c" ;      _Complex
                        "gcc-torture/src/20050121-1.c" ;      _Complex
                        "gcc-torture/src/20050502-1.c" ;      _Complex
                        "gcc-torture/src/930406-1.c"   ;     __label__
                        "gcc-torture/src/960512-1.c" ;     __complex__
                        "gcc-torture/src/980605-1.c" ;     __inline__
                        "gcc-torture/src/991014-1.c" ;       typeof
                        "gcc-torture/src/991228-1.c" ;     __extension__
                        "gcc-torture/src/anon-1.c" ;       nested anonymous entity
                        "gcc-torture/src/builtin-types-compatible-p.c" ;
                        "gcc-torture/src/ffs-1.c"   ;     __volatile
                        "gcc-torture/src/loop-2c.c" ;     __inline__
                        "gcc-torture/src/compndlit-1.c" ;     Colon initializer
                        "gcc-torture/src/restrict-1.c" ;     __restrict__
                        "gcc-torture/src/stdarg-1.c" ;     __builtin_va_arg
                        "gcc-torture/src/stdarg-2.c" ;     __builtin_va_arg
                        "gcc-torture/src/struct-ini-4.c" ;    Conlon initializer
                        )))
          (directory-list "gcc-torture/src/"
                          :add-path? #t
                          :filter (lambda (e) (string-suffix? ".c" e)))))

(for-each (lambda (f)
            (test* f 0 (test-copy f)))
          '("c/CARM2.2.c" "c/foo.c" "c/hello.c" "c/type.c" "c/str.c"))

(test-section "lang.c.c89-gram")
(use lang.c.c89-gram)
(test-module 'lang.c.c89-gram)

(test-section "test gram")

(define (cscan)
  (let ((x (c89-scan)))
    (if (eof-object? x)
      '*eoi*
      (case (token-type x)
        ((sharp-command comment whitespaces) (cscan))
        (else
         (cons (token-type x) x))))))

;;
(define (test-gram str expect)
  (define (conv lis)
    (map (lambda (x)
           (cond ((is-a? x token) (cons (token-type x) (token-string x)))
                 ((pair? x) (conv x))
                 (else x)))
         lis))
  (test* str expect ((if (use-column-port)
                       with-input-from-string/column
                       with-input-from-string)
                     str
                     (lambda ()
                       (conv (c89-gram cscan error))))))

;;
(test-gram "char a;"                    '( (declaration ( (((IDENTIFIER . "a") non-pointer) :init #f) )
                                                        (w/o-storage-class-specifier (CHAR) #f #f)) ))
(test-gram "char *a;"                   '( (declaration ( (((IDENTIFIER . "a") *) :init #f) )
                                                        (w/o-storage-class-specifier (CHAR) #f #f)) ))
(test-gram "static char *a;"            '( (declaration ( (((IDENTIFIER . "a") *) :init #f) )
                                                        (STATIC (CHAR) #f #f)) ))
(test-gram "extern char a, b;"          '( (declaration ( (((IDENTIFIER . "a") non-pointer) :init #f)
                                                          (((IDENTIFIER . "b") non-pointer) :init #f) )
                                                        (EXTERN (CHAR) #f #f)) ))
(test-gram "extern char a, *b;"         '( (declaration ( (((IDENTIFIER . "a") non-pointer) :init #f)
                                                          (((IDENTIFIER . "b") *)           :init #f) )
                                                        (EXTERN (CHAR) #f #f)) ))
(test-gram "extern char a, **b;"        '( (declaration ( (((IDENTIFIER . "a") non-pointer) :init #f)
                                                          (((IDENTIFIER . "b") * *)         :init #f) )
                                                        (EXTERN (CHAR) #f #f)) ))
(test-gram "extern char a, ***b;"       '( (declaration ( (((IDENTIFIER . "a") non-pointer) :init #f)
                                                          (((IDENTIFIER . "b") * * *)       :init #f) )
                                                        (EXTERN (CHAR) #f #f)) ))

(test-gram "extern char a[];"           '( (declaration ( (((IDENTIFIER . "a") #f array non-pointer)      :init #f) )
                                                        (EXTERN (CHAR) #f #f)) ))
(test-gram "extern char *a[];"          '( (declaration ( (((IDENTIFIER . "a") #f array *)                :init #f) )
                                                        (EXTERN (CHAR) #f #f)) ))
(test-gram "extern char (*a)[];"        '( (declaration ( (((IDENTIFIER . "a") * #f array non-pointer)    :init #f) )
                                                        (EXTERN (CHAR) #f #f)) ))
(test-gram "extern char *(*a)[];"       '( (declaration ( (((IDENTIFIER . "a") * #f array *)              :init #f) )
                                                        (EXTERN (CHAR) #f #f)) ))
(test-gram "static char a[10];"         '( (declaration ( (((IDENTIFIER . "a") (CONSTANT (INTEGER-CONSTANT . "10")) array non-pointer)
                                                           :init #f) )
                                                        (STATIC (CHAR) #f #f)) ))
(test-gram "extern char a();"           '( (declaration ( (((IDENTIFIER . "a") #f function non-pointer)   :init #f) )
                                                        (EXTERN (CHAR) #f #f)) ))
(test-gram "extern char (*a)();"        '( (declaration ( (((IDENTIFIER . "a") * #f function non-pointer) :init #f) )
                                                        (EXTERN (CHAR) #f #f)) ))
(test-gram "extern char *(*a)();"       '( (declaration ( (((IDENTIFIER . "a") * #f function *) :init #f) )
                                                                 (EXTERN (CHAR) #f #f)) ))

(test-gram "char (*a)()=&foo;"        '( (declaration ( (((IDENTIFIER . "a") * #f function non-pointer)
                                                                  :init (unary-& (IDENTIFIER . "foo"))) )
                                                        (w/o-storage-class-specifier (CHAR) #f #f)) ))

(test-section "syntax-check")
(use gauche.process)

(define (with-cpp file thunk)
  (with-input-from-process
      #"cc -D'__attribute__(x)=' -U__BLOCKS__ -D'__restrict=' -E ~|file|"
    thunk))

(define (without-cpp file thunk)
  (with-input-from-file file thunk))

(define (syntax-check with file)
  (initialize-c89-scan)
  (with file
    (lambda ()
      (with-input-from-port/mirroring-to-port
          (current-input-port)
          (current-output-port)
        (lambda ()
          (slot-set! (current-input-port) 'name file)
          (c89-gram cscan error)
          0)))))

(define (syntax-check/column with file)
  (initialize-c89-scan)
  (with file
    (lambda ()
      (with-input-from-port/mirroring-to-port
          (current-input-port)
          (current-output-port)
        (lambda ()
          (with-input-from-port/column (current-input-port)
            (lambda ()
              (slot-set! (current-input-port) 'name file)
              (c89-gram cscan error)
              0)))))))

(for-each (lambda (f)
            (if (use-column-port)
              (test* f 0 (syntax-check/column with-cpp f))
              (test* f 0 (syntax-check with-cpp f))))
          (if #f
            (test-c-files)
            '("c/CARM2.2.c" "c/tak.c" "c/type.c" "c/stdh.c" "c/str.c" "c/foo.c" "c/hello.c" )))

(for-each (lambda (f)
            (if (use-column-port)
              (test* f 0 (syntax-check/column without-cpp f))
              (test* f 0 (syntax-check with-cpp f))))
          '("c/CARM2.2.c" "c/foo.c" "c/type.c" "c/str.c" "c/hello.c"))

(test-end :exit-on-failure #t)
