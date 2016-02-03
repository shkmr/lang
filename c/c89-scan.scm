;;;
;;;
;;;
(define-module lang.c.c89-scan (extend lang.core)
  (use gauche.parameter)
  (export c89-scan
          register-typedef-for-c89-scan
          reset-typedef-for-c89-scan
          print-token
          token-type
          token-string
          token-file
          token-line
          <scan-error>
          ))
(select-module lang.c.c89-scan)

;;; gcc of GCC allows dollar sign
;;(define initial-identifier-charset #[A-Za-z_])
;;(define identifier-charset         #[A-Za-z_0-9])
(define initial-identifier-charset   #[A-Za-z_$])
(define identifier-charset           #[A-Za-z_$0-9])
(define operator-charset             #[*~!+\-/\^&%=?<>.|])

;;;
;;;
;;;
(define (c89-scan)
  (parameterize ((file (port-name (current-input-port)))
                 (line (port-current-line (current-input-port))))
    (let ((ch (read-char)))
      (cond ((eof-object? ch) ch)
            ((char-whitespace? ch) (read-whitespaces (peek-char) (list ch)))
            ((char=? #\, ch)  (make-token 'COMMA      (list ch)))
            ((char=? #\: ch)  (make-token 'COLON      (list ch)))
            ((char=? #\; ch)  (make-token 'SEMICOLON  (list ch)))
            ((char=? #\( ch)  (make-token 'LPAREN     (list ch)))
            ((char=? #\) ch)  (make-token 'RPAREN     (list ch)))
            ((char=? #\{ ch)  (make-token 'LCBRA      (list ch)))
            ((char=? #\} ch)  (make-token 'RCBRA      (list ch)))
            ((char=? #\[ ch)  (make-token 'LSBRA      (list ch)))
            ((char=? #\] ch)  (make-token 'RSBRA      (list ch)))
            ((char=? #\# ch)  (read-sharp (peek-char) (list ch)))
            ((char=? #\/ ch)
             (let ((x (peek-char)))
               (cond ((eof-object? x) (make-token #\/ (list ch)))
                     ((char=? #\*  x) (read-char) (read-/*-comment (peek-char) (list x ch)))
                     ((char=? #\/  x) (read-char) (read-//-comment (peek-char) (list x ch)))
                     (else (read-operator (peek-char) (list ch))))))

            ((char=? #\0 ch)
             (let ((x (peek-char)))
               (cond ((eof-object?   x) (make-token 'INTEGER-CONSTANT (list ch)))
                     ((char-ci=? #\x x) (read-char) (read-hexadecimal (peek-char) (list x ch)))
                     ((char=?    #\. x) (read-char) (read-flonum (peek-char) (list x ch) #[0-9] 10 #[Ee]))
                     ((char-ci=? #\e x) (read-flonum (peek-char) (list ch) #[0-9] 10 #[Ee]))
                     ((char-set-contains? #[0-7] x) (read-octal (peek-char) (list ch)))
                     (else (make-token 'INTEGER-CONSTANT (list ch))))))

            ((char-numeric? ch)         (read-decimal (peek-char) (list ch)))

            ((char=? #\. ch)
             (let ((x (peek-char)))
               (cond ((eof-object?   x) (scan-error "Unexpected EOF: " (list ch)))
                     ((char-numeric? x) (read-flonum (peek-char) (list ch) #[0-9] 10 #[Ee]))
                     ((char=? #\.    x)
                      (read-char)
                      (let ((y (peek-char)))
                        (cond ((eof-object? y) (scan-error "Unexpected EOF: " (list x ch)))
                              ((char=? #\.  y)
                               (read-char)
                               (make-token 'ELLIPSIS (list #\. #\. #\.)))
                              (else
                               (scan-error "Got ``..'': " (list #\. #\.))))))
                     (else (make-token 'DOT (list #\.))))))

            ((char=? #\L ch)
             (let ((x (peek-char)))
               (cond ((eof-object? x) (scan-error "Unexpected EOF: " (list ch)))
                     ((char=? #\"  x)
                      (read-char)
                      (read-string (peek-char) (list x ch)))
                     ((char=? #\'  x)
                      (read-char)
                      (read-character (peek-char) (list x ch)))
                     (else
                      (read-identifier (peek-char) (list ch))))))

            ((char=? #\" ch)  (read-string    (peek-char) (list ch)))
            ((char=? #\' ch)  (read-character (peek-char) (list ch)))
            ((char-set-contains? initial-identifier-charset ch) (read-identifier (peek-char) (list ch)))
            ((char-set-contains? operator-charset ch)           (read-operator   (peek-char) (list ch)))
            (else
             ;; we put extra #\x for debugging purpose
             (make-token 'illegal-char (list #\x ch)) ; or raise error?
             )))))

;;;----------------------------------------------------------------
;;;
;;;
;;;
(define (read-whitespaces ch lis)
  (cond ((eof-object? ch) (make-token 'whitespaces lis))
        ((char-whitespace? ch)
         (read-char)
         (read-whitespaces (peek-char) (cons ch lis)))
        (else
         (make-token 'whitespaces lis))))

(define (read-sharp ch lis)
  (read-char)
  (cond ((eof-object? ch) (make-token 'sharp-command lis))
        ((char=? #\nl ch)
         (make-token 'sharp-command (cons ch lis)))
        ((char=? #\\ ch)
         (let ((x (read-char)))
           (read-sharp (peek-char) (cons x (cons ch lis)))))
        (else
         (read-sharp (peek-char) (cons ch lis)))))

(define (read-//-comment ch lis)
  (cond ((eof-object? ch) (make-token 'comment lis))
        ((char=? #\nl ch) (read-char) (make-token 'comment (cons ch lis)))
        (else (read-char) (read-//-comment (peek-char) (cons ch lis)))))

(define (read-/*-comment ch lis)
  (cond ((eof-object? ch) (scan-error "EOF encountered in comment" lis))
        ((char=? #\* ch)
         (read-char)
         (let ((x (read-char)))
           (cond ((eof-object? x) (scan-error "EOF encountered in comment" lis))
                 ((char=? #\/  x) (make-token 'comment (cons x (cons ch lis))))
                 (else (read-/*-comment (peek-char) (cons x (cons ch lis)))))))
        (else
         (read-char)
         (read-/*-comment (peek-char) (cons ch lis)))))

;;;
;;;    read-identifier : returns IDENTIFIER or one of keywords or typedefed.
;;;
(define c-keywords
  '((auto     .  AUTO)
    (break    .  BREAK)
    (case     .  CASE)
    (char     .  CHAR)
    (const    .  CONST)
    (continue .  CONTINUE)
    (default  .  DEFAULT)
    (do       .  DO)
    (double   .  DOUBLE)
    (else     .  ELSE)
    (enum     .  ENUM)
    (extern   .  EXTERN)
    (float    .  FLOAT)
    (for      .  FOR)
    (goto     .  GOTO)
    (if       .  IF)
    (inline   .  INLINE)
    (int      .  INT)
    (long     .  LONG)
    (noreturn .  NORETURN)
    (register .  REGISTER)
    (return   .  RETURN)
    (short    .  SHORT)
    (signed   .  SIGNED)
    (sizeof   .  SIZEOF)
    (static   .  STATIC)
    (struct   .  STRUCT)
    (switch   .  SWITCH)
    (typedef  .  TYPEDEF)
    (union    .  UNION)
    (unsigned .  UNSIGNED)
    (void     .  VOID)
    (volatile .  VOLATILE)
    (while    .  WHILE)
    (__asm             . ASM)
    ;;(__attribute__   . ATTRIBUTE)
    (__alignof__       . ALIGNOF)
    (__builtin_va_list . VA_LIST)
    (__builtin_va_arg  . VA_ARG)
    ))

;;
(define typedefed '())

(define (is-typedefed? id)
  (memq id typedefed))

(define (reset-typedef-for-c89-scan)
  (set! typedefed '()))

(define (register-typedef-for-c89-scan id)
  (unless (is-typedefed? id)
    (push! typedefed id)))

;;
(define (read-identifier ch lis)

  (define (return lis)
    (let* ((s (string->symbol (apply string (reverse lis))))
           (k (assoc-ref c-keywords s #f eq?)))
      (if k
        (make-token k lis)
        (if (is-typedefed? s)
          (make-token 'TYPE_NAME lis)
          (make-token 'IDENTIFIER lis)))))

  (cond ((eof-object? ch) (return lis))
        ((char-set-contains? identifier-charset ch)
         (read-char)
         (read-identifier (peek-char) (cons ch lis)))
        (else (return lis))))

;;;
;;;    read-operator
;;;
(define c-operators
  ;; right to left
  '((( #\= #\> #\> ) .  RIGHT_ASSIGN) ; >>=
    (( #\= #\< #\< ) .  LEFT_ASSIGN ) ; <<=
    (( #\. #\. #\. ) .  ELLIPSIS    )
    (( #\= #\+ )     .  ADD_ASSIGN  ) ; +=
    (( #\= #\- )     .  SUB_ASSIGN  ) ; -=
    (( #\= #\* )     .  MUL_ASSIGN  ) ; *=
    (( #\= #\/ )     .  DIV_ASSIGN  ) ; /=
    (( #\= #\% )     .  MOD_ASSIGN  ) ; %=
    (( #\= #\& )     .  AND_ASSIGN  ) ; &=
    (( #\= #\^ )     .  XOR_ASSIGN  ) ; ^=
    (( #\= #\| )     .  OR_ASSIGN   ) ; |=
    (( #\> #\> )     .  RIGHT_OP    )
    (( #\< #\< )     .  LEFT_OP     )
    (( #\> )         .  >           )
    (( #\< )         .  <           )
    (( #\+ #\+ )     .  INC_OP      )
    (( #\- #\- )     .  DEC_OP      )
    (( #\> #\- )     .  PTR_OP      ) ; ->
    (( #\& #\& )     .  AND_OP      )
    (( #\| #\| )     .  OR_OP       )
    (( #\= #\< )     .  LE_OP       ) ; <=
    (( #\= #\> )     .  GE_OP       ) ; >=
    (( #\= #\= )     .  EQ_OP       )
    (( #\= #\! )     .  NE_OP       )
    (( #\*     )     .  *           )
    (( #\/     )     .  /           )
    (( #\+     )     .  +           )
    (( #\-     )     .  -           )
    (( #\%     )     .  %           )
    (( #\&     )     .  &           )
    (( #\|     )     .  OR          )
    (( #\^     )     .  ^           )
    (( #\=     )     .  =           )
    (( #\?     )     .  ?           )
    (( #\.     )     .  DOT         )
    (( #\!     )     .  !           )
    (( #\~     )     .  ~           )
    ))

;;
(define (read-operator ch lis)

  (define (return lis)
    (let ((s (assoc-ref c-operators lis #f equal?)))
      (if s
        (make-token s lis)
        (scan-error "Something went wrong..." lis))))

  (cond ((eof-object? ch) (return lis))
        ((assoc-ref c-operators (cons ch lis) #f equal?)
         (read-char)
         (read-operator (peek-char) (cons ch lis)))
        (else (return lis))))

;;;
;;;    numbers
;;;
(define (read-number-constant ch lis ics radix ecs)
  (cond ((eof-object? ch) (scan-error "Unpexpected EOF: " lis))
        ((char-set-contains? ics ch)
         (read-char)
         (read-number-constant (peek-char) (cons ch lis) ics radix ecs))
        ((char=? #\. ch)
         (read-char)
         (read-flonum (peek-char) (cons ch lis) ics radix ecs))
        ((char-set-contains? ecs ch)
         (read-char)
         (read-expnum (peek-char) (cons ch lis) radix))
        ((char-set-contains? #[ULul] ch)
         (read-char)
         (read-number-constant (peek-char) (cons ch lis) ics radix ecs))
        (else
         (make-token 'INTEGER-CONSTANT lis))))

(define (read-decimal ch lis)     (read-number-constant ch lis #[0-9]       10 #[Ee]))
(define (read-hexadecimal ch lis) (read-number-constant ch lis #[0-9A-Fa-f] 16 #[Pp]))

(define (read-octal ch lis)
  (cond ((eof-object? ch) (scan-error "Unpexpected EOF: " lis))
        ((char-set-contains? #[0-7] ch)
         (read-char)
         (read-octal (peek-char) (cons ch lis)))
        (else (make-token 'INTEGER-CONSTANT lis))))

(define (read-flonum ch lis ics radix ecs)

  (define (error-if-hex ch)
    (if (= radix 16)
      (scan-error "hexadecimal floating constant require an exponent"
                  (if ch (cons ch lis) lis))))

  (cond ((char-set-contains? ics ch)
         (read-char)
         (read-flonum (peek-char) (cons ch lis) ics radix ecs))
        ((char-set-contains? ecs ch)
         (read-char)
         (read-expnum (peek-char) (cons ch lis) radix))
        ((char-ci=? #\f ch)
         (read-char)
         (error-if-hex ch)
         (make-token 'FLOAT-CONSTANT (cons ch lis)))
        ((char-ci=? #\l ch)
         (read-char)
         (error-if-hex ch)
         (make-token 'LONG-DOUBLE-CONSTANT (cons ch lis)))
        (else
         (error-if-hex #f)
         (make-token 'DOUBLE-CONSTANT lis))))

(define (read-expnum ch lis radix)

  (define (exp1 ch lis)
    (cond ((char-numeric? ch)
           (read-char)
           (exp1 (peek-char) (cons ch lis)))
          ((char-ci=? #\f ch)
           (read-char)
           (make-token 'FLOAT-CONSTANT (cons ch lis)))
          ((char-ci=? #\l ch)
           (read-char)
           (make-token 'LONG-DOUBLE-CONSTANT (cons ch lis)))
          (else
           (make-token 'DOUBLE-CONSTANT lis))))

  (cond ((eof-object? ch) (scan-error "Unpexpected EOF: " lis))
        ((char-set-contains? #[0-9\-\+] ch)
         (read-char)
         (exp1 (peek-char) (cons ch lis)))
        (else
         (error "malformed floating point expression"))))

;;
(define (read-quoted ch lis quote)
  (cond ((eof-object? ch) (scan-error "EOF encountered in a literal: " lis))
        ((char=? quote ch)
         (read-char)
         (cons ch lis))

        ((char=? #\\ ch)
         (read-char)
         (let ((x (read-char)))
           (if (eof-object? x)
             (scan-error "unexpected EOF: " lis)
             (read-quoted (peek-char) (cons x (cons ch lis)) quote))))

        (else
         (read-char)
         (read-quoted (peek-char) (cons ch lis) quote))))

(define (read-string ch lis)
  (make-token 'STRING (read-quoted ch lis #\")))

(define (read-character ch lis)
  (make-token 'CHARACTER-CONSTANT (read-quoted ch lis #\')))

(provide "lang/c/c89-scan")
