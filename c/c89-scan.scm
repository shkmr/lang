;;;
;;;    c89-scan for c89-gram.
;;;
;;;    Goal and limitation.
;;;
;;;      1) Reterns token with
;;;
;;;          1.0 Type of token (IDENTIFIER, STRING, etc)
;;;          1.1 filename, line number, and possibly column (which points the beginning of token string)
;;;          1.2 string from which token is made of.
;;;
;;;      2) Correctly tokenize all standard header files, with the help of cpp. (See test.scm and c/stdh.c)
;;;
;;;      3) Can be usable for both usual C program and preprocessed C program.
;;;
;;;          3.1 When used without cpp, cpp commands are treated as single token.
;;;              For example a line
;;;
;;;                 #include <stdio.h>
;;;
;;;              Is a single token of type sharp-command.
;;;
;;;              We first need to process the C file with cpp to get necessary typedef.
;;;              And then process the same file without cpp.
;;;
;;;          3.3 When used with cpp, "# n file" line is recognized
;;;              and tokens include filename, line number based on this information.
;;;
;;;      4) Whitespaces and comment is a token, not ignored.  So that
;;;         application programs can reproduce original source code
;;;         solely from tokens it receives.
;;;
;;;      5) Backslash-newline-escape is only recognized within strings
;;;         and pre-processor lines.
;;;         You can not break a line within a indentifier or an operator,
;;;         (even within whitespaces.)
;;;
;;;      6) Trigraphs are not supported.
;;;
;;;
;;;    Referece:
;;;
;;;       [CARM] http://careferencemanual.com
;;;
(define-module lang.c.c89-scan (extend lang.core)
  (use gauche.parameter)
  (use srfi-13)
  (export c89-scan
          register-typedef-for-c89-scan
          initialize-c89-scan
          token
          token-type
          token-string
          token-file
          token-line
          token-column
          <scan-error>
          ))
(select-module lang.c.c89-scan)

;;; GCC allows dollar sign
;;(define initial-identifier-charset #[A-Za-z_])
;;(define identifier-charset         #[A-Za-z_0-9])
(define initial-identifier-charset   #[A-Za-z_$])
(define identifier-charset           #[A-Za-z_$0-9])
(define operator-charset             #[*~!+\-/\^&%=?<>.|])

;;;
;;;
;;;
(define filename  (make-parameter #f))
(define base-line (make-parameter (cons 0 0)))

(define (get-filename)
  (if (filename)
    (filename)
    (port-name (current-input-port))))

(define (get-lineno)
  (let1 m (port-current-line (current-input-port))
    (+ (car (base-line))
       (- m (cdr (base-line))))))

(define (get-column)
  (let1 x (port-current-column (current-input-port))
    (or x 0)))

(define (do-sharp str)
  (let* ((x   (string-tokenize str))
         (len (length x)))
    (cond ((and (> len 2) (string=? (list-ref x 0) "#"))
           (let ((n (string->number (list-ref x 1)))
                 (f (list-ref x 2)))
             (when n
               (base-line (cons n (port-current-line (current-input-port))))
               (filename f))))
          (else #f))))
;;;
;;;
;;;
(define (c89-scan)
  (parameterize ((file   (get-filename))
                 (line   (get-lineno))
                 (column (get-column)))
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
            ((char=? #\# ch)
             (let ((x (read-sharp (peek-char) (list ch))))
               (do-sharp (token-string x))
               x))
            ((char=? #\/ ch)
             (let ((x (peek-char)))
               (cond ((eof-object? x) (make-token #\/ (list ch)))
                     ((char=? #\*  x) (read-char) (read-/*-comment (peek-char) (list x ch)))
                     ((char=? #\/  x) (read-char) (read-//-comment (peek-char) (list x ch))) ; c99
                     (else (read-operator (peek-char) (list ch))))))

            ((char=? #\0 ch)
             (let ((x (peek-char)))
               (cond ((eof-object?   x) (make-token 'INTEGER-CONSTANT (list ch)))
                     ((char-ci=? #\x x) (read-char) (read-hexadecimal (peek-char) (list x ch)))
                     ((char=?    #\. x) (read-char) (read-flonum (peek-char) (list x ch) #[0-9] 10 #[Ee]))
                     ((char-ci=? #\e x) (read-flonum (peek-char) (list ch) #[0-9] 10 #[Ee]))
                     ((char-ci=? #\l x) (read-decimal (peek-char) (list ch)))
                     ((char-ci=? #\u x) (read-decimal (peek-char) (list ch)))
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
             (make-token 'illegal-char (list ch)) ; or raise error?
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
        ((char=? #\nl ch) (make-token 'comment lis)) ; CARM 2.2 comment does not include newline.
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
    (inline   .  INLINE)      ; c99 but needed to process standard header file
    (int      .  INT)
    (long     .  LONG)
    (register .  REGISTER)
    ;(restrict .  RESTRICT)   ; c99
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
    ;(_Bool    .  BOOL)       ; c99
    ;(_Complex .  COMPLEX)    ; c99
    ;(_Imaginary . IMAGINARY) ; c99
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

(define (initialize-c89-scan)
  (filename #f)
  (base-line (cons 0 0))
  (set! typedefed '()))

(define (register-typedef-for-c89-scan id)
  (unless (is-typedefed? id)
    (push! typedefed id)))

;;;
;;;
;;;
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
    (( #\% #\< )     .  LCBRA       ) ; <%
    (( #\> #\% )     .  RCBRA       ) ; %>
    (( #\: #\< )     .  LSBRA       ) ; <:
    (( #\> #\: )     .  RSBRA       ) ; :>
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
