;;;
;;;    gauche-scan : tokenize Gauche program.
;;;
;;;    Goal and limitation.
;;;
;;;      1) Reterns token with
;;;
;;;          1.0 Type of token (symbol, number, etc)
;;;          1.1 filename, line number, and possibly column
;;;             (which points the beginning of token string)
;;;          1.2 string from which token is made of.
;;;
;;;      2) Whitespaces and comment is a token, not ignored.  So that
;;;         an application program can reproduce original source code
;;;         solely from tokens it receives.
;;;
;;;      3) Correctly tokenize correct Gauche programs,
;;;         behavior on error will be different.
;;;         (Incorrect input may return a token without raising error)
;;;
;;;      4) Multithread friendly
;;;
(define-module lang.scheme.gauche (extend lang.core)
  (use gauche.parameter)
  (use gauche.uvector)
  (export gauche-read
          gauche-scan
          token?
          token-type
          token-string
          token-file
          token-line
          <scan-error>
          ))
(select-module lang.scheme.gauche)

;;;
;;;  gauche-read : usage example of gauche-scan
;;;
(define sstab (make-parameter #f))

(define (gauche-read)

  (define (process-hash-bang x)
    ;; nothing for now.
    #t)

  (define (token->object x)
    (with-input-from-string (token-string x) read))

  (define uvector-alist
    `(("#s8("  . ,s8vector)
      ("#u8("  . ,u8vector)
      ("#s16(" . ,s16vector)
      ("#u16(" . ,u16vector)
      ("#f16(" . ,f16vector)
      ("#s32(" . ,s32vector)
      ("#u32(" . ,u32vector)
      ("#f32(" . ,f32vector)
      ("#s64(" . ,s64vector)
      ("#u64(" . ,u64vector)
      ("#f64(" . ,f64vector)))

  (define (make-uvector x lis)
    (let ((f (assoc-ref uvector-alist (token-string x) #f string=?)))
      (if f
        (apply f lis)
        (error "Something went wrong" (token-string x) lis))))

  (define (scan)
    (let ((x (gauche-scan)))
      (if (eof-object? x)
        x
        (case (token-type x)
          ((whitespaces comment nested-comment) (scan))
          (else x)))))

  (define (read-pair cch)
    (let ((x (scan)))
      (cond ((eof-object? x) (error "Unexpected EOF"))
            ((eqv? cch (token-type x)) '())
            (else
             (case (token-type x)
               ((#\() (let ((y (read-pair #\)))) (cons y (read-pair cch))))
               ((#\[) (let ((y (read-pair #\]))) (cons y (read-pair cch))))
               ((#\{) (let ((y (read-pair #\}))) (cons y (read-pair cch))))
               ((#\) #\] #\}) (error "Extra close parenthesis: " (token-type x)))
               ((#\.) (let* ((y (read-item))
                             (z (scan)))
                        (if (not (eqv? cch (token-type z)))
                          (error "DOT (.) in wrong context" z)
                          y)))
               ((quote quasiquote unquote unquote-splicing)
                (cons (list (token-type x) (read-item))
                      (read-pair cch)))
               ((ss-defining)  (let ((z (cons #f #f)))
                                 (hash-table-put! (sstab) (token-value x) z)
                                 (let ((y (read-item)))
                                   (if (pair? y)
                                     (begin
                                       (set-car! z (car y))
                                       (set-cdr! z (cdr y))
                                       (cons z (read-pair cch)))
                                     (begin
                                       (hash-table-put! (sstab) (token-value x) y)
                                       (cons y (read-pair cch)))))))
               ((ss-defined)   (let ((y (hash-table-get (sstab) (token-value x))))
                                 (cons y (read-pair cch))))
               ((hash-bang)    (process-hash-bang x) (read-pair cch))
               ((shebang)      (error "#! in wrong place!"))
               ((sexp-comment) (read-item) (read-pair cch))
               ((sharp-comma)  (let ((sym (read-item)))
                                 (if (not (symbol? sym))
                                   (error "error in shap comma:" sym)
                                   (let ((ctor (%get-reader-ctor sym)))
                                     (if ctor
                                       (cons (apply (car ctor) (read-pair #\) ))
                                             (read-pair cch))
                                       (error "unknown reader constructor: " sym))))))
               ((vector-open)  (cons (apply vector   (read-pair #\))) (read-pair cch)))
               ((uvector-open) (cons (make-uvector x (read-pair #\))) (read-pair cch)))
               (else           (cons (token->object x) (read-pair cch))))))))

  (define (read-item)
    (let ((x (scan)))
      (cond ((eof-object? x) x)
            (else
             (case (token-type x)
               ((#\() (read-pair #\)))
               ((#\[) (read-pair #\]))
               ((#\{) (read-pair #\}))
               ((#\) #\] #\}) (error "Extra close parenthesis: " (token-type x)))
               ((#\.) (error "dot in wrong context"))
               ((quote quasiquote unquote unquote-splicing)
                (list (token-type x) (read-item)))
               ((ss-defining)  (let ((z (cons #f #f)))
                                 (hash-table-put! (sstab) (token-value x) z)
                                 (let ((y (read-item)))
                                   (if (pair? y)
                                     (begin
                                       (set-car! z (car y))
                                       (set-cdr! z (cdr y))
                                       z)
                                     (begin
                                       (hash-table-put! (sstab) (token-value x) y)
                                       y)))))
               ((ss-defined)   (hash-table-get (sstab) (token-value x)))
               ((hash-bang)    (process-hash-bang x) (read-item))
               ((shebang)      (read-item)) ; ignore top level shebang (need to check lineno?)
               ((sexp-comment) (read-item) (read-item))
               ((sharp-comma)  (let ((sym (read-item)))
                                 (if (not (symbol? sym))
                                   (error "error in shap comma:" sym)
                                   (let ((ctor (%get-reader-ctor sym)))
                                     (if ctor
                                       (apply (car ctor) (read-pair #\)))
                                       (error "unknown reader constructor: " sym))))))
               ((vector-open)  (apply vector   (read-pair #\) )))
               ((uvector-open) (make-uvector x (read-pair #\) )))
               (else           (token->object x)))))))

  (parameterize ((sstab (make-hash-table 'eqv?)))
    (read-item)))

;;;
;;;
;;;
(define (gauche-scan)
  (parameterize ((file   (port-name (current-input-port)))
                 (point  (port-current-point (current-input-port)))
                 (line   (port-current-line (current-input-port)))
                 (column (let1 x (port-current-column (current-input-port))
                           (or x 0))))
    (let ((ch (read-char)))
      (cond ((eof-object? ch) ch)
            ((char-set-contains? #[(){}\[\]] ch) (make-token ch (list ch)))
            ((char=? #\. ch)
             (let ((x (peek-char)))
               (cond ((eof-object? x) (make-token ch (list ch)))
                     ((char-set-contains? delimiter x) (make-token ch (list ch)))
                     (else
                      (read-char)
                      (read-symbol-or-number (peek-char) (list x ch))))))
            ((char-whitespace? ch) (read-whitespaces (peek-char) (list ch)))
            ((char=? #\; ch) (read-comment (peek-char) (list ch)))
            ((char=? #\" ch) (read-string (peek-char) (list ch)))
            ((char=? #\| ch) (read-escaped-symbol (peek-char) (list ch)))
            ((char=? #\# ch) (read-sharp (peek-char) (list ch)))
            ((char=? #\' ch) (make-token 'quote (list ch)))
            ((char=? #\` ch) (make-token 'quasiquote (list ch)))
            ((char=? #\, ch)
             (let ((x (peek-char)))
               (cond ((eof-object? x) (scan-error "unterminated unquote" (list ch)))
                     ((char=? #\@ x)
                      (read-char)
                      (make-token 'unquote-splicing (list x ch)))
                     (else
                      (make-token 'unquote (list ch))))))

            ((char-set-contains? #[+-\d] ch)
             ;; Notes from Gauche/src/read.c:
             ;;    R5RS doesn't permit identifiers beginning with '+', '-',
             ;;    or digits, but some Scheme programs use such identifiers.
             (read-symbol-or-number (peek-char) (list ch)))
            (else
             (read-symbol (peek-char) (list ch)))))))

;;----------------------------------------------------------------
;; returns lis
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

;; returns lis
(define (read-word ch lis)
  (cond ((eof-object? ch) lis)
        ((char-set-contains? delimiter ch) lis)
        (else
         (read-char)
         (read-word (peek-char) (cons ch lis)))))

;; returns lis
(define (read-until-newline ch lis)
  (cond ((eof-object? ch)       lis)
        ((char=? #\newline ch)
         (read-char)
         (cons ch lis))
        (else
         (read-char)
         (read-until-newline (peek-char) (cons ch lis)))))


;;
(define (read-whitespaces ch lis)
  (cond ((eof-object? ch) (make-token 'whitespaces lis))
        ((char-whitespace? ch)
         (read-char)
         (read-whitespaces (peek-char) (cons ch lis)))
        (else
         (make-token 'whitespaces lis))))

(define (read-string ch lis)
  (make-token 'string (read-quoted ch lis #\")))

(define (read-escaped-symbol ch lis)
  (make-token 'escaped-symbol (read-quoted ch lis #\|)))

(define (check-valid-symbol lis)
  (or #t  ;; Anything is valid for now
      (if (memq #\# lis)
        (scan-error "invalid symbol name" lis)
        #t)))

(define (read-symbol ch lis)
  (let ((lis (read-word ch lis)))
    (check-valid-symbol lis)
    (make-token 'symbol lis)))

(define (read-symbol-or-number ch lis)
  (let ((lis (read-word ch lis)))
    (cond ((string->number (lis->string lis))
           (make-token 'number lis))
          (else
           (check-valid-symbol lis)
           (make-token 'symbol lis)))))

(define (read-comment ch lis)
  (make-token 'comment (read-until-newline ch lis)))

;;---------------------------------------------------------------------
;;
(define (read-sharp ch lis)

  (define (unexpected-eof lis)
    (scan-error "unexpected EOF: " lis))

  (define (unsupported lis)
    (scan-error "unsupported #-syntax: " lis))

  (define-syntax if-followed-by
    (syntax-rules ()
      ((_ x ch body ...)
       (let ((x (read-char)))
         (cond ((eof-object? x) (scan-error "unexpected EOF: " lis))
               ((char=? ch  x)  body ...)
               (else (scan-error "unsupported #-syntax: " lis)))))))

  (read-char)
  (cond ((eof-object? ch) (unexpected-eof lis))
        ((char=? #\( ch)  (make-token 'vector-open   (cons ch lis)))
        ((char=? #\; ch)  (make-token 'sexp-comment  (cons ch lis)))
        ((char=? #\! ch)  (read-hash-bang (peek-char) (cons ch lis)))
        ((char=? #\\ ch)  (read-character (peek-char) (cons ch lis)))
        ((char=? #\[ ch)  (read-char-set  (peek-char) (cons ch lis)))
        ((char=? #\/ ch)  (read-regexp (peek-char) (cons ch lis)))
        ((char=? #\| ch)  (read-nested-comment (peek-char) (cons ch lis)))
        ((char=? #\" ch)  (read-string-interpolation (peek-char) (cons ch lis)))
        ((char=? #\` ch)  (if-followed-by x  #\"  (read-string-interpolation (peek-char) (cons x (cons ch lis)))))
        ((char=? #\* ch)  (if-followed-by x  #\"  (read-incomplete-string (peek-char) (cons x (cons ch lis)))))
        ((char=? #\, ch)  (if-followed-by x  #\(  (make-token 'sharp-comma (cons x (cons ch lis)))))
        ((char=? #\? ch)  (if-followed-by x  #\=  (make-token 'debug-print (cons x (cons ch lis)))))
        ((char=? #\: ch)  (make-token 'uninterned-symbol (read-word (peek-char) (cons ch lis))))
        ((char-set-contains? #[0-9] ch) (read-ssdef-or-number (peek-char) (cons ch lis)))
        ((char-set-contains? #[BDEIOXbdeiox] ch) (read-number (peek-char) (cons ch lis)))
        ((char-set-contains? #[TFSUtfsu] ch)
         (let* ((l   (read-word (peek-char) (list ch)))
                (sym (lis->symbol (map char-foldcase l)))
                (lis (append l lis)))
           (case sym
             ((t true f false) (make-token 'bool lis))
             ((s8 u8 s16 u16 s32 u32 s64 u64 f16 f32 f64)
              (if-followed-by x  #\( (make-token 'uvector-open (cons x lis))))
             (else (unsupported lis)))))
        (else (unsupported (cons ch lis)))))

(define (read-ssdef-or-number ch lis)

  (define (->num lis)
    (string->number (apply string (cdr (reverse lis)))))

  (cond ((eof-object? ch) (scan-error "unexpected EOF: " lis))
        ((char=? #\= ch) (read-char) (make-token 'ss-defining (cons ch lis) (->num lis)))
        ((char=? #\# ch) (read-char) (make-token 'ss-defined  (cons ch lis) (->num lis)))
        ((char-set-contains? #[0-9] ch)
         (read-char)
         (read-ssdef-or-number (peek-char) (cons ch lis)))
        (else
         (read-number ch lis))))

(define (read-hash-bang ch lis)
  (cond ((eof-object? ch) (scan-error "EOF encountered in #! directive: " lis))
        ((char-set-contains? #[ /] ch) (make-token 'shebang (read-until-newline ch lis)))
        (else                          (make-token 'hash-bang (read-word ch lis)))))

(define (read-character ch lis)
  (cond ((eof-object? ch) (scan-error "EOF encountered in character literal: " lis))
        ((char-set-contains? delimiter ch)
         (read-char)
         (make-token 'char (cons ch lis)))
        (else
         (let ((lis (read-word ch lis)))
           (make-token 'char lis)))))

(define (read-char-set ch lis)

  (define (lp ch lis)
    (cond ((eof-object? ch) (scan-error "EOF encountered in a char set literal: " lis))
          ((char=? #\] ch)  (read-char) (cons ch lis))
          ((char=? #\[ ch)
           (read-char) (let ((lis (read-word (peek-char) (cons ch lis))))
                         (cond ((eof-object? (peek-char))
                                (scan-error "EOF encountered in a char set literal: " lis))
                               ((char=? #\]  (peek-char))
                                (read-char)
                                (lp (peek-char) (cons #\] lis)))
                               (else
                                (scan-error "Invalid character set syntax: " (cons (read-char) lis))))))
          ((char=? #\\ ch)
           (read-char) (let ((x (read-char)))
                         (if (eof-object? x)
                           (scan-error "unexpected EOF: " lis)
                           (lp (peek-char) (cons x (cons ch lis))))))

          (else (read-char) (lp (peek-char) (cons ch lis)))))

  (let ((lis (lp ch lis)))
    (make-token 'char-set lis)))

(define (read-incomplete-string ch lis)
  (let ((lis (read-quoted ch lis #\")))
    (make-token 'incomplete-string lis)))

(define (read-string-interpolation ch lis)
  (let ((lis (read-quoted ch lis #\")))
    (make-token 'string-interpolation lis)))

(define (read-regexp ch lis)
  (let ((lis (read-quoted ch lis #\/)))
    (if (eqv? #\i (peek-char))
      (make-token 'regexp (cons (read-char) lis))
      (make-token 'regexp lis))))

(define (read-number ch lis)
  (let ((lis (read-word ch lis)))
    (cond ((string->number (lis->string lis))
           (make-token 'number lis))
          (else
           (scan-error "bad numeric format: " lis)))))

(define (read-nested-comment0 ch lis)
  (let lp ((ch     ch)
           (lis    lis)
           (state 'start)
           (lvl    1))
    (if (eof-object? (read-char))
      (scan-error "EOF encountered in nested comment: " lis))
    (case state
      ((start)
       (cond ((char=? #\| ch) (lp (peek-char) (cons ch lis) 'read-bar  lvl))
             ((char=? #\# ch) (lp (peek-char) (cons ch lis) 'read-sharp lvl))
             (else            (lp (peek-char) (cons ch lis) 'start lvl))))
      ((read-sharp)
       (cond ((char=? #\| ch) (lp (peek-char) (cons ch lis) 'start (+ lvl 1)))
             ((char=? #\# ch) (lp (peek-char) (cons ch lis) 'read-sharp lvl))
             (else            (lp (peek-char) (cons ch lis) 'start lvl))))
      ((read-bar)
       (cond ((char=? #\# ch)
              (if (> lvl 1)
                (lp (peek-char) (cons ch lis) 'start (- lvl 1))
                (make-token 'nested-comment (cons ch lis))))
             ((char=? #\| ch) (lp (peek-char) (cons ch lis) 'read-bar lvl))
             (else            (lp (peek-char) (cons ch lis) 'start lvl))))
      (else
       (error "There's a bug in read-nested-comment")))))

(define (read-nested-comment ch lis)
  (define (lp ch lis state)
    (if (eof-object? (read-char))
      (scan-error "EOF encountered in nested comment: " lis))
    (case state
      ((start)
       (cond ((char=? #\| ch) (lp (peek-char) (cons ch lis) 'read-bar))
             ((char=? #\# ch) (lp (peek-char) (cons ch lis) 'read-sharp))
             (else            (lp (peek-char) (cons ch lis) 'start))))
      ((read-sharp)
       (cond ((char=? #\| ch) (let ((lis (lp (peek-char) (cons ch lis) 'start)))
                                (lp (peek-char) (cons ch lis) 'start)))
             ((char=? #\# ch) (lp (peek-char) (cons ch lis) 'read-sharp))
             (else            (lp (peek-char) (cons ch lis) 'start))))
      ((read-bar)
       (cond ((char=? #\# ch) (cons ch lis))
             ((char=? #\| ch) (lp (peek-char) (cons ch lis) 'read-bar))
             (else            (lp (peek-char) (cons ch lis) 'start))))
      (else
       (error "There's a bug in read-nested-comment"))))
  (let ((lis (lp ch lis 'start)))
    (make-token 'nested-comment lis)))

;;;
;;; emacs does not like char-set symtax....
;;;
(define char-special #[()\[\]{}" \\|;#])
(define delimiter  #[\s|"()\[\]{};'`,])

;;; For Emacs
;; (put 'if-followed-by 'scheme-indent-function 2)

(provide "lang/scheme/gauche")
