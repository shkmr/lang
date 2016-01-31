;;;
;;;
;;;
(define-module lang.scheme.gauche (extend lang.core)
  (use gauche.parameter)
  (use gauche.uvector)
  (export gauche-read
          gauche-scan
          print-token
          token-type
          token-string
          token-file
          token-line
          ))
(select-module lang.scheme.gauche)

;;;
;;;  gauche-read : usage example of gauche-scan
;;;
(define (gauche-read)

  (define (token->object x)
    (with-input-from-string (token-string x) read))

  (define (read-pair cch lis)
    (let ((x (gauche-scan)))
      (cond ((eof-object? x) (error "Unexpected EOF"))
            ((eq? cch (token-type x)) (reverse lis))
            (else
             (case (token-type x)
               ((#\() (read-pair cch (cons (read-pair #\) '()) lis)))
               ((#\[) (read-pair cch (cons (read-pair #\] '()) lis)))
               ((#\{) (read-pair cch (cons (read-pair #\} '()) lis)))
               ((#\) #\] #\}) (error "Unmatched parenthesis: " (token-type x)))
               ((#\.) (append (reverse lis) (gauche-read)))
               ((whitespaces comment nested-comment)         (read-pair cch lis))
               ((quote quasi-quote unquote unquote-splicing)
                (read-pair cch (cons (list (token-type x) (gauche-read)) lis)))
               ((hash-bang)
                ;; FIXME ignore until the end of line... for now
                (read-comment (peek-char) '())
                (read-pair cch lis))
               ((sharp-comma)    (error "#,(tag arg ...) is not supported yet"))
               ((sexp-comment)   (gauche-read) (read-pair cch lis))
               ((vector-open)    (read-pair cch (cons (apply vector    (read-pair #\) '())) lis)))
               ((s8vector-open)  (read-pair cch (cons (apply s8vector  (read-pair #\) '())) lis)))
               ((u8vector-open)  (read-pair cch (cons (apply u8vector  (read-pair #\) '())) lis)))
               ((s16vector-open) (read-pair cch (cons (apply s16vector (read-pair #\) '())) lis)))
               ((u16vector-open) (read-pair cch (cons (apply u16vector (read-pair #\) '())) lis)))
               ((s32vector-open) (read-pair cch (cons (apply s32vector (read-pair #\) '())) lis)))
               ((u32vector-open) (read-pair cch (cons (apply u32vector (read-pair #\) '())) lis)))
               ((s64vector-open) (read-pair cch (cons (apply s64vector (read-pair #\) '())) lis)))
               ((u64vector-open) (read-pair cch (cons (apply u64vector (read-pair #\) '())) lis)))
               ((f16vector-open) (read-pair cch (cons (apply f16vector (read-pair #\) '())) lis)))
               ((f32vector-open) (read-pair cch (cons (apply f32vector (read-pair #\) '())) lis)))
               ((f64vector-open) (read-pair cch (cons (apply f64vector (read-pair #\) '())) lis)))
               (else             (read-pair cch (cons (token->object x) lis))))))))

  (let ((x (gauche-scan)))
    (cond ((eof-object? x) x)
          (else
           (case (token-type x)
             ((#\() (read-pair #\) '()))
             ((#\[) (read-pair #\] '()))
             ((#\{) (read-pair #\} '()))
             ((#\) #\] #\}) (error "Extra close parenthesis: " (token-type x)))
             ((#\.) (error "dot in wrong context"))
             ((whitespaces comment nested-comment) (gauche-read))
             ((quote quasi-quote unquote unquote-splicing)
              (list (token-type x) (gauche-read)))
             ((hash-bang)
              ;; FIXME ignore until the end of line... for now
              (read-comment (peek-char) '())
              (gauche-read))
             ((sharp-comma)    (error "#,(tag arg ...) is not supported yet"))
             ((sexp-comment)   (gauche-read) (gauche-read))
             ((vector-open)    (apply vector    (read-pair #\) '())))
             ((s8vector-open)  (apply s8vector  (read-pair #\) '())))
             ((u8vector-open)  (apply u8vector  (read-pair #\) '())))
             ((s16vector-open) (apply s16vector (read-pair #\) '())))
             ((u16vector-open) (apply u16vector (read-pair #\) '())))
             ((s32vector-open) (apply s32vector (read-pair #\) '())))
             ((u32vector-open) (apply u32vector (read-pair #\) '())))
             ((s64vector-open) (apply s64vector (read-pair #\) '())))
             ((u64vector-open) (apply u64vector (read-pair #\) '())))
             ((f16vector-open) (apply f16vector (read-pair #\) '())))
             ((f32vector-open) (apply f32vector (read-pair #\) '())))
             ((f64vector-open) (apply f64vector (read-pair #\) '())))
             (else             (token->object x)))))))

;;;
;;;
;;;
(define (gauche-scan)
  (parameterize ((file (port-name (current-input-port)))
                 (line (port-current-line (current-input-port))))
    (let ((ch (read-char)))
      (cond ((eof-object? ch) ch)
            ((char-set-contains? #[(){}\[\].] ch) (make-token ch (list ch)))
            ((char-whitespace? ch) (read-whitespaces (peek-char) (list ch)))
            ((char=? #\; ch) (read-comment (peek-char) (list ch)))
            ((char=? #\" ch) (read-string (peek-char) (list ch)))
            ((char=? #\| ch) (read-escaped-symbol (peek-char) (list ch)))
            ((char=? #\# ch) (read-sharp (peek-char) (list ch)))
            ((char=? #\' ch) (make-token 'quote (list ch)))
            ((char=? #\` ch) (make-token 'quasi-quote (list ch)))
            ((char=? #\, ch)
             (let ((x (peek-char)))
               (cond ((eof-object? x) (scan-error "unterminated unquote"))
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
;;
(define (read-whitespaces ch lis)
  (cond ((eof-object? ch) (make-token 'whitespaces lis))
        ((char-whitespace? ch)
         (read-char)
         (read-whitespaces (peek-char) (cons ch lis)))
        (else
         (make-token 'whitespaces lis))))

;;
(define (read-quoted ch lis quote)
  (cond ((eof-object? ch) (scan-error "EOF encountered in a literal: "
                                      (lis->string lis)))
        ((char=? quote ch)
         (read-char)
         (cons ch lis))

        ((char=? #\\ ch)
         (read-char)
         (let ((x (read-char)))
           (if (eof-object? x)
             (scan-error "unexpected EOF: " (lis->string lis))
             (read-quoted (peek-char) (cons x (cons ch lis)) quote))))

        (else
         (read-char)
         (read-quoted (peek-char) (cons ch lis) quote))))

(define (read-string ch lis)
  (make-token 'string (read-quoted ch lis #\")))

(define (read-escaped-symbol ch lis)
  (make-token 'escaped-symbol (read-quoted ch lis #\|)))

(define (read-word ch lis)
  (cond ((eof-object? ch) lis)
        ((char-set-contains? delimiter ch) lis)
        (else
         (read-char)
         (read-word (peek-char) (cons ch lis)))))

(define (check-valid-symbol lis)
  (or #t  ;; Anything is valid for now
      (if (memq #\# lis)
        (scan-error "invalid symbol name" (lis->string lis))
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
;;
(define (read-comment ch lis)
  (cond ((eof-object? ch) (make-token 'comment lis))
        ((char=? #\newline ch)
         (read-char)
         (make-token 'comment (cons ch lis)))
        (else
         (read-char)
         (read-comment (peek-char) (cons ch lis)))))

;;---------------------------------------------------------------------
;;
(define (read-sharp ch lis)

  (define (unexpected-eof lis)
    (scan-error "unexpected EOF: " (lis->string lis)))

  (define (unsupported lis)
    (scan-error "unsupported #-syntax: " (lis->string lis)))

  (define-syntax if-followed-by
    (syntax-rules ()
      ((_ x ch body ...)
       (let ((x (read-char)))
         (cond ((eof-object? x) (scan-error "unexpected EOF: " (lis->string lis)))
               ((char=? ch  x)  body ...)
               (else (scan-error "unsupported #-syntax: " (lis->string lis))))))))

  (read-char)
  (cond ((eof-object? ch) (unexpected-eof lis))
        ((char=? #\( ch)  (make-token 'vector-open   (cons ch lis)))
        ((char=? #\; ch)  (make-token 'sexp-comment  (cons ch lis)))
        ((char=? #\! ch)  (make-token 'hash-bang     (cons ch lis)))
        ((char=? #\\ ch)  (read-character (peek-char) (cons ch lis)))
        ((char=? #\[ ch)  (read-char-set (peek-char) (cons ch lis)))
        ((char=? #\/ ch)  (read-regexp (peek-char) (cons ch lis)))
        ((char=? #\| ch)  (read-nested-comment (peek-char) (cons ch lis) 0))
        ((char=? #\" ch)  (read-string-interpolation (peek-char) (cons ch lis)))
        ((char=? #\` ch)  (if-followed-by x  #\"  (read-string-interpolation (peek-char) (cons x (cons ch lis)))))
        ((char=? #\* ch)  (if-followed-by x  #\"  (read-incomplete-string (peek-char) (cons x (cons ch lis)))))
        ((char=? #\, ch)  (if-followed-by x  #\(  (make-token 'sharp-comma (cons x (cons ch lis)))))
        ((char=? #\? ch)  (if-followed-by x  #\=  (make-token 'debug-print (cons x (cons ch lis)))))
        ((char-set-contains? #[BDEIOXbdeiox1-9] ch) (read-number (peek-char) (cons ch lis)))
        ((char-set-contains? #[TFSUtfsu] ch)
         (let* ((l   (read-word (peek-char) (list ch)))
                (sym (lis->symbol (map char-foldcase l)))
                (lis (append l lis)))
           (case sym
             ((t true f false) (make-token 'bool lis))
             ((s8 u8 s16 u16 s32 u32 s64 u64 f16 f32 f64)
              (if-followed-by x  #\( (make-token (string->symbol #"~|sym|vector-open") (cons x lis))))
             (else (unsupported lis)))))
        (else (unsupported (cons ch lis)))))

(define (read-character ch lis)
  (cond ((eof-object? ch) (scan-error "EOF encountered in character literal"
                                      (lis->string lis)))
        ((char-set-contains? delimiter ch)
         (read-char)
         (make-token 'char (cons ch lis)))
        (else
         (let ((lis (read-word ch lis)))
           (make-token 'char lis)))))

(define (read-char-set ch lis)
  (let ((lis (read-quoted ch lis #\])))
    (make-token 'char-set lis)))

(define (read-incomplete-string ch lis)
  (let ((lis (read-quoted ch lis #\")))
    (make-token 'incomplete-string lis)))

(define (read-string-interpolation ch lis)
  (let ((lis (read-quoted ch lis #\")))
    (make-token 'string-interpolation lis)))

(define (read-regexp ch lis)
  (let ((lis (read-quoted ch lis #\/)))
    (make-token 'regexp lis)))

(define (read-number ch lis)
  (let ((lis (read-word ch lis)))
    (cond ((string->number (lis->string lis))
           (make-token 'number lis))
          (else
           (scan-error "bad numeric format: " (lis->string lis))))))

(define (read-nested-comment ch lis lvl)

  (define-syntax if-followed-by
    (syntax-rules ()
      ((_ x char body ...)
       (let ((x (read-char)))
         (cond ((eof-object? x) (scan-error "EOF encountered in nested comment: "
                                            (lis->string (cons ch lis))))
               ((char=? char  x)  body ...)
               (else (read-nested-comment (peek-char) (cons x (cons ch lis)) lvl)))))))

  (read-char)
  (cond ((eof-object? ch) (scan-error "EOF encountered in nested comment: " (lis->string lis)))
        ((char=? #\| ch)
         (if-followed-by x #\#
           (cond ((= lvl 0) (make-token 'nested-comment (cons x (cons ch lis))))
                 ((> lvl 0) (read-nested-comment (peek-char) (cons x (cons ch lis)) (- lvl 1)))
                 (else  (scan-error "something went wrong: " (cons x (cons ch lis)) lvl)))))
        ((char=? #\# ch)
         (if-followed-by x #\|
           (read-nested-comment (peek-char) (cons x (cons ch lis)) (+ lvl 1))))
        (else
         (read-nested-comment (peek-char) (cons ch lis) lvl))))

;;;
;;; Emacs does not like char-set symtax....
;;;
(define char-special #[()\[\]{}" \\|;#])
(define delimiter  #[\s|"()\[\]{};'`,])

;;; For Emacs
;; (put 'if-followed-by 'scheme-indent-function 2)

(provide "lang/scheme/gauche")
