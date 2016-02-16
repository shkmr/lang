;;; common-test.scm --
;;;

(cond-expand

 ;; -- Guile
 (guile
  (debug-enable 'backtrace)
  (use-modules (ice-9 syncase))
  (define-syntax when
    (syntax-rules ()
      ((_ ?expr ?body ...)
       (if ?expr
	 (let () ?body ...)
         #f)))))

 (else))

;(load "../lalr.scm")
(use lang.lalr.lalr)
(define pprint (with-module lang.lalr.lalr pprint))

(define make-lexical-token
  (case (lalr-version)
    ((2.4.1 v2.4.1 2.5.0 v2.5.0)
     (with-module lang.lalr.lalr make-lexical-token))
    ((2.1.0 v2.1.0)
     (lambda (type loc val)
       (cons type val)))))

(define lexical-token-category
  (case (lalr-version)
    ((2.4.1 v2.4.1 2.5.0 v2.5.0)
     (with-module lang.lalr.lalr lexical-token-category))
    ((2.1.0 v2.1.0)
     (lambda (token) token))))

(define *error* '())

(define (print-error expr result expected)
  (display "Failed test: \n")(pprint expr)    (newline)
  (display "\tresult was: ") (pprint result)  (newline)
  (display "\texpected: ")   (pprint expected)(newline))

(define-syntax check
  (syntax-rules (=>)
    ((_ ?expr => ?expected-result)
     (check ?expr (=> equal?) ?expected-result))

    ((_ ?expr (=> ?equal) ?expected-result)
     (let ((result	?expr)
	   (expected	?expected-result))
       (set! *error* '())
       (when (not (?equal result expected))
         (print-error '?expr result expected))))))

;;; --------------------------------------------------------------------

(define (display-result v)
  (if v
      (begin
        (display "==> ")
        (display v)
        (newline))))

(define eoi-token
  (make-lexical-token '*eoi* #f #f))

(define (make-lexer tokens)
  (lambda ()
    (if (null? tokens)
	eoi-token
      (let ((t (car tokens)))
	(set! tokens (cdr tokens))
	t))))

(define (error-handler message . args)
  (set! *error* (cons `(error-handler ,message . ,(if (pair? args)
						      (lexical-token-category (car args))
						    '()))
		      *error*))
  (cons message args))

;;; end of file
