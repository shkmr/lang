;;;
;;;
;;;
(define-module lang.core
  (use gauche.parameter)
  (use gauche.record))
(select-module lang.core)

(define-condition-type <scan-error> <error> #f (lis))

(define (scan-error msg lis . x)
  (error <scan-error> :lis lis msg (lis->string lis) x))

(define-record-type token
  (%make-token type string file line)
  token?
  (type   token-type)
  (string token-string)
  (file   token-file)
  (line   token-line))

(define-method write-object ((obj token) port)
  (display #"~(token-file obj):~(token-line obj):~(token-type obj):" port)
  (write (token-string obj) port))

(define file (make-parameter #f))
(define line (make-parameter #f))

(define (make-token type lis)
  (%make-token type
               (lis->string lis)
               (file)
               (line)))

(define (print-token token)
  (display #"~(token-file token):~(token-line token):~(token-type token)")
  (display ":") (write (token-string token)) (newline)
  )

(define (lis->string lis)
  (apply string (reverse lis)))

(define (lis->symbol lis)
  (string->symbol (lis->string lis)))

(provide "lang/core")
