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

;;
;;   If you use ggc.port.column,
;;
;;     Function: port-current-colum <column-port>
;;
;;   will be available.
;;   You need to shadow generic function created here
;;   by importing ggc.port.column into your scanner.
;;
;;   Ex.
;;     (use ggc.port.column)
;;     (use lang.c.c89-scan)
;;     (with-moudle lang.c.c89-scan (import ggc.port.column)
;;
;;   Not sure this is the right way...
;;   Cf. http://practical-scheme.net/wiliki/wiliki.cgi?Gauche%3AGenericFunctionとModule
;;
(define-method port-current-column ((port <port>)) #f)

(define-record-type token
  (%make-token type string value file line column)
  token?
  (type   token-type)
  (string token-string)
  (value  token-value)
  (file   token-file)
  (line   token-line)
  (column token-column)
  )

(define-method write-object ((obj token) port)
  (display #"~(token-file obj):~(token-line obj):~(token-column obj):~(token-type obj):" port)
  (write (token-string obj) port))

(define file   (make-parameter #f))
(define line   (make-parameter #f))
(define column (make-parameter #f))

(define (make-token type lis :optional (value #f))
  (%make-token type
               (lis->string lis)
               value
               (file)
               (line)
               (column)))

(define-syntax with-file/line/column-of
  (syntax-rules ()
    ((_ port body ...)
     (parameterize ((file   (port-name port))
                    (line   (port-current-line port))
                    (column (let1 x (port-current-column port)
                              (or x 0))))
       body ...))))

(define (lis->string lis)
  (cond ((pair? lis)   (apply string (reverse lis)))
        ((string? lis) lis)
        (else (scan-error "lis has to be either a list of characters or a string"))))

(define (lis->symbol lis)
  (string->symbol (lis->string lis)))

(provide "lang/core")
