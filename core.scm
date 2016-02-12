;;;
;;;
;;;
(define-module lang.core
  (use gauche.parameter)
  (use gauche.record))
(select-module lang.core)

(define-condition-type <scan-error> <error> #f (lis))

;;
;; if use use ggc.port.colum,
;; port-current-colum <column-port> willbe available.
;;
(define-method port-current-column ((port <port>)) #f)

(define (scan-error msg lis . x)
  (error <scan-error> :lis lis msg (lis->string lis) x))

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
  (apply string (reverse lis)))

(define (lis->symbol lis)
  (string->symbol (lis->string lis)))

(provide "lang/core")
