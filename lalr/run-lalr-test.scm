(add-load-path "../..")
(add-load-path "./tests")
(use lang.lalr.lalr)

(define (main args)
  (let ((v (string->symbol (list-ref args 1)))
        (f (list-ref args 2)))
    (select-lalr-version v)
    (load f)
    (exit 0)))
