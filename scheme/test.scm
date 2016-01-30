(use gauche.test)
(test-start "lang.scheme.gauche")
(add-load-path "../..")
(use lang.scheme.gauche)
(test-module 'lang.scheme.gauche)

(with-input-from-file "gauche.scm" (cut port-for-each print-token scan-gauche))

(use text.tree)
(let ((x (with-input-from-file "gauche.scm" (cut  port-map token-string scan-gauche))))
  (with-output-to-file "fo.scm"
    (lambda () (write-tree x))))


(test-end :exit-on-failure #t)


