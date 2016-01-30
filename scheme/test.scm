(use gauche.test)
(test-start "lang.scheme.gauche")
(add-load-path "../..")
(use lang.scheme.gauche)
(test-module 'lang.scheme.gauche)

(with-input-from-file "gauche.scm" (cut port-for-each print-token gauche-scan))

(use text.tree)
(let ((x (with-input-from-file "gauche.scm" (cut  port-map token-string gauche-scan))))
  (with-output-to-file "fo.scm"
    (lambda () (write-tree x))))

(test-end :exit-on-failure #t)
