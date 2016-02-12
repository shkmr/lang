;;; null port experiment
(use gauche.parameter)
(use gauche.vport)
(use gauche.time)

(define (open-null-input-port)
  (let ((p (make <virtual-input-port>)))
    (define (getb) (eof-object))
    (define (ready char?) #t)
    (slot-set! p 'getb getb)
    (slot-set! p 'ready ready)
    p))
(define (open-null-output-port)
  (let ((p (make <virtual-output-port>)))
    (define (putb c) )
    (slot-set! p 'putb putb)
    (slot-set! p 'puts putb)
    p))
(define null-input-port  (make-parameter (open-null-input-port)))
(define null-output-port (make-parameter (open-null-output-port)))

(define (process)
  (let lp ((ch (read-char))
           (count 1))
    (cond  ((eof-object? ch)
            (= count
               (port-current-line (current-input-port))))
           ((char=? #\newline ch)
            (write-char ch)
            (lp (read-char) (+ count 1)))
           (else
            (write-char ch)
            (lp (read-char) count)))))

(define (vport)
  (with-input-from-file "/usr/share/dict/words"
    (lambda ()
      (with-output-to-port (null-output-port) process))))

(define (file)
  (with-input-from-file "/usr/share/dict/words"
    (lambda ()
      (with-output-to-file "/dev/null" process))))

(time-these/report '(cpu 5.0) `((vport . ,vport)
                                (file  . ,file)))
