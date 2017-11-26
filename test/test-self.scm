(import (rnrs base (6))
        (only (srfi :1 lists) unfold))

(define (test-basic-arithmetic)
  (assert (= (+ 1 1) 2)))

(define test-lists
  (lambda ()
    (let ((a (cons 'x 'y)))
      (assert (eq? (car a) 'x))
      (assert (eq? (cdr a) 'y)))))

(define (not-a-test)
  '(dont run me!))

(define (test-fails)
  (assert (= 0 1)))

(define (test-uses-not-a-test)
  (assert (= (length (not-a-test)) 3)))

