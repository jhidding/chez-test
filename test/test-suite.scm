(import (rnrs (6))
        (srfi :48)
        (chez-test suite)
        (chez-test reports))

(define (test-test-exn)
  (test-exn assertion-violation? (assert #f)))

(define (test-test-exn-fail)
  (test-exn assertion-violation? (raise 0)))

(define (test-test-no-exn)
  (test-no-exn (+ 1 1)))

(define (test-test-no-exn-fail)
  (test-no-exn (assert #f)))

(define (test-test-exn-combos)
  (test-exn assertion-violation? (test-no-exn (raise 0))))

(define (test-test-equal)
  (test-equal 1 1)
  (test-equal '(a b c) (list 'a 'b 'c)))

(define (test-test-equal-fail)
  (test-equal 1 2))

(define (test-test-eqv)
  (test-eqv 1 1))

(define (test-test-eqv-fail)
  (test-eqv (list 1 2) (list 1 2)))

(define (test-test-suite)
  (define-test-suite my-suite "test defining a suite")

  (define-test-case my-suite test1 ()
    (assert (= (+ 1 1) 2)))

  (define-test-case my-suite test2 ()
    (assert (= (* 6 7) 42)))

  (format #t "~%")
  (let ((report (run-suite my-suite)))
    (assert (= 0 (report-n-failures report)))
    (format #t "                          ... ")))
