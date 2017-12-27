(import (rnrs (6))
        (chez-test suite))

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
