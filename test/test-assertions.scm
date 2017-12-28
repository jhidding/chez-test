(import (rnrs (6))
        (chez-test assertions))

(define (test-assert-raises)
  (assert-raises assertion-violation? (assert #f)))

(define (test-assert-raises-fail)
  (assert-raises assertion-violation? (raise 0)))

(define (test-assert-no-raise)
  (assert-no-raise (+ 1 1)))

(define (test-assert-no-raise-fail)
  (assert-no-raise (assert #f)))

(define (test-assert-raises-combos)
  (assert-raises assertion-violation?
                 (assert-no-raise (raise 0))))

(define (test-assert-equal)
  (assert-equal 1 1)
  (assert-equal '(a b c) (list 'a 'b 'c)))

(define (test-assert-equal-fail)
  (assert-equal 1 2))

(define (test-assert-eqv)
  (assert-eqv 1 1))

(define (test-assert-eqv-fail)
  (assert-eqv (list 1 2) (list 1 2)))
