(import (rnrs (6))
        (srfi :48)
        (chez-test suite)
        (chez-test reports))


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
