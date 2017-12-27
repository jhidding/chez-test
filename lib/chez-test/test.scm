(library (chez-test test)
  (export test? make-test test-name test-fixtures test-function apply-test run-test)
  (import (rnrs (6)))

  (define-record-type test
    (fields name function fixtures))

  (define (apply-test t)
    ((test-function t)))

  (define (run-test t)
    (call/cc
      (lambda (return)
        (with-exception-handler
          (lambda (x)
            (if (assertion-violation? x)
              (return x)
              (raise x)))
          (lambda ()
            (apply-test t)
            'success)))))
)
