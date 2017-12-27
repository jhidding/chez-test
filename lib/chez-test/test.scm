(library (chez-test test)
  (export test? make-test test-name test-fixtures test-function run-test)
  (import (rnrs (6)))

  (define-record-type test
    (fields name function fixtures))

  (define (run-test t)
    ((test-function t)))
)
