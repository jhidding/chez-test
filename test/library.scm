(library (test library)
  (export test-library)
  (import (rnrs (6))
          (chez-test suite)
          (chez-test assertions))
  
  (define-test-suite test-library "Testing a library containing a test-suite")

  (define-test-case test-library test0 ()
    (assert #t)
    (assert-equal (+ 1 1) 2))
)
