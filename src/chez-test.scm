(import (rnrs base (6))
        (rnrs io simple (6))

        (chez-test run))

(let* ((f     (open-input-file "./test/test-self.scm"))
       (p     (read-all f)))
  (print-report (run-tests p)))
