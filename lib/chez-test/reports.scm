(library (chez-test reports)
  (export report? make-report report-n-successes report-n-failures report-failures
          report-add-result print-report)
  (import (rnrs (6))
          (srfi :48)
          (chez-test contexts))

  #| Reporting ============================================================= |#
  (define-context report
    (fields n-successes n-failures failures))

  (define (report-add-result report result)
    (with-report report
      (if (assertion-violation? result)
        (make-report
          n-successes (+ 1 n-failures) (cons result failures))
        (make-report
          (+ 1 n-successes) n-failures failures))))

  (define (print-report report)
    (with-report report
      (format #t "~a successes, ~a failures~%" n-successes n-failures)))
)
