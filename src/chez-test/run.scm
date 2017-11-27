(library (chez-test run)

  (export read-all run-test run-tests
          report? with-report print-report)

  (import (rnrs (6))
          (rnrs eval (6))

          (only (srfi :1 lists) unfold append-map)
          (only (srfi :48) format)

          (match)
          (chez-test contexts))

  #|  Generic utility functions
   | ====================================================================== |#

  #| Identity function.
   |#
  (define (identity x) x)

  #| Tests whether a string starts with a certain prefix.
   |#
  (define (string-starts-with? pre str)
    (string=? (substring str 0 (string-length pre))
              pre))

  #| Read all expressions from a port.
   |#
  (define (read-all port)
    (unfold port-eof? read identity port))


  #|  Parsing program structure
   | ====================================================================== |#

  #| Tests whether an expression is an import statement.
   |#
  (define (import-clause? expr)
    (and (pair? expr) (eq? (car expr) 'import)))

  #| Tests whether an expression is a definition.
   |#
  (define (define-clause? expr)
    (and (pair? expr) (eq? (car expr) 'define)))

  #| Tests wether an expression defines a unit test. Unit tests are functions
   | whose name starts with `test`.
   |#
  (define (test-clause? expr)
    (and (define-clause? expr)
         (string-starts-with?
           "test" (symbol->string (get-definition-name expr)))))

  #| Obtains all import clauses from a program.
   |#
  (define (get-imports program)
    (append-map cdr (filter import-clause? program)))

  #| Extract the name symbol from a function definition.
   |#
  (define (get-definition-name expr)
    (match expr
      ((define (,name . ,args) ,body ...)
       name)
      ((define ,name (lambda ,args ,body ...))
       name)))

  #| Transform a function definition to a valid `letrec` clause.
   |#
  (define (define->letrec-clause expr)
    (match expr
      ((define (,name . ,args) ,body ...)
       `(,name (lambda ,args . ,body)))
      ((define ,name (lambda ,args ,body ...))
       `(,name (lambda ,args . ,body)))))

  #| Extract all named test definitions from a program.
   |#
  (define (get-defined-tests program)
    (map get-definition-name (filter test-clause? program)))

  #| Get all definitions from a program.
   |#
  (define (get-definitions program)
    (filter define-clause? program))


  #|  Running the tests
   | ====================================================================== |#

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

  #| Run a test, if it raises an assertion-violation this function
   | return the exception, otherwise return `success`.
   |#
  (define (run-test env defs name)
    (call/cc
      (lambda (cc)
        (with-exception-handler
          (lambda (x)
            (if (assertion-violation? x)
              (cc x)
              (raise x)))
          (lambda ()
            (eval `(letrec ,defs (,name) 'success) env))))))

  #| Run all tests in a program and produce a report
   |#
  (define (run-tests program)
    (let* ((env   (apply environment (get-imports program)))
           (defs  (map define->letrec-clause (get-definitions program)))
           (tests (get-defined-tests program)))
      (fold-left
        (lambda (report test)
          (format #t "~a ... "
                  (string-append
                    (symbol->string test)
                    (make-string (- 25 (string-length (symbol->string test))) #\space)))
          (let ((result (run-test env defs test)))
            (if (assertion-violation? result)
              (format #t ":( \x1B;[31m~a\x1B;[m~%" (condition-message result))
              (format #t ":) \x1B;[32msuccess\x1B;[m~%"))
            (report-add-result report result)))
        (make-report 0 0 '())
        tests)))
)
