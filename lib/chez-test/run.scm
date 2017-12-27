(library (chez-test run)

  (export read-all run-test run-tests
          report? with-report print-report)

  (import (rnrs (6))
          (rnrs eval (6))

          (only (srfi :1 lists) append-map)
          (only (srfi :48) format)

          (match)

          (chez-test generic)
          (chez-test contexts))

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
         (let ((name (symbol->string (get-definition-name expr))))
           (and (< 4 (string-length name))
                (string-starts-with? "test" name)))))

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
      ((define ,name ,rest ...)
<<<<<<< HEAD
       name)
      (,default #f)))
=======
       name)))
>>>>>>> 40748d86833052bf424f56fa28e71406ce7c8375

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

  (define (get-program-body program)
    (filter (lambda (x) (not (import-clause? x))) program))

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
  (define (run-test env body name)
    (call/cc
      (lambda (return)
        (with-exception-handler
          (lambda (x)
            (if (assertion-violation? x)
              (return x)
              (raise x)))
          (lambda ()
            (eval `(let () ,@body (,name) 'success) env))))))

  #| Run all tests in a program and produce a report
   |#
  (define (run-tests program)
    (let* ((env   (apply environment (get-imports program)))
           (body  (get-program-body program))
           (tests (get-defined-tests program)))
      (fold-left
        (lambda (report test)
          (format #t "~a ... "
                  (string-append
                    (symbol->string test)
                    (make-string (- 25 (string-length (symbol->string test))) #\space)))
          (let ((result (run-test env body test)))
            (if (assertion-violation? result)
              (format #t ":( \x1B;[31m~a\x1B;[m~%"
                (apply format #f (condition-message result)
                        (condition-irritants result)))
              (format #t ":) \x1B;[32msuccess\x1B;[m~%"))
            (report-add-result report result)))
        (make-report 0 0 '())
        tests)))
)
