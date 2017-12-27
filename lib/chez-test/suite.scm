(library (chez-test suite)
  (export define-test-suite define-test-case test-case
          test-predicate test-eqv test-exn test-no-exn test-equal)

  (import (rnrs (6))
          (srfi :48)

          (monads)
          (monads maybe)
          
          (chez-test test))

  #| Assertions ============================================================ |#
  (define (test-predicate pred obj)
    (assert (pred obj)))

  (define (test-eqv . args)
    (unless (apply eqv? args)
      (raise (assertion-violation
               'test-eqv
               "arguments not `eqv?`: `~s`"
               args))))

  (define (test-equal . args)
    (unless (apply equal? args)
      (raise (assertion-violation
               'test-equal
               "arguments not `equal?`: `~s`"
               args))))

  (define-syntax test-exn
    (lambda (x)
      (syntax-case x ()
        ((test-exn <exn-pred> <body> ...)
         #'(let ((passing? (call/cc
                             (lambda (return)
                               (with-exception-handler
                                 (lambda (y) (return (<exn-pred> y)))
                                 (lambda () <body> ... #f))))))
             (unless passing?
               (raise (assertion-violation
                        'test-exn
                        "`~s` did not throw expected exception `~s`"
                        '(<body> ...) '<exn-pred>))))))))

  (define-syntax test-no-exn
    (lambda (x)
      (syntax-case x ()
        ((test-no-exn <body> ...)
         #'(let ((passing? (call/cc
                             (lambda (return)
                               (with-exception-handler
                                 (lambda (y) (return #f))
                                 (lambda () <body> ... #t))))))
             (unless passing?
               (raise (assertion-violation
                        'test-exn
                        "`~s` threw unexpected exception"
                        '(<body> ...)))))))))

  #| Syntax ================================================================ |#
  (define-record-type suite
    (fields (mutable register) description))

  (define (suite-list s)
    (map car (suite-register s)))

  (define (suite-get-test s t)
    (cond
      ((assq t (suite-register s)) => cdr)
      (else *failure*)))

  (define (suite-add-test! s t f)
    (suite-register-set!
      s
      (cons (cons t f) (suite-register s))))

  (define-syntax define-test-suite
    (lambda (x)
      (syntax-case x ()
        ((define-test-suite <name> <description>)
         #'(define <name>
             (make-suite '() <description>))))))

  (define-syntax define-test-case
    (lambda (x)
      (syntax-case x ()
        ((define-test-case <suite> <name> (<fixtures> ...) <body> ...)
         #'(suite-add-test <suite> '<name>
                           (make-test '<name>
                                      (lambda (<fixtures> ...) <body> ...)
                                      '(<fixtures> ...))))
                                      
        ((define-test-case <suite> <name> <body>)
         #'(suite-add-test <suite> '<name>
                           (make-test '<name>
                                      (lambda () <body>) '()))))))

  (define-syntax test-case
    (lambda (x)
      (syntax-case x ()
        ((test-case <name> (<fixtures> ...) <body> ...)
         #'(run-test (make-test '<name>
                                (lambda (<fixtures> ...) <body> ...)
                                '(<fixtures> ...))))
                                      
        ((test-case <name> <body>)
         #'(run-test (make-test '<name>
                                (lambda () <body>) '()))))))
)
