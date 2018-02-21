(library (chez-test suite)
  (export define-test-suite define-test-case test-case run-suite make-suite suite?)

  (import (rnrs (6))
          (srfi :48)

          (monads failure)

          (chez-test reports)
          (chez-test tests)
          (chez-test private parsing)
          (chez-test private cut)
          (chez-test private colours))

  (define (string-tokenize str c)
    (p:parse
      (p:loop (p:split-on (cut char=? #\newline <>)))
      str))

  (define (string-join lst sep)
    (if (null? lst)
      ""
      (let loop ((result (car lst))
                 (rest (cdr lst)))
        (if (null? rest)
          result
          (loop (string-append result sep (car rest))
                (cdr rest))))))

  (define (text-indent pre text)
    (string-join
      (map (cut string-append pre <>)
           (string-tokenize text #\newline))
      (string #\newline)))

  #| Syntax ================================================================ |#
  (define-record-type suite
    (fields name (mutable register) description))

  (define (suite-list s)
    (reverse (map car (suite-register s))))

  (define (suite-get-test s t)
    (cond
      ((assq t (suite-register s)) => cdr)
      (else *failure*)))

  (define (suite-add-test! s t f)
    (suite-register-set!
      s
      (cons (cons t f) (suite-register s))))

  (define (remove-last-newline str)
    (cond
      ((string=? str "") "")
      ((eq? (string-ref str (- (string-length str) 1)) #\newline)
       (substring str 0 (- (string-length str) 1)))
      (else str)))

  (define (run-suite s)
    (format #t "Running test suite ~s~%" (suite-name s))
    (format #t "~a═══════════════════════════════════════════════════════════════~a~%"
            (set-colour-blue) (reset-colour))
    (fold-left
      (lambda (report test)
        (format #t "~a ... "
                (string-append
                  (symbol->string test)
                  (make-string (- 25 (string-length (symbol->string test))) #\space)))
        (let ((result (run-test (suite-get-test s test))))
          (if (failed? result)
            (format #t "~a😣  failed~%~a~a~%~a~a~%~a~a~a~%"
              (set-colour-red) (set-colour-dark-red)
              "  ╭────────────────────────────────────────────────────────────"
              (reset-colour)
              (text-indent (format #f "  ~a│~a " (set-colour-dark-red) (reset-colour))
                           (remove-last-newline (result-message result)))
              (set-colour-dark-red)
              "  ╰────────────────────────────────────────────────────────────"
              (reset-colour))
            (format #t "~a☺  passed~a~%"
                    (set-colour-green)
                    (reset-colour)))
          (report-add-result report result)))
      (make-report 0 0 '())
      (suite-list s)))

  (define-syntax define-test-suite
    (lambda (x)
      (syntax-case x ()
        ((define-test-suite <name> <description>)
         #'(define <name>
             (make-suite '<name> '() <description>))))))

  (define-syntax define-test-case
    (lambda (x)
      (syntax-case x ()
        ((define-test-case <suite> <name> <body>)
         #'(suite-add-test! <suite> '<name>
                           (make-test '<name>
                                      (lambda () <body>) '())))

        ((define-test-case <suite> <name> (<fixtures> ...) <body> ...)
         #'(suite-add-test! <suite> '<name>
                           (make-test '<name>
                                      (lambda (<fixtures> ...) <body> ...)
                                      '(<fixtures> ...)))))))


  (define-syntax test-case
    (lambda (x)
      (syntax-case x ()
        ((test-case <name> (<fixtures> ...) <body> ...)
         #'(apply-test (make-test '<name>
                                  (lambda (<fixtures> ...) <body> ...)
                                  '(<fixtures> ...))))

        ((test-case <name> <body>)
         #'(apply-test (make-test '<name>
                                  (lambda () <body>) '()))))))
)
