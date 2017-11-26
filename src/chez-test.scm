(import (rnrs base (6))
        (rnrs io simple (6))

        (match)
        
        (only (srfi :48) format)
        (only (srfi :1 lists) unfold append-map)
        (only (chezscheme) pretty-print))

(define (identity x) x)

(define (read-all port)
  (unfold port-eof? read identity port))

(define (import-clause? expr)
  (and (pair? expr) (eq? (car expr) 'import)))

(define (define-clause? expr)
  (and (pair? expr) (eq? (car expr) 'define)))

(define (string-starts-with? prefix str)
  (string=? (substring str 0 (string-length prefix))
            prefix))

(define (test-clause? expr)
  (and (define-clause? expr)
       (string-starts-with?
         "test" (symbol->string (get-definition-name expr)))))

(define (get-imports program)
  (append-map cdr (filter import-clause? program)))

(define (get-definition-name expr)
  (match expr
    ((define (,name . ,args) ,body ...)
     name)
    ((define ,name (lambda ,args ,body ...))
     name)))

(define (define->letrec-clause expr)
  (match expr
    ((define (,name . ,args) ,body ...)
     `(,name (lambda ,args . ,body)))
    ((define ,name (lambda ,args ,body ...))
     `(,name (lambda ,args . ,body)))))

(define (get-defined-tests program)
  (map get-definition-name (filter test-clause? program)))

(define (get-definitions program)
  (filter (lambda (expr) (not (import-clause? expr)))
          program))

(define (run-test env defs name)
  (call/cc
    (lambda (cc)
      (with-exception-handler
        (lambda (x)
          (if (assertion-violation? x)
            (cc 'fail)
            (raise x)))
        (lambda ()
          (eval `(letrec ,defs (,name) 'success) env))))))

(let* ((f     (open-input-file "chez-test/test/test-self.scm"))
       (p     (read-all f))
       (env   (apply environment (get-imports p)))
       (defs  (map define->letrec-clause (get-definitions p)))
       (tests (get-defined-tests p)))
  (for-each
    (lambda (t)
      (format #t "~a ... " (string-append (symbol->string t) (make-string (- 25 (string-length (symbol->string t))) #\space)))
      (format #t "~s~%" (run-test env defs t)))
    tests))