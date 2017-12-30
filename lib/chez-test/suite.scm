(library (chez-test suite)
  (export define-test-suite define-test-case test-case run-suite make-suite suite?)

  (import (rnrs (6))
          (srfi :48)

          (monads)
          (monads maybe)
          (monads record-contexts)
          (monads parser)
          (monads receive)

          (chez-test reports)
          (chez-test tests)
          (chez-test private cut)
          (chez-test private colours))

  (define-record-context text-cursor
    (fields text start end))

  (define (text-cursor-end? tc)
    (with-text-cursor tc
      (= end (string-length text))))

  (define (string->text-cursor text)
    (make-text-cursor text 0 0))

  (define (text-cursor-ref tc)
    (with-text-cursor tc
      (string-ref text end)))

  (define (text-cursor-select tc)
    (with-text-cursor tc
      (substring text start end)))

  (define (text-cursor-next tc)
    (with-text-cursor tc
      (make-text-cursor text start (+ 1 end))))

  (define (text-cursor-flush tc)
    (with-text-cursor tc
      (make-text-cursor text end end)))

  (define (p:return value)
    (lambda (tc tgt)
      (values value tc tgt)))

  (define (p:>>= p f)
    (lambda (tc tgt)
      (receive (value td tgu) (p tc tgt)
        (if (failure? value)
          (values value td tgu)
          ((f value) td tgu)))))

  (define-record-type end)
  (define *end* (make-end))

  (define (p:split-on pred)
    (lambda (tc tgt)
      (cond
        ((text-cursor-end? tc)
         (values *end* tc (cons (text-cursor-select tc) tgt)))

        ((pred (text-cursor-ref tc))
         (values (text-cursor-ref tc)
                 (text-cursor-flush (text-cursor-next tc))
                 (cons (text-cursor-select tc) tgt)))

        (else
         (values (text-cursor-ref tc)
                 (text-cursor-next tc)
                 tgt)))))

  (define (p:pop tc tgt)
    (values (reverse tgt) tc '()))

  (define (p:repeat p)
    (seq-p (v <- p)
           (if (end? v)
             p:pop
             (p:repeat p))))

  (define (p:choice p . ps)
    (define (p:choice* p1 p2)
      (lambda (tc tgt)
        (receive (v td tgu) (p1 tc tgt)
          (if (failure? v)
            (p2 tc tgt)
            (values v td tgu)))))

    (fold-left p:choice* p ps))

  (define-monad p p:>>= p:return)

  (define (p:parse p str)
    (receive (value _1 _2) (p (string->text-cursor str) '())
      value))

  (define (string-tokenize str c)
    (p:parse
      (p:repeat (p:split-on (cut char=? #\newline <>)))
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
    (format #t "~a═══════════════════════════════════════════════════════════════~a~%" (set-colour-blue) (reset-colour))
    (fold-left
      (lambda (report test)
        (format #t "~a ... "
                (string-append
                  (symbol->string test)
                  (make-string (- 25 (string-length (symbol->string test))) #\space)))
        (let ((result (run-test (suite-get-test s test))))
          (if (failed? result)
            (format #t "~a😣  failed~%~a~%~a~a~%~a~a~a~%"
              (set-colour-red)
              "  ╭───────────────────────────────────────────────────────────────"
              (reset-colour)
              (text-indent (format #f "  ~a│~a " (set-colour-red) (reset-colour))
                           (remove-last-newline (result-message result)))
              (set-colour-red)
              "  ╰───────────────────────────────────────────────────────────────"
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
