(library (chez-test private parsing)

  (export p:return p:>>= with-parsing seq-parsing p:parse
          p:split-on p:pop p:loop *end-p:loop* end-p:loop?
          p:choice)

  (import (rnrs (6))
          (monads)
          (monads receive)
          (chez-test private text-cursors))

  (define (p:return value)
    (lambda (tc tgt)
      (values value tc tgt)))

  (define (p:>>= p f)
    (lambda (tc tgt)
      (receive (value td tgu) (p tc tgt)
        (if (failure? value)
          (values value td tgu)
          ((f value) td tgu)))))

  (define-monad parsing p:>>= p:return)

  (define (p:parse p str)
    (receive (value _1 _2) (p (string->text-cursor str) '())
      value))

  (define-record-type end-p:loop)
  (define *end-p:loop* (make-end-p:loop))

  (define (p:split-on pred)
    (lambda (tc tgt)
      (cond
        ((text-cursor-end? tc)
         (values *end-p:loop* tc (cons (text-cursor-select tc) tgt)))

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

  (define (p:loop p)
    (seq-parsing
      (v <- p)
      (if (end-p:loop? v)
        p:pop
        (p:loop p))))

  (define (p:choice p . ps)
    (define (p:choice* p1 p2)
      (lambda (tc tgt)
        (receive (v td tgu) (p1 tc tgt)
          (if (failure? v)
            (p2 tc tgt)
            (values v td tgu)))))

    (fold-left p:choice* p ps))
)

