(library (chez-test private text-cursors)

  (export text-cursor? make-text-cursor text-cursor-text text-cursor-start
          text-cursor-end with-text-cursor
          string->text-cursor text-cursor-ref text-cursor-select text-cursor-end?
          text-cursor-next text-cursor-flush)

  (import (rnrs (6))
          (monads record-contexts))

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
)
