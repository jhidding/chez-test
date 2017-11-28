(import (rnrs base (6))
        (rnrs io simple (6))

        (srfi :48)
        (chez-test cut)

        (only (chezscheme) file-regular? file-directory? directory-list directory-separator)
        (chez-test run)
        (chez-test generic))

(define (partial f . args1)
  (lambda args2
    (apply f (append args1 args2))))

(define (path-join first . rest)
  (let ((sep (make-string 1 (directory-separator))))
    (if (null? rest)
      first
      (apply path-join (string-append first sep (car rest)) (cdr rest)))))

(define (recursive-directory-list file? dir? path)
  (let ((ls    (map (cut path-join path <>)
                    (directory-list path)))
        (recur (cut recursive-directory-list file? dir? <>)))
    (apply append (filter file? ls) (map recur (filter dir? ls)))))

(define (find-tests path)
  (recursive-directory-list
    (lambda (f)
      (and (file-regular? f)
           (string-ends-with? ".scm" f)))
    (lambda (f)
      (and (file-directory? f)
           (string-starts-with? "test" f)))
    path))

(define (run-test-file path)
  (let ((program (read-all (open-input-file path))))
    (format #t "Running ~a~%" path)
    (print-report (run-tests program))))

(let* ((path  (cadr (command-line)))
       (files (find-tests path)))
  (for-each run-test-file files))
