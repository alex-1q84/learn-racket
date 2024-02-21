#lang racket/base

(require racket/list)

(provide list-chunk)

(define (list-chunk lst n)
  (when (<= n 0)
    (raise-argument-error 'list-chunk "greater than zero" n))
  
  (if (null? lst)
      '()
      (cons (with-handlers
                ([exn:fail:contract?
                  (lambda (exn) lst)])
              (take lst n))
            (list-chunk (with-handlers
                            ([exn:fail:contract?
                              (lambda (exn) '())])
                          (drop lst n))
                        n))))
