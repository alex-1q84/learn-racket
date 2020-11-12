#lang racket

(define (qsort lst)
  (cond
    [(<= (length lst) 1) lst]
    [else
     (define pivote (car lst))
     (append (qsort (less-than pivote (cdr lst)))
             (list pivote)
             (qsort (bigger-than pivote (cdr lst))))
     ]))

(define (less-than p lst)
  (filter (lambda (n) (<= n p)) lst))

(define (bigger-than p lst)
  (filter (lambda (n) (> n p)) lst))

(module+ test
  (require rackunit)

  (check-equal? (qsort '()) '())
  (check-equal? (qsort (list 1)) (list 1))
  (check-equal? (qsort (list 2 1)) (list 1 2))
  (check-equal? (qsort (list 2 1 3)) (list 1 2 3))
  (check-equal? (qsort (list 2 1 3 4)) (list 1 2 3 4))

  "all test run")
