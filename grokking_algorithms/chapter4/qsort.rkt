#lang racket

(define (qsort lst)
  (cond
    [(<= (length lst) 1) lst]
    [else
     (define-values (pivote index) (choose lst))
     (define rest (drop= lst index))
     (append (qsort (less-than pivote rest))
             (list pivote)
             (qsort (bigger-than pivote rest)))
     ]))

(define (choose lst)
  (define index (random (length lst)))
  (values (list-ref lst index) index))

(define (drop= lst index)
  (append (take lst index) (drop lst (+ 1 index))))

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
