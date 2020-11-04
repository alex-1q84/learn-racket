#lang racket

;; 4.1 请编写前述sum函数的代码。递归
(define (add a b)
  (+ a b))

(define (sum nums)
  (if (empty? nums)
      0
      (add (car nums) (sum (cdr nums)))))


(module+ test
  (require rackunit rackunit/text-ui)

  (check-equal? (add 1 0) 1)
  (check-equal? (add 1 1) 2)
  
  (check-equal? (sum (list 1 2 3 4)) 10)
  (check-equal? (sum (list 1 2)) 3)
  (check-equal? (sum (list 1)) 1)
  (check-equal? (sum (list 0)) 0)
  (check-equal? (sum (list)) 0)
  "all tests runs")