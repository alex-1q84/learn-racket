#lang racket

;; 4.1 请编写前述sum函数的代码。递归
(define (add a b)
  (+ a b))

(define (sum nums)
  (if (empty? nums)
      0
      (add (car nums) (sum (cdr nums)))))

;; 4.2 编写一个递归函数来计算列表包含的元素数。
(define (my-length lst)
  (if (empty? lst)
      0
      (add 1 (my-length (cdr lst)))))

;; 4.3 找出列表中最大的数字。
(define (big a b)
  (if (> a b) a b))

(define (my-max lst)
  (if (empty? (cdr lst))
      (car lst)
      (big (car lst) (my-max (cdr lst)))))

(module+ test
  (require rackunit rackunit/text-ui)

  (check-equal? (add 1 0) 1)
  (check-equal? (add 1 1) 2)

  (check-equal? (sum (list 1 2 3 4)) 10)
  (check-equal? (sum (list 1 2)) 3)
  (check-equal? (sum (list 1)) 1)
  (check-equal? (sum (list 0)) 0)
  (check-equal? (sum (list)) 0)

  (check-equal? (my-length '()) 0)
  (check-equal? (my-length '(1)) 1)
  (check-equal? (my-length '(1 2)) 2)

  (check-equal? (my-max (list 1)) 1)
  (check-equal? (my-max (list 2 1)) 2)
  (check-equal? (my-max (list 2 1 3)) 3)

  "all tests runs")
