#lang racket

;; remove the first euqal member from list
(define (rember a lst)
  (cond
    [(null? lst) lst]
    [(not (eq? a (car lst))) (cons (car lst) (rember a (cdr lst)))]
    [else (cdr lst)]))

(define (multirember a lst)
  (cond
    [(null? lst) '()]
    [(eq? a (car lst)) (multirember a (cdr lst))]
    [else (cons (car lst) (multirember a (cdr lst)))]))

(define (o+ n m)
  (cond
    [(zero? m) n]
    [else (add1 (o+ n (sub1 m)))]))

(define (o- n m)
  (cond
    [(zero? m) n]
    [else (sub1 (o- n (sub1 m)))]))

(define (x n m)
  (cond
    [(zero? m) 0]
    [else (o+ n (x n (sub1 m)))]))

(define (atom? v)
  (and (not (null? v))
       (not (pair? v))))

(define (eqan? n m)
  (cond
    [(and (number? n)
          (number? m))
     (= n m)]
    [(or (number? n)
         (number? m))
     #f]
    [else
     (eq? n m)]))

(define (eqlist? l1 l2)
  (cond
    [(and (null? l1) (null? l2)) #t]
    [(or (null? l1) (null? l2)) #f]
    [(and (atom? (car l1))
          (atom? (car l2)))
     (and (eqan? (car l1) (car l2))
          (eqlist? (cdr l1) (cdr l2)))]
    [(or (atom? (car l1))
         (atom? (car l2)))
     #f]
    [else
     (and (eqlist? (car l1) (car l2))
          (eqlist? (cdr l1) (cdr l2)))]))

(module+ test
  (require rackunit)
  
  (check-equal? (rember 'a '()) '())
  (check-equal? (rember 'a '(a)) '())
  (check-equal? (rember 'a '(a b)) '(b))
  (check-equal? (rember 'a '(b a)) '(b))
  (check-equal? (rember 'a '(b a b a)) '(b b a))

  (check-equal? (multirember 'a '()) '())
  (check-equal? (multirember 'a '(a)) '())
  (check-equal? (multirember 'a '(a b)) '(b))
  (check-equal? (multirember 'a '(b a)) '(b))
  (check-equal? (multirember 'a '(b a b a)) '(b b))

  (check-equal? (o+ 2 0) 2)
  (check-equal? (o+ 2 3) 5)

  (check-equal? (o- 3 2) 1)
  (check-equal? (o- 3 3) 0)

  (check-equal? (x 3 4) 12)
  (check-equal? (x 3 1) 3)
  (check-equal? (x 3 0) 0)

  (check-true (atom? 'a))
  (check-true (atom? 1))
  (check-false (atom? '()))
  (check-false (atom? '(1)))
  (check-false (atom? '(1 . 2)))

  (check-true (eqan? 1 1))
  (check-true (eqan? 'a 'a))
  (check-true (eqan? "a" "a"))
  (check-false (eqan? 1 'a))
  (check-false (eqan? "a" 'a))

  (check-true (eqlist? '(1 2) '(1 2)))
  (check-true (eqlist? '((1) 2) '((1) 2)))
  (check-true (eqlist? '() '()))
  (check-false (eqlist? '(1) '(2)))
  (check-false (eqlist? '(a b) '((a) b)))
  (check-false (eqlist? '((a) b) '((c) b)))
  
  "all test run")