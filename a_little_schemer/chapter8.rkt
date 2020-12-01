#lang racket

(define (evens-only* lst)
  (cond
    [(null? lst) (list)]
    [(atom? (car lst))
     (cond
       [(even? (car lst))
        (cons (car lst) (evens-only* (cdr lst)))]
       [else
        (evens-only* (cdr lst))])]
    [else
     (cons (evens-only* (car lst))
           (evens-only* (cdr lst)))]))

(define (atom? a)
  (and (not (pair? a))
       (not (null? a))))

(define (evens-only*&co lst col)
  (cond
    [(null? lst)
     (col '() 1 0)]
    [(atom? (car lst))
     (cond
       [(even? (car lst))
        (evens-only*&co (cdr lst)
                        (lambda (newlst p s)
                          (col (cons (car lst) newlst)
                               (* (car lst) p) s)))]
       [else
        (evens-only*&co (cdr lst)
                        (lambda (newlst p s)
                          (col newlst
                               p (+ (car lst) s))))])]
    [else
     (evens-only*&co (car lst)
                     (lambda (al ap as)
                       (evens-only*&co (cdr lst)
                                       (lambda (dl dp ds)
                                         (col (cons al dl)
                                              (* ap dp)
                                              (+ as ds))))))]))

(module+ test
  (require rackunit)

  (check-equal? (evens-only* '(1 2 3 4)) '(2 4))
  (check-equal? (evens-only* '(1 (2) 3 4)) '((2) 4))

  (define (the-last-friend lst product sum)
    (cons sum
          (cons product
                lst)))
  
  (check-equal? (evens-only*&co '(1 2 3 4 (5 6)) the-last-friend) '(9 48 2 4 (6)))


  
  "all test run")