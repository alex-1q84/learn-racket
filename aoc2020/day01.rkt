#lang racket

(define (tow-entry-of-2020 lnums)
  (cond
    [(null? lnums) #f]
    [else
     (define a (car lnums))
     (define n (find-rest-for-2020 a (cdr lnums)))
     (cond
       [(boolean? n) (tow-entry-of-2020 (cdr lnums))]
       [else (list a n)])]))

(define (find-rest-for-2020 a lnums)
  (cond [(null? lnums) #f]
        [(= 2020 (+ a (car lnums))) (car lnums)]
        [else (find-rest-for-2020 a (cdr lnums))]))

(define lnums (call-with-input-file "input01.txt"
  (lambda (in)
    (for/list ([line (in-lines in)])
      (string->number line)))))

(apply * (tow-entry-of-2020 lnums))

(define sum/2020 (for*/list ([a (in-list lnums)]
            [b (in-list lnums)]
            [c (in-list lnums)]
            #:unless (eqv? a b)
            #:unless (eqv? b c)
            #:when (= 2020 (+ a b c)))
  (list a b c)))

(define (*/sum lsums)
  (cond
    [(null? lsums) null]
    [else
     (cons (apply * (car lsums)) (*/sum (cdr lsums)))]))

(*/sum sum/2020)

(module+ test
  (require rackunit)

  (check-equal? (tow-entry-of-2020 '(1 3 5 2021 2019 2015)) '(1 2019))
  "all test run")