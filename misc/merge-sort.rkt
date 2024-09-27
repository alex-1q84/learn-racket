#lang racket/base
(require racket/match
         racket/list)

(define (merge-sort lst cmp)
  (match lst
    [(list) '()]
    [(list a) lst]
    [(list lst-a lst-b ...) (merge (merge-sort `(,lst-a) cmp)
                                   (merge-sort lst-b cmp)
                                   cmp)]))

(define (merge lst-a lst-b cmp)
  (cond
    [(empty? lst-a) lst-b]
    [(empty? lst-b) lst-a]
    [else
     (match-let ([(list a lst-a-rest ...) lst-a]
                 [(list b lst-b-rest ...) lst-b])
       (if (cmp a b)
           (cons a (merge lst-a-rest lst-b cmp))
           (cons b (merge lst-a lst-b-rest cmp))))]))

;;====== tests======
(merge-sort '(1 5 3 4 5 8 9 7) <=)
(merge-sort '(1 5 3) <=)
(merge-sort '(1) <=)

(merge-sort (for/list ([i (in-range 100)])
              (random 1 1000))
            >=)