#lang racket/base
(require racket/contract)

;; https://leetcode-cn.com/problems/xoh6Oh/
;; 整数除法

(define/contract (divide a b)
  (-> exact-integer? exact-integer? exact-integer?)
  (define-values (INT-MIN INT-MAX) (values (- (expt 2 31)) (- (expt 2 31) 1)))
  (cond
    [(and (= a INT-MIN) (= b -1)) INT-MAX]
    [else
     (define op (cond
                  [(not (equal? (negative? a) (negative? b))) -]
                  [else +]))

     (define-values (n m) (values (abs a) (abs b)))
     (let-values ([(answer _)
                   (for/fold ([ans 0] [d n])
                             ([i (in-range 31 -1 -1)])
                     (cond
                       [(>= (- (arithmetic-shift d (- i)) m) 0)
                        (values (+ ans (arithmetic-shift 1 i))
                                (- d (arithmetic-shift m i)))]
                       [else (values ans d)]))])
       (op answer))]))
