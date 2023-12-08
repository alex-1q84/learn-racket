#lang racket

; 随机选择一个餐，吃它，每周每种餐有数量上限，吃过一次减 1
; 备选餐可以预先设置
; 不同餐可设置不同的概率

(define (what-to-eat)
  null)

(define (weights->weight-buckets weights)
  (for/fold ([base 0]
             [buckets null]
             #:result (reverse buckets))
            ([w (in-list weights)])
    (values (+ base w) (cons (+ base w) buckets))))

(define (random-choice lst weights)
  (let* ([weight-buckets (weights->weight-buckets weights)]
         [seed (apply max weight-buckets)]
         [rand (random seed)]
         [idx (for/fold ([item 0]) ([m (in-list weight-buckets)]
                                    #:break (< rand m))
                ;(println (format "seed ~A rand ~A buckets ~A" seed rand weight-buckets))
                (add1 item))])
    (list-ref lst idx)))

(module+ test
  (require rackunit)

  (what-to-eat)
  (define-values (a b) (for/fold ([a 0] [b 0]) ([i (in-range 100000)])
                         (if (eq? 'abc (random-choice '(abc 123) '(1 5)))
                             (values (add1 a) b)
                             (values a (add1 b)))))
  (printf (format "a ~A\nb ~A\na/b ~A" a b (exact->inexact (/ a b))))
  (check-true (< (abs (- (/ 1 5) (/ a b))) 0.01) (format "~A" (exact->inexact (abs (- (/ 1 5) (/ a b))))))
  (println "all tests pass")
  )