#lang racket/base
;; 二分搜索
;; https://leetcode-cn.com/problems/binary-search/
(require racket/contract)

(define/contract (search nums target)
  (-> (listof exact-integer?) exact-integer? exact-integer?)
  (define vnums (list->vector nums))
  (define (b-search from to)
    (define mid (middle from to))
    (cond
      [(= target (vector-ref vnums to))
       to]
      [(= target (vector-ref vnums mid))
       mid]
      [(= from mid)
       -1]
      [(> target (vector-ref vnums mid))
       (b-search mid to)]
      [(< target (vector-ref vnums mid))
       (b-search from mid)]))
  (b-search 0 (sub1 (vector-length vnums))))

(define (middle a b)
  (floor (quotient (+ a b) 2)))
