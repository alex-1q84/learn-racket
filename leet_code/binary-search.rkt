#lang racket/base
;; 二分搜索
;; https://leetcode-cn.com/problems/binary-search/
(require racket/contract)

(define/contract (search nums target)
  (-> (listof exact-integer?) exact-integer? exact-integer?)
  (define array (list->vector nums))

  (define (b-search from to)
    (if (> from to) #f
        (let ([mid (inexact->exact (quotient (+ to from) 2))])
        (cond
          [(= (vector-ref array mid) value) mid]
          [(> (vector-ref array mid) value) (b-search from (sub1 mid))]
          [(< (vector-ref array mid) value) (b-search (add1 mid) to)]))))

  (b-search 0 (sub1 (vector-length array))))

(define (middle a b)
  (floor (quotient (+ a b) 2)))
