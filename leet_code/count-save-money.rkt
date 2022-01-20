#lang racket/base
;; https://leetcode-cn.com/problems/calculate-money-in-leetcode-bank/
(require racket/contract
         racket/match)

(define/contract (total-money n)
  (-> exact-integer? exact-integer?)
  ;; 计算给定天数总共有多少个完整星期以及最后一个星期结束时间是星期几
  ;; 每星期一的存钱金额是按星期递增的，而每星期内周一到周日的存钱数是按填递增的
  ;; 我们只要知道周一存入金额和当周存钱截止天数就能计算出该周应存入多少钱
  (define (sum-week-money begin-save to-week-day)
    (for/fold ([total 0]) ([week-day (in-range to-week-day)]
                            [money (in-naturals begin-save)])
               (+ total money)))

  ;; 计算总共星期数及最后一天是星期几
  (define-values (weeks last-day) (quotient/remainder n 7))
  ;; 构建星期清单
  (define week-list
    (let ([wl (for/list ([w (in-range weeks)])
                (list (add1 w) 7))])
      (match last-day
        [0 wl]
        [else (append wl (list (list (add1 weeks) last-day)))])))
  ;; 计算所有星期的存钱金额并累加
  (for/fold ([total 0]) ([w (in-list week-list)])
             (+ total (apply sum-week-money w))))
