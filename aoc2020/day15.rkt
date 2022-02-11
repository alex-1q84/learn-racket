#lang racket/base

(require racket/string
         racket/list)

(define start-nums
  (call-with-input-file "input15.txt"
    (lambda (in)
      (map string->number (string-split (read-line in) ",")))))

;; 用 hash 表来存储每个数字最近出现的位置
;; 用计数器来计算当前报的数字是第几次报的

(define (interp nums turns)
  (define counts (make-hasheq))
  ;; 用起始数字初始化 counts
  (for ([(c i) (in-indexed nums)])
    (hash-set! counts c (add1 i)))
  ;; 轮流报数
  (for/fold ([prev (last nums)])
            ([turn (in-range (length nums) turns)])
    (define new-count
      (let ([t (hash-ref counts prev #f)])
        (if t (- turn t) 0)))
    (hash-set! counts prev turn)
    new-count))

(interp start-nums 2020)
