#lang racket

(define (first-attack-num nums preamble-count)
  (for*/first ([i (in-range preamble-count (vector-length nums))]
               #:when (not (contain-count (vector-ref nums i)
                                          (take-preamble nums i preamble-count))))
    (vector-ref nums i)))

(define (contain-count sum nums)
  (for*/first ([a-pos (in-range (vector-length nums))]
               [b-pos (in-range (add1 a-pos) (vector-length nums))]
               [expect (in-value (- sum (vector-ref nums a-pos)))]
               #:when (eq? (vector-ref nums b-pos) expect))
    (list (vector-ref nums a-pos) (vector-ref nums b-pos))))

(define (take-preamble nums from preamble-count)
  (vector-take-right (vector-take nums from) preamble-count))


(define (find-contiguous-run nums num)
  ;; 从头开始累加，如果刚好累加到期望值 num , 则返回已累加的数字列表，
  ;; 否则如果累加值大于期望值 num,则减去第一个值，直到累加值小于期望值，则继续累加一个新值到末尾，
  ;; 如此循环，知道得到期望数列或遍历完整个 nums 数列
  (let loop ([acc (car nums)]
             [contiguous (list (car nums))]
             [rest (cdr nums)])
    (cond
      [(= acc num) contiguous]
      [(empty? rest) #f]
      [(< acc num) (loop (+ acc (car rest))
                         (append contiguous
                                 (list (car rest)))
                         (cdr rest))]
      [(> acc num) (loop (- acc (car contiguous))
                         (cdr contiguous)
                         rest)])))


(define nums
  (call-with-input-file "input09.txt"
    (lambda (in)
      (for/vector ([line (in-lines in)])
        (string->number line)))))

(define attack-num (first-attack-num nums 25))

(let ([contiguous (find-contiguous-run (vector->list nums) attack-num)])
  (+ (apply min contiguous)
     (apply max contiguous)))

(module+ test

  (define nums #(15 25 47 40 62 55 65 95 102 117 150 182 127 219 299 277 309 576))
  (define attack-num (first-attack-num nums 5))

  (let ([contiguous (find-contiguous-run (vector->list nums) attack-num)])
    (+ (apply min contiguous)
       (apply max contiguous)))

  "all test run")
