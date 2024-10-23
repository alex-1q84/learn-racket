#lang racket
;; 毕氏三数组求取方法

(define (bis-num q interval total)
  (let* ([p (+ q interval)]
              [p2 (expt p 2)]
              [q2 (expt q 2)]
              [a (* 2 p q)]
              [b (- p2 q2)]
              [c (+ p2 q2)])
    (if (zero? total)
        null
        (cons (list a b c) (bis-num (add1 q) interval (sub1 total))))))

(bis-num 1 3 5)