#lang racket

(require threading)

(define (read-calories file-path)
  (~> (open-input-file file-path)
      port->string
      (string-split _ "\n\n")
      (map (lambda (str)
             (string-split str "\n"))
           _)
      (map (lambda (group)
             (map string->number group))
           _)))

(define (group-sum groups)
  (map (lambda (x)
         (apply + x))
       groups))


(define calories (group-sum (read-calories "input01.txt")))

;; most calories an Elf carring
(apply max calories)

;; total of top 3 calories
(~> (sort calories >)
    (take _ 3)
    (apply + _))


(module+ tests
  (require rackunit)

  "all test run")