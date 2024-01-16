#lang racket/base
(require threading)

(define (string-reverse str)
  (~> str
      string->list
      reverse
      list->string))

(define (reverse-number num)
  (string->number (string-reverse (number->string num))))

(define (find-reverse-of-4x num)
  (let ([4x (* 4 num)])
    (cond
      [(eq? 4x (reverse-number num)) num]
      [(> 4x 999999) -1]
      [else (find-reverse-of-4x (add1 num))])))