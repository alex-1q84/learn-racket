#lang racket

(define (hello name)
  ;; 两个输出效果一样
  (displayln (string-join `("hello" ,name) " "))
  (displayln (string-join (list "hello" name) " ")))

(hello "world")
