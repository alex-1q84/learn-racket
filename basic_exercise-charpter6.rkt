#lang racket

; common lisp 的共享变量的多个闭包，在 racket 里看起来没法这样实现——局部变量 counter 可以被使用位置看到
(let ([counter 0])
  (define (reset)
    (set! counter 0)
    counter)
  (define (stamp)
    (set! counter (+ counter 1))
    counter)
  (list (stamp) (stamp) (reset) (stamp)))

; racket 版本的共享变量的多个闭包
(define-values (stamp reset)
  (let ([counter 0])
  (define (reset)
    (set! counter 0)
    counter)
  (define (stamp)
    (set! counter (+ counter 1))
    counter)
  (values stamp reset)))

(list (stamp) (stamp) (reset) (stamp))

; 根据某个评分函数(scoring function),返回列表中最高分的元素。它返回两个值,获胜的元素以及它的分数
(define (most fn lst)
  (if (null? lst)
      lst
      (let* ([wins (car lst)]
            [max (fn wins)])
        (for ([obj (cdr lst)])
          (let ([score (fn obj)])
            (when (> score max)
              (set! wins obj)
              (set! max score))))
        (values wins max))))

(most length '((a b) (a b c) (a)))

; 修改 most 函数,使其返回 2 个数值,一个列表中最高分的两个元素