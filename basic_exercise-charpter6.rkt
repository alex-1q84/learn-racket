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
(define (most2 fn lst)
  (if (null? lst)
      lst
      (let* ([champion (car lst)]
             [second null]
             [max (fn champion)])
        (for ([obj (cdr lst)])
          (let ([score (fn obj)])
            (when (> score max)
              (set! second champion)
              (set! champion obj)
              (set! max score))))
        (values champion second))))

(most2 length '((a b) (a b c) (a)))

; 定义一个函数,接受一个参数——一个数字,并返回目前传入参数中最大的那个
(define-values (max-ever)
  (let ([the-max null])
    (define (max-ever arg)
      (if (null? the-max)
          (set! the-max arg)
          (set! the-max (max the-max arg)))
      the-max)
    max-ever))

(displayln "max-ever")
(max-ever 1)
(max-ever 3)
(max-ever 2)
(max-ever 1)

(define max-ever2
  (let ([the-max null])
    (define (max-ever2 arg)
      (if (null? the-max)
          (set! the-max arg)
          (set! the-max (max the-max arg)))
      the-max)
    max-ever2))

(displayln "max-ever2")
(max-ever2 1)
(max-ever2 3)
(max-ever2 2)
(max-ever2 1)

; 定义一个函数,接受一个参数——一个数字,若传入参数比上个参数大时,返回真。函数第一次调用时应返回 null
(define big-than-previous-ever
  (let* ([prev null]
         [comp null])
    (define (big-than-previous-ever current)
      (unless (null? prev)
        (set! comp (> current prev)))
      (set! prev current)
      comp)
    big-than-previous-ever))

(displayln "big-than-previous-ever")
(big-than-previous-ever 5)
(big-than-previous-ever 1)
(big-than-previous-ever 2)

; 假设 expensive 是一个接受一个参数的函数,一个介于 0 至 100 的整数(包含 100),返回一个耗时的计算结果。
; 定义一个函数 frugal 来返回同样的答案,但仅在没见过传入参数时调用 expensive
(define (expensive num)
  (displayln (format "calling expensive with ~A" num))
  num)

(define frugal
  (let ([cache (make-hash)])
    (define (inner-expensive num)
      (let ([result
             (with-handlers ([exn:fail?
                              (lambda (exn)
                                (hash-set! cache num (expensive num)))])
               (hash-ref cache num))])
        (if (void? result)
            (hash-ref cache num)
            result)))
    inner-expensive))

(displayln "frugal")
(frugal 1)
(frugal 1)
(frugal 2)
(frugal 2)
(frugal 3)
(frugal 4)

; 这个函数接受一个单参数函数作为参数，并返回一个可缓存的相同功能函数
(define (cacheable fn)
  (let ([cache (make-hash)])
    (define (inner-expensive num)
      (let ([result
             (with-handlers ([exn:fail?
                              (lambda (exn)
                                (hash-set! cache num (fn num)))])
               (hash-ref cache num))])
        (if (void? result)
            (hash-ref cache num)
            result)))
    inner-expensive))

(define frugal2
  (cacheable expensive))

(displayln "frugal2")
(frugal2 1)
(frugal2 1)
(frugal2 2)
(frugal2 2)
(frugal2 3)
(frugal2 4)

(displayln "format 的使用")
(display (format "~A~%" "abc"))