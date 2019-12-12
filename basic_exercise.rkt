#lang racket
;使用 car 与 cdr 来定义一个函数,返回一个列表的第四个元素
(define (fourth-item lst)
  (car(cdr (cdr (cdr lst)))))

(fourth-item '(a b c d e))

;定义一个函数,接受两个实参,返回两者当中较大的那个
(define (max a b)
  (if (> a b)
      a
      b))

(max 1 2)

(define (help)
  (println "hello world"))

(help)

(if true 13 (/ 1 0)) ;短路

(car '(a b c))

;判断列表是否为空
(if (null? '()) 'a 'b)

;定义一个函数,它接受一个列表作为实参,如果有一个元素是列表时,就返回真
(define (contain-list? lst)
  (if (null? lst)
      false
      (if (list? (car lst))
          true
          (contain-list? (cdr lst)))))

(contain-list? '(a '(b c) d)) ;#t
(contain-list? '(a b c)) ;#f

;接受一个正整数,并打印出数字数量的点
(define (print-dots count)
  "递归版"
  (if (> count 0)
      (begin
        (print ".")
        (print-dots (- count 1)))
      'done)
  )

(print-dots 5) ;.....
(newline)
(print-dots 1) ;.

(define (print-dots-loop count)
  "迭代版"
  (for ([i count])
    (print ".")))

(print-dots-loop 5)
(newline)
(print-dots-loop 1)
(newline)

;接受一个列表,并返回 a 在列表里所出现的次数
(define (count-a-recursion lst)
  (if (null? lst)
      0
      (+ (count-a-recursion (cdr lst))
         (if (eq? 'a (car lst))
             1
             0))))

(println "result of count-a-recursion")
(count-a-recursion '())
(count-a-recursion '(b))
(count-a-recursion '(b a))
(count-a-recursion '(b a a))
(count-a-recursion '(a))
(count-a-recursion '(a b))
(count-a-recursion '(a a b))
(count-a-recursion '(a b a))

(define (count-a-loop lst)
  (let ([count 0])
    (for ([i (in-list lst)])
           (set! count (+ count (if (eq? 'a i)
                                      1
                                      0))))
    count))

(println "result of count-a-loop")
(count-a-loop '())
(count-a-loop '(b))
(count-a-loop '(b a))
(count-a-loop '(b a a))
(count-a-loop '(a))
(count-a-loop '(a b))
(count-a-loop '(a a b))
(count-a-loop '(a b a))

(null? '())
(null? null)
(eq? '() null)

;写一个函数,返回列表里所有非 null 元素的和
(define (summit-recursion lst)
  (if (null? lst)
      0
      (+ (summit-recursion (cdr lst))
         (if (null? (car lst))
             0
             (car lst)))))

(println "result of summit-recursion")
(summit-recursion (list '()))
(summit-recursion (list 1 2 '()))

(define (summit-iteration lst)
  (let ([count 0])
    (for ([i (in-list lst)])
      (if (null? i)
          'continue
          (set! count (+ count i))))
    count))

(println "result of summit-iteration")
(summit-iteration (list '()))
(summit-iteration (list 1 2 '()))


(println "string 操作")
(define (string-last str)
  (string-ref str (- (string-length str) 1)))

(string-last "abc")

(string->list "abc")