#lang racket

;列表

;写一个保留原本列表中元素顺序的 union 版本
(define (new-union a-lst b-lst)
  (let ([result a-lst])
    (for ([i (in-list b-lst)])
      (if (member i a-lst)
          'continue
          (set! result (append result (list i)))))
    result))

(new-union '(a b c) '(b a d e f g)) ;(a b c d e f g)


(define (new-union-recursion a-lst b-lst)
  (if (null? b-lst)
      a-lst
      (if (member (car b-lst) a-lst)
          (new-union-recursion a-lst (cdr b-lst))
          (new-union-recursion (append a-lst (list (car b-lst))) (cdr b-lst)))))

(new-union-recursion '(a b c) '(b a d e f g)) ;(a b c d e f g)

(equal? (cons 'a null) (cons 'a null)) ;#t 类似 java 中的 equals
(eq? (cons 'a null) (cons 'a null)) ;#f 类似 java 中的 ==
(eq? 1 1)
(eq? (* 6 7) 42)
(eq? #\a #\a) ;#t
(eq? (integer->char 67) (integer->char 67)) ;#t
(eq? (integer->char 255) (integer->char 255)) ;#t
(eq? (integer->char 256) (integer->char 256)) ;#f
(eq? (integer->char 955) (integer->char 955)) ;#f

(println "cons list and pair")
(cons? (cons 'a null))
(pair? (cons 'a null))
(cons? (cons 'b (cons 'a null)))
(list? (cons 'b (cons 'a null)))
(pair? (cons 'b (cons 'a null)))
;list 可以看作由第一个元素和剩余所有元素组成的 pair
(pair? (list 'a 'b 'c)) ;#t


;游程遍历
(define (compress lst)
  "压缩"
  (if (pair? lst)
      (compr (car lst) 1 (cdr lst))
      lst))

(define (compr element n lst)
  (if (null? lst)
      (list (n-elements element n))
      (let ([next (car lst)])
        (if (equal? next element)
            (compr element (+ n 1) (cdr lst))
            (cons (n-elements element n) (compr next 1 (cdr lst)))))))

(define (n-elements element n)
  (if (> n 1)
      (list n element)
      element))

(println "游程遍历压缩")
(compress '(1 1 1 0 1 0 0 0 0 1))
(compress '())
(compress '(1))

(define (uncompress lst)
  (if (null? lst)
      null
      (append (uncompr (car lst)) (uncompress (cdr lst)))))

(define (uncompr lst)
  (if (list? lst)
      (make-list (car lst) (car (cdr lst)))
      (list lst)))

(println "游程遍历解压缩")
(uncompress '((3 1) 0 1 (4 0) 1))

;定义任意数量参数函数
(define 2sum (lambda lst
  (* 2 (apply + lst))))

(2sum 1 2 3 4)

;定义必选参数加可选参数函数
(define carry 
  (lambda (a . b)
    (if (not (null? b))
        (my-pow a (car b))
        a)))

(define (my-pow a b)
  (if (<= b 1)
      a
      (my-pow (* a a) (- b 1))))


(carry 2)
(carry 2 1)
(carry 2 2)

;定义一个函数,接受一个列表并返回一个列表,指出相等元素出现的次数,并由最常见至最少见的排序
;(occurrences '(a b a d a c d c a))
;((A . 4) (C . 2) (D . 2) (B . 1))

;符号顺序比较
(symbol<? 'a 'b)

(define (occurrences lst)
  (sort (my-count (sort lst symbol<?)) (lambda (a b) (> (cdr a) (cdr b)))))


(define (my-count lst)
  "'(a b a d a c d c a) -> ((A . 4) (C . 2) (D . 2) (B . 1))"
  (if (pair? lst)
      (my-cnt (car lst) 1 (cdr lst))
      lst))

(define (my-cnt element n lst)
  (if (null? lst)
      (list (cons element n))
      (let ([next (car lst)])
        (if (equal? next element)
            (my-cnt element (+ n 1) (cdr lst))
            (cons (cons element n) (my-cnt next 1 (cdr lst)))))))

(println "occurrences")
(occurrences '(a b c a b a d))
(occurrences '(a b a d a c d c a))

;list 总是 pair 类型，但反过来不是
(println "difference of list and pair")
(pair? (cons 'a 'b)) ;#t
(list? (cons 'a 'b)) ;#f
(cdr (cons 'a 'b)) ;'b
(cdr (list 'a 'b)) ;'(b)
(list? '(a . b)) ;#f
(cons 'a 'b)