#lang racket

;1. 将下列表达式翻译成没有使用 let 与 let* ,并使同样的表达式不被求值 2 次。
#|
(a) (let ((x (car y)))
      (cons x x))
|#
((lambda (y)
   (define x (car y))
   (cons x x)) '(a b))

#|
(b) (let* ((w (car x))
           (y (+ w z)))
      (cons w y))
|#
; 没实现...

;定义一个返回其实参平方的函数,而当实参是一个正整数且小于等于 5 时,不要计算其平方。
(define (square x)
  (if (and (> x 5) (exact-integer? x))
      (* x x)
      x))

(square 5)
(square 6)
(square 6.0)

; 日期计算
(define mon '(31 28 31 30 31 30 31 31 30 31 30 31))

mon

(define *month* (list->vector (reverse (foldl (lambda (x y)
                                                (cons (+ x (first y)) y))
                                              '(0)
                                              mon))))

*month*

;使用 case 与 svref 重写 month-num

(define *yzero* 2000)

(define (leap? y)
  (and (zero? (remainder y 4))
       (or (zero? (remainder y 400))
           (not (zero? (remainder y 100))))))

(leap? 1900)

#;(define (month-num m y)
    (+ (vector-ref *month* (- m 1))
       (if (and (> m 2) (leap? y)) 1 0)))

(define (month-num m y)
  (+ (vector-ref *month* (- m 1))
     (case (and (> m 2) (leap? y))
       [(#t) 1]
       [else 0])))

(month-num 3 2000)
(month-num 3 1999)

; 定义一个迭代与递归版本的函数,接受一个对象 x 与向量 v ,并返回一个列表,包含了向量 v 当中,所有直接在 x 之前的对象

(define (precedes-iter elt vec)
  (let ([lst '()] [vlen (vector-length vec)])
    (for ([i (in-range 1 vlen)])
      (when (equal? (vector-ref vec i) elt)
        (set! lst (cons (vector-ref vec (- i 1)) lst))))
    lst))

(precedes-iter 'a #(a b a c d a e f r a))

(define (precedes-recuration elt vec)
  (define (prec elt vec pos length result)
    (if (>= pos length)
        result
        (if (equal? (vector-ref vec pos) elt)
            (prec elt vec (+ pos 1) length (cons (vector-ref vec (- pos 1)) result))
            (prec elt vec (+ pos 1) length result))))
  (prec elt vec 1 (vector-length vec) '()))

(precedes-recuration 'a #(a b a c d a e f r a))

; 定义一个迭代与递归版本的函数,接受一个对象与列表,并返回一个新的列表,在原本列表的对象之间加上传入的对象
(define (intersperse obj lst)
  )


(intersperse '- '(a b c d))