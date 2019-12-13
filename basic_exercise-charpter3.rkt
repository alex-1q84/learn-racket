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

(equal? (cons 'a null) (cons 'a null))