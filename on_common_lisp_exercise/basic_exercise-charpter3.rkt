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

(displayln "cons list and pair")
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

(displayln "游程遍历压缩")
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

(displayln "游程遍历解压缩")
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

(displayln "occurrences")
(occurrences '(a b c a b a d))
(occurrences '(a b a d a c d c a))

;list 总是 pair 类型(null 例外)，但反过来不是
(displayln "difference of list and pair")
(pair? (cons 'a 'b)) ;#t
(list? (cons 'a 'b)) ;#f
(cdr (cons 'a 'b)) ;'b
(cdr (list 'a 'b)) ;'(b)
(list? '(a . b)) ;#f
(cons 'a 'b)

;和 common lisp 的表现不一样
(member '(a) '((a) (b))) ; not #f

;定义一个函数接受一个列表并返回把每个元素加上自己的位置的列表
(define (recusion-pos+ lst)
  (if (null? lst)
      lst
      (inner-pos+ 0 lst)))

(define (inner-pos+ n lst)
  (if (null? lst)
      lst
      (cons (+ (car lst) n) (inner-pos+ (+ n 1) (cdr lst)))))

(recusion-pos+ '(7 5 1 4)) ; '(7 6 3 7)
(recusion-pos+ null)


(define (iteration-pos+ lst)
  (if (null? lst)
      lst
      (let ([i 0] [result null])
        (for ([item (in-list lst)])
          (begin
            (set! result (append result (list (+ i item))))
            (set! i (+ i 1))))
        result)))

(iteration-pos+ '(7 5 1 4))

;这个函数实现有 bug
(define (showdots lst)
  "'(a b c) -> (A . (B . (C . NULL)))"
  (begin
    (if (null? lst)
        (display "NULL")
        (begin
          (let ([first (car lst)])
            (if (list? first)
                (showdots first)
                (display (string-append "(" (symbol->string first) " . "))))
          (showdots (cdr lst))
          ))
    (display ")")))

(showdots '(a b c))
(newline)
(showdots '(a b c () d))
(newline)

;关联列表 —— 列表中元素由 pair 组成的列表
(define trans '((+ . "add") (- . "subtract")))
trans

(define (our-assoc key alist)
  "一个受限版本的 assoc"
  (and (pair? alist)
       (let ([apair (car alist)])
         (if (equal? key (car apair))
             apair
             (our-assoc key (cdr alist))))))

(our-assoc '+ trans)
(our-assoc '- trans)
(our-assoc '* trans)

(pair? null) ;#f

(displayln "abc")
(display #\c)
(newline)

(substring "abc" 1)

(define (sublist lst from to)
  "(sublist '(a b c) 1 2) => '(b)"
  (define (collect-to n lst)
    (if (= n 0)
        null
        (cons (car lst) (collect-to (sub1 n) (cdr lst)))))
  (collect-to (- to from) (list-tail lst from)))

(displayln "function sublist demos")
(sublist '(a b c) 1 2)
(sublist '(a b c d e) 1 3)
(sublist '(a b c) 1 3)
;; (sublist '(a b c) 1 4) ;will raise error

(define (sublist2 lst from to)
  "(sublist '(a b c) 1 2) => '(b)"
  (take (list-tail lst from) (- to from)))

(displayln "function sublist2 demos")
(sublist2 '(a b c) 1 2)
(sublist2 '(a b c d e) 1 3)
(sublist2 '(a b c) 1 3)
;; (sublist2 '(a b c) 1 4) ;should raise error

(map (lambda (x)
       (* x 10))
     '(1 3 5 6 8))

;最短路径
;搜索网络中最短路径
(define (shortest-path start end net)
  (bfs end (list (list start)) net))

(define (bfs end queue net)
  (if (null? queue)
      null
      (let ([path (car queue)])
        (let ([node (car path)])
          (if (equal? node end)
              (reverse path)
              (bfs end
                   (append (cdr queue)
                           ;子节点加入队列末尾
                           (new-paths path node net))
                   net))))))

(define (new-paths path node net)
  (map (lambda (n)
         (cons n path))
       ;和 ANSI common lisp 的 assoc 不一样的是这里 assoc 如果没有找到值会返回 #f 而不是 '()
       (let ([apath (assoc node net)])
         (if apath
             (cdr apath)
             '()))))

(define min '((a b c) (b c) (c d)))
(shortest-path 'a 'd min)

(define min2 '((a b c) (b c d e) (c e d) (d e)))
(shortest-path 'a 'd min2)
(shortest-path 'a 'e min2)

;写一个程序来找到 3.15 节里表示的网络中,最长有限的路径 (不重复)。网络可能包含循环
(define (longest-path start end net)
  (dfs end (list (list start)) net '()))

(define (dfs end stack net long-path)
  (if (null? stack)
      (reverse long-path)
      (let ([path (car stack)])
        (let ([node (car path)])
          (if (and (equal? node end)
                   (> (length path) (length long-path)))
              ;收集所有可达路径，并比较长度
              (dfs end
                   (append (new-paths path node net)
                         (cdr stack))
                   net
                   path)
              (dfs end
                   (append (new-paths path node net)
                         (cdr stack))
                   net
                   long-path))))))

(longest-path 'a 'd min)
(longest-path 'a 'e min2)
