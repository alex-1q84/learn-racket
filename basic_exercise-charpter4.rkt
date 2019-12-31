#lang racket

;向量，类似一维数组
(vector 1 2 3)
(vector? '#(1 2 3 a))

(list->string (sort (string->list "elbow") char<?))

;使用 reduce 函数来定义 copy-list 和 reverse(针对列表)
;Racket 中没有预定义 reduce 函数，相应地有 foldl 和 foldr 函数
#;(define (reduce func lst)
    (if (null? (cdr lst))
        (car lst)
        (func (car lst) (reduce func (cdr lst)))))

(define (my-reduce func lst)
  (if (null? lst)
      lst
      (foldl func (car lst) (cdr lst))))

(define my-add
  (lambda (a b)
    (+ a b)))

(my-reduce my-add
           '(1 2 3 4))
(my-reduce my-add '())

(define (copy-list lst)
  (foldr cons '() lst))

(copy-list '(a b c d))

(define (reverse lst)
  (foldl cons '() lst))

(reverse '(a b c d))


;结构 struct
;最常规的结构体定义
(struct a-struct (left mid right) #:inspector #f)

(a-struct 1 2 3)

;定义一个可以当作函数执行的 struct
(struct mood-procedure (base rating)
  ; 使用 prop:procedure 指定这个结构体的实例当作函数执行时应该取哪个值作为具体执行的函数
  ; struct-field-index 即是指定具体 procedure 定义位置的
  #:property prop:procedure (struct-field-index base))

(define happy+ (mood-procedure add1 10))
(happy+ 2) ;3
(happy+ 4) ;5

(struct mood-procedure2 (base rating)
  ; 使用 prop:procedure 指定这个结构体的实例当作函数执行时应该取哪个值作为具体执行的函数
  ; struct-field-index 即是指定具体 procedure 定义位置的
  #:property prop:procedure 0)

(define happy2+ (mood-procedure2 add1 10))
(happy2+ 2)


;二叉搜索树
;define-struct 默认会提供 #:extra-constructor-name 提供一个 make- 前缀的 struct 构造方法，
;如果我们既没有指定 #:extra-constructor-name 也没有指定 #:constructor-name 的话
(define-struct node (elt l r) #:transparent)

(make-node null null null)

#;(define (bst-insert obj bst <)
    (if (null? bst)
        (node :elt obj)
        null))
