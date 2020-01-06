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
;二叉搜索树是一种二叉树,给定某个排序函数,比如 < ,每个元素的左子树都 < 该元素,而该元素 < 其右子树
;
;define-struct 默认会提供 #:extra-constructor-name 提供一个 make- 前缀的 struct 构造方法，
;如果我们既没有指定 #:extra-constructor-name 也没有指定 #:constructor-name 的话
(define-struct node (elt l r) #:transparent)

(make-node null null null)

;插入二叉搜索树
(define (bst-insert obj bst <)
  (if (null? bst)
      (make-node obj null null)
      (let ([elt (node-elt bst)])
        (if (equal? obj elt)
            bst
            (if (< obj elt)
                (make-node elt
                           (bst-insert obj (node-l bst) <)
                           (node-r bst))
                (make-node elt
                           (node-l bst)
                           (bst-insert obj (node-r bst) <)))))))

(define (bst-find obj bst <)
  (if (null? bst)
      null
      (let ([elt (node-elt bst)])
        (if (equal? obj elt)
            bst
            (if (< obj elt)
                (bst-find obj (node-l bst) <)
                (bst-find obj (node-r bst) <))))))

;最小值——最左侧叶子节点
(define (bst-min bst)
  (if (null? (node-l bst))
      bst
      (bst-min (node-l bst))))

;最大值——最右侧叶子节点
(define (bst-max bst)
  (if (null? (node-r bst))
      bst
      (bst-max (node-r bst))))

(define nums null)
(for ([x (in-list '(5 8 4 2 1 9 6 7 3))])
  (set! nums (bst-insert x nums <)))

nums

(bst-find 12 nums <)
(bst-find 4 nums <) ;返回找到的节点及以其作为根节点的子树

;racket 中只有 #f 的 bool 求值是 false，其他的 bool 求值都是 true
(and '() #t)
(and '() #f)
(and #t '())
(and #f '())
(equal? #f '()) ;#f
(equal? #t '()) ;#f
(not '()) ;#f
(if '()
    'true
    'false) ;'true

(bst-min nums)
(bst-max nums)

;从二叉搜索树里移除元素，这个实现有 bug
(define (bst-remove obj bst <)
  (if (null? bst)
      null
      (let ([elt (node-elt bst)])
        (if (equal? obj elt)
            (percolate bst)
            (if (< obj elt)
                (make-node elt
                           (bst-remove obj (node-l bst))
                           (node-r bst))
                (make-node elt
                           (node-l bst)
                           (bst-remove obj (node-r bst) <)))))))

(define (percolate bst)
  (cond [(null? (node-l bst))
         (if (null? (node-r bst))
             null
             (rperc bst))]
        [(null? (node-r bst))
         (lperc bst)]
        [else
         (if (zero? (random 2))
             (lperc bst)
             (rperc bst))]))

(define (rperc bst)
  (make-node (node-elt (node-r bst))
             (node-l bst)
             (percolate (node-r bst))))

(define (lperc bst)
  (make-node (node-elt (node-l bst))
             (node-r bst)
             (percolate (node-l bst))))

(bst-remove 12 nums <)
(bst-remove 5 nums <)

;二叉树遍历
(define (bst-traverse fn bst)
  (when (not (null? bst))
    (bst-traverse fn (node-l bst))
    (fn (node-elt bst))
    (bst-traverse fn (node-r bst))))

(bst-traverse (lambda (value)
                (displayln (format "node: ~a" value)))
              nums)

;定义一个函数,接受一棵二叉搜索树,并返回由此树元素所组成的,一个由大至小排序的列表。
(define (bst-traverse-big-to-small fn bst)
  (when (not (null? bst))
    (bst-traverse-big-to-small fn (node-r bst))
    (fn (node-elt bst))
    (bst-traverse-big-to-small fn (node-l bst))))

(displayln "bst-traverse-big-to-small")
(bst-traverse-big-to-small (lambda (value)
                             (displayln (format "node: ~a" value)))
                           nums)

;定义一个函数，接受一个关联列表,并返回一个对应的哈希表
(define (assoc-list->hashtable lst)
  (let ([amap (make-hash)])
    (for ([p (in-list lst)])
      (hash-set! amap (car p) (cdr p)))
    amap))

(define amap (assoc-list->hashtable '((a . 1) (b . 2) (c . 3))))
amap
(hash-ref amap 'a)
(hash-ref amap 'c)

;定义一个函数,接受一个哈希表,并返回一个对应的关联列表
(define (hashtable->assoc-list amap)
  (hash-map amap (λ (k v)
                   (cons k v))))

(hashtable->assoc-list amap)