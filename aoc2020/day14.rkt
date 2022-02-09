#lang racket/base

(require racket/match)

(define mask-re #px"mask = (.{36})")
(define mem-re #px"mem\\[(.+)\\] = (.+)")

;; 赋值指令，一个赋值指令包含一个地址及一个值
(struct assignment (addr val) #:transparent)

;; 分组，一个分组包含一个掩码及多个赋值指令
(struct group (mask assignments) #:transparent)

(define-match-expander mask
  (syntax-rules ()
    [(_ id)
     (regexp mask-re (list _ id))]))

(define-match-expander instr
  (syntax-rules ()
    [(_ addr-id val-id)
     (regexp mem-re (list _
                          (app string->number addr-id)
                          (app string->number val-id)))]))

;; 这个方法的思想是把输入文件按掩码分割并解析成多个组
(define groups
  (call-with-input-file "input14.txt"
    (lambda (in)
      (match-define (mask m)
        (read-line in))
      (for/fold ([m m]
                 [as null]
                 [gs null]
                 #:result (reverse (cons (group m (reverse as)) gs)))
                ([line (in-lines in)])
        (match line
          ;; 每当匹配到一个新的掩码则新建一个组加入到分组集合中，并重置赋值集合为空
          [(mask new-m)
           (values new-m null (cons (group m (reverse as)) gs))]

          ;; 当匹配到赋值指令时，则把赋值指令加入到当前赋值集合中，保持分组不变
          [(instr addr val)
           (values m (cons (assignment addr val) as) gs)])))))

(define (mask-apply m n)
  (for*/fold ([n n]
              [s 0]
              ;; 截取到 36 位数范围内
              #:result (bitwise-and n (sub1 (expt 2 36))))
             ([i (in-range (sub1 (string-length m)) -1 -1)]
              [c (in-value (string-ref m i))])
    (case c
      [(#\X) (values n (add1 s))]
      [(#\0) (values (bitwise-and n (bitwise-not (arithmetic-shift 1 s))) (add1 s))]
      [(#\1) (values (bitwise-ior n (arithmetic-shift 1 s)) (add1 s))])))

(define part1
  (for*/fold ([mem (hash)] #:result (apply + (hash-values mem)))
             ([g (in-list groups)]
              [m (in-value (group-mask g))]
              [a (in-list (group-assignments g))])
    (hash-set mem
              (assignment-addr a)
              (mask-apply m (assignment-val a)))))
