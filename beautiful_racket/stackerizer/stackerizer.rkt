#lang br/quicklang
(provide + *)
(provide (rename-out [stackerizer-mb #%module-begin]))

(define-macro (stackerizer-mb EXPR)
  #'(#%module-begin
     (for-each displayln (reverse (flatten EXPR)))))

; 一次性定义多个操作符
(define-macro (define-ops OP ...)
  #'(begin
      (define-macro-cases OP ; `OP` from `OP ...`
        [(OP FIRST) #'FIRST]
        ; (... ...) 转义 ...
        [(OP FIRST NEXT (... ...))
         #'(list 'OP FIRST (OP NEXT (... ...)))])
      ...)) ; `...` from `OP ...`

(define-ops + *)

; 上面 `define-ops` 的原理演示
(define-macro (wrap ARG ...)
  #'(list '(ARG 42) ...)) ; `...` 会按前面形式自动展开后面剩余参数

(wrap "foo" "bar" "baz")

(define-macro (wrap2 ARG ...)
  #'(list '(ARG 42 ARG) ...))
(wrap2 "foo" "bar" "baz")