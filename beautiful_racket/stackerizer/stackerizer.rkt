#lang br/quicklang
(provide + *)
(provide (rename-out [stackerizer-mb #%module-begin]))

(define-macro (stackerizer-mb EXPR)
  #'(#%module-begin
     (for-each displayln (reverse (flatten EXPR)))))

(define-macro (define-op OP)
  #'(define-macro-cases OP
      [(OP FIRST) #'FIRST]
      ; (... ...) 转义 ...
      [(OP FIRST NEXT (... ...))
       #'(list 'OP FIRST (OP NEXT (... ...)))]))

(define-op +)
(define-op *)