#lang br/quicklang

(require "parser.rkt")

(define (read-syntax path port)
  (define parse-tree (parse path (make-tokenizer port)))
  (define module-datum `(module bf-mod "expander.rkt"
                          ,parse-tree))
  (datum->syntax #f module-datum))

(provide read-syntax)

(require brag/support)
(define (make-tokenizer port)
  (define (next-token)
    (define bf-lexer
      ; lexer 创建一个按指定规则从 port 里读取 token 的方法
      (lexer
       [(char-set "><+-.,[]") lexeme]
       [any-char (next-token)]))
    (bf-lexer port))
  next-token)