#lang racket
;;一个简单的小脚本把文本转换成 html 表格
(require xml)


(define input (open-input-file (expand-user-path "~/Downloads/datacenter-api-sql注入风险.txt")
                               #:mode 'text))
; 按空行分隔成表格行
(define files (string-split (port->string input) "\n\n"))
(define rows (for/list ([seg (in-list files)]
                        [n (in-naturals)])
               `(tr (td ,(number->string (add1 n)))
                    ; 多行分隔成 html 行
                    ,@(map (lambda (x) `(br ,x)) (string-split seg "\n")))))

(displayln (xexpr->string `(html (body ,(cons 'table rows)))))