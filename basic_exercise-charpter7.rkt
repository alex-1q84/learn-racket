#lang racket
; 定义一个函数,接受一个文件名并返回一个由字符串组成的列表,来表示文件里的每一行
(define (read-file file-path)
  (let* ([in (open-input-file file-path)]
         [result '()])
    (define (read-all)
      (let ([line (read-line in)])
        (unless (eof-object? line)
          (set! result (cons line result))
          (read-all))))
    (read-all)
    (close-input-port in)
    (reverse result)))

(read-file "test-in.txt")

; 假设有某种格式的文件文件,注解是由 % 字符表示。从这个字符开始直到行尾都会被忽略。
; 定义一个函数,接受两个文件名称,并拷贝第一个文件的内容去掉注解,写至第二个文件
(define (copy-data in-file out-file)
  null)

(copy-data "test-in.txt" "test-out.txt")