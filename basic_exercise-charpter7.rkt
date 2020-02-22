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
  (let* ([in (open-input-file in-file)]
         [out (open-output-file out-file #:exists 'truncate)])
    (define (transform-fn str)
      (string-trim str #px"%.*"))
    
    (define (copy-to transform-fn)
      (let ([line (read-line in)])
        (unless (eof-object? line)
          ; write out
          (displayln (transform-fn line) out)
          (copy-to transform-fn))))
    
    (copy-to transform-fn)
    (close-input-port in)
    (close-output-port out)))

(displayln "copy-data")
(copy-data "test-in.txt" "test-out.txt")

; 字符串代换
(struct buf
  (vec start used new end)
  #:mutable)

(define (bref-index buf n)
  (remainder n (vector-length (buf-vec buf))))

(define (bref buf n)
  (vector-ref (buf-vec buf)
              (bref-index buf n)))

#;(define (set!-bref val buf n)
    (vector-set! (buf-vec buf)
                 (bref-index buf n)
                 val))

(define (new-buf len)
  (buf (make-vector len) -1 -1 -1 -1))

(define (buf-insert x b)
  ; increase buf end
  (set-buf-end! b (add1 (buf-end b)))
  (vector-set! (buf-vec b)
               (bref-index b (buf-end b))
               x))

(define (buf-pop b)
  (set-buf-start! b (add1 (buf-start b)))
  (set-buf-used! b (buf-start b))
  (set-buf-new! b (buf-end b))
  (bref b (buf-start b)))

(define (buf-next b)
  (if (< (buf-used b) (buf-new b))
      (begin
        (set-buf-used! b (add1 (buf-used b)))
        (bref b (buf-used b)))
      null))

(define (buf-reset b)
  (set-buf-used! b (buf-start b))
  (set-buf-new! b (buf-end b)))

(define (buf-clear b)
  (set-buf-start! b -1)
  (set-buf-used! b -1)
  (set-buf-new! b -1)
  (set-buf-end! b -1))

(define (buf-flush b str)
  (define (flush-char start current end)
    (unless (> current (buf-end b))
      ; 这行调用有错误
      (print (bref b current) str)
      (displayln "buf-flush")
      (flush-char start (add1 current) end)))
  (flush-char (add1 (buf-used b))
              (add1 (buf-used b))
              (buf-end b)))

(define (file-subset old new file-in file-out)
  (call-with-input-file file-in
    (lambda (in)
      (call-with-output-file file-out #:exists 'truncate
        (lambda (out)
          (stream-subst old new in out))))))

; 基本思路——创建与待替换字符串长度相同的缓冲区，每读取一个字符则检查是否和待替换字符串对应位置字符相同，
; 如是则放入缓冲区，继续读下一个，如果缓冲区已使用长度与待替换字符串长度相同，则说明匹配到待替换字符串，输出新的替换字符串并清空缓冲区，重置缓冲区，
; 如果读取到的字符和待替换字符串对应位置字符不同，则说明此段缓冲区内字符与待替换字符串不同，弹出缓冲区顶部第一个字符并输出
(define (stream-subst old new in out)
  (let* ([pos 0]
         [len (string-length old)]
         [buf (new-buf len)]
         [from-buf null])
    (define (loop character)
      (unless (eof-object? character)
        (define from-buf (buf-next buf))
        (cond [(char=? character (string-ref old pos))
               (set! pos (add1 pos))
               (if (= pos len)
                   (begin
                     (display new out)
                     (set! pos 0)
                     (buf-clear buf))
                   (buf-insert character buf))]
              [(zero? pos)
               (display character out)
               (when (not (null? from-buf))
                 (buf-pop buf)
                 (buf-reset buf))]
              [else
               (when (null? from-buf)
                 (displayln (format "insert to buf ~A" character))
                 (buf-insert character buf))
               (display (buf-pop buf) out)
               (buf-reset buf)
               (set! pos 0)])
        (loop (read-char in))))
    (loop (read-char in))
    (buf-flush buf out)))

(file-subset "th" "z" "stream-subst-in.txt" "stream-subst-out.txt")

(call-with-input-file "stream-subst-in.txt"
  (lambda (in)
    (displayln (read-string 1024 in))))

(call-with-input-file "stream-subst-out.txt"
  (lambda (in)
    (displayln (read-string 1024 in))))