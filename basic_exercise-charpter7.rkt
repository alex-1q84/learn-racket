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
  (remainder n (length (buf-vec buf))))

(define (bref buf n)
  (vector-ref (buf-vec buf)
              (bref-index buf n)))

(define (set!-bref val buf n)
  (vector-set! (buf-vec buf)
               (bref-index buf n)
               val))

(define (new-buf len)
  (buf (make-vector len) -1 -1 -1 -1))

(define (buf-insert x b)
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
  (when (< (buf-used b) (buf-new b))
    (set-buf-used! b (add1 (buf-used b)))
    (bref b (buf-used b))))

(define (buf-reset b)
  (set-buf-used! b (buf-start b))
  (set-buf-new! b (buf-end b)))

(define (buf-clear b)
  (set-buf-start! -1)
  (set-buf-used! -1)
  (set-buf-new! -1)
  (set-buf-end! -1))

(define (buf-flush b str)
  (define (flush-char start current end)
    (unless (> current (buf-end b))
      (print (bref b current) str)
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

(define (stream-subst old new in out)
  (let* ([pos 0]
         [len (string-length old)]
         [buf (new-buf len)]
         [from-buf null])
    (define (loop char)
      (unless (eof-object? char)
        (display char)
        (loop (read-char in))))
    (loop (read-char in))))

(file-subset "racket" "Racket" "test-in.txt" "test-out.txt")