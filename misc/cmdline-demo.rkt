#lang racket
(require racket/cmdline)


(define verbose? (make-parameter #f))
(define profile (make-parameter #f))

(define (main)
  (define filename
    (command-line
     #:once-each
     ; 不带参数的指令选项
     [("-v" "--verbose") "run with verbose messages"
                         (verbose? #t)]
     ; 带参数的指令选项
     [("-p" "--profile") pf ("" "run with given profile")
            (profile pf)]
     ; 预期参数
     #:args (filename)
     ; 命令行解析返回值
     filename))
  (when (verbose?)
    (printf "filename ~A\n" filename))
  (when (profile)
    (printf "run with profile ~A\n" (profile))))

(main)