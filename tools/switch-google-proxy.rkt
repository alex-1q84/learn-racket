#lang racket/base

(require racket/string
         racket/list)

(define IS-GOOGLE #f)
(define GOOGLE-TOG-STATUS #f)

(define (toggle-google-proxy! action-file)
  (define confs
    (call-with-input-file action-file
      (lambda (in)
        (toggle-proxy (read-lines in)))))
  (call-with-output-file action-file
    #:exists 'replace
    (lambda (out)
      (write-lines confs out)))
  (display-toggle-status GOOGLE-TOG-STATUS "google"))

(define (read-lines in)
  ;;; read all lines from input port
  (for/list ([line (in-lines in)])
   line))

(define (toggle-proxy confs)
  (if (empty? confs)
      null
      (cons (tog (first confs) (google-conf? (first confs)))
            (toggle-proxy (rest confs)))))

(define (google-conf? conf)
  (cond [(string-prefix? conf "# google-begin")
         (begin0
           IS-GOOGLE
           (set! IS-GOOGLE #t))]
        [(string-prefix? conf "# google-end")
         (set! IS-GOOGLE #f)
         IS-GOOGLE]
        [else IS-GOOGLE]))

(define (tog conf need-tog)
  (if need-tog
      (if (string-prefix? conf "#")
          (begin
            (when (not GOOGLE-TOG-STATUS)
              (set! GOOGLE-TOG-STATUS #t))
            (substring conf 1))
          (string-append "#" conf))
      conf))

(define (write-lines lst out)
  (unless (empty? lst)
    (displayln (first lst) out)
    (write-lines (rest lst) out)))

(define (display-toggle-status status name)
  (if status
      (displayln (format "turn ~A proxy on" name))
      (displayln (format "turn ~A proxy off" name))))

(toggle-google-proxy! "/usr/local/etc/privoxy/wall.action")
