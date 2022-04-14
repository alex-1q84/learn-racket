#lang racket/base

(require racket/string
         racket/list
         racket/cmdline
         racket/port
         racket/match)

(define GOOGLE-TOG-STATUS #f)

;; ============== toggle google proxy =============
(define (toggle-google-proxy rule-groups)
  (begin0
      (hash-set rule-groups "ssh" (toggle-proxy (hash-ref rule-groups "ssh")))
    (display-toggle-status GOOGLE-TOG-STATUS "google")))

(define is-google? #f)
(define (toggle-proxy confs)
  (define (google-conf? conf)
    (cond [(string-prefix? conf "# google-begin")
           (begin0
               is-google?
             (set! is-google? #t))]
          [(string-prefix? conf "# google-end")
           (set! is-google? #f)
           is-google?]
          [else is-google?]))

  (define (tog conf need-tog)
    (if need-tog
        (if (string-prefix? conf "#")
            (begin
              (when (not GOOGLE-TOG-STATUS)
                (set! GOOGLE-TOG-STATUS #t))
              (substring conf 1))
            (string-append "#" conf))
        conf))

  (if (empty? confs)
      null
      (cons (tog (first confs) (google-conf? (first confs)))
            (toggle-proxy (rest confs)))))

(define (write-lines lst out)
  (for ([line (in-list lst)])
    (displayln line out)))

(define (display-toggle-status status name)
  (if status
      (displayln (format "turn ~A proxy on" name))
      (displayln (format "turn ~A proxy off" name))))

;; ================= append rule ===================
(define (url->rule url)
  (define domain
    (match url
     [(regexp #px"\\w+://([^/]+).*" (list _ domain)) domain]
     [else #f]))
  (cond
    [domain (if (string-prefix? domain "www.")
                (string-replace domain "www" "" #:all? #f)
                (string-append "." domain))]
    [else #f]))

(define (append-rule rule-groups group-name rule)
  (define rules (hash-ref rule-groups group-name))
  (cond
    [rules
     (cond
       [(not (member rule rules))
        (hash-set rule-groups group-name (append rules (list rule)))]
       [else rule-groups])]
    [else
     rule-groups]))

(define (display-rule-group title rules [port (current-output-port)])
  (cond
    [rules
     (cond
       [title
        (displayln (format "{~A}" title) port)
        (displayln (string-join rules "\n") port)]
       [else
        (displayln (string-join rules "\n") port)])]))

(define (parse-to-rule-groups rules)
  (for/fold ([groups (hash)] [group-name "alias"]
                             #:result groups)
            ([rule (in-list rules)])
    (match rule
      ["{default}"
       (values (hash-set groups "default" (list))
               "default")]
      ["{ssh}"
       (values (hash-set groups "ssh" (list))
               "ssh")]
      [else
       (define group (hash-ref groups group-name (list)))
       (values (hash-set groups group-name (append group (list rule)))
               group-name)])))

(define (rule-groups->file rule-groups file-path)
  (call-with-output-file file-path
    #:exists 'replace
    (lambda (out)
      (display-rule-group #f (hash-ref rule-groups "alias" #f) out)
      (display-rule-group "default" (hash-ref rule-groups "default" #f) out)
      (display-rule-group "ssh" (hash-ref rule-groups "ssh" #f) out))))

(define (file->lines file-path)
  (call-with-input-file* file-path
    (lambda (in)
      (port->lines in))))

(define toggle-google? (make-parameter #f))
(define rule (make-parameter #f))

(define (main)
  (command-line
   #:once-any
   ; 不带参数的指令选项
   [("-g" "--toggle-google") "toggle google proxy rule"
                             (toggle-google? #t)]
   ; 带参数的指令选项
   [("-a" "--append-rule") url ("" "append rule")
                           (rule (url->rule url))])

  (when (toggle-google?)
    (define action-file "/usr/local/etc/privoxy/wall.action")
    (define rule-groups (parse-to-rule-groups (file->lines action-file)))
    (rule-groups->file (toggle-google-proxy rule-groups)
                       action-file))

  (when (rule)
    (define action-file "/usr/local/etc/privoxy/wall.action")
    (define rule-groups (parse-to-rule-groups (file->lines action-file)))
    (rule-groups->file (append-rule rule-groups "ssh" (rule))
                       action-file)))

(main)

;; =====================================
;; ==            tests                ==
;; =====================================
(module+ test
  (require rackunit)

  (define action-file "/usr/local/etc/privoxy/wall.action")
  (define rule-groups (parse-to-rule-groups (file->lines action-file)))

  (check-equal? (url->rule "http://www.abc.com") ".abc.com")
  (check-equal? (last (hash-ref (append-rule rule-groups "ssh" (url->rule "http://www.abc.com"))
                           "ssh"))
                ".abc.com")
  (toggle-proxy (list "# google-begin -----" ".google.com" "# google-end --------")))
