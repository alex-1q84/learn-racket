#lang racket/base

(require racket/string
         racket/list
         racket/cmdline
         racket/port
         racket/match)


;; ============== toggle google proxy =============
(define (toggle-google-proxy enable? rule-groups)
  (let ([rule-name "google"]
        [rule-group "ssh"])
    (begin0
      (hash-set rule-groups rule-group (toggle-proxy rule-name enable? (hash-ref rule-groups rule-group)))
      (display-toggle-status enable? rule-name))))


(define (rule-begin rule-name)
  (format "# ~A-begin" rule-name))


(define (rule-end rule-name)
  (format "# ~A-end" rule-name))


(define (begin-rule-border? conf rule-name)
  (string-prefix? conf (rule-begin rule-name)))

(define (end-rule-border? conf rule-name)
  (string-prefix? conf (rule-end rule-name)))

(define is-google? #f)
(define (toggle-proxy rule-name enable? confs)
  (define (google-conf? conf)
    (cond [(begin-rule-border? conf rule-name)
           (begin0
             is-google?
             (set! is-google? #t))]
          [(end-rule-border? conf rule-name)
           (set! is-google? #f)
           is-google?]
          [else is-google?]))

  (define (tog conf need-tog)
    (if need-tog
        (if enable?
            (turn-on-rule conf)
            (turn-off-rule conf))
        conf))

  (if (empty? confs)
      null
      (cons (tog (first confs) (google-conf? (first confs)))
            (toggle-proxy rule-name enable? (rest confs)))))


(define (turn-on-rule conf)
  (if (string-prefix? conf "#")
      (substring conf 1)
      conf))


(define (turn-off-rule conf)
  (if (string-prefix? conf "#")
      conf
      (string-append "#" conf)))


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


(define (echo p)
  (displayln p))


(define enable-google? (make-parameter #f))
(define disable-google? (make-parameter #f))
(define rule (make-parameter #f))

(define (main)
  (define (switch-google-proxy enable?)
    (define action-file "/usr/local/etc/privoxy/wall.action")
    (define rule-groups (parse-to-rule-groups (file->lines action-file)))
    (rule-groups->file (toggle-google-proxy enable? rule-groups)
                       action-file))
  
  (command-line
   #:once-any
   ; 不带参数的指令选项
   [("--google-proxy-on") "turn google proxy rule on"
                          (switch-google-proxy #t)]
   [("--google-proxy-off") "turn google proxy rule off"
                           (switch-google-proxy #f)]
   ; 带参数的指令选项
   [("-a" "--append-rule") url ("" "append rule")
                           (begin
                             (define action-file "/usr/local/etc/privoxy/wall.action")
                             (define rule-groups (parse-to-rule-groups (file->lines action-file)))
                             (rule-groups->file (append-rule rule-groups "ssh" (rule))
                                                action-file))]))

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

  (check-equal? (toggle-proxy "google" #t (list "# google-begin -----" "#.google.com" "# google-end --------"))
                (list "# google-begin -----" ".google.com" "# google-end --------"))
 
  (check-equal? (toggle-proxy "google" #f (list "# google-begin -----" "#.google.com" "# google-end --------"))
                (list "# google-begin -----" "#.google.com" "# google-end --------")))
