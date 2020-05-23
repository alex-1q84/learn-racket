#lang racket

;(require racket/enter)

;(enter! "lab/racket/system_programming/server.rkt")

(require xml net/url)

(define (server port-no)
  (define main-cust (make-custodian))
  
  (parameterize ([current-custodian main-cust])
    (define listener (tcp-listen port-no 5 #t))
    (define (loop)
      (accept-and-handle listener)
      (loop))
    ; start the loop method in a new thread
    (thread loop))
  ; return a method that can stop this server
  (lambda ()
    (custodian-shutdown-all main-cust)))

(define (accept-and-handle listener)
  (define cust (make-custodian))
  (parameterize ([current-custodian cust])
    (define-values (in out) (tcp-accept listener))
    (thread
     (lambda ()
       (handle in out)
       (close-input-port in)
       (close-output-port out))))
  ; watcher thread:
  (thread (lambda ()
            (sleep 10)
            (custodian-shutdown-all cust))))

(define (handle in out)
  (define req
    (regexp-match #rx"^GET (.+) HTTP/[0-9]+\\.[0-9]+"
                  (read-line in)))
  (when req
    ;Discard the request header (up to blank line):
    (regexp-match #rx"(\r\n|^)\r\n" in)
    (displayln req)
    ; Dispatch:
    (let ([xexpr (dispatch (list-ref req 1))])
      ;Send reply:
      (display "HTTP/1.0 200 OK\r\n" out)
      (display "Server: k\r\nContent-Type: text/html\r\n\r\n" out)
      (display (xexpr->string xexpr) out))))

(define (dispatch str-path)
  ; Parse the request as url:
  (define url (string->url str-path))
  ; Extract the path part:
  (define path (map path/param-path (url-path url)))
  ; Find a handler based on the path's first element:
  (displayln (car path))
  (define h (hash-ref dispatch-table (car path) #f))
  (if h
      ; Call a handler:
      (h (url-query url))
      ; No handler found:
      `(html (head (title "Error"))
             (body
              (font ((color "red"))
                    "Unknown page: "
                    ,str-path)))))

(define dispatch-table (make-hash))

(hash-set! dispatch-table "hello"
           (lambda (query)
             `(html (body "Hello, World!"))))
