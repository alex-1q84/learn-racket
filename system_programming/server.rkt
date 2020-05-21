#lang racket

;(require racket/enter)

;(enter! "lab/racket/system_programming/server.rkt")

(define (server port-no)
  (define listener (tcp-listen port-no 5 #t))
  (define (loop)
    (accept-and-handle listener)
    (loop))
  ; start the loop method in a new thread
  (define t (thread loop))
  ; return a method that can stop this server
  (lambda ()
    (kill-thread t)
    (tcp-close listener)))

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
  ;Discard the request header (up to blank line):
  (regexp-match #rx"(\r\n|^)\r\n" in)
  ;Send reply:
  (display "HTTP/1.0 200 OK\r\n" out)
  (display "Server: k\r\nContent-Type: text/html\r\n\r\n" out)
  (display "<html><body>Hello, world!</body></html>" out))
