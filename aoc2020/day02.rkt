#lang racket

(define (parse-to-pwd-and-policy str)
  (define p (string-split str " "))
  (list (parse-to-num-pair (first p))
        (parse-key-char (second p))
        (third p)))

(define (parse-to-num-pair num-str)
  (map string->number (string-split num-str "-")))

(define (parse-key-char keystr)
  (string-ref keystr 0))

(define (validate-key-char-count pwd-and-policy)
  (define key-count (count (lambda (ch)
                             (eq? ch (key-char pwd-and-policy)))
                           (string->list (password pwd-and-policy))))
  (and (>= key-count (lower-num pwd-and-policy))
       (<= key-count (upper-num pwd-and-policy))))

(define (validate-key-char-pos pwd-and-policy)
  (cond [(eq?/pos (password pwd-and-policy)
                       (lower-num pwd-and-policy)
                       (key-char pwd-and-policy))
         (not (eq?/pos (password pwd-and-policy)
                       (upper-num pwd-and-policy)
                       (key-char pwd-and-policy)))]
        [(not (eq?/pos (password pwd-and-policy)
                       (lower-num pwd-and-policy)
                       (key-char pwd-and-policy)))
         (eq?/pos (password pwd-and-policy)
                       (upper-num pwd-and-policy)
                       (key-char pwd-and-policy))]
        [else #f]))

(define (eq?/pos str pos ch)
  (eq? (string-ref str (sub1 pos))
       ch))

(define (key-char pwd-and-policy)
  (second pwd-and-policy))

(define (password pwd-and-policy)
  (third pwd-and-policy))

(define (lower-num pwd-and-policy)
  (first (first pwd-and-policy)))

(define (upper-num pwd-and-policy)
  (second (first pwd-and-policy)))

(define lpwd-and-policy (call-with-input-file "input02.txt"
                          (lambda (in)
                            (for/list ([line (in-lines in)])
                              (parse-to-pwd-and-policy line)))))

(count (lambda (f)
         (eq? f #t))
       (for/list ([pwd-and-policy (in-list lpwd-and-policy)])
         (validate-key-char-count pwd-and-policy)))

(count (lambda (f)
         (eq? f #t))
       (for/list ([pwd-and-policy (in-list lpwd-and-policy)])
         (validate-key-char-pos pwd-and-policy)))

(module+ test
  (require rackunit)

  (check-equal? (validate-key-char-count '((1 3) #\a "abaa")) #t)
  (check-equal? (validate-key-char-count '((1 2) #\a "abaa")) #f)
  (check-equal? (validate-key-char-count '((1 2) #\a "bb")) #f)
  (check-equal? (validate-key-char-count '((1 1) #\a "a")) #t)
  "all test run")