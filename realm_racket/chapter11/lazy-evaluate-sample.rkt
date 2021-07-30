#lang racket

;; ================ lazy evaluate sample ====================
(define (make-layz+ i)
  (lambda ()
    (apply + (build-list (* 500 i) values))))

(define long-big-list (build-list 5000 make-layz+))


(define (computer-every-nth l n)
  (for/list ([thunk l] [i (in-naturals)]
                                 #:when (zero? (remainder i n)))
  (thunk)))

(computer-every-nth long-big-list 500)

;; =============== memorize sample =========================
(define (memorize suspended-c)
  (define hidden #f)
  (define run? #f)
  (lambda ()
    (cond [run? hidden]
          [else (set! hidden (suspended-c))
                (set! run? #t)
                hidden])))

(define lazy+
  (lambda ()
    (apply + (build-list 5000000 values))))


(define mlazy+ (memorize lazy+))
;(mlazy+)
; should return quickly
;(mlazy+)


(define (memorize.v2 suspended-c)
  (define (hidden)
    ;; this implement evaluate the suspended-c once first
    ;; and then replace this function with simply return the value
    (define the-value (suspended-c))
    (set! hidden (lambda () the-value))
    the-value)
  (lambda () (hidden)))

(define mlazy+.v2 (memorize.v2 lazy+))

; delay is a inner function that support lazy evaluation and memorization
(define mlazy+.v3 (delay (apply + (build-list 5000000 values))))
;(force mlazy+.v3)