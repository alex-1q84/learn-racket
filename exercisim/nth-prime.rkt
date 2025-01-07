#lang racket
(require racket/generator)

(provide nth-prime)

;; 定义一个 producer 函数，用于生成整数序列
(define (make-producer start interval)
  (generator ()
             (let loop ([current start])
               (yield current)
               (loop (+ current interval)))))

(define (prime? number)
  (for/fold ([prime true]) ([n (in-range 2 (+ 2 (integer-sqrt number)))]
                            #:break (false? prime))
    (cond
      [(= n number) true]
      [(zero? (remainder number n))
       false]
      [else true])))

(define (nth-prime number)
  (define (find-nth-prime)
    (for/fold ([nth 2] [prime 0]
                       #:result prime)
              ([n (in-producer (make-producer 5 2))]
               #:break (= nth number))
      ;(displayln n)
      (if (prime? n)
          (values (add1 nth) n)
          (values nth prime))))
  
  (case number
    [(0) (error "there is no zeroth prime")]
    [(1) 2]
    [(2) 3]
    [else (find-nth-prime)]))

(module+ test
  (require rackunit rackunit/text-ui)
  (define suite
    (test-suite
     "nth prime tests"
     (test-equal? "first prime"
                  (nth-prime 1)
                  2)
     (test-equal? "second prime"
                  (nth-prime 2)
                  3)
     (test-equal? "sixth prime"
                  (nth-prime 6)
                  13)
     (test-equal? "big prime"
                  (nth-prime 10001)
                  104743)
     (test-exn "there is no zeroth prime"
               exn:fail? (lambda () (nth-prime 0)))))
  (run-tests suite))
