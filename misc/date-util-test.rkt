#lang racket
(require rackunit
         racket/date)
(require "date-util.rkt")


(define (test-milliseconds->date)
  (check-equal? (milliseconds->date 1000) (seconds->date 1)))

(define (test-date->milliseconds)
  (check-equal? (date->milliseconds (seconds->date 1)) 1000))

(define (test-plus-days)
  (let* ([now (seconds->date (current-seconds))]
         [tomorrow (plus-days now 1)])
    (check-equal? (date->seconds tomorrow) (+ (date->seconds now) 86400))))

(define (test-plus-minutes)
  (let* ([now (seconds->date (current-seconds))]
         [next-minute (plus-minutes now 1)])
    (check-equal? (date->seconds next-minute) (+ (date->seconds now) 60))))

(define (test-plus-seconds)
  (let* ([now (seconds->date (current-seconds))]
         [next-second (plus-seconds now 1)])
    (check-equal? (date->seconds next-second) (+ (date->seconds now) 1))))

#;(define (test-parse-date)
  (let ([parsed-date (parse-date "2023-10-05 12:34:56")])
    (check-equal? (date->seconds parsed-date) (date->seconds (seconds->date 1696509296)))))

#;(define (test-format-date)
  (let* ([now (seconds->date (current-seconds))]
         [formatted-date (format-date now)])
    (check-equal? formatted-date (string-append (number->string (date-year now)) "-"
                                                (number->string (date-month now)) "-"
                                                (number->string (date-day now)) " "
                                                (number->string (date-hour now)) ":"
                                                (number->string (date-minute now)) ":"
                                                (number->string (date-second now))))))

(test-milliseconds->date)
(test-date->milliseconds)
(test-plus-days)
(test-plus-minutes)
(test-plus-seconds)
#;(test-parse-date)
#;(test-format-date)

"all tests passed"