#lang racket
(require racket/date)

(provide meetup-day)

;; 13teenth ~ 19teenth
(define (meetup-day year month weekday week-of-month)
  ; check week-of-month is teenth or not
  ; if is teenth then count from 13teenth day of month to get the specificated weekday
  ; else if week-of-month is the nth then count from the first same weekday
  ; until get the given nth weekday
  (define a-month-days (days-in-month year month))
  (cond
    [(eq? week-of-month 'teenth)
     (make-date year month (+ 13 (weekday-distance (week-of-day year month 13) (weekday->number weekday))))]
    [(eq? week-of-month 'last)
     (make-date year month (- a-month-days (weekday-distance (date-week-day (make-date year month a-month-days)) (weekday->number weekday))))]
    [else
     (make-date year month (month/nth-weekday year month (weekday->number weekday) (to-number week-of-month)))]))

(define (month/nth-weekday year month weekday week-of-month)
  (define month-start-weekday (date-week-day (make-date year month 1)))
  (define first-weekday/month-day
    (+ 1 (remainder (+ weekday 7 (- month-start-weekday)) 7)))
  (+ first-weekday/month-day (* week-of-month 7)))

(define (to-number nth)
  (match nth
    ['first 0]
    ['second 1]
    ['third 2]
    ['fourth 3]))

(define (leap-year? year)
  (or 
   (and (zero? (remainder year 4)) 
        ((negate zero?) (remainder year 100)))
   (= (remainder year 400) 0)))

(define (days-in-month year month)
  (cond
    [(or (= month 1) (= month 3) (= month 5) (= month 7) (= month 8) (= month 10) (= month 12)) 31]
    [(or (= month 4) (= month 6) (= month 9) (= month 11)) 30]
    [(and (= month 2) (leap-year? year)) 29]
    [else 28]))

(define (weekday->number weekday)
  (match weekday
    ['Monday 1]
    ['Tuesday 2]
    ['Wednesday 3]
    ['Thursday 4]
    ['Friday 5]
    ['Saturday 6]
    ['Sunday 7]))

(define (weekday-distance from to)
  (- to from))

(define (week-of-day year month day)
  (date-week-day (make-date year month day)))

(define (make-date year month day)
    (seconds->date (find-seconds 0 0 0 day month year #f) #f))


(module+ test
  (require rackunit rackunit/text-ui)

  (define (make-date year month day)
    (seconds->date (find-seconds 0 0 0 day month year #f) #f))
  (define suite
    (test-suite
     "Tests for the meetup exercise"
     (check-equal? (meetup-day 2013 5 'Monday 'teenth)
                   (make-date 2013 5 13))
     (check-equal? (meetup-day 2013 2 'Saturday 'teenth)
                   (make-date 2013 2 16))
     (check-equal? (meetup-day 2013 5 'Tuesday 'first)
                   (make-date 2013 5 7))
     (check-equal? (meetup-day 2013 4 'Monday 'second)
                   (make-date 2013 4 8))
     (check-equal? (meetup-day 2013 9 'Thursday 'third)
                   (make-date 2013 9 19))
     (check-equal? (meetup-day 2013 3 'Sunday 'fourth)
                   (make-date 2013 3 24))
     (check-equal? (meetup-day 2013 10 'Thursday 'last)
                   (make-date 2013 10 31))
     (check-equal? (meetup-day 2012 2 'Wednesday 'last)
                   (make-date 2012 2 29))))
  (run-tests suite))
