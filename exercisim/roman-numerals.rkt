#lang racket

(provide to-roman)


(define (to-roman number)
  ;; split into thousands hundreds tens and ones parts
  ;; then convert each part into roman number and concatenate each roman number
  (define number-parts (number-split number))
  (string-append* (map number->roman number-parts)))

(define (number-split number)
  (define parts (let loop ([times (values 0)]
                           [q (quotient number 10)]
                           [r (remainder number 10)]
                           [result '()])
                  (cond
                    [(zero? q) (cons (* r (expt 10 times)) result)]
                    [else
                     (loop (add1 times)
                           (quotient q 10)
                           (remainder q 10)
                           (cons (* r (expt 10 times)) result))])))
  (filter positive? parts))

(define (number->roman number)
  (cond
    [(>= number 1000) (string-append* (make-list (/ number 1000) "M"))]
    [(>= number 100) (roman-part (/ number 100) "C" "D" "M")]
    [(>= number 10) (roman-part (/ number 10) "X" "L" "C")]
    [(>= number 1) (roman-part number "I" "V" "X")]))

(define (roman-part times low mid high)
  (cond
    [(= times 9) (string-append low high)]
    [(>= times 5) (string-append* mid (make-list (- times 5) low))]
    [(= times 4) (string-append low mid)]
    [else (string-append* (make-list times low))]))


(module+ test
  (require rackunit rackunit/text-ui)
  (define suite
    (test-suite
     "Roman Numeral tests"
     (test-equal? "1 is a single I"
                  (to-roman 1)
                  "I")
     (test-equal? "2 is two I's"
                  (to-roman 2)
                  "II")
     (test-equal? "3 is three I's"
                  (to-roman 3)
                  "III")
     (test-equal? "4, being 5 - 1, is IV"
                  (to-roman 4)
                  "IV")
     (test-equal? "5 is a single V"
                  (to-roman 5)
                  "V")
     (test-equal? "6, being 5 + 1, is VI"
                  (to-roman 6)
                  "VI")
     (test-equal? "9, being 10 - 1, is IX"
                  (to-roman 9)
                  "IX")
     (test-equal? "20 is two X's"
                  (to-roman 20)
                  "XX")
     (test-equal? "48 is not 50 - 2 but rather 40 + 8"
                  (to-roman 48)
                  "XLVIII")
     (test-equal? "49 is not 40 + 5 + 4 but rather 50 - 10 + 1"
                  (to-roman 49)
                  "XLIX")
     (test-equal? "50 is a single L"
                  (to-roman 59)
                  "LIX")
     (test-equal? "90, being 100 - 10, is XC"
                  (to-roman 93)
                  "XCIII")
     (test-equal? "100 is a single C"
                  (to-roman 141)
                  "CXLI")
     (test-equal? "60, being 50 + 10, is LX"
                  (to-roman 163)
                  "CLXIII")
     (test-equal? "400, being 500 - 100, is CD"
                  (to-roman 402)
                  "CDII")
     (test-equal? "500 is a single D"
                  (to-roman 575)
                  "DLXXV")
     (test-equal? "900, being 1000 - 100, is CM"
                  (to-roman 911)
                  "CMXI")
     (test-equal? "1000 is a single M"
                  (to-roman 1024)
                  "MXXIV")
     (test-equal? "3000 is three M's"
                  (to-roman 3000)
                  "MMM")
     ))
  (run-tests suite))
