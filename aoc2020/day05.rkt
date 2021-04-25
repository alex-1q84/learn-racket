#lang racket
(require racket/match)


(define seats
  (call-with-input-file "input05.txt"
    (lambda (in)
      (for/list ([line (in-lines in)])
        (string->list line)))))

(define (seat-row sr)
  (define (bsp lower upper sr)
    #;(displayln (format "~A ~A ~A" lower upper (empty? sr)))
    (cond
      [(empty? sr) (cond
                     [(eq? lower upper) lower]
                     [else (displayln (format "~A ~A" lower upper))])]
      [else (case (car sr)
              [(#\F) (let-values ([(lower upper) (lower-half lower upper)])
                       (bsp lower upper (cdr sr)))]
              [(#\B) (let-values ([(lower upper) (upper-half lower upper)])
                       (bsp lower upper (cdr sr)))])])
    )
  (bsp 0 127 sr))

(define (seat-column sr)
  (define (bsp lower upper sr)
    (cond
      [(empty? sr) (cond
                     [(eq? lower upper) lower]
                     [else (displayln (format "~A ~A" lower upper))])]
      [else (case (car sr)
              [(#\L) (let-values ([(lower upper) (lower-half lower upper)])
                       (bsp lower upper (cdr sr)))]
              [(#\R) (let-values ([(lower upper) (upper-half lower upper)])
                       (bsp lower upper (cdr sr)))])]))
  (bsp 0 7 sr))

(define (seat-id sr)
  (+ (* (seat-row (take-seat-row-def sr)) 8)
     (seat-column (take-seat-column-def sr))))

(define (lower-half lower upper)
  (values lower (floor (middle-of lower upper))))

(define (upper-half lower upper)
  (values (ceiling (middle-of lower upper)) upper))

(define (middle-of lower upper)
  (+ lower (/ (- upper lower) 2)))

(define (take-seat-row-def sr)
  (take sr 7))

(define (take-seat-column-def sr)
  (take-right sr 3))

(apply max (map seat-id seats))

(module+ test
  (require rackunit)

  "all test run")