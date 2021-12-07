#lang racket

(define (first-attack-num nums preamble-count)
  (for*/first ([i (in-range preamble-count (vector-length nums))]
               #:when (not (contain-count (vector-ref nums i)
                                          (take-preamble nums i preamble-count))))
    (vector-ref nums i)))

(define (contain-count sum nums)
  (for*/first ([a-pos (in-range (vector-length nums))]
               [b-pos (in-range (add1 a-pos) (vector-length nums))]
               [expect (in-value (- sum (vector-ref nums a-pos)))]
               #:when (eq? (vector-ref nums b-pos) expect))
    (list (vector-ref nums a-pos) (vector-ref nums b-pos))))

(define (take-preamble nums from preamble-count)
  (vector-take-right (vector-take nums from) preamble-count))


;; test
(first-attack-num #(15 25 47 40 62 55 65 95 102 117 150 182 127 219 299 277 309 576)
                  5)

(define nums
  (call-with-input-file "input09.txt"
    (lambda (in)
      (for/vector ([line (in-lines in)])
        (string->number line)))))

(first-attack-num nums 25)
