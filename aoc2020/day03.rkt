#lang racket

(struct position (x y) #:transparent)
(struct worldmap (w h m) #:transparent)

(define (slid posn x-step y-step)
  (position (+ x-step (position-x posn))
            (+ y-step (position-y posn))))

(define (encounter map posn)
  (cond
    [(= (position-y posn) (worldmap-h map)) 0]
    [else
     (+ (tree? map posn)
        (encounter map (slid posn 3 1)))]))

(define (tree? map posn)
  (if (vector-ref (vector-ref (worldmap-m map) (position-y posn))
              (remainder (position-x posn) (worldmap-w map)))
       1
       0))

(define map
  (call-with-input-file "input03.txt"
    (lambda (in)
      (define m
        (for/vector ([line (in-lines in)])
          (for/vector ([c (in-string line)])
            (case c
              [(#\.) #f]
              [(#\#) #t]))))
      (worldmap (vector-length (vector-ref m 0))
                (vector-length m)
                m))))

(encounter map (position 0 0))

(module+ test
  (require rackunit)

  "all test run")