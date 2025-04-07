#lang racket

(provide create-queen
         can-attack?)

(struct position (row col))

(define (create-queen row column)
  (unless (<= 0 row 7) (error "queen must have positive row"))
  (unless (<= 0 column 7) (error "queen must have positive column"))
  (position row column))

(define (can-attack? white-queen black-queen)
  (or (= (position-row white-queen) (position-row black-queen)) ; same rown
      (= (position-col white-queen) (position-col black-queen)) ; same column
      ; same diagonal
      (= (abs (- (position-row white-queen) (position-row black-queen)))
         (abs (- (position-col white-queen) (position-col black-queen))))))


(module+ test
  (require rackunit rackunit/text-ui)
  (define suite
    (test-suite
     "queen-attack tests"
     (test-not-exn "queen with a valid position"
                   (lambda () (create-queen 2 2)))
     (test-exn "queen must have positive row"
                exn:fail?
                (lambda () (create-queen -2 2)))
     (test-exn "queen must have row on board"
                exn:fail?
                (lambda () (create-queen 8 4)))
     (test-exn "queen must have positive column"
                exn:fail?
                (lambda () (create-queen 2 -2)))
     (test-exn "queen must have column on board"
                exn:fail?
                (lambda () (create-queen 4 8)))
     (test-false "cannot attack"
                 (can-attack? (create-queen 2 4)
                              (create-queen 6 6)))
    (test-true "can attack on same row"
               (can-attack? (create-queen 2 4)
                            (create-queen 2 6)))
    (test-true "can attack on same column"
               (can-attack? (create-queen 4 5)
                            (create-queen 2 5)))
    (test-true "can attack on first diagonal"
               (can-attack? (create-queen 2 2)
                            (create-queen 0 4)))
    (test-true "can attack on second diagonal"
               (can-attack? (create-queen 2 2)
                            (create-queen 3 1)))
    (test-true "can attack on third diagonal"
               (can-attack? (create-queen 2 2)
                            (create-queen 1 1)))
    (test-true "can attack on fourth diagonal"
               (can-attack? (create-queen 1 7)
                            (create-queen 0 6)))
    (test-false "cannot attack if falling diagonals are only the same when reflected across the longest falling diagonal"
                (can-attack? (create-queen 4 1)
                             (create-queen 2 5)))))
  (run-tests suite))
