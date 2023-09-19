#lang racket
(require threading)

#;(A rock
     B paper
     C scissors)

#;(X rock 1
     Y paper 2
     Z scissors 3)

#;(lost 0
        draw 3
        won  6)

(define pairs
  (~> (open-input-file "input02.txt")
      port->lines
      (map (lambda (s)
             (string-split s " "))
           _)))

; part one
(for/sum ([p (in-list pairs)])
  (match p
    ['("A" "X") (+ 1 3)]
    ['("A" "Y") (+ 2 6)]
    ['("A" "Z") (+ 3 0)]
    ['("B" "X") (+ 1 0)]
    ['("B" "Y") (+ 2 3)]
    ['("B" "Z") (+ 3 6)]
    ['("C" "X") (+ 1 6)]
    ['("C" "Y") (+ 2 0)]
    ['("C" "Z") (+ 3 3)]))

; part two
(for/sum ([p (in-list pairs)])
  (match p
    ['("A" "X") (+ 3 0)]
    ['("A" "Y") (+ 1 3)]
    ['("A" "Z") (+ 2 6)]
    ['("B" "X") (+ 1 0)]
    ['("B" "Y") (+ 2 3)]
    ['("B" "Z") (+ 3 6)]
    ['("C" "X") (+ 2 0)]
    ['("C" "Y") (+ 3 3)]
    ['("C" "Z") (+ 1 6)]))