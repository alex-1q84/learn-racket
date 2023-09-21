#lang racket
(require threading)

(define assignment-pairs
  (~> (port->lines (open-input-file "input04.txt"))
      (map (lambda (s)
             (define pair (string-split s ","))
             (~> pair
                 (map (lambda (s)
                        (define assignments (string-split s "-"))
                        (map string->number assignments))
                      _)))
           _)))

; part one
(define (fully-contains? assign1 assign2)
  (and (<= (first assign1) (first assign2))
       (>= (second assign1) (second assign2))))

(for/sum ([assignment-pair (in-list assignment-pairs)])
  (cond
    [(or (fully-contains? (first assignment-pair) (second assignment-pair))
         (fully-contains? (second assignment-pair) (first assignment-pair)))
     1]
    [else 0]))


;part two
(define (overlap? assign1 assign2)
  (or (and (>= (first assign1) (first assign2))
           (<= (first assign1) (second assign2)))
      (and (>= (second assign1) (first assign2))
           (<= (second assign1) (second assign2)))
      (fully-contains? assign1 assign2)))

(for/sum ([assignment-pair (in-list assignment-pairs)])
  (cond
    [(overlap? (first assignment-pair) (second assignment-pair)) 1]
    [else 0]))