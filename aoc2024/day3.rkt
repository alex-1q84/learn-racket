#lang racket

(define (extract-ops pattern str)
  (regexp-match* pattern str))

(define (execute-ops1 ops)
  (for/sum ([op (in-list ops)])
    (match op
      [(regexp #px"mul\\((\\d+),(\\d+)\\)" (list _ a b))
       (* (string->number a) (string->number b))])))

;=================== part 1 ====================
(for/sum ([line (in-lines (open-input-file "input3.txt" #:mode 'text))])
  (execute-ops1 (extract-ops #px"mul\\(\\d+,\\d+\\)" line)))

;=================== part 2 ====================
(define (execute-ops2 ops)
  (for/fold ([acc 0]
             [acc? #t]
             #:result acc)
            ([op (in-list ops)])
    (match op
      [(regexp #px"mul\\((\\d+),(\\d+)\\)" (list _ a b))
       (if acc?
           (values (+ acc (* (string->number a) (string->number b))) acc?)
           (values acc acc?))]
      ["don't()" (values acc #f)]
      ["do()" (values acc #t)])))

(execute-ops2 (extract-ops #px"mul\\(\\d+,\\d+\\)|do\\(\\)|don't\\(\\)"
                           (port->string (open-input-file "input3.txt" #:mode 'text))))
