#lang racket/base
(require racket/match)

(define (big-add n1 n2)
  (for/foldr ([num null]
              [acc 0]
              #:result (match acc
                         [0 (apply string-append (map number->string num))]
                         [else (apply string-append (map number->string (cons acc num)))]))
    ([c1 n1]
     [c2 n2])
    (define-values (n remain)
      (quotient/remainder
       (+ (string->number (string c1))
          (string->number (string c2))
          acc)
       10))
    (values (cons remain num)
            n)))

(big-add "9000" "1001")
(big-add "9009" "1001")
(big-add "99999999999999999999999999999999999999999999999999999999999999999999999999999"
         "99999999999999999999999999999999999999999999999999999999999999999999999999999")
(+ 99999999999999999999999999999999999999999999999999999999999999999999999999999
         99999999999999999999999999999999999999999999999999999999999999999999999999999)
