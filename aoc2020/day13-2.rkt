#lang racket/base
(require racket/string
         racket/port)

;; 没有搞懂这个问题的意思，这题放弃
(define (earliest-all-bus-depart-ts note)
  ;; '(bus-id position)
  (define bus-poses (parse note))
  (define step (caar bus-poses))

  (define (all-match? n)
    (define num (* n step))
    (displayln (format "num ~A" num))
    (for/and ([bus-pos (in-list bus-poses)])
      (equal? (remainder num (car bus-pos))
              (cdr bus-pos))))

  (for/fold ([ts 0]) ([t (in-naturals)]
                      #:break (all-match? t))
    (* t step)))

(define (parse note)
  (define id-pos-pairs
    (for/list ([n (in-list (string-split note ","))]
               [p (in-naturals)])
      (cons n p)))
  (define id-poses (filter (lambda (n)
                           (not (equal? (car n) "x")))
                         id-pos-pairs))
  (map (λ (id-pos)
         (cons (string->number (car id-pos))
               (cdr id-pos)))
       id-poses))

(module+ test
  (require rackunit)

  (check-equal? (earliest-all-bus-depart-ts "17,x,13,19")
                3417)

  (check-equal? (parse "16,x,6,x,x,5") '((16 . 0) (6 . 2) (5 . 5)))

  "all tests run")
