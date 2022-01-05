#lang racket/base
(require racket/string)
(require racket/port)

(define (parse ldata)
  (values
   (string->number (car ldata))
   (map string->number (filter (lambda (n)
                                 (not (equal? n "x")))
                               (string-split (cadr ldata) ",")))))

;; find earliest bus in a list
;; the list structure is under
;; '((7 . 6) (13 . 10) (59 . 5) (31 . 22) (19 . 11))
;; '((bus-id . remain-timestamp-to-depart) (13 . 10))
(define (earliest-bus bus-remainders)
  (let loop ([bus (car bus-remainders)]
             [buses (cdr bus-remainders)])
    (cond
      [(null? buses) bus]
      [(> (cdr bus) (cdar buses))
       (loop (car buses) (cdr buses))]
      [else
       (loop bus (cdr buses))])))

(define (calc-min notes)
  (let-values ([(depart-ts bus-ids) (parse notes)])
    (define bus-remainders
      (for/list ([bus-id (in-list bus-ids)])
        (cons bus-id (- bus-id (remainder depart-ts bus-id)))))
    (define bus (earliest-bus bus-remainders))
    (* (car bus) (cdr bus))))

(calc-min (port->list read-line (open-input-file "input13.txt")))

(module+ test
  (require rackunit)

  (check-equal? (calc-min (list
              "939"
              "7,13,x,x,59,x,31,19"))
           295)


  "all tests run")
