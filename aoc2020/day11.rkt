#lang racket

(define seats
  (call-with-input-file "input11.txt"
    (lambda (in)
      (for/vector ([line (in-lines in)])
        (list->vector (string->list line))))))

;; 返回座位修改状态
(define (occupy-seats seat-map)
  (define rows (vector-length seat-map))
  (define cols (vector-length (vector-ref seat-map 0)))

  (for/vector ([row (vector-length seat-map)])
    (for/vector ([col (vector-length (vector-ref seat-map row))])
      (define pos (pos-at seat-map row col))
      (cond
        ;; - If a seat is empty (L) and there are no occupied seats adjacent to it,
        ;; the seat becomes occupied.
        ;; - If a seat is occupied (#) and four or more seats adjacent to it
        ;; are also occupied, the seat becomes empty.
        ;; - Otherwise, the seat's state does not change.
        [(not (is-seat? (pos-at seat-map row col))) pos]
        [(and (not (occupied? pos))
              (= (count-occupied (get-adjacent-seats seat-map row col rows cols)) 0))
         #\#]
        [(and (occupied? pos)
              (>= (count-occupied (get-adjacent-seats seat-map row col rows cols)) 4))
         #\L]
        [else pos]))))

(define (occupied? pos)
  (equal? #\# pos))

(define (count-occupied seats)
  (for/sum ([s (in-list seats)])
    (if (occupied? s)
        1
        0)))

(define (get-adjacent-seats seat-map row col rows cols)
  (define poses (for*/list ([r (in-range (max 0 (- row 1)) (min rows (+ row 2)))]
               [c (in-range (max 0 (- col 1)) (min cols (+ col 2)))]
               #:when (not (equal? (list r c) (list row col))))
     (list r c)))
  (for/list ([pos (in-list poses)])
    (pos-at seat-map (first pos) (second pos))))

(define (is-seat? seat)
  (or (equal? seat #\L)
      (equal? seat #\#)))

(define (pos-at seat-map row col)
  (vector-ref (vector-ref seat-map row) col))

(define (count-occupied-seats-stable seat-map)
  (let loop ([new-seat-map (occupy-seats seat-map)])
    (cond
      [(equal? new-seat-map (occupy-seats new-seat-map))
       (values (count-occupied (to-seats-list new-seat-map))
               new-seat-map)]
      [else (loop (occupy-seats new-seat-map))])))

(define (to-seats-list seat-map)
  (for*/list ([row (in-vector seat-map)]
              [col (in-vector row)])
    col))

(define (disply-seat-map seat-map)
  (displayln
   (string-join
    (for/list ([row (in-vector seat-map)])
      (list->string (vector->list row)))
    "\n")))

(define-values (count _) (count-occupied-seats-stable seats))
(displayln count)

(module+ test

  (define sample-seat-map-data (list
                           "L.LL.LL.LL"
                           "LLLLLLL.LL"
                           "L.L.L..L.."
                           "LLLL.LL.LL"
                           "L.LL.LL.LL"
                           "L.LLLLL.LL"
                           "..L.L....."
                           "LLLLLLLLLL"
                           "L.LLLLLL.L"
                           "L.LLLLL.LL"))
  (define sample-seat-map
    (for/vector ([row (in-list sample-seat-map-data)])
      (list->vector (string->list row))))

  (disply-seat-map (occupy-seats sample-seat-map))

  (define-values (count new-seat-map)
    (count-occupied-seats-stable sample-seat-map))

  (displayln count)
  (disply-seat-map new-seat-map)

  (displayln (length (to-seats-list sample-seat-map)))

  "all test run")
