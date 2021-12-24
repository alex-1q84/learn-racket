#lang racket

(define seats
  (call-with-input-file "input11.txt"
    (lambda (in)
      (for/vector ([line (in-lines in)])
        (list->vector (string->list line))))))

;; 返回座位修改状态
(define (occupy-seats seat-map [gas get-adjacent-seats] [max-oc-seats 4])
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
              (= (count-occupied (gas seat-map row col rows cols))
                 0))
         #\#]
        [(and (occupied? pos)
              (>= (count-occupied (gas seat-map row col rows cols))
                  max-oc-seats))
         #\L]
        [else pos]))))

(define (occupied? pos)
  (equal? #\# pos))

(define (count-occupied seats [test occupied?])
  (for/sum ([s (in-list seats)])
    (if (test s)
        1
        0)))

(define (get-adjacent-seats seat-map row col rows cols)
  (define poses (for*/list ([r (in-range (max 0 (- row 1)) (min rows (+ row 2)))]
               [c (in-range (max 0 (- col 1)) (min cols (+ col 2)))]
               #:when (not (equal? (list r c) (list row col))))
     (list r c)))
  (for/list ([pos (in-list poses)])
    (pos-at seat-map (first pos) (second pos))))

;; 八个方向上获取每一个方向上能看到的最近座位
(define (get-each-direction-first-seats seat-map row col rows cols)
  (define (first-seen-seat seat-map row col step-row step-col)
    (let loop ([new-row (+ row step-row)]
               [new-col (+ col step-col)])
      (cond
        [(or (< new-row 0) (< new-col 0)) false]
        [(or (= new-row rows) (= new-col cols)) false]
        [(is-seat? (pos-at seat-map new-row new-col))
         (pos-at seat-map new-row new-col)]
        [else
         (loop (+ new-row step-row) (+ new-col step-col))])))

  (filter (lambda (p)
            (not (false? p)))
          (list
           (first-seen-seat seat-map row col -1 -1)
           (first-seen-seat seat-map row col -1 0)
           (first-seen-seat seat-map row col -1 1)
           (first-seen-seat seat-map row col 0 -1)
           (first-seen-seat seat-map row col 0 1)
           (first-seen-seat seat-map row col 1 -1)
           (first-seen-seat seat-map row col 1 0)
           (first-seen-seat seat-map row col 1 1))))

(define (is-seat? seat)
  (or (equal? seat #\L)
      (equal? seat #\#)))

(define (pos-at seat-map row col)
  (vector-ref (vector-ref seat-map row) col))

(define (count-occupied-seats-stable seat-map [gas get-adjacent-seats] [max-oc-seats 4])
  (let loop ([new-seat-map (occupy-seats seat-map gas max-oc-seats)])
    (cond
      [(equal? new-seat-map (occupy-seats new-seat-map gas max-oc-seats))
       (values (count-occupied (to-seats-list new-seat-map))
               new-seat-map)]
      [else (loop (occupy-seats new-seat-map gas max-oc-seats))])))

(define (to-seats-list seat-map)
  (for*/list ([row (in-vector seat-map)]
              [col (in-vector row)])
    col))

(define (disply-seat-map seat-map)
  (displayln (printable-seat-map seat-map)))

(define (printable-seat-map seat-map)
  (string-join
   (for/list ([row (in-vector seat-map)])
     (list->string (vector->list row)))
   "\n"))

(let-values ([(count _) (count-occupied-seats-stable seats)])
  (displayln count))

(let-values ([(count _) (count-occupied-seats-stable seats get-each-direction-first-seats 5)])
  (displayln count))

(module+ test
  (require rackunit)

  (define (string-list->seat-map l)
    (for/vector ([row (in-list l)])
      (list->vector (string->list row))))

  (define sample-seat-map
    (string-list->seat-map (list
                            "L.LL.LL.LL"
                            "LLLLLLL.LL"
                            "L.L.L..L.."
                            "LLLL.LL.LL"
                            "L.LL.LL.LL"
                            "L.LLLLL.LL"
                            "..L.L....."
                            "LLLLLLLLLL"
                            "L.LLLLLL.L"
                            "L.LLLLL.LL")))

  (disply-seat-map (occupy-seats sample-seat-map get-adjacent-seats 4))

  (displayln "occupy seats with new rule")
  (disply-seat-map (occupy-seats
                    (occupy-seats sample-seat-map get-each-direction-first-seats 5)
                    get-each-direction-first-seats
                    5))

  (let-values ([(count new-seat-map)
                (count-occupied-seats-stable sample-seat-map)])
    (displayln count)
    (disply-seat-map new-seat-map))

  (let-values ([(count new-seat-map)
                (count-occupied-seats-stable sample-seat-map get-each-direction-first-seats 5)])
    (displayln count)
    (disply-seat-map new-seat-map))

  (let ([sample-seat-map (string-list->seat-map (list
                                                 ".......#."
                                                 "...#....."
                                                 ".#......."
                                                 "........."
                                                 "..#L....#"
                                                 "....#...."
                                                 "........."
                                                 "#........"
                                                 "...#....."))])
    (get-each-direction-first-seats sample-seat-map 4 3 10 10)
    (check-equal? (count-occupied (get-each-direction-first-seats sample-seat-map 4 3 10 10))
                  8))

  (let ([sample-seat-map (string-list->seat-map (list
                                                 "............."
                                                 ".L.L.#.#.#.#."
                                                 "............."
                                                 ))])
    (check-equal? (count-occupied
                   (get-each-direction-first-seats sample-seat-map 1 1 3 10)
                   (lambda (pos)
                     (equal? pos #\L)))
                  1))

  "all test run")
