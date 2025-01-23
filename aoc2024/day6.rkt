#lang racket

;; (guard (43 52) (0 1))
(struct guard (pos dir) #:transparent)

(define EAST '(0 1))
(define SOUTH '(1 0))
(define WEST '(0 -1))
(define NORTH '(-1 0))

(define-values (aguard amap)
  ;; find the guard init position and build the map as a 2D vector matrix
  (for/fold ([aguard '(-1 -1)] [amap '()] #:result (values (guard aguard NORTH) (list->vector (reverse amap))))
            ([line (in-lines (open-input-file "input6.txt" #:mode 'text))]
             [r (in-naturals)]
             #:do ((define poses ((compose1 list->vector string->list) line))))
    (if (vector-member #\^ poses)
        (values (list r (vector-member #\^ poses))
                (cons poses amap))
        (values aguard (cons poses amap)))))


(define map-height (vector-length amap))
(define map-width (vector-length (vector-ref amap 0)))


(define (forward aguard)
  (guard `(,(+ (first (guard-pos aguard)) (first (guard-dir aguard)))
           ,(+ (second (guard-pos aguard)) (second (guard-dir aguard))))
         (guard-dir aguard)))


(define (out-border? pos (m-height map-height) (m-width map-width))
  (let ([r (first pos)]
        [c (second pos)])
    (or (< r 0) (>= r m-width)
        (< c 0) (>= c m-height))))


(define (blocked? pos (amap amap))
  (eq? #\#
       (apply (curry matrix-ref amap) pos)))


(define (matrix-ref amap r c)
  (vector-ref (vector-ref amap r) c))


(define (turn-right aguard)
  (define (_turn-right dir)
    (cond
      [(equal? dir EAST) SOUTH]
      [(equal? dir SOUTH) WEST]
      [(equal? dir WEST) NORTH]
      [(equal? dir NORTH) EAST]
      [else (error "not match")]))
  
  (guard (guard-pos aguard) (_turn-right (guard-dir aguard))))


(define (count-visits amap (mark #\X))
  (for*/sum ([r (in-vector amap)]
             [c (in-vector r)])
    (if (eq? c mark) 1 0)))


(define (visit pos amap (mark #\X))
  (vector-set! (vector-ref amap (first pos)) (second pos) mark))

;;===================== part 1 =====================

;; the primary approach is marking each visited position
;; and then count the total number of visited positions
(visit (guard-pos aguard) amap)

(let loop ([pos aguard])
  (define front-pos (forward pos))     
  (cond
    [(out-border? (guard-pos front-pos)) (count-visits amap)]
    [(blocked? (guard-pos front-pos))
     (loop (turn-right pos))]
    [else
     (visit (guard-pos front-pos) amap)
     (loop (forward pos))]))

;;====================== part 2 =======================