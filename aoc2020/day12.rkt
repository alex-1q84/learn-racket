#lang racket

(define-struct pos (we sn) #:transparent)
;;  +
;;  ^
;; < > +
;;  v
;; up ^ for north, rigth > for east, down v for south, left < for west
;; - facing
;;   - east 0
;;   - north 90
;;   - west 180
;;   - south 270
(define-struct (ferry-pos pos) (facing) #:transparent)

;; we 货轮东西向位置
;; sn 货轮南北向位置
;; wp-rel-we 目标位置相对货船东西位置
;; wp-rel-sn 目标位置相对货船南北位置
(define-struct ferry-waypoint-pos (ferry-p rel-wp-p) #:transparent)

(define (ferry/move-through pos actions)
  (for/fold ([pos+ pos]) ([action (in-list actions)])
    (ferry/step pos+ action)))

(define (ferry/step pos act)
  (define dir (car act))
  (cond
    [(member dir '(L R)) (ferry/rotate pos dir (cadr act))]
    [(member dir '(W E S N F)) (ferry/move pos dir (cadr act))]))

(define (ferry/rotate pos dir degree)
  (match dir
    ['L (ferry-pos
         (pos-we pos)
         (pos-sn pos)
         (modulo (- (ferry-pos-facing pos)
                    (modulo degree 360))
                 360))]
    ['R (ferry-pos
         (pos-we pos)
         (pos-sn pos)
         (modulo (+ (ferry-pos-facing pos)
                    (modulo degree 360))
                 360))]))

(define (ferry/move pos dir delta)
  (match dir
    ['W (ferry-pos
         (- (pos-we pos) delta)
         (pos-sn pos)
         (ferry-pos-facing pos))]
    ['E (ferry-pos
         (+ (pos-we pos) delta)
         (pos-sn pos)
         (ferry-pos-facing pos))]
    ['S (ferry-pos
         (pos-we pos)
         (- (pos-sn pos) delta)
         (ferry-pos-facing pos))]
    ['N (ferry-pos
         (pos-we pos)
         (+ (pos-sn pos) delta)
         (ferry-pos-facing pos))]
    ['F (ferry/move pos (degree->facing (ferry-pos-facing pos)) delta)]))


;;====================== part 2 ================================

(define (ferry-waypoint/move-through p actions)
  (for/fold ([pos+ p]) ([action (in-list actions)])
    (ferry-waypoint/step pos+ action)))

(define (ferry-waypoint/step p act)
  (define dir (car act))
  (cond
    [(member dir '(L R)) (ferry-waypoint-pos
                          (ferry-waypoint-pos-ferry-p p)
                          (ferry-waypoint/rotate (ferry-waypoint-pos-rel-wp-p p) dir (cadr act)))]
    [(member dir '(W E S N F)) (ferry-waypoint/move p dir (cadr act))]))

;; 货船路点整体系统移动计算方法
(define (ferry-waypoint/move p dir delta)
  (match dir
    ['W (ferry-waypoint-pos
         (ferry-waypoint-pos-ferry-p p)
         (pos (- (fw/wp-rel-we p) delta)
              (fw/wp-rel-sn p)))]
    ['E (ferry-waypoint-pos
         (ferry-waypoint-pos-ferry-p p)
         (pos (+ (fw/wp-rel-we p) delta)
              (fw/wp-rel-sn p)))]
    ['S (ferry-waypoint-pos
         (ferry-waypoint-pos-ferry-p p)
         (pos (fw/wp-rel-we p)
              (- (fw/wp-rel-sn p) delta)))]
    ['N (ferry-waypoint-pos
         (ferry-waypoint-pos-ferry-p p)
         (pos (fw/wp-rel-we p)
              (+ (fw/wp-rel-sn p) delta)))]
    ['F (ferry-waypoint-pos
         (pos (+ (fw/pos-we p) (* delta (fw/wp-rel-we p)))
              (+ (fw/pos-sn p) (* delta (fw/wp-rel-sn p))))
         (ferry-waypoint-pos-rel-wp-p p))]))

(define (fw/pos-we p)
  (pos-we (ferry-waypoint-pos-ferry-p p)))

(define (fw/pos-sn p)
  (pos-sn (ferry-waypoint-pos-ferry-p p)))

(define (fw/wp-rel-we p)
  (pos-we (ferry-waypoint-pos-rel-wp-p p)))

(define (fw/wp-rel-sn p)
  (pos-sn (ferry-waypoint-pos-rel-wp-p p)))

(define (ferry-waypoint/rotate p dir degree)
  (define steps (abs (quotient degree 90)))
  (define r-method (match dir
                     ['L waypoint/rotate-left]
                     ['R waypoint/rotate-right]))
  (for/fold ([new-p p]) ([step (in-range steps)])
    (r-method new-p)))

(define (waypoint/rotate-left p)
  (cond
    [(and (> (pos-we p) 0)
          (>= (pos-sn p) 0))
     (pos (- (pos-sn p)) (pos-we p))]
    [(and (> (pos-we p) 0)
          (< (pos-sn p) 0))
     (pos (- (pos-sn p)) (pos-we p))]
    [(and (< (pos-we p) 0)
          (>= (pos-sn p) 0))
     (pos (- (pos-sn p)) (pos-we p))]
    [(and (< (pos-we p) 0)
          (< (pos-sn p) 0))
     (pos (- (pos-sn p)) (pos-we p))]))

(define (waypoint/rotate-right p)
  (cond
    [(and (> (pos-we p) 0)
          (>= (pos-sn p) 0))
     (pos (pos-sn p) (- (pos-we p)))]
    [(and (> (pos-we p) 0)
          (< (pos-sn p) 0))
     (pos (pos-sn p) (- (pos-we p)))]
    [(and (< (pos-we p) 0)
          (>= (pos-sn p) 0))
     (pos (pos-sn p) (- (pos-we p)))]
    [(and (< (pos-we p) 0)
          (< (pos-sn p) 0))
     (pos (pos-sn p) (pos-we p))]))

(define (degree->facing degree)
  (match degree
    [0 'E]
    [90 'S]
    [180 'W]
    [270 'N]))

(define (facing->degree facing)
  (match facing
    ['E 0]
    ['S 90]
    ['W 180]
    ['N 270]))

(define (list->actions l)
  (for/list ([act (in-list l)])
    (match act
      [(regexp #px"(\\w)(\\d+)" (list _ (app string->symbol face) (app string->number n)))
       (list face n)])))

(define (manhattan-distance pos)
  (+ (abs (pos-we pos))
     (abs (pos-sn pos))))

(let ([actions (list->actions (port->list read-line (open-input-file "input12.txt")))])
  (manhattan-distance (ferry/move-through (ferry-pos 0 0 0) actions)))

(module+ test
  (require rackunit)

  (let ([actions (list->actions (list
                                 "F10"
                                 "N3"
                                 "F7"
                                 "R90"
                                 "F11"))])

    (ferry-waypoint/move-through (ferry-waypoint-pos (pos 0 0) (pos 10 1)) actions)

    (check-equal? (manhattan-distance (ferry/move-through (ferry-pos 0 0 0) actions)) 25)
    (check-equal? (manhattan-distance (ferry-waypoint-pos-ferry-p
                                       (ferry-waypoint/move-through (ferry-waypoint-pos (pos 0 0) (pos 10 1)) actions)))
                  286))

  (check-equal? (waypoint/rotate-left (pos 3 2)) (pos -2 3))
  (check-equal? (waypoint/rotate-left (pos 3 -2)) (pos 2 3))
  (check-equal? (waypoint/rotate-left (pos -3 2)) (pos -2 -3))
  (check-equal? (waypoint/rotate-left (pos -3 -2)) (pos 2 -3))

  (check-equal? (waypoint/rotate-right (pos 3 2)) (pos 2 -3))
  (check-equal? (waypoint/rotate-right (pos 3 -2)) (pos -2 -3))
  (check-equal? (waypoint/rotate-right (pos -3 2)) (pos 2 3))
  (check-equal? (waypoint/rotate-right (pos -3 -2)) (pos -2 -3))

  (check-equal? (ferry-waypoint/rotate (pos 3 2) 'L 180) (pos -3 -2))

  (check-equal? (ferry-waypoint/move (ferry-waypoint-pos (pos 0 0) (pos 10 1)) 'F 2)
                (ferry-waypoint-pos (pos 20 2) (pos 10 1)))
  (check-equal? (ferry-waypoint/move (ferry-waypoint-pos (pos 0 0) (pos 10 1)) 'W 2)
                (ferry-waypoint-pos (pos 0 0) (pos 8 1)))
  (check-equal? (ferry-waypoint/move (ferry-waypoint-pos (pos 0 0) (pos 10 1)) 'E 2)
                (ferry-waypoint-pos (pos 0 0) (pos 12 1)))
  (check-equal? (ferry-waypoint/move (ferry-waypoint-pos (pos 0 0) (pos 10 1)) 'N 2)
                (ferry-waypoint-pos (pos 0 0) (pos 10 3)))
  (check-equal? (ferry-waypoint/move (ferry-waypoint-pos (pos 0 0) (pos 10 1)) 'S 2)
                (ferry-waypoint-pos (pos 0 0) (pos 10 -1)))


  (check-equal? (ferry-waypoint/step (ferry-waypoint-pos (pos 0 0) (pos 10 1)) (list 'S 2))
                (ferry-waypoint-pos (pos 0 0) (pos 10 -1)))

  "all test run")
