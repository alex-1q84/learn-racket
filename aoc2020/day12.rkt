#lang racket

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
(define-struct ferry-pos (facing we sn) #:transparent)

(define (move-through pos actions)
  (for/fold ([pos+ pos]) ([action (in-list actions)])
    (step pos+ action)))

(define (step pos act)
  (define dir (car act))
  (cond
    [(member dir '(L R)) (turn pos dir (cadr act))]
    [(member dir '(W E S N F)) (move pos dir (cadr act))]))

(define (turn pos dir delta)
  (match dir
    ['L (ferry-pos (modulo (- (ferry-pos-facing pos)
                              (modulo delta 360))
                           360)
                   (ferry-pos-we pos)
                   (ferry-pos-sn pos))]
    ['R (ferry-pos (modulo (+ (ferry-pos-facing pos)
                              (modulo delta 360))
                           360)
                   (ferry-pos-we pos)
                   (ferry-pos-sn pos))]))

(define (move pos dir delta)
  (match dir
    ['W (ferry-pos
         (ferry-pos-facing pos)
         (- (ferry-pos-we pos) delta)
         (ferry-pos-sn pos))]
    ['E (ferry-pos
         (ferry-pos-facing pos)
         (+ (ferry-pos-we pos) delta)
         (ferry-pos-sn pos))]
    ['S (ferry-pos (ferry-pos-facing pos)
                   (ferry-pos-we pos)
                   (- (ferry-pos-sn pos) delta))]
    ['N (ferry-pos (ferry-pos-facing pos)
                   (ferry-pos-we pos)
                   (+ (ferry-pos-sn pos) delta))]
    ['F (move pos (degree->facing (ferry-pos-facing pos)) delta)]))

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
  (+ (abs (ferry-pos-we pos))
     (abs (ferry-pos-sn pos))))

(let ([actions (list->actions (port->list read-line (open-input-file "input12.txt")))])
  (manhattan-distance (move-through (ferry-pos 0 0 0) actions)))

(module+ test
  (require rackunit)

  (let ([actions (list->actions (list
                                 "F10"
                                 "N3"
                                 "F7"
                                 "R90"
                                 "F11"))])
    (manhattan-distance (move-through (ferry-pos 0 0 0) actions)))

  "all test run")
