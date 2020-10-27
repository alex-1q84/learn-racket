#lang racket

(require 2htdp/universe
         2htdp/image)

;; src: index of board or #f if no territory has been marked yet
(struct dice-world (src board gt))
(struct territory (index player dice x y))
(struct game (board player moves))
(struct move (action gt))


(define (roll-the-dice)
  (big-bang (create-world-of-dice-and-doom)
    (on-key interact-with-board)
    (on-draw draw-dice-world)
    (stop-when no-more-moves-in-world?
               draw-end-of-dice-world)))

(define (create-world-of-dice-and-doom)
  ;; FIXME
  (empty-scene 300 200))

(define (interact-with-board world key)
  ;; FIXME
  (displayln (format "~A key: ~A" interact-with-board key))
  world)

(define (draw-dice-world world)
  ;; FIXME
  (empty-scene 300 200))

(define (draw-end-of-dice-world world)
  ;; FIXME
  (empty-scene 300 200))

(define (no-more-moves-in-world? world)
  ;; FIXME
  #f)