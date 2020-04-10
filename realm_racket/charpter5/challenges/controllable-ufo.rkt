#lang racket
; Difficult.
; Add a feature to the UFO animation so that
; you can control the UFO using direction keys.
; Once you’ve done this, make the UFO leave a trail of gray circles
; when it moves so that it looks like smoke is coming out of it

(require 2htdp/universe 2htdp/image)

(define WIDTH 300)
(define HEIGHT 300)
(define UFO-IMAGE (bitmap "../../ufo-1_100px.png"))

; 一个包含状态和方向的结构，这样就可以把每次的位置和方向存下来
(struct world (state dir))
(struct posn (x y))
(struct direction (toward))

(define DOWN (direction +))
(define UP (direction -))
(define RIGHT (direction +))
(define LEFT (direction -))

(define (add-3-to-state current-state)
  (world (move (world-state current-state)
               (world-dir current-state)
               3)
         (world-dir current-state)))

(define (move pos direc step)
  (if (or (eq? direc LEFT) (eq? direc RIGHT))
      (posn ((direction-toward direc) (posn-x pos) step)
            (posn-y pos))
      (posn (posn-x pos)
            ((direction-toward direc) (posn-y pos) step))))

(define (set-ufo-direction w key)
  (displayln key)
  (world (world-state w)
         (cond
           [(key=? key "up") UP]
           [(key=? key "down") DOWN]
           [(key=? key "left") LEFT]
           [(key=? key "right") RIGHT]
           [else (world-dir w)])))

(define (draw-a-ufo-onto-an-empty-scene current-state)
  (place-image UFO-IMAGE
               (posn-x (world-state current-state))
               (posn-y (world-state current-state))
               (empty-scene WIDTH HEIGHT)))

(define (state-is-300 current-state)
  (or (>= (posn-y (world-state current-state)) 300)
      (>= (posn-x (world-state current-state)) 300)))

(big-bang (world (posn (/ WIDTH 2) 0)
                 DOWN)
  (on-tick add-3-to-state)
  (on-key set-ufo-direction)
  (to-draw draw-a-ufo-onto-an-empty-scene)
  (stop-when state-is-300))