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
(struct world (state direction))

(define (add-3-to-state current-state)
  (world (+ (world-state current-state)
            ((world-direction current-state) 3))
         (world-direction current-state)))

(define (set-ufo-direction w key)
  (displayln key)
  (world (world-state w)
         (cond
           [(key=? key "up") -]
           [(key=? key "down") +]
           [else (world-direction w)])))

(define (draw-a-ufo-onto-an-empty-scene current-state)
  (place-image UFO-IMAGE (/ WIDTH 2) (world-state current-state)
               (empty-scene WIDTH HEIGHT)))

(define (state-is-300 current-state)
  (>= (world-state current-state) 300))

(big-bang (world 0 +)
  (on-tick add-3-to-state)
  (on-key set-ufo-direction)
  (to-draw draw-a-ufo-onto-an-empty-scene)
  (stop-when state-is-300))