#lang racket
; Create an animation that runs the locomotive
; from just past the left margin to just past the right margin of the screen.
; Next, modify your program so the locomotive wraps around
; to the left side of the screen after passing the right margin.

(require 2htdp/universe 2htdp/image)

(define WIDTH 500)
(define HEIGHT 300)
(define LOCOMOTIVE-IMAGE (bitmap "../../steam-locomotive.png"))

(struct state (pos direction) #:mutable)

(define (add-3-to-state current-state)
  (when (is-on-horizontal-side (state-pos current-state))
    (set! LOCOMOTIVE-IMAGE (flip-horizontal LOCOMOTIVE-IMAGE)))
  (define direction
    (if (is-on-horizontal-side (state-pos current-state))
        (reverse-direction (state-direction current-state))
        (state-direction current-state)))
  (state (+ (state-pos current-state)
            (direction 3))
         direction))

(define (reverse-direction direction)
  (if (equal? direction +)
      -
      +))

(define (is-on-horizontal-side pos)
  (or (just-past-the-left pos)
      (just-past-the-right pos)))

(define (draw-a-locomotive-onto-an-empty-scene current-state)
  (place-image LOCOMOTIVE-IMAGE (state-pos current-state) (/ HEIGHT 2)
               (empty-scene WIDTH HEIGHT)))

(define (just-past-the-left pos)
  (<= pos 0))

(define (just-past-the-right pos)
  (>= pos WIDTH))

(big-bang (state 0 -)
  (on-tick add-3-to-state)
  (to-draw draw-a-locomotive-onto-an-empty-scene))