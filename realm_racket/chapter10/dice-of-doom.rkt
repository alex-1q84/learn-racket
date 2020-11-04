#lang racket

(require 2htdp/universe
         2htdp/image)

;; src: index of board or #f if no territory has been marked yet
(struct dice-world (src board gt))
(struct territory (index player dice x y))
(struct game (board player moves))
(struct move (action gt))

(define INIT-PLAYER #f)
(define INIT-SPARE-DICE #f)

(define TEXT-SIZE 16)
(define TEXT-COLOR 'black)
(define WIDTH 400)
(define INFO-X-OFFSET 50)
(define INFO-Y-OFFSET 50)
(define FOCUS (text "FOCUS" TEXT-SIZE 'yellow))
;; FIXME use make-color with transparent
(define COLORS `('red 'blue))
(define IMG-LIST (list
                  (bitmap "graphics/dice1.png")
                  (bitmap "graphics/dice2.png")
                  (bitmap "graphics/dice3.png")
                  (bitmap "graphics/dice4.png")))


(define (ISCENE) (empty-scene 300 200))
(define (PLAIN) (empty-scene 300 200))


(define (roll-the-dice)
  (big-bang (create-world-of-dice-and-doom)
    (on-key interact-with-board)
    (on-draw draw-dice-world)
    (stop-when no-more-moves-in-world?
               draw-end-of-dice-world)))


(define (create-world-of-dice-and-doom)
  (define board (territory-build))
  (define gamet (game-tree board INIT-PLAYER INIT-SPARE-DICE))
  (define new-world (dice-world #f board gamet))
  (if (no-more-moves-in-world? new-world)
      (create-world-of-dice-and-doom)
      new-world))


(define (territory-build)
  ;; FIXME
  '())


(define (game-tree board p dice-point)
  (game board p (list (move '() #f))))


(define (interact-with-board world key)
  ;; FIXME
  (displayln (format "~A key: ~A" interact-with-board key))
  (cond [(key=? "left" key)
         (refocus-board world "left")]
        [(key=? "right" key)
         (refocus-board world "right")]
        [(key=? "p" key)
         (pass world)]
        [(key=? "\r" key)
         (mark world)]
        [(key=? "d" key)
         (unmark world)]
        [else world]))


(define (refocus-board world direction)
  ;; FIXME
  (displayln (format "refocus-board ~A" direction))
  world)


(define (pass world)
  ;; FIXME
  (displayln "pass world")
  world)


(define (mark world)
  ;; FIXME
  (displayln "mark world")
  world)


(define (unmark world)
  (displayln "unmark world")
  world)

(define (draw-dice-world world)
  ;; FIXME
  (add-player-info
   (game-player (dice-world-gt world))
   (add-board-to-scene world (ISCENE))))


(define (add-player-info player a-scene)
  ;; FIXME
  (define str (whose-turn player))
  (define txt (text str TEXT-SIZE TEXT-COLOR))
  (place-image txt (- WIDTH INFO-X-OFFSET) INFO-Y-OFFSET a-scene))


(define (whose-turn player)
  (format "Now it's player ~A turn" player))


(define (add-board-to-scene world a-scene)
  ;; FIXME
  (define board (dice-world-board world))
  (define player (game-player (dice-world-gt world)))
  (define focus? (dice-world-src world))
  (define trtry1 (first board))
  (define p-focus (territory-player trtry1))
  (define t-image (draw-territory trtry1))
  (define image (draw-focus focus? p-focus player t-image))
  (define base-s (add-territory trtry1 image a-scene))
  (for/fold ([s base-s]) ([t (rest board)])
    (add-territory t (draw-territory t) s)))


(define (add-territory t image a-scene)
  (place-image image (territory-x t) (territory-y t) a-scene))


(define (draw-focus marked? p-in-focus p t-image)
  (if (or (and (not marked?) (= p-in-focus p))
          (and marked? (not (= p-in-focus p))))
      (overlay FOCUS t-image)
      t-image))


(define (draw-territory trtry)
  (define color (color-chooser (territory-player trtry)))
  (overlay (hexagon color) (draw-dice (territory-dice trtry))))


(define (color-chooser n)
  (list-ref COLORS n))


(define (hexagon color)
  ;; FIXME
  color)


(define (draw-dice n)
  (define first-dice (get-dice-image 0))
  (define height-dice (image-height first-dice))
  (for/fold ([s first-dice]) ([i (- n 1)])
    (define dice-image (get-dice-image (+ i 1)))
    (define y-offset (* height-dice (+ .5 (* i .25))))
    (overlay/offset s 0 y-offset dice-image)))


(define (get-dice-image n)
  (list-ref IMG-LIST (modulo n (length IMG-LIST))))


(define (draw-end-of-dice-world world)
  (define board (dice-world-board world))
  (define message (text (won world) TEXT-SIZE TEXT-COLOR))
  (define background (add-board-to-scene world (PLAIN)))
  (overlay message background))


(define (won world)
  "You WON")


(define (no-more-moves-in-world? world)
  (define tree (dice-world-gt world))
  (define board (dice-world-board world))
  (define player (game-player tree))
  #f
  #;(or (no-more-moves? tree)
        (for/and ([t board]) (= territory-player t) player)))


(define (no-more-moves? tree)
  ;; FIXME
  #f)


