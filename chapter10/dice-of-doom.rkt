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



(define (ISCENE) (empty-scene 300 200))
(define (PLAIN) (empty-scene 300 200))


(define (roll-the-dice)
  (big-bang (create-world-of-dice-and-doom)
    (on-key interact-with-board)
    (on-draw draw-dice-world)
    (stop-when no-more-moves-in-world?
               draw-end-of-dice-world)))


(define (create-world-of-dice-and-doom)
  ;; FIXME
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
  world)


(define (draw-dice-world world)
  ;; FIXME
  (add-player-info
   (game-player (dice-world-gt world))
   (add-board-to-scene world (ISCENE))))


(define (add-player-info p a-scene)
  ;; FIXME
  a-scene)


(define (add-board-to-scene world a-scene)
  ;; FIXME
  a-scene)


(define (draw-end-of-dice-world world)
  ;; FIXME
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


