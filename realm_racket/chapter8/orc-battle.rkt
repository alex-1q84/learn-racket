#lang racket
(require 2htdp/universe
         2htdp/image)

;; orc-world (player list-of-monsters how-many-attacks-the player-may-execute)
(struct orc-world (player lom attack# target) #:mutable)

(struct player (health agility strength) #:mutable #:transparent)

(define MAX-HEALTH 35)
(define MAX-AGILITY 35)
(define MAX-STRENGTH 35)

(define MONSTER-HEALTH0 9)
(define CLUB-STRENGTH 10)

(define DAMAGE 2)

(define ATTACK# 4)

;; game end message
(define LOSE "YOU LOSE")
(define WIN "YOU WIN")

;; monster per row
(define PER-ROW 4)
(define MONSTER# 6)
(define SLIMINESS 5)
(define ORC-IMAGE (bitmap "graphics/orc.png"))
(define HYDRA-IMAGE (bitmap "graphics/hydra.png"))
(define SLIME-IMAGE (bitmap "graphics/slime.bmp"))
(define BRIGAND-IMAGE (bitmap "graphics/brigand.bmp"))

(struct monster ([health #:mutable]) #:transparent)
(struct imageable (image))
(struct orc monster (club) #:transparent)
(struct hydra monster () #:transparent)
(struct slime monster (sliminess) #:transparent)
(struct brigand imageable () #:transparent)

;; (list (orc MONSTER-HEALTH0 (add1 (random CLUB-STRENGTH))))

(define (interval+ base delta mx)
  (min (max 0 (+ base delta))
       mx))

(define (stab-orc an-orc)
  (set-monster-health! an-orc (- (monster-health an-orc) DAMAGE)))

(define (player-update! setter selector mx)
  (lambda (player delta)
    (setter player (interval+ (selector player) delta mx))))

(define player-health+
  (player-update! set-player-health! player-health MAX-HEALTH))

(define player-agility+
  (player-update! set-player-agility! player-agility MAX-AGILITY))

(define player-strength+
  (player-update! set-player-strength! player-strength MAX-STRENGTH))


(define (start)
  (big-bang (initialize-orc-world)
    (on-key player-act-on-monsters)
    (to-draw render-orc-battle)
    (stop-when end-of-orc-world? render-the-end)))


(define (initialize-orc-world)
  (define player0 (initialize-player))
  (define lom0 (initialize-monsters))
  (orc-world player0 lom0 (random-number-of-attacks player0) 0))


(define (end-of-orc-world? w)
  (or (win? w) (lose? w)))


(define (lose? w)
  (<= (player-health (orc-world-player w))
      0))


(define (win? w)
  (empty? (orc-world-lom w)))


(define (render-orc-battle w)
  (render-orc-world w (orc-world-target w) (instructions w)))


(define (render-the-end w)
  (render-orc-world w #f (message (if (lose? w) LOSE WIN))))


(define (render-orc-world world target msg)
  ;; FIXME
  (displayln world)
  (displayln target)
  (displayln msg)
  (rectangle 100 100 'solid 'white))


(define (player-act-on-monsters world key)
  (displayln (format "key: ~A" key))
  (cond
     [(zero? (orc-world-attack# world)) (void)]
     [(key=? "s" key) (stab world)]
     [(key=? "h" key) (heal world)]
     [(key=? "f" key) (flail world)]
     [(key=? "e" key) (end-turn world)]
     [(key=? "n" key) (initialize-orc-world)]
     [(key=? "right" key) (move-target world +1)]
     [(key=? "left" key)  (move-target world -1)]
     [(key=? "down" key)  (move-target world (+ PER-ROW))]
     [(key=? "up" key)    (move-target world (- PER-ROW))])
  (give-monster-turn-if-attack#=0 world)
  world)

(define (stab world)
  (displayln "stab")
  world)

(define (heal world)
  (displayln "heal")
  world)

(define (flail world)
  (displayln "flail")
  world)

(define (end-turn world)
  (displayln "end-turn")
  world)

(define (move-target world steps)
  (displayln (format "move-target ~A" steps))
  world)

(define (give-monster-turn-if-attack#=0 world)
  (displayln "give-monster-turn-if-attack#=0")
  world)

(define (initialize-player)
  (player MAX-HEALTH MAX-AGILITY MAX-STRENGTH))


(define (initialize-monsters)
  (build-list
   MONSTER#
   (lambda (_)
     (define health (random+ MONSTER-HEALTH0))
     (case (random 4)
       [(0) (orc ORC-IMAGE health (random CLUB-STRENGTH))]
       [(1) (hydra HYDRA-IMAGE health)]
       [(2) (slime SLIME-IMAGE health (random+ SLIMINESS))]
       [(3) (brigand BRIGAND-IMAGE health)]))))


(define (random-number-of-attacks p)
  (random-quotient (player-agility p) ATTACK#))


(define (random-quotient x y)
  (define div (quotient x y))
  (if (> 0 div) 0 (random+ (add1 div))))


(define (random+ n)
  (add1 (random n)))


(define (instructions world)
  "Welcome to Ocr Battle World")


(define (message status)
  (format "game ended with ~A" status))


(module+ test
  (require rackunit rackunit/text-ui)
  (check-equal?  (let ([p (player 1 2 3)])
                   (player-health+ p 5)
                   p)
                 (player 6 2 3))

  (check-equal? (let ([p (player 1 2 3)])
                  (player-strength+ p -3)
                  p)
                (player 1 2 0))

  "all tests run")
