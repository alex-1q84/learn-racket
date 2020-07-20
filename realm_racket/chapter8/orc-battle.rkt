#lang racket

;; orc-world (player list-of-monsters how-many-attacks-the player-may-execute)
(struct orc-world (player lom attack#))

(struct player (health agility strength) #:mutable #:transparent)

(define MAX-HEALTH 35)
(define MAX-AGILITY 35)
(define MAX-STRENGTH 35)

(define MONSTER-HEALTH0 9)
(define CLUB-STRENGTH 10)

(define DAMAGE 2)

(struct monster ([health #:mutable]) #:transparent)
(struct orc monster (club) #:transparent)
(struct hydra monster () #:transparent)
(struct slime monster (sliminess) #:transparent)
(struct brigand () #:transparent)

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



(module+ test
  (require rackunit rackunit/text-ui)
  (check-equal?  (let ([p (player 1 2 3)])
                   (player-health+ p 5)
                   p)
                 (player 6 2 3))
  "all tests run")
