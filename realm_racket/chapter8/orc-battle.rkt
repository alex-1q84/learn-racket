#lang racket
(require 2htdp/universe
         2htdp/image)

;; orc-world (player list-of-monsters how-many-attacks-the player-may-execute)
(struct orc-world (player lom attack# target) #:mutable)

(struct player (health agility strength armor) #:mutable #:transparent)

(define MAX-HEALTH 35)
(define MAX-AGILITY 35)
(define MAX-STRENGTH 35)
(define MAX-ARMOR 35)

(define MONSTER-HEALTH0 9)
(define CLUB-STRENGTH 10)

(define DAMAGE 2)
(define HEALING 2)
(define AGILITY-GAIN 2)
(define STRENGTH-GAIN 2)
(define ARMOR-GAIN 5)
(define STAB-DAMAGE 2)

(define FLAIL-DAMAGE 4)
(define ATTACK# 4)

;; brigand damages
(define HEALTH-DAMAGE 2)
(define AGILITY-DAMAGE 2)
(define STRENGTH-DAMAGE 2)

(define INSTRUCTION-TEXT-SIZE 16)
(define INSTRUCTION-COLOR "black")
(define ATTACK-COLOR 'red)
(define REMAINING "Remain turn point ")
(define INSTRACTION-TEXT (text "Ah! ah! ah! You are stuck in this Orc Battle World"
                               INSTRUCTION-TEXT-SIZE
                               INSTRUCTION-COLOR))

(define MESSAGES-SIZE 24)
(define MESSAGE-COLOR 'black)

(define V-SPACER (rectangle 0 10 "solid" "white"))
(define H-SPACER (rectangle 10 0 "solid" "white"))

;; status bar
(define STRENGTH-COLOR "orange")
(define STRENGTH "Strength")
(define AGILITY-COLOR "green")
(define AGILITY "Agility")
(define HEALTH-COLOR "RED")
(define HEALTH "Health")
(define ARMOR-COLOR "gold")
(define ARMOR "Armor")

(define HEALTH-BAR-WIDTH 60)
(define HEALTH-BAR-HEIGH 10)
(define HEALTH-SIZE 14)

; (define TARGET (rectangle 40 2 'solid "yellow"))
(define DEAD-TEXT (text "Dead" HEALTH-SIZE "red"))
(define MONSTER-COLOR "light blue")

;; game end message
(define LOSE "YOU LOSE")
(define WIN "YOU WIN")

;; monster per row
(define PER-ROW 4)
(define MONSTER# (* PER-ROW 4))
(define SLIMINESS 5)

;; charactor images
(define ORC-IMAGE (bitmap "graphics/orc.png"))
(define HYDRA-IMAGE (bitmap "graphics/hydra.png"))
(define SLIME-IMAGE (bitmap "graphics/slime.bmp"))
(define BRIGAND-IMAGE (bitmap "graphics/brigand.bmp"))
(define PLAYER-IMAGE (bitmap "graphics/player.bmp"))

(struct imageable (image) #:transparent)

(struct monster imageable ([health #:mutable]) #:transparent)
(struct orc monster (club) #:transparent)
(struct hydra monster () #:transparent)
(struct slime monster (sliminess) #:transparent)
(struct brigand monster () #:transparent)

;; (list (orc MONSTER-HEALTH0 (add1 (random CLUB-STRENGTH))))

(define (interval+ base delta mx)
  (min (max 0 (+ base delta))
       mx))

(define (stab-orc an-orc)
  (set-monster-health! an-orc (interval- (monster-health an-orc) DAMAGE)))

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
  (player-dead? (orc-world-player w)))


(define (win? w)
  (all-dead? (orc-world-lom w)))


(define (player-dead? p)
  (or (= (player-health p) 0)
      (= (player-strength p) 0)
      (= (player-agility p) 0)))


(define (all-dead? lom)
  (not (ormap monster-alive? lom)))


(define (monster-alive? m)
  (> (monster-health m) 0))


(define (render-orc-battle w)
  (render-orc-world w (orc-world-target w) (instructions w)))


(define (render-the-end w)
  (render-orc-world w #f (message (if (lose? w) LOSE WIN))))


(define (render-orc-world world target msg)
  (displayln world)
  (displayln (format "target to ~A" target))
  (displayln (format "msg ~A" msg))
  (define i-player (render-player (orc-world-player world)))
  (define i-monster (render-monsters (orc-world-lom world) target))
  (above V-SPACER
         (beside H-SPACER
                 i-player
                 H-SPACER H-SPACER H-SPACER
                 (above i-monster
                        V-SPACER V-SPACER V-SPACER
                        msg)
                 H-SPACER)
         V-SPACER))


(define (render-player p)
  (define s (player-strength p))
  (define a (player-agility p))
  (define h (player-health p))
  (define armor (player-armor p))
  (above/align
   "left"
   (status-bar s MAX-STRENGTH STRENGTH-COLOR STRENGTH)
   V-SPACER
   (status-bar a MAX-AGILITY AGILITY-COLOR AGILITY)
   V-SPACER
   (status-bar h MAX-HEALTH HEALTH-COLOR HEALTH)
   V-SPACER
   (status-bar armor MAX-HEALTH ARMOR-COLOR ARMOR)
   V-SPACER V-SPACER V-SPACER
   PLAYER-IMAGE))


(define (render-monsters lom with-target)
  ;; the currently targeted monster (if needed)
  (define target
    (if (number? with-target)
        (list-ref lom with-target)
        'a-silly-symbol-that-cannot-be-eq-to-an-orc))
  
  (define (render-one-monster m)
    (define monster-image (imageable-image m))
    (define image
      (if (eq? m target)
          (underlay (rectangle (+ 2 (image-width monster-image))
                               (+ 2 (image-height monster-image))
                               'solid
                               "orange")
                    monster-image)
          (imageable-image m)))
    (define health (monster-health m))
    (define health-bar
      (if (= health 0)
          (overlay DEAD-TEXT (status-bar 0 1 'white ""))
          (status-bar health MONSTER-HEALTH0 MONSTER-COLOR "")))
    (above health-bar image))

  (arrange (map render-one-monster lom)))


(define (arrange lom)
  (cond
    [(empty? lom) empty-image]
    [else (define r (apply beside (take lom PER-ROW)))
          (above r (arrange (drop lom PER-ROW)))]))


(define (status-bar v-current-point v-max-point color label)
  (displayln (format "~A ~A" label v-current-point))
  (define w (* (/ v-current-point v-max-point) HEALTH-BAR-WIDTH))
  (define f (rectangle w HEALTH-BAR-HEIGH 'solid color))
  (define b (rectangle HEALTH-BAR-WIDTH HEALTH-BAR-HEIGH 'solid "white"))
  (define bar (overlay/align "left" "top" f b))
  (beside bar H-SPACER (text label HEALTH-SIZE color)))


(define (player-act-on-monsters world key)
  (displayln (format "key: ~A" key))
  (cond
    [(zero? (orc-world-attack# world)) (void)]
    [(key=? "s" key) (stab world)]
    [(key=? "h" key) (heal world)]
    [(key=? "a" key) (regain-agility world)]
    [(key=? "t" key) (regain-strength world)]
    [(key=? "d" key) (equipment-shield world)]
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
  (decrease-attack# world)
  (define target
    (list-ref (orc-world-lom world) (orc-world-target world)))
  (define damage
    (random-quotient (player-strength (orc-world-player world))
                     STAB-DAMAGE))
  (damage-monster target damage))


(define (heal world)
  (displayln "heal")
  (decrease-attack# world)
  (player-health+ (orc-world-player world) HEALING))


(define (regain-agility world)
  (displayln "regain agility")
  (decrease-attack# world)
  (player-agility+ (orc-world-player world) AGILITY-GAIN))


(define (regain-strength world)
  (displayln "regain strength")
  (decrease-attack# world)
  (player-strength+ (orc-world-player world) STRENGTH-GAIN))

(define (equipment-shield world)
  (define p (orc-world-player world))
  (when (= (player-armor p) 0)
    (decrease-attack# world)
    (set-player-armor! p ARMOR-GAIN)))

(define (flail world)
  (displayln "flail")
  (decrease-attack# world)
  (define target (current-target world))
  (define alive (filter monster-alive? (orc-world-lom world)))
  (define pick#
    (min
     (random-quotient (player-strength (orc-world-player world))
                      FLAIL-DAMAGE)
     (length alive)))
  (define getem (cons target (take alive pick#)))
  (for-each (lambda (m) (damage-monster m 1)) getem))


(define (current-target world)
  (list-ref (orc-world-lom world) (orc-world-target world)))


(define (decrease-attack# world)
  (set-orc-world-attack#! world (sub1 (orc-world-attack# world))))


(define (damage-monster m delta)
  (set-monster-health! m (interval- (monster-health m) delta)))


(define (damage-player p damage)
  (cond
    [(> (player-armor p) 0)
     (define current-armor (player-armor p))
     (set-player-armor! p (interval- current-armor damage))
     (player-health+ p (interval- damage current-armor))]
    [else
     (player-health+ p damage)]))


(define (interval- base delta)
  (if (< base delta)
      0
      (- base delta)))


(define (end-turn world)
  (displayln "end-turn")
  (set-orc-world-attack#! world 0))


(define (move-target world steps)
  (displayln (format "move-target ~A" steps))
  (define new (+ (orc-world-target world) steps))
  (set-orc-world-target! world (modulo new MONSTER#)))


(define (give-monster-turn-if-attack#=0 world)
  (displayln "give-monster-turn-if-attack#=0")
  (when (zero? (orc-world-attack# world))
    (define player (orc-world-player world))
    (all-monsters-attack-player player (orc-world-lom world))
    (set-orc-world-attack#! world (random-number-of-attacks player))))


(define (all-monsters-attack-player player lom)
  (define (one-monster-attacks-player m)
    (cond
      [(orc? m)
       (damage-player player (random+ (orc-club m)))]
      [(hydra? m)
       (damage-player player (random+ (monster-health m)))]
      [(slime? m)
       (damage-player player -1)
       (player-agility+ player (- (random+ (slime-sliminess m))))]
      [(brigand? m)
       (case (random 3)
         [(0) (damage-player player HEALTH-DAMAGE)]
         [(1) (player-agility+ player (- AGILITY-DAMAGE))]
         [(2) (player-strength+ player (- STRENGTH-DAMAGE))])]))

  (define live-monsters (filter monster-alive? lom))
  (for-each one-monster-attacks-player live-monsters))


(define (initialize-player)
  (player MAX-HEALTH MAX-AGILITY MAX-STRENGTH 0))


(define (initialize-monsters)
  (build-list
   MONSTER#
   (lambda (_)
     (define health (random+ MONSTER-HEALTH0))
     (case (random 4)
       [(0) (orc ORC-IMAGE health (random+ CLUB-STRENGTH))]
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
  (define na (number->string (orc-world-attack# world)))
  (define ra (string-append REMAINING na))
  (define txt (text ra INSTRUCTION-TEXT-SIZE ATTACK-COLOR))
  (above txt INSTRACTION-TEXT))


(define (message status)
  (format "game ended with ~A" status)
  (text status MESSAGES-SIZE MESSAGE-COLOR))


(module+ test
  (require rackunit rackunit/text-ui)
  (check-equal?  (let ([p (player 1 2 3 4)])
                   (player-health+ p 5)
                   p)
                 (player 6 2 3 4))

  (check-equal? (let ([p (player 1 2 3 3)])
                  (player-strength+ p -3)
                  p)
                (player 1 2 0 3))

  "all tests run")
