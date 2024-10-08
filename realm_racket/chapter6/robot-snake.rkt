#lang racket

(require 2htdp/universe 2htdp/image racket/random)

(define TICK-RATE 1/3)
(define SIZE 30)
(define EXPIRATION-TIME 100)
(define ENDGAME-TEXT-SIZE 80)

; snake head
(define HEAD-IMG (bitmap "graphics/head.gif"))
(define SEG-IMG (bitmap "graphics/body.gif"))

(define HEAD-LEFT-IMG HEAD-IMG)
(define HEAD-DOWN-IMG (rotate 90 HEAD-LEFT-IMG))
(define HEAD-UP-IMG (flip-vertical HEAD-DOWN-IMG))
(define HEAD-RIGHT-IMG (flip-horizontal HEAD-LEFT-IMG))
(define SEG-SIZE 15)

; goo
(define GOO-IMG (bitmap "graphics/goo.gif"))
(define RED-GOO-IMG (bitmap "graphics/red-goo.gif"))

(define MT-SCENE (empty-scene (* SEG-SIZE SIZE) (* SEG-SIZE SIZE)))


(struct pit (snake goos))

(struct snake (dir segs))

(struct posn (x y))

(struct goo (loc expire type))

(define (start-game)
  (big-bang (pit (snake "right" (list (posn 1 1)))
                 (random-list 3 8 fresh-goo))
    (on-tick next-pit TICK-RATE)
    (on-key direct-snake)
    (to-draw render-pit)
    (stop-when dead? render-end)))

(define (random-list min max generater)
  (define amount (random min (add1 max)))
  (let fill-list ([n amount])
    (if (zero? n)
        empty
        (cons (generater) (fill-list (sub1 n))))))

(define (next-pit w)
  (define snake (pit-snake w))
  (define goos (pit-goos w))
  (define goo-to-eat (can-eat snake goos))
  (match goo-to-eat
    [(goo _ _ 'green)
     (pit (grow snake) (age-goo (eat goos goo-to-eat)))]
    [(goo _ _ 'red)
     (pit (grow (grow snake)) (age-goo (eat goos goo-to-eat)))]
    [#f
     (pit (slither snake) (age-goo goos))]))

(define (can-eat snake goos)
  (cond [(empty? goos) #f]
        [else (if (close? (snake-head snake) (first goos))
                  (first goos)
                  (can-eat snake (rest goos)))]))

(define (close? s g)
  (posn=? s (goo-loc g)))

(define (eat goos goo-to-eat)
  (cons (fresh-goo) (remove goo-to-eat goos)))

(define (grow sn)
  (snake (snake-dir sn)
         (cons (next-head sn) (snake-segs sn))))

(define (slither sn)
  (snake (snake-dir sn)
         (cons (next-head sn) (all-but-last (snake-segs sn)))))

(define (all-but-last segs)
  (cond [(empty? (rest segs)) empty]
        [else (cons (first segs) (all-but-last (rest segs)))]))

(define (next-head sn)
  (define head (snake-head sn))
  (define dir (snake-dir sn))
  (cond [(string=? dir "up") (posn-move head 0 -1)]
        [(string=? dir "down") (posn-move head 0 1)]
        [(string=? dir "left") (posn-move head -1 0)]
        [(string=? dir "right") (posn-move head 1 0)]))

(define (posn-move p dx dy)
  (posn (+ (posn-x p) dx)
        (+ (posn-y p) dy)))

(define (age-goo goos)
  (rot (renew goos)))

(define (rot goos)
  (cond [(empty? goos) empty]
        [else (cons (decay (first goos)) (rot (rest goos)))]))

(define (decay g)
  (if (> 0 (goo-expire g))
      (goo (goo-loc g) (sub1 (goo-expire g)))
      g))

(define (renew goos)
  (cond [(empty? goos) empty]
        [(rotten? (first goos))
         (cons (fresh-goo) (renew (rest goos)))]
        [else
         (cons (first goos) (renew (rest goos)))]))

(define (rotten? g)
  (zero? (goo-expire g)))

(define (fresh-goo)
  (goo (posn (add1 (random (sub1 SIZE)))
             (add1 (random (sub1 SIZE))))
       EXPIRATION-TIME
       (random-ref '(green red))))

(define (direct-snake w ke)
  (cond [(dir? ke) (world-change-dir w ke)]
        [else w]))

(define (dir? ke)
  (or (key=? ke "up")
      (key=? ke "down")
      (key=? ke "left")
      (key=? ke "right")))

(define (world-change-dir w d)
  (define the-snake (pit-snake w))
  (cond [(and (opposite-dir? (snake-dir the-snake) d)
              ;; consists of the head and at least one segment
              (cons? (rest (snake-segs the-snake))))
         (stop-with w)]
        [else
         (pit (snake-change-dir the-snake d) (pit-goos w))]))

(define (render-pit w)
  (snake+scene (pit-snake w)
               (goo-list+scene (pit-goos w) MT-SCENE)))

(define (snake+scene sn scene)
  (define snake-body-scene
    (img-list+scene (snake-body sn) SEG-IMG scene))
  (define dir (snake-dir sn))
  (img+scene (snake-head sn)
             (cond [(string=? "up" dir) HEAD-UP-IMG]
                   [(string=? "down" dir) HEAD-DOWN-IMG]
                   [(string=? "left" dir) HEAD-LEFT-IMG]
                   [(string=? "right" dir) HEAD-RIGHT-IMG])
             snake-body-scene))

(define (img-list+scene posns img scene)
  (cond [(empty? posns) scene]
        [else (img+scene
               (first posns)
               img
               (img-list+scene (rest posns) img scene))]))

(define (img+scene posn img scene)
  (place-image img
               (* (posn-x posn) SEG-SIZE)
               (* (posn-y posn) SEG-SIZE)
               scene))

(define (goo-list+scene goos scene)
  (define (get-posns-from-goo goos)
    (cond [(empty? goos) empty]
          [else (cons (goo-loc (first goos))
                      (get-posns-from-goo (rest goos)))]))

  (define (goos+scene goo-list scene)
    (if (empty? goo-list)
        scene
        (img-list+scene
         (get-posns-from-goo goo-list)
         (goo-img (first goo-list))
         scene)))
  (define-values (a b) (group-goos goos))
  (goos+scene b (goos+scene a scene)))

(define (goo-img g)
  (cond [(eq? (goo-type g) 'green) GOO-IMG]
        [(eq? (goo-type g) 'red) RED-GOO-IMG]))

(define (group-goos goos)
  (values
   (filter (lambda (g) (eq? (goo-type g) 'green)) goos)
   (filter (lambda (g) (eq? (goo-type g) 'red)) goos)))

(define (dead? w)
  (define snake (pit-snake w))
  (or (self-colliding? snake)
      (wall-colliding? snake)))

(define (render-end w)
  (overlay (text
            (format "Game Over\nEaten ~A goos"
                    (length (snake-body (pit-snake w))))
            ENDGAME-TEXT-SIZE "black")
           (render-snake-world w)))

(define (self-colliding? snake)
  (cons? (member (snake-head snake) (snake-body snake))))

(define (wall-colliding? snake)
  (define x (posn-x (snake-head snake)))
  (define y (posn-y (snake-head snake)))
  (or (= 0 x) (= x SIZE)
      (= 0 y) (= y SIZE)))

(define (render-snake-world w)
  (render-pit w))

(define (opposite-dir? d1 d2)
  (cond [(string=? d1 "up") (string=? d2 "down")]
        [(string=? d1 "down") (string=? d2 "up")]
        [(string=? d1 "left") (string=? d2 "right")]
        [(string=? d1 "right") (string=? d2 "left")]))

(define (posn=? a b)
  (and (= (posn-x a) (posn-x b))
       (= (posn-y a) (posn-y b))))

(define (snake-head sn)
  (first (snake-segs sn)))

(define (snake-body sn)
  (rest (snake-segs sn)))

(define (snake-tail sn)
  (last (snake-segs sn)))

(define (snake-change-dir sn d)
  (snake d (snake-segs sn)))
