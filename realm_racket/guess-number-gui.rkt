#lang racket
(require 2htdp/universe 2htdp/image)

(define WIDTH 550)
(define HEIGHT 300)
(define TEXT-SIZE 24)
(define TEXT-X 12)
(define TEXT-UPPER-Y 12)
(define TEXT-LOWER-Y (- HEIGHT TEXT-SIZE TEXT-X))
(define SIZE 48)

(struct interval (small big))

(define HELP-TEXT
  (text "↑ for larger numbers, ↓ for smaller ones"
        TEXT-SIZE
        "blue"))

(define HELP-TEXT2
  (text "Press = when your number is guessed; q to quit."
        TEXT-SIZE
        "blue"))

(define COLOR "red")

(define MT-SC
  (place-image/align
   HELP-TEXT TEXT-X TEXT-UPPER-Y "left" "top"
   (place-image/align
    HELP-TEXT2 TEXT-X TEXT-LOWER-Y "left" "bottom"
    (empty-scene WIDTH HEIGHT))))

(define (deal-with-guess w key)
  (cond [(key=? key "up") (bigger w)]
        [(key=? key "down") (smaller w)]
        [(key=? key "q") (stop-with w)]
        [(key=? key "=") (stop-with w)]
        [else w]))

(define (smaller w)
  (interval (interval-small w)
            (max (interval-small w) (sub1 (guess w)))))

(define (bigger w)
  (interval (min (interval-big w) (add1 (guess w)))
            (interval-big w)))

(define (guess w)
  (quotient (+ (interval-small w) (interval-big w)) 2))

(define (render w)
  (overlay (text (number->string (guess w)) SIZE COLOR) MT-SC))

(define (render-last-scenen w)
  (overlay (text "End" SIZE COLOR) MT-SC))

(define (single? w)
  (= (interval-small w) (interval-big w)))

(define (start lower upper)
  (big-bang (interval lower upper)
    (on-key deal-with-guess)
    (to-draw render)
    (stop-when single? render-last-scenen)))