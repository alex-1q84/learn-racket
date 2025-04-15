#lang racket

(provide rows)

;; 行数 = 2n+1
;; 列数 = 2n+1
;; n = high-char - 'A'
;; 位置 = (n - row-index), ((n - row-index) + 2row-index)
;; 字母 = x in [0, n]: 'A' + x
;;       x in (n, 0]: 'A' + x
(define (rows high-char)
  (let* ([high-n (char-distance #\A high-char)]
        [line-len (+ (* 2 high-n) 1)])
    (append
     ;top half
     (for/list ([x (in-range (add1 high-n))])
       (draw-line (pos->char x) line-len (list (- high-n x) (+ (- high-n x) (* 2 x)))))
     ;bottom half
     (for/list ([x (in-range (sub1 high-n) -1 -1)])
       (draw-line (pos->char x) line-len (list (- high-n x) (+ (- high-n x) (* 2 x))))))))

(define (draw-line chr len pos-lst)
  (let ([line (make-string len #\space)])
    (for ([p pos-lst])
      (string-set! line p chr))
    line))

(define (pos->char pos)
  (integer->char (+ (char->integer #\A) pos)))

(define (char-distance begin end)
  (- (char->integer end) (char->integer begin)))

(module+ test
  (require rackunit rackunit/text-ui))
(module+ test
  (define suite
    (test-suite "diamond tests"
                (test-equal? "Degenerate case with a single 'A' row"
                             (rows #\A)
                             '(
                               "A"
                               ))
                (test-equal? "Degenerate case with no row containing 3 distinct groups of spaces"
                             (rows #\B)
                             '(
                               " A "
                               "B B"
                               " A "
                               ))
                (test-equal? "Smallest non-degenerate case with odd diamond side length"
                             (rows #\C)
                             '(
                               "  A  "
                               " B B "
                               "C   C"
                               " B B "
                               "  A  "
                               ))
                (test-equal? "Smallest non-degenerate case with even diamond side length"
                             (rows #\D)
                             '(
                               "   A   "
                               "  B B  "
                               " C   C "
                               "D     D"
                               " C   C "
                               "  B B  "
                               "   A   "
                               ))
                (test-equal? "Largest possible diamond"
                             (rows #\Z)
                             '(
                               "                         A                         "
                               "                        B B                        "
                               "                       C   C                       "
                               "                      D     D                      "
                               "                     E       E                     "
                               "                    F         F                    "
                               "                   G           G                   "
                               "                  H             H                  "
                               "                 I               I                 "
                               "                J                 J                "
                               "               K                   K               "
                               "              L                     L              "
                               "             M                       M             "
                               "            N                         N            "
                               "           O                           O           "
                               "          P                             P          "
                               "         Q                               Q         "
                               "        R                                 R        "
                               "       S                                   S       "
                               "      T                                     T      "
                               "     U                                       U     "
                               "    V                                         V    "
                               "   W                                           W   "
                               "  X                                             X  "
                               " Y                                               Y "
                               "Z                                                 Z"
                               " Y                                               Y "
                               "  X                                             X  "
                               "   W                                           W   "
                               "    V                                         V    "
                               "     U                                       U     "
                               "      T                                     T      "
                               "       S                                   S       "
                               "        R                                 R        "
                               "         Q                               Q         "
                               "          P                             P          "
                               "           O                           O           "
                               "            N                         N            "
                               "             M                       M             "
                               "              L                     L              "
                               "               K                   K               "
                               "                J                 J                "
                               "                 I               I                 "
                               "                  H             H                  "
                               "                   G           G                   "
                               "                    F         F                    "
                               "                     E       E                     "
                               "                      D     D                      "
                               "                       C   C                       "
                               "                        B B                        "
                               "                         A                         "
                               ))))
  (run-tests suite))
