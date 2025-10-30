#lang racket/base

(provide (all-defined-out))

(module+ main
  (displayln "Exercisim"))

(module+ test
  (require rackunit)
  (check-equal? 1 1))
