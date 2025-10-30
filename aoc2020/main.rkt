#lang racket/base

(provide (all-defined-out))

(module+ main
  (displayln "Advent of Code 2020"))

(module+ test
  (require rackunit)
  (check-equal? 1 1))
