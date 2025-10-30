#lang racket/base

(provide hello)

(define (hello name)
  (format "Hello, ~a!" name))

(module+ test
  (require rackunit)
  (check-equal? (hello "world") "Hello, world!"))
