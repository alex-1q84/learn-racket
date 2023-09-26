#lang racket/base
;; install package
;; raco pkg install --link chw-common

(require racket/format)

(provide format-display)

(define (format-display num #:precision [precision 4])
  (~r (real->double-flonum num) #:precision precision))
