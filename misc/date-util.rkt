#lang racket
;; (require gregor)
(require racket/date)

(provide milliseconds->date
         date->milliseconds
         plus-days
         plus-minutes
         plus-seconds)

; milli seconds to date
(define (milliseconds->date mseconds)
  (seconds->date (/ mseconds 1000.0)))

; date to milli seconds
(define (date->milliseconds d)
  (* (date->seconds d) 1000))

; plus days
(define (plus-days d days)
  (seconds->date (+ (date->seconds d) (* days 86400))))

; plus minutes
(define (plus-minutes d minutes)
  (seconds->date (+ (date->seconds d) (* minutes 60))))

; plus seconds
(define (plus-seconds d seconds)
  (seconds->date (+ (date->seconds d) seconds)))

;; ; parse date
;; (define (parse-date str)
;;   (define date-parser (make-date-parser "~Y-~m-~d ~H:~M:~S"))
;;   (date-parser str))
;; 
;; ; format date
;; (define (format-date d)
;;   (define date-formatter (make-date-formatter "~Y-~m-~d ~H:~M:~S"))
;;   (date-formatter d))
