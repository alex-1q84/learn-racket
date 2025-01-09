#lang racket

(provide on-mercury
         on-venus
         on-earth
         on-mars
         on-jupiter
         on-saturn
         on-uranus
         on-neptune)

;; Constants for orbital periods in Earth years
(define earth-year-in-seconds 31557600)
(define mercury-orbital-period 0.2408467)
(define venus-orbital-period 0.61519726)
(define mars-orbital-period 1.8808158)
(define jupiter-orbital-period 11.862615)
(define saturn-orbital-period 29.447498)
(define uranus-orbital-period 84.016846)
(define neptune-orbital-period 164.79132)

(define (calculate-age seconds orbital-period)
  (/ seconds (* earth-year-in-seconds orbital-period)))

;; Age calculation functions for each planet
(define (on-mercury seconds)
  (calculate-age seconds mercury-orbital-period))

(define (on-venus seconds)
  (calculate-age seconds venus-orbital-period))

(define (on-earth seconds)
  (calculate-age seconds 1.0))

(define (on-mars seconds)
  (calculate-age seconds mars-orbital-period))

(define (on-jupiter seconds)
  (calculate-age seconds jupiter-orbital-period))

(define (on-saturn seconds)
  (calculate-age seconds saturn-orbital-period))

(define (on-uranus seconds)
  (calculate-age seconds uranus-orbital-period))

(define (on-neptune seconds)
  (calculate-age seconds neptune-orbital-period))


(module+ test
  (require rackunit rackunit/text-ui)
  (define suite
    (test-suite
      "space-age tests"
      (test-= "age on Earth"
              (on-earth 1000000000)
              31.69
              0.01)
      (test-= "age on Mercury"
              (on-mercury 2134835688)
              280.88
              0.01)
      (test-= "age on Venus"
              (on-venus 189839836)
              9.78
              0.01)
      (test-= "age on Mars"
              (on-mars 2129871239)
              35.88
              0.01)
     (test-= "age on Jupiter"
             (on-jupiter 901876382)
             2.41
             0.01)
     (test-= "age on Saturn"
             (on-saturn 2000000000)
             2.15
             0.01)
     (test-= "age on Uranus"
             (on-uranus 1210123456)
             0.46
             0.01)
     (test-= "age on Neptune"
             (on-neptune 1821023456)
             0.35
             0.01)))
 (run-tests suite))
