#lang racket

(provide primes)

(define (primes limit)
  (cond
    [(< limit 2) empty]
    [else
     (let* ([limit (add1 limit)]
            [is-prime (make-vector limit #t)])
       (vector-set! is-prime 0 #f)
       (vector-set! is-prime 1 #f)
       ; mark non-prime numbers
       (for ([n (in-range 2 limit)])
         (when (vector-ref is-prime n)
           (for ([j (in-range (* n n) limit n)])
             (vector-set! is-prime j #f))))
       ; collect prime numbers
       (for/list ([p (in-vector is-prime)]
                  [n (in-naturals)]
                  #:when p)
         n))]))

(module+ test
  (require rackunit rackunit/text-ui))
(module+ test
  (define suite
    (test-suite
     "sieve tests"
     (test-equal? "no primes under two"
                  (primes 1) '())
     (test-equal? "find first prime"
                  (primes 2) '(2))
     (test-equal? "find primes up to 10"
                  (primes 10) '(2 3 5 7))
     (test-equal? "limit is a prime"
                  (primes 13) '(2 3 5 7 11 13))
     (test-equal? "find primes up to 1000"
                  (primes 1000) '(
                                  2   3   5   7  11  13  17  19  23  29  31  37  41  43
                                      47  53  59  61  67  71  73  79  83  89  97 101 103 107
                                      109 113 127 131 137 139 149 151 157 163 167 173 179 181
                                      191 193 197 199 211 223 227 229 233 239 241 251 257 263
                                      269 271 277 281 283 293 307 311 313 317 331 337 347 349
                                      353 359 367 373 379 383 389 397 401 409 419 421 431 433
                                      439 443 449 457 461 463 467 479 487 491 499 503 509 521
                                      523 541 547 557 563 569 571 577 587 593 599 601 607 613
                                      617 619 631 641 643 647 653 659 661 673 677 683 691 701
                                      709 719 727 733 739 743 751 757 761 769 773 787 797 809
                                      811 821 823 827 829 839 853 857 859 863 877 881 883 887
                                      907 911 919 929 937 941 947 953 967 971 977 983 991 997))))
  (run-tests suite))
