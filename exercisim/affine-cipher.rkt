#lang racket/base
(require racket/contract
         racket/string)

(provide (contract-out
          [encode (string?
                   exact-nonnegative-integer?
                   exact-nonnegative-integer? . -> . string?)]
          [decode (string?
                   exact-nonnegative-integer?
                   exact-nonnegative-integer? . -> . string?)]))

;; Helper function to find the modular inverse of 'a' modulo 26
(define (modular-inverse a m)
  (define result
    (for/first ([i (in-range 1 m)]
                #:when (= 1 (modulo (* a i) m)))
      i))
  (if result
      result
      (error "No modular inverse found for a and m")))

;; Helper function to convert a character to its numeric equivalent (0-25)
(define (char->index c)
  (- (char->integer (char-downcase c)) (char->integer #\a)))

;; Helper function to convert a numeric value (0-25) to a character
(define (index->char n)
  (integer->char (+ n (char->integer #\a))))

;; Function to encode a message using the Affine Cipher
(define (split-string str)
  (string-join
   (for/list ([i (in-range 0 (string-length str) 5)])
     (substring str i (min (+ i 5) (string-length str))))
   " "))

(define (encode msg a b)
  (unless (= 1 (gcd a 26))
    (error "The value of 'a' must be coprime with 26"))
  (define m 26)

  (define imsg (string-replace msg #rx"[,. ]" ""))
  (split-string
   (list->string
    (for/list ([c (in-string imsg)])
      (if (char-alphabetic? c)
          (index->char (modulo (+ (* a (char->index c)) b) m))
          c)))))

;; Function to decode a message using the Affine Cipher
(define (decode msg a b)
  (unless (= 1 (gcd a 26))
    (error "The value of 'a' must be coprime with 26"))
  (define m 26)
  (define a-inv (modular-inverse a m))

  (define imsg (string-replace msg " " ""))
  (list->string
   (for/list ([c (in-string imsg)])
     (if (char-alphabetic? c)
         (index->char (modulo (* a-inv (- (char->index c) b)) m))
         c))))


(module+ test
  (require rackunit rackunit/text-ui)
  (define encode-suite
    (test-suite "affine-cipher encoding tests"
                (test-equal? "encode 'yes'"
                             (encode "yes" 5 7)
                             "xbt")
                (test-equal? "encode 'no"
                             (encode "no" 15 18)
                             "fu")
                (test-equal? "encode 'OMG"
                             (encode "OMG" 21 3)
                             "lvz")
                (test-equal? "encode 'O M G"
                             (encode "O M G" 25 47)
                             "hjp")
                (test-equal? "encode 'mindblowingly'"
                             (encode "mindblowingly" 11 15)
                             "rzcwa gnxzc dgt")
                (test-equal? "encode 'Testing,1 2 3, testing.'"
                             (encode "Testing,1 2 3, testing." 3 4)
                             "jqgjc rw123 jqgjc rw")
                (test-equal? "encode 'Truth is fiction.'"
                             (encode "Truth is fiction." 5 17)
                             "iynia fdqfb ifje")
                (test-equal? "encode 'The quick brown fox jumps over the lazy dog.'"
                             (encode "The quick brown fox jumps over the lazy dog." 17 33)
                             "swxtj npvyk lruol iejdc blaxk swxmh qzglf")
                (test-exn "test that encode raises a meaningful exception"
                          exn:fail? (lambda () (encode "This is a test" 6 17)))))
  (define decode-suite
    (test-suite "affine-cipher decoding tests"
                (test-equal? "decode 'tytgn fjr'"
                             (decode "tytgn fjr" 3 7)
                             "exercism")
                (test-equal? "decode 'qdwju nqcro muwhn odqun oppmd aunwd o'"
                             (decode "qdwju nqcro muwhn odqun oppmd aunwd o" 19 16)
                             "anobstacleisoftenasteppingstone")
                (test-equal? "decode 'odpoz ub123 odpoz ub'"
                             (decode "odpoz ub123 odpoz ub" 25 7)
                             "testing123testing")
                (test-equal? "decode 'swxtj npvyk lruol iejdc blaxk swxmh qzglf'"
                             (decode "swxtj npvyk lruol iejdc blaxk swxmh qzglf" 17 33)
                             "thequickbrownfoxjumpsoverthelazydog")
                (test-equal? "decode 'swxtjnpvyklruoliejdcblaxkswxmhqzglf'"
                             (decode "swxtjnpvyklruoliejdcblaxkswxmhqzglf" 17 33)
                             "thequickbrownfoxjumpsoverthelazydog")
                (test-equal? "decode 'vszzm    cly   yd cg    qdp'"
                             (decode "vszzm    cly   yd cg    qdp" 15 16)
                             "jollygreengiant")
                (test-exn "test that decode raises a meaningfun exception"
                          exn:fail? (lambda () (decode "Test" 13 5)))))
  (begin
    (run-tests encode-suite)
    (run-tests decode-suite)))
