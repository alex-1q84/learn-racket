#lang racket

(provide plants)

(define children (hash "Alice" 1
                       "Bob" 2
                       "Charlie" 3
                       "David" 4
                       "Eve" 5
                       "Fred" 6
                       "Ginny" 7
                       "Harriet" 8
                       "Ileana" 9
                       "Joseph" 10
                       "Kincaid" 11
                       "Larry" 12))

(define (plants diagram student)
  (let-values ([(b e) (plants-belong-to student)])
    (map encode->plant
         (flatten (map string->list
                       (map (curryr substring b e) (string-split diagram "\n")))))))

(define (plants-belong-to child)
  (let ([end (* 2 (hash-ref children child))])
    (values (- end 2) end)))

(define (encode->plant encode)
  (match encode
    [#\C "clover"]
    [#\G "grass"]
    [#\R "radishes"]
    [#\V "violets"]
    [else (error (format "no such encode ~A" encode))]))

(module+ test
  (require rackunit rackunit/text-ui))

(module+ test
  (define suite
    (test-suite "kindergarten garden tests"
                (test-equal? "garden with single student"
                             (plants "RC\nGG" "Alice")
                             '("radishes" "clover" "grass" "grass"))
                (test-equal? "different garden with single student"
                             (plants "VC\nRC" "Alice")
                             '("violets" "clover" "radishes" "clover"))
                (test-equal? "garden with two students"
                             (plants "VVCG\nVVRC" "Bob")
                             '("clover" "grass" "radishes" "clover"))
                (test-equal? "second student's garden"
                             (plants "VVCCGG\nVVCCGG" "Bob")
                             '("clover" "clover" "clover" "clover"))
                (test-equal? "third student's garden"
                             (plants "VVCCGG\nVVCCGG" "Charlie")
                             '("grass" "grass" "grass" "grass"))
                (test-equal? "for Alice first student's garden"
                             (plants
                              "VRCGVVRVCGGCCGVRGCVCGCGV\nVRCCCGCRRGVCGCRVVCVGCGCV"
                              "Alice")
                             '("violets" "radishes" "violets" "radishes"))
                (test-equal? "for Bob second student's garden"
                             (plants
                              "VRCGVVRVCGGCCGVRGCVCGCGV\nVRCCCGCRRGVCGCRVVCVGCGCV"
                              "Bob")
                             '("clover" "grass" "clover" "clover"))
                (test-equal? "for Charlie"
                             (plants
                              "VRCGVVRVCGGCCGVRGCVCGCGV\nVRCCCGCRRGVCGCRVVCVGCGCV"
                              "Charlie")
                             '("violets" "violets" "clover" "grass"))
                (test-equal? "for David"
                             (plants
                              "VRCGVVRVCGGCCGVRGCVCGCGV\nVRCCCGCRRGVCGCRVVCVGCGCV"
                              "David")
                             '("radishes" "violets" "clover" "radishes"))
                (test-equal? "for Eve"
                             (plants
                              "VRCGVVRVCGGCCGVRGCVCGCGV\nVRCCCGCRRGVCGCRVVCVGCGCV"
                              "Eve")
                             '("clover" "grass" "radishes" "grass"))
                (test-equal? "for Fred"
                             (plants
                              "VRCGVVRVCGGCCGVRGCVCGCGV\nVRCCCGCRRGVCGCRVVCVGCGCV"
                              "Fred")
                             '("grass" "clover" "violets" "clover"))
                (test-equal? "for Ginny"
                             (plants
                              "VRCGVVRVCGGCCGVRGCVCGCGV\nVRCCCGCRRGVCGCRVVCVGCGCV"
                              "Ginny")
                             '("clover" "grass" "grass" "clover"))
                (test-equal? "for Harriet"
                             (plants
                              "VRCGVVRVCGGCCGVRGCVCGCGV\nVRCCCGCRRGVCGCRVVCVGCGCV"
                              "Harriet")
                             '("violets" "radishes" "radishes" "violets"))
                (test-equal? "for Ileana"
                             (plants
                              "VRCGVVRVCGGCCGVRGCVCGCGV\nVRCCCGCRRGVCGCRVVCVGCGCV"
                              "Ileana")
                             '("grass" "clover" "violets" "clover"))
                (test-equal? "for Joseph"
                             (plants
                              "VRCGVVRVCGGCCGVRGCVCGCGV\nVRCCCGCRRGVCGCRVVCVGCGCV"
                              "Joseph")
                             '("violets" "clover" "violets" "grass"))
                (test-equal? "for Kincaid second to last student's garden"
                             (plants
                              "VRCGVVRVCGGCCGVRGCVCGCGV\nVRCCCGCRRGVCGCRVVCVGCGCV"
                              "Kincaid")
                             '("grass" "clover" "clover" "grass"))
                (test-equal? "for Larry last student's garden"
                             (plants
                              "VRCGVVRVCGGCCGVRGCVCGCGV\nVRCCCGCRRGVCGCRVVCVGCGCV"
                              "Larry")
                             '("grass" "violets" "clover" "violets"))))
  (run-tests suite))