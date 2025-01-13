#lang racket

(define-values (left right)
  (match/values (for/fold ([left '()] [right '()])
                          ([line (in-lines (open-input-file "input1.txt" #:mode 'text))])
                  (let-values ([(l r) (apply values (string-split line))])
                    (values (cons l left) (cons r right))))
                
                ((left right) (values (map string->number left) (map string->number right)))))

;;================== first part distance ==================
(for/sum ([l (in-list (sort left >))]
          [r (in-list (sort right >))])
  (abs (- l r)))

;;================== second part similarity score ==================
(for/sum ([l (in-list left)])
  (* l (count (lambda (x) (= l x)) right)))