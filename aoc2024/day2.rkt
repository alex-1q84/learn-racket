#lang racket

(define (safe-sequence? seq)
  (let* ([rest-seq (rest seq)]
         [flag (positive? (- (first rest-seq) (first seq)))])
    (for/and ([a (in-list seq)]
              [b (in-list rest-seq)]
              #:do ((define diff (- b a))))
      (and (eq? flag (positive? diff))
           (<= 1 (abs diff) 3)))))

;============== part 1 ====================
(for/sum ([line (in-lines (open-input-file "input2.txt" #:mode 'text))])
  (let* ([report (map string->number (string-split line))])
    (if (safe-sequence? report)
     1
     0)))

;============== part 2 ====================
(define (safe-report? report)
  (or (safe-sequence? report)
      (for/or ([i (in-range (length report))]) ; 我想多了，简单粗暴，直接遍历检查好漂亮
        (safe-sequence? (append (take report i) (drop report (add1 i)))))))

(for/sum ([line (in-lines (open-input-file "input2.txt" #:mode 'text))])
  (let ([report (map string->number (string-split line))])
    (if (safe-report? report) 1 0)))
