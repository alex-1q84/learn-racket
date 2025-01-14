#lang racket

(for/sum ([line (in-lines (open-input-file "input2.txt" #:mode 'text))])
  (if (let* ([report (map string->number (string-split line))]
             [rest-report (rest report)]
             [flag (positive? (- (first rest-report) (first report)))])
        (for/and ([r (in-list report)]
                  [rr (in-list rest-report)]
                  #:do ((define differ (- rr r))))
          (and (eq? flag (positive? differ))
               (if flag
                   (<= 1 differ 3)
                   (<= -3 differ -1)))))
      1
      0))