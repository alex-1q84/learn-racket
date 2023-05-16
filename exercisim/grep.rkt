#lang racket

(provide grep)

(struct match-item (filename linenum content))

;flags support [n l i v x]
; -n Print the line numbers of each matching line.
; -l Print only the names of files that contain at least one matching line.
; -i Match line using a case-insensitive comparison.
; -v Invert the program -- collect all lines that fail to match the pattern.
; -x Only match entire lines, instead of lines that contain a match.
(define (grep flags pattern files)
  ; -l ignore print lines, only print filenames
  ; -n print linenumber, for multi files put linenumber follow filename
  ; files if has multi files then print filename before line content
  ; need meta datas: filename linenumber row-content
  (define true? (negate false?))
  (define print-leading-filename? (> (length files) 1))
  (define print-linenum? (true? (member "-n" flags)))
  (define only-print-filename? (true? (member "-l" flags)))
  (define case-insensitive? (true? (member "-i" flags)))
  (define invert-match? (true? (member "-v" flags)))
  (define match-entire-line? (true? (member "-x" flags)))
  
  (define (match-mathod)
    (match (list match-entire-line? case-insensitive?)
      [(list #t #t) (curryr string-ci=? pattern)]
      [(list #t #f) (curryr string=? pattern)]
      [(list #f #t) (curryr string-ci-contains? pattern)]
      ['(#f #f) (curryr string-contains? pattern)]))
  
  (define matches
    (reverse (for/fold ([matches '()]) ([f (in-list files)])
               (cons (my-grep (lambda (line)
                                (if invert-match?
                                    ((negate (match-mathod)) line)
                                    ((match-mathod) line)))
                              f)
                     matches))))
  (cond
    [only-print-filename? (uniq-filename (flatten matches))]
    [else (map (build-format print-leading-filename? print-linenum?) (flatten matches))]))


(define (uniq-filename matches)
  (for/fold ([result '()] [file-set (set)] #:result (reverse result))
            ([m (in-list matches)])
    (define f (match-item-filename m))
    (if (set-member? file-set f)
        (values result file-set)
        (values (cons f result) (set-add file-set f)))))


(define (build-format print-leading-filenames? print-linenum?)
  (match (list print-leading-filenames? print-linenum?)
    [(list #t #t) (lambda (a-match-item)
                    (format "~A:~A:~A" (match-item-filename a-match-item)
                            (match-item-linenum a-match-item)
                            (match-item-content a-match-item)))]
    [(list #t #f) (lambda (a-match-item)
                    (format "~A:~A" (match-item-filename a-match-item)
                            (match-item-content a-match-item)))]
    [(list #f #t) (lambda (a-match-item)
                    (format "~A:~A" (match-item-linenum a-match-item)
                            (match-item-content a-match-item)))]
    [(list #f #f) (lambda (a-match-item)
                    (format "~A" (match-item-content a-match-item)))]))


(define (my-grep matcher filename)
  (define in (open-input-file filename #:mode 'text))
  (begin0
    (reverse
     (for/fold ([lines '()]) ([line (in-lines in)]
                              [n (in-naturals)])
       (if (matcher line)
           (cons (match-item filename (add1 n) line) lines)
           lines)))
    (close-input-port in)))


(define (string-ci-contains? str1 str2)
  (string-contains? (string-foldcase str1) (string-foldcase str2)))
