#lang racket

(define (text->chapters f-path title-pattern)
  (for/fold ([chapters '(())]
             #:result (reverse (map reverse chapters)))
            ([line (in-lines (open-input-file f-path))])
    (cond
      ;; found title put previous chapter into and create a new chapter to store this new chapter
      [(regexp-match title-pattern line) (values (cons `(,line) chapters))]
      [else (values (cons (cons line (car chapters)) (cdr chapters)))]
      )))


(define (list-chunk lst n)
  (if (null? lst)
      '()
      (cons (with-handlers
                ([exn:fail:contract?
                  (lambda (exn) lst)])
              (take lst n))
            (list-chunk (with-handlers
                            ([exn:fail:contract?
                              (lambda (exn) '())])
                          (drop lst n))
                        n))))


(define (write-to-books books base-title base-path)
  (for ([book (in-list books)]
        [n (in-naturals)])
    (for ([chpt (in-list book)])
      (display-lines-to-file chpt (build-path base-path (format "~A-~A.txt" base-title (add1 n)))
                             #:mode 'text
                             #:exists 'append
                             ))))

(define chapters (text->chapters
                  (expand-user-path "~/Documents/ebook/凡人修仙之仙界篇.txt")
                  #rx"^\\s*第[0-9]+章.*"))

(write-to-books (list-chunk chapters 100)
                "凡人修仙之仙界篇"
                (expand-user-path "~/Documents/ebook/fr"))

;(length chapters)
;(cadr chapters)
