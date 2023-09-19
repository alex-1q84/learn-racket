#lang racket
;; 统计表名及字段名中各单词出现频率

(require csv-reading
         csv-writing)


(define (read-csv-rows path-string)
  (csv->list
   (open-input-file
    (expand-user-path path-string))))


;(displayln table-names)

(define (uniq-list alist)
  (set->list (list->set alist)))


(define (split-and-count-word word-list sep)
  (for/fold ([word-map (make-hash)]) ([complex-word (in-list word-list)])
    (for ([word (string-split complex-word sep)])
      (hash-set! word-map word
                 (add1 (hash-ref! word-map word 0))))
    word-map))


(define (sort-car-pair-list pair-list)
  (sort pair-list
        (lambda (kv1 kv2) (string<? (car kv1) (car kv2)))))


(define files '("~/Downloads/spreadsheets/2023/columns-1.csv"
                "~/Downloads/spreadsheets/2023/columns-2.csv"
                "~/Downloads/spreadsheets/2023/columns-3.csv"
                "~/Downloads/spreadsheets/2023/columns.csv"))

(define csv-rows
  (append* (for/list ([f files])
             (read-csv-rows f))))

(define table-names
  (map (lambda (row) (second row)) csv-rows))

;; ============= statistic table names =================
(define table-name-words
  (sort-car-pair-list (hash->list (split-and-count-word (uniq-list table-names) "_"))))

#;(displayln table-name-words)
(call-with-output-file
    (expand-user-path "~/Downloads/mc-table-name-words.csv")
  #:exists 'replace
  (lambda (out)
    (display-table (for/list ([word-pair table-name-words])
                     (flatten word-pair))
                   out)))

;; ============= statistic column names =================
(define col-names
  (map (lambda (row) (third row)) csv-rows))

(define col-name-words (sort-car-pair-list (hash->list (split-and-count-word col-names "_"))))
;(displayln col-name-words)
(call-with-output-file
    (expand-user-path "~/Downloads/mc-col-name-words.csv")
  #:exists 'replace
  (lambda (out)
    (display-table (for/list ([word-pair col-name-words])
                     (flatten word-pair))
                   out)))