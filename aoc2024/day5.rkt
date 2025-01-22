#lang racket

(define-values (rules orders)
  (for/fold ([rules (make-hash)] [orders '()])
            ([line (in-lines (open-input-file "input5.txt" #:mode 'text))])
    ;; read the input data and parse then save into rules and page orders
    (cond
      [(string-contains? line "|")
       (let* ([rule (map string->number (string-split line "|"))]
              [before (first rule)]
              [after (second rule)])
         ;; group all after numbers by before number then save into hash table for indexing
         (set-add! (hash-ref! rules before (mutable-set)) after)
         (values rules orders))]
      [(string-contains? line ",")
       (values rules (cons (map string->number (string-split line ",")) orders))]
      [else
       (values rules orders)])))

(define (order-valid? order rules)
  (for/and ([n (in-range (length order))]
            #:do ((define ord (drop order n))))
    (subset? (list->set (drop ord 1)) (hash-ref rules (first ord) (set)))))

(define (center order)
  (list-ref order (floor (/ (length order) 2))))

;; ======================= part 1 ====================

(for/sum ([order (in-list orders)])
  (if (order-valid? order rules)
      (center order)
      0))

;; ======================= part 2 ====================

(define (before? a b (rules rules))
  (set-member? (hash-ref rules a) b))

(for/sum ([order (in-list orders)])
  (if ((negate order-valid?) order rules)
      (center (sort order before?))
      0))