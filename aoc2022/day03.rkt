#lang racket
(require threading)

(define (lines->rucksack-items lines)
  (~> lines
      (map string->list _)
      (map (lambda (lst)
             (define middle (/ (length lst) 2))
             (let-values ([(a b) (split-at lst middle)])
               (list a b)))
           _)))

;'(((a b c) (e f g)) (((e f g) (e F G)))
(define rucksacks
  (~> (open-input-file "input03.txt")
      port->lines
      lines->rucksack-items))

(define (both-contains alst blst)
  (define bset (list->set blst))
  (for/list ([item (in-set (list->set alst))]
             #:when (set-member? bset item))
    item))

(define (char->priority chr)
  (cond
    [(and (char>=? chr #\a) (char<=? chr #\z))
     (+ 1 (- (char->integer chr) (char->integer #\a)))]
    [else
     (+ 27 (- (char->integer chr) (char->integer #\A)))]))


(define (sum-priorities-of-both-apears rucksacks)
  (~> (map (lambda (p)
             (both-contains (first p) (second p)))
           rucksacks)
      flatten
      (map char->priority _)
      (apply + _)))

;; part one
(sum-priorities-of-both-apears rucksacks)

;; part two
; split into chunks every size
(define (split-into-chunks lst chunk-size)
  (for/fold ([result '()]
             [chunk '()]
             #:result (cond
                        [(not (empty? chunk))
                         (reverse (cons (reverse chunk) result))]
                        [else
                         (reverse result)]))
            ([item (in-list lst)]
             [seq (in-cycle (range chunk-size))])
    (cond
      [(= seq (sub1 chunk-size))
       (values (cons (reverse (cons item chunk)) result) '())]
      [else
       (values result (cons item chunk))])))

(define elf-groups
  (~> (open-input-file "input03.txt")
      port->lines
      (map string->list _)
      (split-into-chunks _ 3)))

(for/sum ([g (in-list elf-groups)])
  (~> (both-contains (first g) (second g))
      (both-contains (third g) _)
      (map char->priority _)
      (apply + _)))


(module+ test
  (require rackunit)

  (check-eq? (char->priority #\P) 42)
  (check-eq? (char->priority #\a) 1)
  (check-eq? (char->priority #\A) 27)
  (check-eq? (char->priority #\s) 19)

  (define samples '("vJrwpWtwJgWrhcsFMMfFFhFp"
                    "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
                    "PmmdzqPrVvPwwTWBwg"
                    "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
                    "ttgJtRGJQctTZtZT"
                    "CrZsJsPPZsGzwwsLwLmpwMDw"))
  (check-eq? (sum-priorities-of-both-apears (lines->rucksack-items samples)) 157)
  "all tests run")