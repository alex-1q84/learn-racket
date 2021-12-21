#lang racket
(require memoize)

(define (count-jolts adapters)
  (define sorted-adapters (sort adapters <))
  (let loop ([one-count 0]
             [three-count 0]
             [base 0]
             [rest sorted-adapters])
    (cond
      [(empty? rest) (values one-count (add1 three-count))]
      [(= 1 (- (car rest) base)) (loop (add1 one-count)
                                       three-count
                                       (car rest)
                                       (cdr rest))]
      [(= 3 (- (car rest) base)) (loop one-count
                                       (add1 three-count)
                                       (car rest)
                                       (cdr rest))]
      [else (loop one-count
                  three-count
                  (car rest)
                  (cdr rest))])))

(define adapters
  (call-with-input-file "input10.txt"
    (lambda (in)
      (sort (for/list ([line (in-lines in)])
              (string->number line))
            <))))

(let-values ([(one three) (count-jolts adapters)])
  (* one three))

;;============== copy ====================
(define start 0)
(define end (+ 3 (last adapters)))
(define all-adapters (sort (list* start end adapters) <))
(define known-adapters (apply set all-adapters))
(define vertices
  (for*/fold ([vertices (hasheqv)])
             ([adapter (in-list all-adapters)]
              [reach (in-range (- adapter 3) adapter)])
    (if (set-member? known-adapters reach)
        (hash-update vertices reach (lambda (vs) (cons adapter vs)) null)
        vertices)))

(define/memo* (subpaths from to)
  (define targets
    (hash-ref vertices from))
  (cond
    [(null? targets) 0]
    [(member to targets) 1]
    [else (for/sum ([t (in-list targets)])
            (subpaths t to))]))

(define arrangements
  (subpaths start end))

arrangements

(module+ test

  (define sample-adapters '(16 10 15 5 1 11 7 19 6 12 4))

  (count-jolts sample-adapters)

  "all test run")
