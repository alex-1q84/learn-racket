#lang racket/base

(require racket/hash
         racket/string
         racket/match)

(struct vertex (from to n) #:transparent)

(define (parse-edges from tos)
  (for/list ([to-str (in-list (string-split tos ", "))])
    (match to-str
      [(regexp #px"(\\d+) (.+) bags?.?" (list _ (app string->number n) to))
       (vertex from to n)])))

(define vertices
  (call-with-input-file "input07.txt"
    (lambda (in)
      (for/fold ([vertices null])
                ([line (in-lines in)])
        (match line
          [(regexp #rx"^(.+) bags contain no other bags.") vertices]
          ; match -
          ; light red bags contain 1 bright white bag, 2 muted yellow bags.
          ; dark orange bags contain 3 bright white bags, 4 muted yellow bags.
          ; bright white bags contain 1 shiny gold bag.
          [(regexp #rx"^(.+) bags contain (.+)" (list _ who what))
           ; what - such as
           ; 1 bright white bag, 2 muted yellow bags.
           ; 3 bright white bags, 4 muted yellow bags.
           ; 1 shiny gold bag.
           ;
           ; and the what will be parsed by parse-edges method
           (append vertices (parse-edges who what))])))))

(define (holders-of to)
  (for/fold ([holders (hash)])
            ([v (in-list vertices)])
    (match v
      [(vertex from (== to) _)
       (hash-union
        (hash-set holders  from #t)
        (holders-of from)
        #:combine (lambda (a _) a))]
      [_ holders])))

(define total-holders
  (hash-count (holders-of "shiny gold")))

(define (bags-held from)
  (for/sum ([v (in-list vertices)])
    (match v
      [(vertex (== from) to n)
       (+ n (* n (bags-held to)))]
      [_ 0])))

(define total-bags-held
  (bags-held "shiny gold"))

(displayln (format "~A\n~A" total-holders total-bags-held))