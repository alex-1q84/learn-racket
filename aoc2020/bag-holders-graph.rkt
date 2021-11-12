#lang racket/base

(require racket/hash
         racket/string
         racket/match
         "./graphviz-rendering.rkt")

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

(define (holders-of vertices)
  (for/fold ([holders ""]
             #:result (wrap-graphviz holders))
            ([v (in-list vertices)])
    (string-append holders "\n" (build-graphviz-expr v) ";")))

(define (build-graphviz-expr vertice)
  (label (direct-to (node-name (vertex-from vertice))
             (node-name (vertex-to vertice)))
         (vertex-n vertice)))

(define (label g-expr l)
  (string-append g-expr (format "[label=\"~A\"]" l)))

(define (direct-to from to)
  (format "~A -> ~A" from to))

(define (node-name name)
  (string-replace name " " "_"))

(define (wrap-graphviz graphviz-expr)
  (format "digraph G {\n~A\n}" graphviz-expr))

;**************************************************************************************;
;****                            Display Graphviz Graph                            ****;
;**************************************************************************************;

(define graphviz-src (holders-of vertices))
(displayln graphviz-src)
(graphviz-render (open-input-string graphviz-src) (string-length graphviz-src) ".")
