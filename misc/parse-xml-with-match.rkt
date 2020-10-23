#lang racket

(require xml)

(module+ test
  (require rackunit))

;; -----------------------------------------------------------------------------
;; recognizing an XML that belongs to a grammar 

;; E = <plus>E E</plus> | <number value=String />

(define (ex i) (format "<number value=\"~a\" />" i))
(define (ex2 i j) (string-append "<plus>" (ex i) (ex j) "</plus>"))

;; String -> Boolean
;; does E produce e?

(module+ test
  (check-true (parse (ex2 5 42)))
  (check-false (parse (ex "hello world")))
  (check-false (parse (ex2 5 "hello world"))))

(define (parse e:str)
  (define e:xml (read-xml/element (open-input-string e:str)))
  (define e:xexpr (xml->xexpr e:xml))
  (let parse ([e e:xexpr])
    (match e
      [`(number ((value ,v))) (number? (string->number v))]
      [`(plus () ,e1 ,e2) (and (parse e1) (parse e2))]
      [else #f])))

;; -----------------------------------------------------------------------------
;; creating a parse tree for a valid XML 

;; A = Number | (list '+ Number Number)

;; String -> [maybe/c A]
;; does E produce e? if so, create an a, otherwise #f

(module+ test
  (check-equal? (parse-to (ex2 5 42)) '(+ 5 42))
  (check-false (parse-to (ex "hello world")))
  (check-false (parse-to (ex2 5 "hello world"))))

(define (parse-to e:str)
  (define e:xml (read-xml/element (open-input-string e:str)))
  (define e:xexpr (xml->xexpr e:xml))
  (let parse-to ([e e:xexpr])
    (match e
      [`(number ((value ,v)))
       (define x (string->number v))
       (if (number? x) x #f)]
      [`(plus () ,e1 ,e2)
       (define p1 (parse e1))
       (cond
         [(boolean? p1) #f]
         [else (define p2 (parse p2))
               (and p2 `(+ ,p1 ,p2))])]
      [else #f])))