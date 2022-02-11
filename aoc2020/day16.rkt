#lang racket/base

(require racket/list
         racket/string
         racket/match)

(struct num-range (low high) #:transparent)

(define p-validator #px"(.*): (\\d+)-(\\d+) or (\\d+)-(\\d+)")

;; 用 hash 表存储校验规则
;; 用 list 来存储一个车票的所有数字字段
;; 用 list 来存储周围所有人的车票

(define (make-num-range num-lst)
  (num-range (first num-lst) (second num-lst)))

(define-values (validators your-ticket nearby-tickets _)
  (call-with-input-file "input16.txt"
    (lambda (in)
      (for/fold ([validators (hash)]
                 [your-ticket null]
                 [nearby-tickets null]
                 [in-your-ticket-group? #f])
                ([line (in-lines in)])
        (match line
          [(regexp p-validator (list _ name a b c d))
           (values (hash-set validators
                             name
                             (list
                              (num-range (string->number a)
                                         (string->number b))
                              (num-range (string->number c)
                                         (string->number d))))
                   your-ticket
                   nearby-tickets
                   in-your-ticket-group?)]
          [string
           (cond
             [in-your-ticket-group?
              (values validators
                      (map string->number
                           (string-split line ","))
                      nearby-tickets
                      #f)]
             [(equal? line "your ticket:")
              (values validators
                      your-ticket
                      nearby-tickets
                      #t)]
             [(or (equal? line "") (equal? line "nearby tickets:"))
              (values validators
                      your-ticket
                      nearby-tickets
                      in-your-ticket-group?)]
             [else
              (values validators
                      your-ticket
                      (cons (map string->number (string-split line ",")) nearby-tickets)
                      in-your-ticket-group?)])])))))


(define (error-rate tickets valids)
  (define (validate? num r)
    (and (>= num (num-range-low r))
         (<= num (num-range-high r))))

  (define (_check num valids)
    (for/fold ([valid? #f])
              ([v (in-list (hash-values valids))])
      (or valid? (validate? num (first v)) (validate? num (second v)))))

  (for*/fold ([rate 0])
             ([t (in-list tickets)]
              [n (in-list t)])
    (+ rate
       (if (_check n valids) 0 n))))

(error-rate (cons your-ticket nearby-tickets) validators)
