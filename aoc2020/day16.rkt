#lang racket/base

(require racket/list
         racket/set
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

(define (validate-range? num r)
  (and (>= num (num-range-low r))
       (<= num (num-range-high r))))

;; valids 所有验证规则清单 list
(define (valid-all-ranges? num valids)
  (for/fold ([valid? #f])
            ([v (in-list valids)])
    (or valid? (validate-range? num (first v)) (validate-range? num (second v)))))

(define (error-rate ticket valids)
  (for*/fold ([rate 0])
             ([n (in-list ticket)])
    (+ rate
       (if (valid-all-ranges? n valids) 0 n))))

(define (count-total-error-rate tickets valids)
  (for*/fold ([rate 0])
             ([t (in-list tickets)])
    (+ rate (error-rate t (hash-values valids)))))

(count-total-error-rate (cons your-ticket nearby-tickets) validators)

;; ========================= part 2 ===========================

(define (pick-valid-tickets tickets valids)
  (for*/fold ([tiks null])
             ([t (in-list tickets)])
    (if (= (error-rate t (hash-values valids)) 0)
        (cons t tiks)
        tiks)))

;; 测试给定约束是否对全部有效车票的指定位数字适用，如果适用则认为这个约束是用于这个位的，
;; 用 hash 表存储约束和数字位置的对应关系

(define (group-by-pos tickets)
  (for*/fold ([groups (hash)])
             ([t (in-list tickets)]
              [(n i) (in-indexed t)])
    (hash-set groups i (cons n (hash-ref groups i null)))))

(define (validator->poses-mapping valids tickets)
  (define pos-nums (group-by-pos tickets))
  (for/fold ([mapping (hash)])
            ([kv (in-hash-pairs valids)])
    (hash-set mapping (car kv) (possible-poses (cdr kv) pos-nums))))

(define (possible-poses r groups)
  (for/fold ([poses null])
            ([kv (in-hash-pairs groups)])
    (if (= (error-rate (cdr kv) (list r)) 0)
        (cons (car kv) poses)
        poses)))

(define (find-pos-valid-mapping pos-valids-mappings)
  (let loop ([m (hash)] [used-vs (set)] [pvm pos-valids-mappings])
    (cond
      [(hash-empty? pvm) m]
      [else
       (define-values (_m _vs _pvm)
         (for/fold ([m* m] [used-vs* used-vs] [pvm* pvm])
                   ([kv (in-hash-pairs pvm)])
           (if (only-one (cdr kv))
               (values (hash-set m* (car kv) (cadr kv))
                       (set-add used-vs* (cadr kv))
                       (hash-remove pvm* (car kv)))
               (values m*
                       used-vs*
                       (hash-set pvm* (car kv) (remove-used-valids (cdr kv) used-vs*))))))
       (loop _m _vs _pvm)])))

(define (only-one lst)
  (= (length lst) 1))

(define (remove-used-valids lst a-set)
  (for/fold ([r null])
            ([v (in-list lst)])
    (if (set-member? a-set v)
        r
        (cons v r))))

(define (multiply-departure-nums ticket valid-pos-mapping)
  (define v-ticket (list->vector ticket))
  (for/fold ([nums null] #:result (apply * nums))
            ([kv (in-hash-pairs valid-pos-mapping)])
    (if (string-prefix? (car kv) "departure")
        (cons (vector-ref v-ticket (cdr kv)) nums)
        nums)))

(define valid-tickets
  (map list->vector (pick-valid-tickets nearby-tickets validators)))

(define valid-poses-mappings (validator->poses-mapping validators valid-tickets))

(define pos-valids-mappings
  (for*/fold ([m (hash)])
             ([kv (in-hash-pairs valid-poses-mappings)]
              [p (in-list (cdr kv))])
    (hash-set m p (cons (car kv) (hash-ref m p null)))))

(define pos-valid-mapping (find-pos-valid-mapping pos-valids-mappings))

(define valid-pos-mapping
  (for/fold ([m (hash)])
            ([kv (in-hash-pairs pos-valid-mapping)])
    (hash-set m (cdr kv) (car kv))))

(multiply-departure-nums your-ticket valid-pos-mapping)
