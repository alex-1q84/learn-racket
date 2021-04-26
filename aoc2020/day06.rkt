#lang racket
(require racket/match)

(define answer-groups
  (call-with-input-file "input06.txt"
    (lambda (in)
      (for/fold ([answer-groups null]
                 [answers (list)]
                 ; for/fold 方法收集的结果是倒序的,所以这里需要反转一次以得到跟输入一致的顺序
                 #:result (reverse
                           ; compose1 方法组合两个函数并形成一个新的函数,这个函数会强制检查被组合的两个函数是否都只接受一个参数
                           (filter (compose1 not set-empty?)
                                   (cons answers answer-groups))))
                ([line (in-lines in)])
        (cond
          [(string=? line "")
           ; 使用 values 方法确保返回结果的数量和顺序跟 for/fold 方法初始
           (values (cons answers answer-groups) (list))]
          [else
           (values answer-groups (cons (string->list line) answers))
           ])))))

;;============================
;;       question 1
;;============================
(define (org-answers init-func op-func answer-group)
  (for/fold ([answers (init-func)])
            ([answer (in-list answer-group)])
            (op-func answers (list->set answer))))

(define org-answers-uniq
  (curry org-answers set set-union))

(apply + (map
          (compose1 length set->list)
          (map org-answers-uniq answer-groups)))

;;============================
;;       question 2
;;============================

(define (default-set)
  ((compose1 list->set string->list) "abcdefghijklmnopqrstuvwxyz"))

(define (org-answers-everyone-yesses answer-group)
  (org-answers default-set set-intersect answer-group))

(apply + (map
          (compose1 length set->list)
          (map org-answers-everyone-yesses answer-groups)))

(module+ test
  (require rackunit)

  "all test run")