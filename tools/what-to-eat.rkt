#lang racket

; 随机选择一个餐，吃它，每周每种餐有数量上限，吃过一次减 1
; 备选餐可以预先设置
; 不同餐可设置不同的概率

(define MENU (make-hash))

(define (init-menu)
  (hash-set*! MENU "金拱门" '(1 1)
              "水饺" '(2 2)
              "秦味观" '(1 1)
              "番茄炒蛋盖浇" '(2 2)
              "开封菜" '(1 1)
              ))

(define MEAL-HIS '())

(define (list-menu)
  (for ([(m d) (in-hash MENU)])
    (println (format "~A : ~A" m (car d)))))

(define (list-meal-his)
  (for ([m (in-list (reverse MEAL-HIS))]
        [i (in-naturals)])
    (println (format "~A : ~A" (add1 i) m))))

(define (what-to-eat)
  (cond
    [(hash-empty? MENU)
     (error "没有设定菜单或者您已经吃完了所有的菜")]
    [else
     (define-values (meals weights) (menu->meal-options MENU))
     (define meal (random-choice meals weights))
     (set! MEAL-HIS (cons meal MEAL-HIS))
     ; 检查本次用餐余额是不是用完了
     (define meal-def (hash-ref MENU meal))
     (if (only-on-meal meal-def)
         (hash-remove! MENU meal)
         (hash-set! MENU meal (eat-one meal-def)))
     meal]))

(define (eat-one meal-def)
  (list (sub1 (car meal-def)) (cadr meal-def)))

(define (only-on-meal meal-def)
  (= 1 (car meal-def)))

(define (menu->meal-options menu)
  (values (hash-keys menu) (map cadr (hash-values menu))))

(define (weights->weight-buckets weights)
  (for/fold ([base 0]
             [buckets null]
             #:result (reverse buckets))
            ([w (in-list weights)])
    (values (+ base w) (cons (+ base w) buckets))))

(define (random-choice lst weights)
  (let* ([weight-buckets (weights->weight-buckets weights)]
         [seed (apply max weight-buckets)]
         [rand (random seed)]
         [idx (for/fold ([item 0]) ([m (in-list weight-buckets)]
                                    #:break (< rand m))
                ;(println (format "seed ~A rand ~A buckets ~A" seed rand weight-buckets))
                (add1 item))])
    (list-ref lst idx)))

(module+ test
  (require rackunit)

  (define-values (a b) (for/fold ([a 0] [b 0]) ([i (in-range 100000)])
                         (if (eq? 'abc (random-choice '(abc 123) '(1 5)))
                             (values (add1 a) b)
                             (values a (add1 b)))))
  ;(printf (format "a ~A\nb ~A\na/b ~A" a b (exact->inexact (/ a b))))
  (check-true (< (abs (- (/ 1 5) (/ a b))) 0.01)
              (format "~A" (exact->inexact (abs (- (/ 1 5) (/ a b))))))

  ;(init-menu)
  ;(for ([i (in-naturals)]) (println (what-to-eat)))
  (println "all tests pass")
  )