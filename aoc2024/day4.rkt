#lang racket
(require racket/generator)

(define (matrix-ref mat row col)
  (vector-ref (vector-ref mat row) col))

(define (matrix-row-length mat)
  (vector-length mat))

(define (matrix-col-length mat)
  (vector-length (vector-ref mat 0)))

;; 定义一个 producer 函数，用于生成整数序列
(define (make-producer start interval)
  (generator ()
             (let loop ([current start])
               (yield current)
               (loop (+ current interval)))))

;; (gen-pos (make-producer 1 0) (make-producer 1 1) 4)
(define (gen-pos a-proc b-proc num)
  (for/list ([n (in-range num)]
             [a (in-producer a-proc)]
             [b (in-producer b-proc)])
    `(,a ,b)))


(define (list-poses r c pos-len)
  (for/list ([conf (in-list `(((,r 0) (,c 1))
                              ((,r 1) (,c 1))
                              ((,r 1) (,c 0))
                              ((,r 1) (,c -1))
                              ((,r 0) (,c -1))
                              ((,r -1) (,c -1))
                              ((,r -1) (,c 0))
                              ((,r -1) (,c 1))))])
    (gen-pos (apply make-producer (first conf))
             (apply make-producer (second conf))
             pos-len)))

;; get a list of chars from given matrix with a list of postions
(define (matrix-pos-list-ref mat mat-pos-list)
  (for/list ([pos (in-list mat-pos-list)])
    (apply (curry matrix-ref mat) pos)))

;; valid positions like ((1 2) (1 3) (1 4) (1 5)) in matrix
(define (valid-matrix-pos vec-poses rowl coll)
  (and (<= 0 (first (last vec-poses)) (sub1 rowl))
       (<= 0 (second (last vec-poses)) (sub1 coll))))


(define word-matrix
  (list->vector
   (map (compose1 list->vector string->list)
        (port->lines (open-input-file "input4.txt" #:mode 'text)))))

(define matrix-row-len (matrix-row-length word-matrix))
(define matrix-col-len (matrix-col-length word-matrix))

;;========================== part 1 ==========================

(define TARGET-WORD (string->list "XMAS"))
(define TARGET-WORD-LENGTH (length TARGET-WORD))

(for*/sum ([r (in-range matrix-row-len)]
           [c (in-range matrix-col-len)]
           #:when (equal? (matrix-ref word-matrix r c) #\X))
  (length (filter (curry equal? TARGET-WORD)
                  (map (curry matrix-pos-list-ref word-matrix)
                       (filter (lambda (v)
                                 (valid-matrix-pos v matrix-row-len matrix-col-len))
                               (list-poses r c TARGET-WORD-LENGTH))))))

;;========================== part 2 ===========================

(define TARGET-WORD-SET (set #\M #\A #\S))

; because we find the X when reach the X cross point, and the cross will never on border
(for*/sum ([r (in-range 1 (sub1 matrix-row-len))]
       [c (in-range 1 (sub1 matrix-col-len))]
       #:when (equal? (matrix-ref word-matrix r c) #\A)) ; the X cross point is char A
  (let ([x (map (curry matrix-pos-list-ref word-matrix)
              `(((,(sub1 r) ,(sub1 c)) (,r ,c) (,(add1 r) ,(add1 c)))
                ((,(sub1 r) ,(add1 c)) (,r ,c) (,(add1 r) ,(sub1 c)))))])
    (if (and (set=? (list->set (first x)) TARGET-WORD-SET)
             (set=? (list->set (second x)) TARGET-WORD-SET))
        1
        0)))