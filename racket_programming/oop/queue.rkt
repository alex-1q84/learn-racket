#lang racket

(define queue%
  (class object%
    ;;optional init parameter
    (init [queue-list '()])
    ;;fields
    (define head '())
    (define tail '())
    ;;call super init first
    (super-new)

    ;;define methods
    (define/public (enqueue val)
      (let [(t (mcons val '()))]
        (if (null? head)
            (begin
              (set! head t)
              (set! tail t))
            (begin
              (set-mcdr! tail t)
              (set! tail t)))))

    (define/public (dequeue)
      (if (null? head)
          (error "queue is empty.")
          (begin0
            (mcar head)
            (set! head (mcdr head))
            (when (null? head) (set! tail head)))))

    (define/public (print-queue)
      (define (prt queue)
        (if (null? queue)
            (newline)
            (begin
              (printf "~A " (mcar queue))
              (prt (mcdr queue)))))
      (prt head))

    ;;self init
    (for ([v (in-list queue-list)])
      (enqueue v))))


(define queue (new queue%))
(send queue print-queue)
(send queue enqueue "abc")
(send queue print-queue)
(send queue enqueue "def")
(send queue enqueue 1)
(send queue print-queue)
(send queue dequeue)
(send queue print-queue)
(send queue enqueue "hoho")
(send queue print-queue)
(send queue dequeue)
(send queue print-queue)