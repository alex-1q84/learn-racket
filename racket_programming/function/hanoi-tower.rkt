#lang racket
;; 设定把所有磁盘移出的钉子为 f
;; 目标钉子为 t
;; 剩余磁盘所在钉子为 u
;; 则移动步骤如下
;; 1. 将 n-1 个磁盘从 f 移动到 u。
;; 2. 将单个圆盘从 f 移动到 t。
;; 3. 将 n-1 个磁盘从 u 移动到 t。
(define (hanoi n f t)
  (if (= n 1) (list (list f t))
      ;; 所有 3 个钉子编号为 0 1 2 ，则 u 的编号是编号总和 3 减去 f 和 t 钉子编号的结果
      (let* ([u (- 3 (+ f t))]
             ;; 把 n-1 个盘从 f 移动到 u
             [m1 (hanoi (sub1 n) f u)]
             ;; 把最后一个盘子从 f 移动到 t
             [m2 (list f t)]
             ;; 重复移动过程，把剩下的 n - 1 个盘子从 u 移动到 t
             [m3 (hanoi (sub1 n) u t)])
        (append m1 (cons m2 m3)))))