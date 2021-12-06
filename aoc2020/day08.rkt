#lang racket

(define (exec-game-instructions instrs)
  ;需要知道退出状态是正常执行完退出还是指令重复执行而退出
  ; return 'state acc

  (let loop ([pos 0]
             [acc 0]
             [seen (set)])
    (define-values (new-pos new-acc)
      (exec-instr instrs pos acc))
    (cond
      [(set-member? seen new-pos) (values 'break acc)]
      [(eq? new-pos (length instrs)) (values 'end acc)]
      [else
       (loop new-pos new-acc (set-add seen new-pos))])))


(define (exec-instr instrs pos acc)
  (match (list-ref instrs pos)
    [(list "nop" _) (values (add1 pos) acc)]
    [(list "acc" opnum) (values (add1 pos) (+ acc opnum))]
    [(list "jmp" opnum) (values (+ pos opnum) acc)]))

(define (patch-instr instrs pos)
  (list-set instrs pos
            (match (list-ref instrs pos)
              [(list "nop" op) (list "jmp" op)]
              [(list "jmp" op) (list "nop" op)])))

; 遍历整个指令集合并找出 nop jmp 指令，逐个测试并找出修改后可以最终执行完的新指令集合
(define (fix-instrs instrs)
  (define poses
    (for/list ([i (in-range (length instrs))]
               #:when (or (equal? "nop" (first (list-ref instrs i)))
                          (equal? "jmp" (first (list-ref instrs i)))))
      i))
  (for*/or ([pos (in-list poses)]
            [new-instrs (in-value (patch-instr instrs pos))])
    (define-values (exit acc)
      (exec-game-instructions new-instrs))
    (and (eq? 'end exit)
         acc)))

(define (parse-to-instructions input)
  (map parse-to-instrion (port->lines input)))

(define (parse-to-instrion str)
  (match str
    [(regexp #px"(acc|nop|jmp)\\s+([+-]?\\d+)" (list _ op (app string->number opnum)))
     (list op opnum)]))


;; (exec-game-instructions '(("acc" 1) ("jmp" -1)))
(exec-game-instructions (parse-to-instructions (open-input-file "input08.txt")))

(fix-instrs (parse-to-instructions (open-input-file "input08.txt")))
