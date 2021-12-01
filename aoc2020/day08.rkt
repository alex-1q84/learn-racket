#lang racket
(define *acc* 0)
(define *pos* 0)
(define *instructions* empty)
(define *instr-exec-counts* (make-hash))

(define (next-pos)
  (set! *pos* (add1 *pos*)))

(define (nop opnum)
  (next-pos))

(define (acc opnum)
  (next-pos)
  (set! *acc* (+ *acc* opnum)))

(define (jmp opnum)
  (set! *pos* (+ *pos* opnum)))

(define (exec-game-instructions)
  ;需要知道退出状态是正常执行完退出还是指令重复执行而退出
  ; return 'state acc
  (cond
    [(empty? *instructions*) (values 'end *acc*)]
    [(pos-executed? *pos*) (values 'break *acc*)]
    [(all-instr-executed?) (values 'end *acc*)]
    [else
     (match (instr-at-pos *pos*)
       [#f *acc*]
       [(list op opnum)
        (match op
          ["nop" (nop opnum)]
          ["acc" (acc opnum)]
          ["jmp" (jmp opnum)]
          [else (raise-argument-error 'exec-game-instructions
                                      "nop or acc or jmp"
                                      op)])
        (count-pos-executions *pos*)
        (exec-game-instructions)])]))

(define (all-instr-executed?)
  (eq? (hash-count *instr-exec-counts*)
       (length *instructions*)))

(define (count-pos-executions pos)
  (hash-set! *instr-exec-counts* pos
             (add1 (hash-ref *instr-exec-counts* pos 0))))

(define (pos-executed? pos)
  (> (hash-ref *instr-exec-counts* pos 0) 1))

(define (instr-at-pos pos)
  (with-handlers ([exn:fail:contract?
                   (lambda (e) false)])
    (list-ref *instructions* pos)))

(define (parse-to-instructions input)
  (map parse-to-instrion (port->lines input)))

(define (parse-to-instrion str)
  (match str
    [(regexp #px"(\\w+)\\s+([+-]?\\d+)" (list _ op (app string->number opnum)))
     (list op opnum)]))


(set! *instructions* (parse-to-instructions (open-input-file "input08.txt")))
(exec-game-instructions)
