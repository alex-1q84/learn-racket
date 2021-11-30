#lang racket
(define *acc* 0)
(define *pos* 0)
(define *directions* empty)
(define *direct-exec-counts* (make-hash))

(define (next-pos)
  (set! *pos* (add1 *pos*)))

(define (nop opnum)
  (next-pos))

(define (count-pos-executions pos)
  (hash-set! *direct-exec-counts* pos
             (add1 (hash-ref *direct-exec-counts* pos 0))))

(define (acc opnum)
  (next-pos)
  (set! *acc* (+ *acc* opnum)))

(define (jmp opnum)
  (set! *pos* (+ *pos* opnum)))

(define (exec-game-directions)
  (cond
    [(empty? *directions*) *acc*]
    [(pos-executed? *pos*) *acc*]
    [else
     (match (direct-at-pos *pos*)
       [#f *acc*]
       [(list op opnum)
        (match op
          ["nop" (nop opnum)]
          ["acc" (acc opnum)]
          ["jmp" (jmp opnum)]
          [else (raise-argument-error 'exec-game-directions
                                      "nop or acc or jmp"
                                      op)])
        (count-pos-executions *pos*)
        (exec-game-directions)])]))

(define (pos-executed? pos)
  (> (hash-ref *direct-exec-counts* *pos* 0) 0))

(define (direct-at-pos pos)
  (with-handlers ([exn:fail:contract?
                   (lambda (e) false)])
    (list-ref *directions* pos)))

(define (parse-to-directions input)
  (map parse-to-direction (port->lines input)))

(define (parse-to-direction str)
  (match str
    [(regexp #px"(\\w+)\\s+([+-]?\\d+)" (list _ op (app string->number opnum)))
     (list op opnum)]))


(set! *directions* (parse-to-directions (open-input-file "input08.txt")))
(exec-game-directions)