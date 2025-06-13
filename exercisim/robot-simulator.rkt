#lang racket

(provide robot%)

(define robot%
  (class object%
    (init-field position direction)
    (super-new)

;; 定义四个方向，按顺序排列，这样如果我们知道当前方向的位置就能通过该位置的前一个或后一个元素指出向左转或向右转到方向
;; 再定义四个前进方向所对应的单位坐标改变

    (define directions #(north east south west))
    (define dir-pos-map
      (for/fold ([m (hash)] [pos 0] #:result m) ([d (in-vector directions)])
        (values (hash-set m d pos) (add1 pos))))
    
    (define (turn lr)
      (match lr
        [#\L (vector-ref directions (modulo (sub1 (hash-ref dir-pos-map direction)) 4))]
        [#\R (vector-ref directions (modulo (add1 (hash-ref dir-pos-map direction)) 4))]))

    (define (move-ahead pos dir)
      (match dir
        ['north `(,(+ (first pos) 0) ,(+ (second pos) 1))]
        ['east `(,(+ (first pos) 1) ,(+ (second pos) 0))]
        ['south `(,(+ (first pos) 0) ,(+ (second pos) -1))]
        ['west `(,(+ (first pos) -1) ,(+ (second pos) 0))]))
    
    (define/public (move directions)
      (for ([d (in-string directions)])
        (match d
          [#\L (set! direction (turn d))]
          [#\R (set! direction (turn d))]
          [#\A (set! position (move-ahead position direction))])))
    ))

(require racket/class)
(module+ test
  (require rackunit rackunit/text-ui))
(module+ test
  (define suite
    (test-suite "robot simulator tests"
                (test-case "Create robot at origin facing north"
                           (define robot (new robot% [position '(0 0)] [direction 'north]))
                           (check-equal? (get-field position robot) '(0 0))
                           (check-equal? (get-field direction robot) 'north))
        
                (test-case "Create robot at negative position facing south"
                           (define robot (new robot% [position '(-1 -1)] [direction 'south]))
                           (check-equal? (get-field position robot) '(-1 -1))
                           (check-equal? (get-field direction robot) 'south))
    
                (test-case "Rotating clockwise changes north to east"
                           (define robot (new robot% [position '(0 0)] [direction 'north]))
                           (send robot move "R")
                           (check-equal? (get-field position robot) '(0 0))
                           (check-equal? (get-field direction robot) 'east))
        
                (test-case "Rotating clockwise changes east to south"
                           (define robot (new robot% [position '(0 0)] [direction 'east]))
                           (send robot move "R")
                           (check-equal? (get-field position robot) '(0 0))
                           (check-equal? (get-field direction robot) 'south))
        
                (test-case "Rotating clockwise changes south to west"
                           (define robot (new robot% [position '(0 0)] [direction 'south]))
                           (send robot move "R")
                           (check-equal? (get-field position robot) '(0 0))
                           (check-equal? (get-field direction robot) 'west))
        
                (test-case "Rotating clockwise changes west to north"
                           (define robot (new robot% [position '(0 0)] [direction 'west]))
                           (send robot move "R")
                           (check-equal? (get-field position robot) '(0 0))
                           (check-equal? (get-field direction robot) 'north))
    
                (test-case "Rotating counter-clockwise changes north to west"
                           (define robot (new robot% [position '(0 0)] [direction 'north]))
                           (send robot move "L")
                           (check-equal? (get-field position robot) '(0 0))
                           (check-equal? (get-field direction robot) 'west))
        
                (test-case "Rotating counter-clockwise changes west to south"
                           (define robot (new robot% [position '(0 0)] [direction 'west]))
                           (send robot move "L")
                           (check-equal? (get-field position robot) '(0 0))
                           (check-equal? (get-field direction robot) 'south))
        
                (test-case "Rotating counter-clockwise changes south to east"
                           (define robot (new robot% [position '(0 0)] [direction 'south]))
                           (send robot move "L")
                           (check-equal? (get-field position robot) '(0 0))
                           (check-equal? (get-field direction robot) 'east))
        
                (test-case "Rotating counter-clockwise changes east to north"
                           (define robot (new robot% [position '(0 0)] [direction 'east]))
                           (send robot move "L")
                           (check-equal? (get-field position robot) '(0 0))
                           (check-equal? (get-field direction robot) 'north))
    
                (test-case "Moving forward one facing north increments Y"
                           (define robot (new robot% [position '(0 0)] [direction 'north]))
                           (send robot move "A")
                           (check-equal? (get-field position robot) '(0 1))
                           (check-equal? (get-field direction robot) 'north))
        
                (test-case "Moving forward one facing south decrements Y"
                           (define robot (new robot% [position '(0 0)] [direction 'south]))
                           (send robot move "A")
                           (check-equal? (get-field position robot) '(0 -1))
                           (check-equal? (get-field direction robot) 'south))
        
                (test-case "Moving forward one facing east increments X"
                           (define robot (new robot% [position '(0 0)] [direction 'east]))
                           (send robot move "A")
                           (check-equal? (get-field position robot) '(1 0))
                           (check-equal? (get-field direction robot) 'east))
        
                (test-case "Moving forward one facing west decrements X"
                           (define robot (new robot% [position '(0 0)] [direction 'west]))
                           (send robot move "A")
                           (check-equal? (get-field position robot) '(-1 0))
                           (check-equal? (get-field direction robot) 'west))
    
                (test-case "Follow series of instructions - moving east and north from README"
                           (define robot (new robot% [position '(7 3)] [direction 'north]))
                           (send robot move "RAALAL")
                           (check-equal? (get-field position robot) '(9 4))
                           (check-equal? (get-field direction robot) 'west))
        
                (test-case "Follow series of instructions - moving west and north"
                           (define robot (new robot% [position '(0 0)] [direction 'north]))
                           (send robot move "LAAARALA")
                           (check-equal? (get-field position robot) '(-4 1))
                           (check-equal? (get-field direction robot) 'west))
        
                (test-case "Follow series of instructions - moving west and south"
                           (define robot (new robot% [position '(2 -7)] [direction 'east]))
                           (send robot move "RRAAAAALA")
                           (check-equal? (get-field position robot) '(-3 -8))
                           (check-equal? (get-field direction robot) 'south))
        
                (test-case "Follow series of instructions - moving east and north"
                           (define robot (new robot% [position '(8 4)] [direction 'south]))
                           (send robot move "LAAARRRALLLL")
                           (check-equal? (get-field position robot) '(11 5))
                           (check-equal? (get-field direction robot) 'north))))
  (run-tests suite))