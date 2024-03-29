#lang racket
(require racket/match)

(define fld-re #px" ?([a-z]+):([^ \n]+)")

(define passports
  (call-with-input-file "input04.txt"
    (lambda (in)
      (for/fold ([passports null]
                 [passport (hash)]
                 ; for/fold 方法收集的结果是倒序的，所以这里需要反转一次以得到跟输入一致的顺序
                 #:result (reverse
                           ; compose1 方法组合两个函数并形成一个新的函数，这个函数会强制检查被组合的两个函数是否都只接受一个参数
                           (filter (compose1 not hash-empty?)
                                   (cons passport passports))))
                ([line (in-lines in)])
        (cond
          [(string=? line "")
           ; 使用 values 方法确保返回结果的数量和顺序跟 for/fold 方法初始
           (values (cons passport passports) (hash))]
          [else
           (define line-in (open-input-string line))
           (let loop ([passport passport])
             (match (regexp-try-match fld-re line-in)
               [#f (values passports passport)]
               [(list _
                      (app bytes->string/utf-8 fld)
                      (app bytes->string/utf-8 val))
                (loop (hash-set passport fld val))]))])))))

(define (passport-valid?/old p)
  (equal?
   '("byr" "ecl" "eyr" "hcl" "hgt" "iyr" "pid")
   (sort (hash-keys (hash-remove p "cid")) string<?)))

(define byr-re #rx"^(19[2-9][0-9]|2000|2001|2002)$")
(define iyr-re #rx"^(201[0-9]|2020)$")
(define eyr-re #rx"^(202[0-9]|2030)$")
(define hgt-re #rx"^((1[5-8][0-9]|190|191|192|193)cm|(59|6[0-9]|7[0-6])in)$")
(define hcl-re #px"^#[0-9a-f]{6}")
(define ecl-re #rx"^(amb|blu|brn|gry|grn|hzl|oth)$")
(define pid-re #px"^\\d{9}$")

(define (passport-valid?/new p)
  (and (passport-valid?/old p)
       (regexp-match? byr-re (hash-ref p "byr"))
       (regexp-match? iyr-re (hash-ref p "iyr"))
       (regexp-match? eyr-re (hash-ref p "eyr"))
       (regexp-match? hgt-re (hash-ref p "hgt"))
       (regexp-match? hcl-re (hash-ref p "hcl"))
       (regexp-match? ecl-re (hash-ref p "ecl"))
       (regexp-match? pid-re (hash-ref p "pid"))))

(for/sum ([p (in-list passports)]
          #:when (passport-valid?/old p))
  1)

(for/sum ([p (in-list passports)]
          #:when (passport-valid?/new p))
  1)

(module+ test
  (require rackunit)

  "all test run")