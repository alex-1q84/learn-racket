#lang racket
(require xml)

(xexpr->string `(html
        (head (title "hello"))
        (body (div "world!"))))

(xexpr->string `(books ((coding "utf-8"))
                       (book
                        (author "bruce")
                        (isbn "IS123421412"))))

(define n (make-base-namespace))

(define (hello name)
  (displayln (format "hello ~A" name)))

(hello "hi!")

(xexpr->string
 (string->xexpr "<?xml version=\"1.0\" encoding=\"UTF-8\"?><books><book><author>nile</author></book></books>"))

; not work in DrRacket
#;(eval `(hello "world") n)


; base64 demo
(require net/base64)

(bytes->string/locale (base64-encode (string->bytes/locale "abc")))

(base64-decode (string->bytes/locale "YWJj"))

(bytes->string/locale (base64-decode (base64-encode (string->bytes/locale "abc"))))


(display "for usage demos")
; 注意 #:unless 和 #:when 属于条件语句，当只会让 for 在符合所指定条件时执行 body 子句，并不会让 for 终止
(for ([n (in-naturals)] #:unless (> n 100))
  (displayln n))

(for ([n (in-naturals)] #:when (< n 100))
  (displayln n))

; #:break 和 #:final 是 for 的终止条件，两者区别看运行示例即可明了
(for ([n (in-naturals)] #:break (= n 100))
  (displayln n))

(for ([n (in-naturals)] #:final (= n 100))
  (displayln n))
