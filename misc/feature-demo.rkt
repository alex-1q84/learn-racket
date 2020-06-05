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
(eval `(hello "world") n)