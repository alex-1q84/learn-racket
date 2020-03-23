#lang racket

(require json)

(struct bookmark (title
                  url
                  children
                  ))

(define bookmark-json-str
  #<<EOF
{"title": "title1", "url": "http://www.google.com",
"children":[{"title": "title2", "url": "http://www.163.com", "children": null},
{"title": "title3", "url": "http://www.qq.com", "children": null}]}
EOF
  )

;(print bookmark-json-str)

(define jsexpr
  (read-json (open-input-string bookmark-json-str)))

(println jsexpr)

(define (walk bookmark)
  (when bookmark
      (println (hash-ref bookmark 'url))
      (walk (hash-ref bookmark 'children))
      ))

(walk jsexpr)