#lang racket

(require json)

(struct bookmark (guid
                  title
                  index
                  dateAdded
                  lastModified
                  id
                  typeCode
                  type
                  uri
                  iconuri
                  tags
                  charset
                  root
                  children))

(define json-data
  (with-input-from-file "/Users/mywo/Downloads/bookmarks-2019-09-12.json"
    (λ () (read-json))))

