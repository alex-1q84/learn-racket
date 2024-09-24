#lang racket/base
;; format aliyun dataworks 数据地图表概况

(require racket/string
         chw-common/list
         threading)


(define (format-desc str)
  (define desc-lst (~> str
                       (string-split "\n")
                       reverse
                       (list-chunk 2)
                       (reverse)))
  (string-join
   (map (lambda (p)
          (format "~A ~A" (car p) (cadr p)))
        desc-lst)
   "\n"))

(displayln
 (format-desc "6
浏览次数
9535
读取次数
2
收藏次数"))