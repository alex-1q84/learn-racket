#lang racket

(require rackunit rackunit/text-ui)
(require "{{main-file}}")

(module+ test
  (define suite
    (test-suite
     "{{package-name}} tests"
     ;; 添加测试用例
     (test-equal? "示例测试"
                  (被测试函数 输入参数)
                  期望结果)))
  (run-tests suite))
