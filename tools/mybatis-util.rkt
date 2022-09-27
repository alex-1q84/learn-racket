#lang racket/base

(require racket/string
         racket/cmdline
         racket/match)

(define (port->result-map in-port)
  (string-join (for/list ([line (in-lines in-port)])
                 (sql-column->result-map line))
               "\n"))

(define (sql-column->result-map col)
  (define str (string-downcase col))
  (format "<result property=\"~A\" column=\"~A\"/>" (string-camelCase str) col))

(define (string-camelCase str)
  (string-join (for/list ([v (in-list (string-split str "_"))]
                          [i (in-naturals)])
                 (if (> i 0)
                     (string-titlecase v)
                     v)) ""))

(define (port->optional-update in-port)
  (string-join
   (for/list ([line (in-lines in-port)])
     (java->optional-update line))
   "\n"))

(define (java->optional-update line)
  (define-values (type property) (extract-properties line))
  (define template
    (string-append "<if test=\"~A != null\">\n"
                   "    ~A=#{~A:~A},\n"
                   "</if>"))
  (format template
          property (java->sql property) property (java-type->sql-type type)))

(define (extract-properties line)
  (match (string-trim line)
    [(regexp #px"(\\w+)\\s+(\\w+)\\s+(\\w+)\\s*;?" (list _ _ type property))
     (values type property)]
    [(regexp #px"(\\w+)\\s+(\\w+)\\s*;?" (list _ type property))
     (values type property)]
    [else
     (error "unrecognized java property" line)]))

(define (java->sql property)
  ;; (regexp-match* "([a-z0-9_$]+|[A-Z0-9_$]+[a-z0-9_$]*)" "ab$A_aABa1")
  ;; => '("ab$" "A_a" "ABa1")
  (define parts (regexp-match* #px"([a-z0-9_$]+|[A-Z0-9_$]+[a-z0-9_$]*)" property))
  (string-join (for/list ([part (in-list parts)])
                 (string-upcase part))
               "_"))

(define (java-type->sql-type type)
  (match type
    ["String" "VARCHAR"]
    ["Integer" "NUMERIC"]
    ["Long" "NUMERIC"]
    ["int" "NUMERIC"]
    ["long" "NUMERIC"]
    ["Boolean" "NUMERIC"]
    ["bool" "NUMERIC"]
    ["Date" "TIMESTAMP"]
    [else "VARCHAR"]))

;; ========= command line portal ===========


(define (main)
  (define sql->java-result-map? (make-parameter #f))
  (define java->update-optional? (make-parameter #f))
  
  (command-line
   #:once-any
   ; 不带参数的指令选项
   [("--s2jm" "--sql-to-java-map") "generate result map" (sql->java-result-map? #t)]
   [("--j2ou" "--java-to-optional-update") "generate optional updates" (java->update-optional? #t)])

  
  (cond
    [(sql->java-result-map?)
     ;; read from std input
     (display (port->result-map (current-input-port)))]
    [(java->update-optional?)
     (display (port->optional-update (current-input-port)))]
    [else
     (error "fail" "please specify a flag, type mybatis-util --help for more details.")]))


(main)

