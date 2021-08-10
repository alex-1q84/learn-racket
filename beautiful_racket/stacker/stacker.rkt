#lang br/quicklang

(provide read-syntax)
(provide (rename-out [stacker-module-begin #%module-begin]))
(provide handle)
(provide + *)

(define (read-syntax path port)
  (define src-lines (port->lines port))
  (define src-datums (format-datums '(handle ~a) src-lines))
  (define module-datum `(module stacker-mod "stacker.rkt"
                          ,@src-datums))
  (datum->syntax #f module-datum))

;**************************************************************************************;
;****                            Provide #%Module-Begin                            ****;
;**************************************************************************************;
(define-macro (stacker-module-begin HANDLE-EXPR ...)
  #'(#%module-begin
     HANDLE-EXPR ...
     (display (first stack))))

;***************************************************************************************************************************************;
;**** Imple­Ment A Stack, With An Inter­Face For Storing, Reading, And Doing Oper­A­Tions On Argu­Ments, That Can Be Used By Handle ****;
;***************************************************************************************************************************************;
(define stack empty)

(define (pop-stack!)
  (define arg (first stack))
  (set! stack (rest stack))
  arg)

(define (push-stack! arg)
  (set! stack (cons arg stack)))

;*******************************************************************************************************************************************************************;
;**** Provide Bind­Ings For Three Iden­Ti­Fiers: Handle, Which Deter­Mines What To Do With Each Argu­Ment; +, A Stack Oper­Ator; And *, Another Stack Oper­Ator ****;
;*******************************************************************************************************************************************************************;
(define (handle [arg #f])
  (cond
    [(number? arg) (push-stack! arg)]
    [(or (equal? + arg) (equal? * arg))
     (define op-result (arg (pop-stack!) (pop-stack!)))
     (push-stack! op-result)]))

