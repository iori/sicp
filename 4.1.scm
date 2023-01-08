(add-load-path "./packages/" :relative)
(load "eval-apply.scm")

; ex-4.1

#|
(define (list-of-values exps env)
  (if (no-operands? exps)
    '()
    (cons (eval (first-operand exps) env)
          (list-of-values (rest-operands exps) env))))
|#

;; 被演算子を左から右へ評価する list-of-values
#|
(define (list-of-values exps env)
  (if (no-operands? exps)
    '()
    (let ((first-eval (eval (first-operand exps) env)))
      (cons first-eval
            (list-of-values (rest-operands exps) env)))))
|#

;; 被演算子を右から左へ評価する list-of-values
(define (list-of-values exps env)
  (if (no-operands? exps)
    '()
    (let ((first-eval (list-of-values (rest-operands exps) env)))
      (cons (eval (first-operand exps) env)
            first-eval))))

;(define val 10)
; (define expression '((set! val (+ val 2)) (set! val (* val 2))))

(define the-global-environment (setup-environment))
; (list-of-values '((* 10 100) (+ 10 10)) (setup-environment))
; (list-of-values expression the-global-environment)
