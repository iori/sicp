(add-load-path "./packages/" :relative)
(load "eval-apply.scm")

; ex-4.1
(define genv (setup-environment))
(define expression '((set! val (+ val 2)) (set! val (* val 2))))

(define (list-of-values exps env)
  (if (no-operands? exps)
    '()
    (cons (eval (first-operand exps) env)
          (list-of-values (rest-operands exps) env))))
(define-variable! 'val 10 genv)
(print (list-of-values expression genv))

;; 被演算子を左から右へ評価する list-of-values
(define (list-of-values exps env)
  (if (no-operands? exps)
    '()
    (let ((first-eval (eval (first-operand exps) env)))
      (cons first-eval
            (list-of-values (rest-operands exps) env)))))
(define-variable! 'val 10 genv)
(print (list-of-values expression genv))

;; 被演算子を右から左へ評価する list-of-values
(define (list-of-values exps env)
  (if (no-operands? exps)
    '()
    (let ((first-eval (list-of-values (rest-operands exps) env)))
      (cons (eval (first-operand exps) env)
            first-eval))))

(define-variable! 'val 10 genv)
(print (list-of-values expression genv))
