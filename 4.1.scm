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

; ex-4.2
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ; 手続き作用を代入の前にする
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        (else
          (error "Unknown expression type - - EVAL" exp))))

(define expression '(set! val (+ 1 2)))
; a. application?がassigmentに到達させない
;
;    gosh$ (eval expression genv)
;    *** ERROR: Unbound variable set!

; b.
(define (application? exp)
  (tagged-list? exp 'call))

(define (eval exp env)
  ; 手続き作用を最初にする
  (cond ((application? exp)
         (let ((remove-call-exp (cdr exp)))
           (apply (eval (operator remove-call-exp) env)
                  (list-of-values (operands remove-call-exp) env))))
        ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        (else
          (error "Unknown expression type - - EVAL" exp))))

(define expression '(call + 1 2))
; (eval expression genv)
; 3

; ex-4.3
; 今後使わないっぽいのでskip

(load "eval-apply.scm")

; ex-4.4

; eval-and
; gosh$ (and (= 1 1))
; #t
; gosh$ (and (= 1 2))
; #f
(define (and? exp) (tagged-list? exp 'and))
(define (eval-and exp env)
  (define (iter exp result)
    (if (null? exp)
      result
      (let ((first-eval (eval (car exp) env)))
        (if (true? first-eval)
          (iter (cdr exp) first-eval)
          #f))))
  (if (null exp)
    #t
    (iter exp '())))

; eval-or
; gosh$ (or (= 1 1))
; #t
; gosh$ (or (= 1 2) (= 1 1))
; #t
; gosh$ (or (= 1 2) (= 1 3))
; #f
(define (or? exp) (tagged-list? exp 'or))
(define (eval-or exp env)
  (define (iter exp result)
    (if (null? exp)
      #f
      (let ((first-eval (eval (car exp) env)))
        (if (true? first-eval)
          #t
          (iter (cdr exp) first-eval)))))
  (if (null exp)
    #t
    (iter exp '())))

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((and? exp) (eval-and exp env))
        ((or? exp) (eval-or exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
          (error "Unknown expression type - - EVAL" exp))))

