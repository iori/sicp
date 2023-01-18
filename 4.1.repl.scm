(add-load-path "./packages/" :relative)
(load "repl.scm")

; これrepl.scmのオーバーライドしたいけどどうやるんだろう
(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        ;; 基本手続きが続く
        (list '+ +)
        (list '* *)
        ))

; ex-4.6
; (define (foo x) (let ((a 2) (b 5)) (+ (* x a) b)))
; (foo 3)
; 11
(define (let? exp) (tagged-list? exp 'let))

(define (let-clauses exp) (cdr exp))

(define (let-bindings clauses) (car clauses))

(define (let-body clauses) (cdr clauses))

(define (let->combination exp)
  (expand-let-clauses (let-clauses exp)))

(define (expand-let-clauses clauses)
  (if (null? (let-bindings clauses))
    '()
    (cons (make-lambda (map car (let-bindings clauses)) (let-body clauses))
          (map cadr (let-bindings clauses)))))

(define (eval exp env)
  (cond ((self-evaluating? exp)
         (print " > eval:then self-evaluating? " exp)
         exp)
        ((variable? exp)
         (print " > eval:then variable? " exp)
         (lookup-variable-value exp env))
        ((quoted? exp)
         (print " > eval:then quoted? " exp)
         (text-of-quotation exp))
        ((assignment? exp)
         (print " > eval:then assignment? " exp)
         (eval-assignment exp env))
        ((definition? exp)
         (print " > eval:then definition? " exp)
         (eval-definition exp env))
        ((if? exp)
         (print " > eval:then if? " exp)
         (eval-if exp env))
        ((let? exp)
         (print " > eval:then let? " exp)
         (eval (let->combination exp) env))
        ((lambda? exp)
         (print " > eval:then lambda? " exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (print " > eval:then begin? " exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp)
         (print " > eval:then cond? " exp)
         (eval (cond->if exp) env))
        ((application? exp)
         (print " > eval:application " exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
          (error "Unknown expression type -- EVAL" exp))))

; ex-4.7
;gosh$ (let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (* x z))
; 39
;
; (let* ((x 3)
;        (y (+ x 2)) ; 5
;        (z (+ x y 5))) ; 3 + 5 + 5 = 13
;  (* x z)) ; 13 * 3 = 39

(driver-loop)
