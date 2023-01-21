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

; ここでは(let)はなくなっていて、
; let-bindingsは変数宣言部分
; let-bodyは関数の本体
;
; (define (foo x) (let ((a 2) (b 5)) (+ (* x a) b)))
; これは以下になって渡される.
; (((a 2) (b 5)) (+ (* x a) b))
; 最後の(cons)では
; ((lambda (a b) (+ (* x a) b)) 2 5)
; 以下になる。ここではmake-lambdaされているだけで実行はしない、式を作るだけ
(define (expand-let-clauses clauses)
  (print " >>> let-clauses: " clauses)
  (print " >>> let-bindings: " (let-bindings clauses))
  (print " >>> let-body: " (let-body clauses))

  (if (null? (let-bindings clauses))
    '()
    (cons (make-lambda (map car (let-bindings clauses)) (let-body clauses))
          (map cadr (let-bindings clauses)))))

; ex-4.7
;gosh$ (let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (* x z))
; 39
;
; (let* ((x 3)
;        (y (+ x 2)) ; 5
;        (z (+ x y 5))) ; 3 + 5 + 5 = 13
;  (* x z)) ; 13 * 3 = 39
(define (let*? exp) (tagged-list? exp 'let*))
(define (let*-clauses exp) (cdr exp))
(define (let*-bindings clauses) (car clauses))
(define (let*-body clauses) (cadr clauses))
(define (make-let* defs body)
  (list 'let defs body))

; (let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (* x z))
; >>> let*:bindings: ((x 3) (y (+ x 2)) (z (+ x y 5)))
; >>> let*:body: (* x z)
; >>> let*:rest-bindings: ((x 3) (y (+ x 2)) (z (+ x y 5)))
; >>> let*:rest-bindings: ((y (+ x 2)) (z (+ x y 5)))
; >>> let*:rest-bindings: ((z (+ x y 5)))
; >>> let*:rest-bindings: ()
; (let ((x 3)); defs
;   ; body
;   (let ((y (+ x 2))) ; defs
;     ; body
;     (let ((z (+ x y 5))) ; defs
;       ; body
;       (* x z))))
(define (let*->nested-lets exp)
  (if (null? exp)
    #f
    (let ((clauses (let*-clauses exp)))
      (let ((bindings (let*-bindings clauses))
            (body (let*-body clauses)))
        (define (iter rest-bindings)
          (print " >>> let*:rest-bindings: " rest-bindings)

          (if (null? rest-bindings)
            body
            (make-let* (list (car rest-bindings))
                       (iter (cdr rest-bindings)))))

        (print " >>> let*:bindings: " bindings)
        (print " >>> let*:body: " body)
        (iter bindings)))))

; ex-4.8
; (let ⟨var⟩ ⟨bindings⟩ ⟨body⟩)
; (let var ((a 1)) (* a a))
;
; (define (fib n)
;   (let fib-iter ((a 1)
;                  (b 0)
;                  (count n)) (if (= count 0)
;                  b
;                  (fib-iter (+ a b) a (- count 1)))))
;
; (define (fib n) (let fib-iter ((a 1) (b 0) (count n)) (if (= count 0) b (fib-iter (+ a b) a (- count 1)))))
; (fib 3)

(define (named-let-var clauses) (car clauses))
(define (named-let-bindings clauses) (cadr clauses))
(define (named-let-body clauses) (caddr clauses))

(define (let->combination exp)
  (if (pair? (car (let-clauses exp)))
    (expand-let-clauses (let-clauses exp))
    (expand-named-let-clauses (let-clauses exp))))

; >>> let-clauses: (fib-iter ((a 1) (b 0) (count n)) (if (= count 0) b (fib-iter (+ a b) a (- count 1))))
; >>> let-var: fib-iter
; >>> let-bindings: fib-iter
; >>> let-body: (((a 1) (b 0) (count n)) (if (= count 0) b (fib-iter (+ a b) a (- count 1))))
;
; 以下を生成する
; (begin (define (fib-iter a b count) (if (= count 0) b (fib-iter (+ a b) a (- count 1)))) (fib-iter 1 0 n))
(define (expand-named-let-clauses clauses)
  (print " >>> named-let-clauses: " clauses)
  (print " >>> named-let-var: " (named-let-var clauses))
  (print " >>> named-let-bindings: " (let-bindings clauses))
  (print " >>> named-let-body: " (let-body clauses))

  (make-begin
    (list
      (list 'define (cons (named-let-var clauses) ; (fib-iter
                          (map car (named-let-bindings clauses))) ; a b count)
            (named-let-body clauses)) ; (if (= count 0) b (fib-iter (+ a b) a (- count 1)))
      (cons (named-let-var clauses) ; (fib-iter
            (map cadr (named-let-bindings clauses)))))) ; (+ a b) a (- count 1))

; ex-4.9
; (begin (define x 0) (while (= x 10) (set! x (+ x 1))))

(define (while->let exp env) (eval-while (cdr exp) env))

(define (while? exp) (tagged-list? exp 'while))
(define (while-predicate exp) (car exp))
(define (while-body exp) (cadr exp))

; ((= x 10) (+ x 1))
(define (eval-while exp env)
  (if (eval (while-predicate exp) env)
    #t
    (begin
      (eval (while-body exp) env)
      (eval-while exp env))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
        ((while? exp)
         (print " > eval:then while? " exp)
         (while->let exp env))
        ((let*? exp) (eval (let*->nested-lets exp) env))
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

(driver-loop)
