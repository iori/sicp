; https://www.serendip.ws/archives/1817

;; 基盤の apply への参照を apply-in-underlying-scheme へ退避させる（こうすることで、基盤の apply に apply-in-underlying-scheme という名前でアクセスできる）。
(define apply-in-underlying-scheme apply)

;;;; apply の定義
(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (else
          (error
            "Unknown procedure type -- APPLY" procedure))))

;;;; eval の定義
(define (eval exp env)
  (prompt-for-input exp)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((unbind? exp) (eval-unbinding exp env))
        ((if? exp) (eval-if exp env))
        ((let? exp) (eval (let->combination exp) env))
        ((letrec? exp) (eval (letrec->let exp) env))
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
          (error "Unknown expression type -- EVAL" exp))))

;;;; 手続きの引数
(define (list-of-values exps env)
  (if (no-operands? exps)
    '()
    (cons (eval (first-operand exps) env)
          (list-of-values (rest-operands exps) env))))

;;;; 条件式
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
    (eval (if-consequent exp) env)
    (eval (if-alternative exp) env)))

;;;; 並び
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

;;;; 代入と定義
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

;;;; 4.1.2 式の表現

;;;; 自己評価式は数と文字だけ
(define (self-evaluating? exp)
  (cond ((number? exp) #t)
        ((string? exp) #t)
        (else #f)))

;;;; 変数は記号で表現
(define (variable? exp) (symbol? exp))

;;;; クォート式は (quote <text-of-quotation>) の形
(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
    (eq? (car exp) tag)
    #f))

;;;; 代入は (set! <var> <value>) の形
(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))

;;;; 定義
(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
    (cadr exp)
    (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
    (caddr exp)
    (make-lambda (cdadr exp)      ; 仮パラメタ
                 (cddr exp))))    ; 本体

;;;; lambda 式は記号 lambda で始まるリスト
(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

;;;; 条件式
(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
    (cadddr exp)
    #f))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

;;;; begin
(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

;;;; 手続き作用
(define (application? exp) (pair? exp))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))

;;;; cond 式
(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
    #f
    (let ((first (car clauses))
          (rest (cdr clauses)))
      (if (cond-else-clause? first)
        (if (null? rest)
          (sequence->exp (cond-actions first))
          (error "ELSE clause isn't last -- COND->IF"
                 clauses))
        (make-if (cond-predicate first)
                 (sequence->exp (cond-actions first))
                 (expand-clauses rest))))))

;;;; 4.1.3 評価器のデータ構造

;;;; 述語のテスト
(define (true? x)
  (not (eq? x #f)))

(define (false? x)
  (eq? x #f))

;;;; 手続きの表現
;(define (make-procedure parameters body env)
;  (list 'procedure parameters body env))
; ex-4.16.b
(define (make-procedure parameters body env)
  (list 'procedure parameters (scan-out-defines body) env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))

; (define (procedure-body p) (caddr p))
; ex-4.16.b
(define (procedure-body p)
  (scan-out-defines (caddr p)))

(define (procedure-environment p) (cadddr p))

;;;; 環境に対する操作
(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))

(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

; ex-4.11
; (define (make-frame variables values)
;   (define (make-frame-iter variables values)
;     (if (null? variables)
;       '()
;       (cons (cons (car variables)
;                   (car values))
;             (make-frame-iter (cdr variables)
;                              (cdr values)))))
;   (make-frame-iter variables values))
;
; (define (frame-variables frame)
;   (if (null? frame)
;     '()
;     (cons (caar frame)
;           (frame-variables (cdr frame)))))
;
; (define (frame-values frame)
;   (if (null? frame)
;     '()
;     (cons (cdar frame)
;           (frame-values (cdr frame)))))
;
; (define (add-binding-to-frame! var val frame)
;  (set-cdr! frame (cons (cons var val) (cdr frame))))


(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
    (cons (make-frame vars vals) base-env)
    (if (< (length vars) (length vals))
      (error "Too many arguments supplied" vars vals)
      (error "Too few arguments supplied" vars vals))))

; default-start
; (define (lookup-variable-value var env)
;   (define (env-loop env)
;     (define (scan vars vals)
;       (cond ((null? vars)
;              (env-loop (enclosing-environment env)))
;             ((eq? var (car vars))
;              (car vals))
;             (else (scan (cdr vars) (cdr vals)))))
;     (if (eq? env the-empty-environment)
;       (error "Unbound variable" var)
;       (let ((frame (first-frame env)))
;         (scan (frame-variables frame)
;               (frame-values frame)))))
;   (print " >> env: " env)
;   (env-loop env))
;
; (define (set-variable-value! var val env)
;   (define (env-loop env)
;     (define (scan vars vals)
;       (cond ((null? vars)
;              (env-loop (enclosing-environment env)))
;             ((eq? var (car vars))
;              (set-car! vals val))
;             (else (scan (cdr vars) (cdr vals)))))
;     (if (eq? env the-empty-environment)
;       (error "Unbound variable -- SET!" var)
;       (let ((frame (first-frame env)))
;         (scan (frame-variables frame)
;               (frame-values frame)))))
;   (env-loop env))
;
; (define (define-variable! var val env)
;   (let ((frame (first-frame env)))
;     (define (scan vars vals)
;       (cond ((null? vars)
;              (add-binding-to-frame! var val frame))
;             ((eq? var (car vars))
;              (set-car! vals val))
;             (else (scan (cdr vars) (cdr vals)))))
;     (scan (frame-variables frame)
;          (frame-values frame))))
; default-end

; ex-4.12
(define (scan var vars vals)
  (cond ((null? vars) '())
        ((eq? var (car vars)) vals)
        (else (scan var (cdr vars) (cdr vals)))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (let ((frame (first-frame env)))
        (print "frame: " frame)
        (let ((res (scan var (frame-variables frame) (frame-values frame))))
          (print "res: " res)
          (print "var: " var)
          ; ex-4.16-a
          (cond ((null? res) (env-loop (enclosing-environment env)))
                ((eq? var '*unassigned*) (error "*unassigned*"))
                (else (car res)))))))
          ;(if (null? res)
          ;  (env-loop (enclosing-environment env))
          ;  (car res))))))
  (env-loop env))

; ex-4.16-b
(define (scan-out-defines body)
  (define (iter exp vars sets exps)
    (if (null? exp)
      (list (reverse vars) (reverse sets) (reverse exps))
      (if (definition? (car exp))
        (iter (cdr exp) (cons (list (definition-variable (car exp)) ''*unassigned*) vars) (cons (list 'set! (definition-variable(car exp)) (definition-value (car exp))) sets) exps)
        (iter (cdr exp) vars sets (cons (car exp) exps)))))

  (define (include-define? exp)
    (if (null? exp)
      #f
      (if (definition? (car exp))
        #t
        (include-define? (cdr exp)))))

  (if (include-define? body)
    (let ((var-val-exp-list (iter body '() '() '())))
      (list (cons 'let (cons (car var-val-exp-list) (append (cadr var-val-exp-list) (caddr var-val-exp-list))))))
    body))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
      (error "Unbound variable -- SET!" var)
      (let ((frame (first-frame env)))
        (let ((res (scan var (frame-variables frame) (frame-values frame))))
          (print "res2: " res)
          (if (null? res)
            (env-loop (enclosing-environment env))
            (set-car! res val))))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (print "frame: " frame)
    (let ((res (scan var (frame-variables frame) (frame-values frame))))
      (print "res3: " res)
      (if (null? res)
        (add-binding-to-frame! var val frame)
        (set-car! vals val)))))

; ex-4.13
(define (unbind? exp) (tagged-list? exp 'unbind!))
(define (unbinding-varialbe exp) (cadr exp))
(define (eval-unbinding exp env)
    (unbind-variable! (unbinding-varialbe exp) env)
      'ok)

; (define (unbind-variable! var env)
;   (let ((frame (first-frame env)))
;     (let ((vars (car frame)) (vals (cdr frame)))
;       (set-car! frame
;                 (remove!
;                   (lambda (x)
;                     (begin
;                       (print "x: " x)
;                       (print "var: " var)
;                       (print "eq: " (eq? x var))
;                       (eq? x var)))
;                   vars)))))

(define (unbind-variable! var env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (error "Unbound variabl --UNBIND-VARIABLE:" var))
            ((eq? var (car vars))
             (set-car! vars (cadr vars))
             (set-cdr! vars (cddr vars))
             (set-car! vals (cadr vals))
             (set-cdr! vals (cddr vals)))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))


;;;; 4.1.4 評価器をプログラムとして走らせる
(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        ;; 基本手続きが続く
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '= =)
        ))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (setup-environment)
  (let ((initial-env
          (extend-environment (primitive-procedure-names)
                              (primitive-procedure-objects)
                              the-empty-environment)))
    (define-variable! 'true #t initial-env)
    (define-variable! 'false #f initial-env)
    initial-env))

(define the-global-environment (setup-environment))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
    (primitive-implementation proc) args))


;;;; 基盤の Lisp システムの"読み込み-評価-印字"ループをモデル化する"駆動ループ(driver loop)"を用意する。
(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
    (display (list 'compound-procedure
                   (procedure-parameters object)
                   (procedure-body object)
                   '<procedure-env>))
    (display object)))

; ex-4.6
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


; (letrec ((fact (lambda (n) (if (= n 1) 1 (* n (fact (- n 1))))))) (fact 10))
; (letrec ((fact
;            (lambda (n)
;              (if (= n 1)
;                1
;                (* n (fact (- n 1)))))))
;  (fact 10))
; ex-4.20
(define (letrec? exp) (tagged-list? exp 'letrec))

(define (letrec->let exp)
  (let ((vars (map car (cadr exp)))
        (exps (map cdr (cadr exp)))
        (body (cddr exp)))
    (cons 'let
          (cons (map (lambda (x) (list x ''*unassigned*)) vars)
                (append (map (lambda (x y) (cons 'set! (cons x y))) vars exps)
                        body)))))
