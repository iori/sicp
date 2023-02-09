(add-load-path "./packages/" :relative)
(load "repl.scm")

; ex-4.11
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
;
;; defaultのenv
; リストのペア
; (
;  (false true car cdr cons null? + - * =)
;  #f
;  #t
;  (primitive #<subr (car obj)>)
;  (primitive #<subr (cdr obj)>)
;  (primitive #<subr (cons obj1 obj2)>)
;  (primitive #<subr (null? obj)>)
;  (primitive #<subr (+ :rest args)>)
;  (primitive #<subr (- arg1 :rest args)>)
;  (primitive #<subr (* :rest args)>)
;  (primitive #<subr (= arg0 arg1 :rest args :optarray oarg)>)
; )
;
;; 4.11のenv
; 束縛のリスト
; (
;  (car primitive #<subr (car obj)>)
;  (false . #f)
;  (true . #t)
;  (cdr primitive #<subr (cdr obj)>)
;  (cons primitive #<subr (cons obj1 obj2)>)
;  (null? primitive #<subr (null? obj)>)
;  (+ primitive #<subr (+ :rest args)>)
;  (- primitive #<subr (- arg1 :rest args)>)
;  (* primitive #<subr (* :rest args)>)
;  (= primitive #<subr (= arg0 arg1 :rest args :optarray oarg)>)
; )

; ex-4.12
; (define (scan var vars vals)
;   (cond ((null? vars) '())
;         ((eq? var (car vars)) vals)
;         (else (scan var (cdr vars) (cdr vals)))))
;
; (define (lookup-variable-value var env)
;   (define (env-loop env)
;     (if (eq? env the-empty-environment)
;       (error "Unbound variable" var)
;       (let ((frame (first-frame env)))
;         (let ((res (scan var (frame-variables frame) (frame-values frame))))
;           (print "res: " res)
;           (if (null? res)
;             (env-loop (enclosing-environment env))
;             (car res))))))
;   (env-loop env))
;
; (define (set-variable-value! var val env)
;   (define (env-loop env)
;     (if (eq? env the-empty-environment)
;       (error "Unbound variable -- SET!" var)
;       (let ((frame (first-frame env)))
;         (let ((res (scan var (frame-variables frame) (frame-values frame))))
;           (print "res2: " res)
;           (if (null? res)
;             (env-loop (enclosing-environment env))
;             (set-car! res val))))))
;   (env-loop env))
;
; (define (define-variable! var val env)
;   (let ((frame (first-frame env)))
;     (let ((res (scan var (frame-variables frame) (frame-values frame))))
;       (print "res3: " res)
;       (if (null? res)
;         (add-binding-to-frame! var val frame)
;         (set-car! vals val)))))

; ex-4.13
; (define (unbind? exp) (tagged-list? exp 'unbind!))
; (define (unbinding-varialbe exp) (cadr exp))
; (define (eval-unbinding exp env)
;     (unbind-variable! (unbinding-varialbe exp) env)
;       'ok)
;
; これだとvalsが消せない(indexとれないの...)
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
;
; (define (unbind-variable! var env)
;   (let ((frame (first-frame env)))
;     (define (scan vars vals)
;       (cond ((null? vars)
;              (error "Unbound variabl --UNBIND-VARIABLE:" var))
;             ((eq? var (car vars))
;              (set-car! vars (cadr vars))
;              (set-cdr! vars (cddr vars))
;              (set-car! vals (cadr vals))
;              (set-cdr! vals (cddr vals)))
;             (else (scan (cdr vars) (cdr vals)))))
;     (scan (frame-variables frame)
;           (frame-values frame))))

(driver-loop)

(define (scan-out-defines procedure-body)
  (let ((lets '())
        (sets '()))
    (define (scan body)
      (cond ((tagged-list? (car body) 'define)
             (set! lets (cons (list (cadar body) '*unassigned) lets))
             (set! sets (cons (cons 'set! (cdar body)) sets))
             (scan (cdr body)))
            (else body)))
    (display "scan-out entered") (newline)
    (define body (scan procedure-body))
    (display body) (newline)
    (cons 'let
          (cons (reverse lets)
                (append (reverse sets) body)))))


(define (tagged-list? exp tag)
  (if (pair? exp)
    (eq? (car exp) tag)
    false))

(define body
  '((define even?
      (lambda (n)
        (if (= n 0)
          true
          (odd? (- n 1)))))
    (define odd?
      (lambda (n)
        (if (= n 0)
          false
          (even? (- n 1)))))
    (even? x)))

(scan-out-defines body)

