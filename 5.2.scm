(add-load-path "./packages/" :relative)
(load "5.2.register-simulator.scm")

; p.307
(define gcd-machine
  (make-machine
    '(a b t)
    (list (list 'rem remainder) (list '= =))
    '(test-b
       (test (op =) (reg b) (const 0))
       (branch (label gcd-done))
       (assign t (op rem) (reg a) (reg b))
       (assign a (reg b))
       (assign b (reg t))
       (goto (label test-b))
       gcd-done)))

; gcd-machine実行結果
; gosh$ (set-register-contents! gcd-machine 'a 206)
; done
; gosh$ (set-register-contents! gcd-machine 'b 40)
; done
; gosh$ (start gcd-machine)
; done
; gosh$ (get-register-contents gcd-machine 'a)
; 2

; ex-5.7
(define expt-machine
  (make-machine
    '(continue n b val)
    (list (list '= =) (list '* *) (list '- -))
    '(
      initialize
       (assign continue (label expt-done))
      expt-loop
        (test (op =) (reg n) (const 0))
        (branch (label base-case))
        (save continue)
        (save n)
        (assign n (op -) (reg n) (const 1))
        (assign continue (label after-expt))
        (goto (label expt-loop))
      after-expt
        (restore n)
        (restore continue)
        (assign val (op *) (reg b) (reg val))
        (goto (reg continue))
      base-case
        (assign val (const 1))
        (goto (reg continue))
      expt-done)))

; gosh$ (set-register-contents! expt-machine 'b 3)
; done
; gosh$ (set-register-contents! expt-machine 'n 4)
; done
; gosh$ (start expt-machine)
; done
; gosh$ (get-register-contents expt-machine 'val)
; 81

; ex-5.8

(define test-machine
  (make-machine
    '(a)
    '()
    '(start
       (goto (label here))
      here
       (assign a (const 3))
       (goto (label there))
      here
       (assign a (const 4))
       (goto (label there))
      there)))

; gosh$ (set-register-contents! test-machine 'a 0)
; done
; gosh$ (start test-machine)
; done
; gosh$ (get-register-contents test-machine 'a)
; 3

; ex-5.9
; p.328
; > expは評価すべき式を保持し、
;
; diff --git a/packages/5.2.register-simulator.scm b/packages/5.2.register-simulator.scm
; index f25274d..5bc1649 100644
; --- a/packages/5.2.register-simulator.scm
; +++ b/packages/5.2.register-simulator.scm
; @@ -335,7 +335,9 @@
;    (let ((op (lookup-prim (operation-exp-op exp) operations))
;          (aprocs
;            (map (lambda (e)
; -                 (make-primitive-exp e machine labels))
; +                 (if (label-exp? e)
; +                   (error "Operations can be used only with registers and constants -- ASSEMBLE" e)
; +                   (make-primitive-exp e machine labels)):
;                 (operation-exp-operands exp))))
;      (lambda ()
;        (apply op (map (lambda (p) (p)) aprocs)))))

; ex-5.10
; a. (restore y)はどのレジスタから値が来たかに関係なく、スタックに退避した最後の値をyに置く.
;
;afterfib-n-2
;   (assign n (reg val))
;   (restore val)
;   ...
; ↓
; afterfib-n-2
;   (restore n)
;   ...
;
; -------------------------
;
; afterfib-n-1
;   ...
;   (save val)
;   (goto (label fib-loop))
; fib-loop
;   (test ...)
;   (branch (label immediate-answer)
; immediate-answer
;   (assign val (reg n))
;   (goto (reg continue))
; afterfib-n-2
;   (assign n (reg val))
;   (restore val)
;
; 最後にsaveしたのがafterfib-n-1のval
; だから(restore n)だけで、valからnに値が束縛されて、且つvalがrestoreされた事になる
;
; b,cはskip

; ex-5.20
; 以下で作られたリスト構造の箱とポインタ表記およびfreeポインタは最初にp1であるとして、図5.14のようなメモリーベクタ表現を書け.
; freeの最後の値は何か.どのポインタがx,yを表しているか.
(define x (cons 1 2))
(define y (list x x))

; (1 . 2) -> |・|・| → |・|/| 3
;           2 ||--------|
;             ↓↓
;            |・|・| → |2|
;           1 ↓
;            |1|


; 0  1 2  3
;| |n1|p1|p1|
;| |n2|p3|e0|

; 5.21-a
(define (count-leaves tree)
  (cond ((null? tree) 0)
        ((not (pair? tree)) 1)
        (else (+ (count-leaves (car tree))
                 (count-leaves (cdr tree))))))

(controller
    (assign continue (label count-leaves-done)) ; 最終番地
    (assign val (const 0))
  test
    ; ((null? tree) 0)
    (test (op null?) (reg tree))
    (branch (label null))
    ; ((not (pair? tree)) 1)
    (test (op (not (op pair?))) (reg tree))
    (branch (label not-pair))
    ; (else ...)
    ; (+ (count-leaves (car tree))
    (save continue)
    (assign continue (label count-leaves-with-car))
    (save tree)
    (assign tree (op car) (reg tree))
    (goto (label count-leaves-loop))

  null
    (assign val (const 0))
    (goto (reg continue))

  not-pair
    (assign val (const 1))
    (goto (reg continue))

  count-leaves-with-car
    (restore tree)
     (restore continue)
     ;; (count-leaves (cdr tree)) を実行するように設定
     (assign tree (op cdr) (reg tree))
     (save continue)
     (assign continue (label count-leaves-with-cdr))
     (save val) ;; (count-leaves (car tree)) を退避
     (goto (label count-leaves-loop))

  count-leaves-with-cdr
     (assign val-tmp (reg val))
     (restore val)
     (restore continue)
     (assign val (op +) (reg val) (reg val-tmp))
     (goto (reg continue))
  count-leaves-done)
