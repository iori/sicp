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
