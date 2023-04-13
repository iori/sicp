; ex-5.1
(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

; |a| <-x-- |b| -> =
;  |---↓ ↓--|↑     ↑
;      rem   x     0
;       |    |
;       x    |
;       ↓    |
;      |t|----

(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
      product
      (iter (* counter product)
            (+ counter 1))))
  (iter 1 1))

;    1             1
;    x             x
;    ↓             ↓
; |product|     |counter|  -> >
;  | ↑           |  ↑ |       ↑
;  | x           |  x |       n
;  | |           |  | |
;  → * ←---------   + ←
;                   ↑
;                   1

; ex-5.2
(controller
  test-b
    (test (op =) (reg b) (const 0))
    (branch (label gcd-done))
    (assign t (op rem) (reg a) (reg b))
    (assign a (reg b))
    (assign b (reg t))
    (goto (label test-b))
  gcd-done)

; 自分
(controller
  iter
    test-counter
      (test (op >) (reg counter) (reg n))
      (branch (label iter-done))
      (assign product (op *) (reg counter) (reg product))
      (assign counter (op +) (reg counter) (const 1))
      (goto (label iter))
    iter-done
  (assign product (const 1))
  (assign counter (const 1))
  (goto iter))

; 正解
(controller
  initialize
    (assign product (const 1))
    (assign counter (const 1))
  test-counter
    (test (op >) (reg counter) (reg n))
    (branch (label factorial-done))
    (assign product (op *) (reg product) (reg counter))
    (assign counter (op +) (reg counter) (const 1))
    (goto (label test-counter))
  factorial-done)


; ex-5.3
(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
      guess
      (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

(controller
  initialize
    (assign x (op read))
    (assign guess (const 1.0))
  test-guess
    (test (op good-enough?) (reg guess))
    (branch (label sqrt-iter-done))
    (assign t guess)
    (assign guess ((op improve) (reg t)))
    (goto (label test-guess))
  sqrt-iter-done)

; ex-5.4
; a. 再帰的べき乗
(define (expt b n)
  (if (= n 0)
    1
    (* b (expt b (- n 1)))))

(controller
    (assign continue (label expt-done)) ; 最終帰り番地設定
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
  expt-done)

; b. 反復的べき乗
(define (expt b n)
  (define (expt-iter counter product)
    (if (= counter 0)
      product
      (expt-iter (- counter 1) (* b product))))
  (expt-iter n 1))

(controller
  initialize
    (assign b (op read))
    (assign n (op read))
    (assign counter (reg n))
    (assign product (const 1))
  expt-loop
    (test (op =) (reg counter) (const 0))
    (branch (label expt-done))
    (assign counter (op -) (reg counter) (const 1))
    (assign product (op *) (reg b) (reg product))
    (goto (label expt-loop))
  expt-done)

; ex-5.5
; 1. n = 2
(save continue) ;; stack: ((label fact-done))
(save n) ;;  stack (2 (label fact-done))
(assign n (op -) (reg n) (const 1)) ;; n =1
(assign continue (label after-fact)) ;; continue: (label after-fact)

; 2. n = 3
; どっか間違えているっぽい(2なのに0+1になっちゃう)
(assign continue (label fib-done)) ; continue-stack: (fib-done))
fib-loop
  (test (op <) (reg n) (const 2)) ; n = 3, false
  (save continue) ; continue-stack: (fib-done)
  (assign continue (label afterfib-n-1)) ; continue = afterfib-n-1
  (save n) ; n-stack: (3)
  (assign n (op -) (reg n) (const 1)) ; n = 2
  (goto (label fib-loop))
fib-loop
  (test (op <) (reg n) (const 2)) ; n = 2, false
  (save continue) ; continue-stack: (afterfib-n-1 fib-done)
  (assign continue (label afterfib-n-1)) ; continue = (afterfib-n-1)
  (save n) ; n-stack: (2 3)
  (assign n (op -) (reg n) (const 1)) ; n = 1
  (goto (label fib-loop))
fib-loop
  (test (op <) (reg n) (const 2)) ; n = 1, true
  (branch (label immediate-answer)
immediate-answer
  (assign val (reg n)) ; val = 1
  (goto (reg continue)) ; afterfib-n-1
afterfib-n-1
  (restore n) ; n = 2, n-stack: (3)
  (restore continue) ; continue = afterfib-n-1, continue-stack: (fib-done)
  (assign n (op -) (reg n) (const 2)) ; n = 0
  (save continue) ; continue-stack: (fib-done)
  (assign continue (label afterfib-n-2)) ; continue = afterfib-n-2
  (save val) ; val-stack: (1)
  (goto (label fib-loop))
fib-loop
  (test (op <) (reg n) (const 2)) ; n = 0, true
  (branch (label immediate-answer)
immediate-answer
  (assign val (reg n)) ; val = 0
  (goto (reg continue)) ; afterfib-n-2
afterfib-n-2
  (assign n (reg val)) ; n = 0
  (restore val) ; val = 1, val-stack: ()
  (restore continue) ; continue = fib-done, continue-stack: ()
  (assign val (op +) (reg val) (reg n)) ; val = 0 + 1
  (goto (reg continue))
fib-done

; ex-5.6
; afterfib-n-1のsaveとcontinue
