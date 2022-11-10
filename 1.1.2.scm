(define size 2)
(print (* 5 size))

(define pi 3.14159)
(define radius 10)
(print
  (* pi (* radius radius))
)

(define circumference (* 2 pi radius))
(print circumference)

(define (square x) (* x x))
(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))

(print (/ (+ 5 4 (- 2 (- 3 (+ 6 4/5)))) (* 3 (- 6 2) (- 2 7))))

; ex-1.3
(define (hoge a b c)
  (if (> a b)
    (if (> b c)
      (sum-of-squares a b)
      (sum-of-squares a c)
    )
    (if (> a c)
      (sum-of-squares a b)
      (sum-of-squares b c)
    )))
(print (hoge 1 2 3))

; ex-1.4
#|
(define (p) (p))
(define (test x y)
  (if (= x 0)
    0
    y))

(p)でpを評価しようとして無限ループ
(print (test 0 (p)))
|#

; abs 絶対値
(define (abs x)
  (if (< x 0)
    (- x)
    x))

; 前もって決めた値`0.001`より小さくなるように改善する
(define (good-enough? guess x)
  ;(print "------------")
  ;(print (abs (- (square guess) x)))
  (< (abs (- (square guess) x)) 0.001))

(define (average x y)
  (/ (+ x y) 2))

; improve 改善
(define (improve guess x)
  (average guess (/ x guess)))

; guess 推測
(define (sqrt-iter guess x)
  (print "------------")
  (print guess)
  (print (improve guess x))
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x)
               x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

; ex-1.6
; gaucheだから無限ループに陥らないっぽいが、
; 本来condはpredicateの真偽に関わらず、then,else両方評価するので無限ループに陥る
#|
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (new-sqrt-iter guess x)
  (new-if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x)
               x)))

(define (new-sqrt x)
  (new-sqrt-iter 1.0 x))
|#

; ex-1.7
; 大きい値だと0.001未満に到達する前に同じ値から動かなくなってループする
; gosh$ (sqrt 10000000000000)
; 0.001953125 good-enough?のabsをprintしたもの、ずっとこの値が出力されて無限ループ
; goshの標準実装のsqrtと比べると、小さい値だと誤差が生じてくる。
;
; $ gosh -l ./1.1.2.scm
; gosh$ (sqrt 0.001)
; 0.04124542607499115
;
; $ gohs
; gosh$ (sqrt 0.001)
; 0.03162277660168379


; 大きい数字には有効だが小さい数字には無効
(define (new-sqrt-iter guess x)
  (print "------------")
  (print guess)
  (print (improve guess x))
  ; abs < 0.1
  (if (< (abs (- guess x)) 1)
    guess
    (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x))))

(define (new-sqrt x)
  (sqrt-iter 1.0 x))


; ex-1.8
; guressがy
(define (cube-root-good-enough? previous-guess guess)
    (< (abs (/ (- guess previous-guess) guess)) 0.00000000001))

(define (cube-root-improve guess x)
  (/ (+ (/ x (* guess guess)) (* guess 2)) 3))

(define (cube-root-iter guess x)
  (if (cube-root-good-enough? (cube-root-improve guess x) guess)
    guess
    (cube-root-iter (cube-root-improve guess x) x)))

(define (cube-root x)
  (cube-root-iter 1.0 x))


; ex-1.11
; n<3に対してf(n)=n, n=>3に対してf(n)=f(n-1)+2f(n-2)+3f(n-3)
; 再帰
(define (re-f n)
  (if(< n 3) n
    (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))

; 反復
(define (p-f n)
  (define (f-iter a b c count)
    (cond ((< n 3) n)
          ((<= count 0) a)
          (else
            (f-iter (+ a (* 2 b) (* 3 c)) a b (- count 1)))))
  (f-iter 2 1 0 (- n 2)))

(define (f n)
  (define (f-i a b c count)
    (cond ((< n 3) n)
          ((<= count 0) a)
          (else
            ;    a2 b1 c0
            ;    (+ 2 (* 2 1) (* 3 0)) = 4
            ;    a4 b2 c1
            ;    (+ 4 (* 2 2) (* 3 1)) = 11
            (f-i (+ a (* 2 b) (* 3 c)) a b (- count 1))
          )))
  (f-i 2 1 0 (- n 2)))

(define (fn1 n)
  (define (fn-iter a b c i)
    (if (= n i)
      (+ (* 3 a) (* 2 b) c)
      (fn-iter b c (+ (* 3 a) (* 2 b) c) (+ i 1))))
  (if (< n 3)
    n
    (fn-iter 0 1 2 3)))

; ex1.12
(define (pascal row col)
  (cond ((= row 1) 1)
        ((= row 2) 1)
        ((= col 1) 1)
        ((= row col) 1)
        (else
          (+ (pascal (- row 1) col) (pascal (- row 1) (- col 1))))))

; ex1.13 数学の証明問題でプログラム書く訳じゃないのでパス

; ex2.1
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

(define (make-rat n d)
  (let ((g (gcd (abs n) (abs d))))
    (let ((g-n (/ n g))
          (g-d (/ d g)))
      (if (< d 0)
        (cons (- g-n) (- g-d))
        (cons g-n g-d)))))



(define x (cons 1 2))
(define y (cons 3 4))
(define z (cons x y))

#|
(define (cons x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1 -- CONS" m))))
    dispatch)

(define (car z) (z 0))
(define (cdr z) (z 1))
|#

; ex-2.4
#|
(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))
|#



; ex-2.17
(define (last-pair l)
  (if (null? (cdr l))
    (car l)
    (last-pair (cdr l))))

; ex-2.18
(define (reverse l)
  (define (reverse-iter l r)
    (if (null? l)
      r
      (reverse-iter (cdr l) (cons (car l) r))))
  (reverse-iter l ()))

; ex-2.20
(define (append l1 l2)
  (if (null? l1)
    l2
    (cons (car l1) (append (cdr l1) l2))))

(define (same-parity n . l)
  (define (same-parity-iter n l s)
    (if (null? l)
      s
      (if (even? n)
        (if (even? (car l))
          (same-parity-iter n (cdr l) (append s (list (car l))))
          (same-parity-iter n (cdr l) s))
        (if (odd? (car l))
          (same-parity-iter n (cdr l) (append s (list (car l))))
          (same-parity-iter n (cdr l) s)))))
  (same-parity-iter n l ()))


; ex-2.21
#|
(define (square-list items)
  (if (null? items)
    ()
    (cons (* (car items) (car items)) (square-list (cdr items)))))
|#

(define (square-list items)
  (map (lambda (x) (* x x)) items))

; ex-2.23
(define (for-each proc items)
  (proc (car items))
  (if (not (null? (cdr items)))
    (for-each proc (cdr items))))


; ex-2.27
;
(define x (list (list 1 2) (list 3 4)))

; ((1 2) (3 4))
;
; (reverse x)
; ((3 4) (1 2))
;
; (deep-reverse x)
; ((4 3) (2 1))
#|
(define (deep-reverse items)
  (cond ((not (pair? items)) items)
        (else
          (reverse (map deep-reverse items)))))

(define (deep-reverse tree)
  (cond ((null? tree)
         '())
        ((not (pair? tree))
         tree)
        (else
          (reverse (list (deep-reverse (car tree))
                         (deep-reverse (cadr tree)))))))

|#
(define (deep-reverse l)
  (define (reverse-iter l r)
    (if (null? l)
      r
      (reverse-iter (cdr l) (cons (car l) r))))

  (print l)
  (if (not (pair? l))
    l
    (reverse-iter (map deep-reverse l) ())))


; ex-2.28
; (fringer (list (list 1 2) (list 3 4)))
; (1 2 3 4)
(define (fringer l)
  (if (null? l)
    ()
    (if (not (pair? l))
      (list l)
      (append (fringer (car l)) (fringer (cdr l))))))

