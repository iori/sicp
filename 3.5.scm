(add-load-path "./packages/" :relative)
(load "prime.scm")
(load "delay-memo.scm")

(print (stream-car (stream-cdr (stream-filter prime? (stream-enumerate-interval 10000 1000000)))))

; ex-3.50
; p.60のmap
; (define (map proc items)
;   (if (null? items)
;     ()
;     (cons (proc (car items))
;           (map proc (cdr items)))))
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
    the-empty-stream
    (cons-stream
      (apply proc (map stream-car argstreams))
      (apply stream-map
             (cons proc (map stream-cdr argstreams))))))

; ex-3.51
(print "***** ex-3.51")
(define (show x)
  (display-line x)
  x)

(define x (stream-map show (stream-enumerate-interval 0 10)))
; gosh$ (define x (stream-map show (stream-enumerate-interval 0 10)))
; 0
; x

; (stream-ref x 5)
; gosh$  (stream-ref x 5)
;
; 1
; 2
; 3
; 4
; 55

; (stream-ref x 7)
; gosh$ (stream-ref x 7)
;
; 6
; 77

; ex-3.52
(print "***** ex-3.52")
(print "********** memo")

(define sum 0)
(define (accum x)
  (set! sum (+ x sum))
  sum)

(print "sum " sum)
; 0

(define seq (stream-map accum (stream-enumerate-interval 1 20)))
(print "sum " sum)
; 1

; seqの偶数だけ
(define y (stream-filter even? seq))
(print "sum " sum)
; 6

; seqの5で割り切れる数だけ
(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq))
(print "sum " sum)
; 10

; yの8番目(0start)
(print "(stream-ref y 7): " (stream-ref y 7))
(print "sum " sum)
; 136

(display-stream z)
(newline)
(print "sum " sum)
; 210

(print "********** no memo")
(load "delay.scm")

(define sum2 0)
(define (accum2 x)
  (set! sum2 (+ x sum2))
  sum2)
(print "sum2: " sum2)
; 0

(define seq2 (stream-map accum2 (stream-enumerate-interval 1 20)))
(print "sum2: " sum2)
; 1

(define y2 (stream-filter even? seq2))
(print "sum2: " sum2)
; 6

(define z2 (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq2))
(print "sum2: " sum2)
; 15

(stream-ref y2 7)
(print "sum2: " sum2)
; 162

(display-stream z2)
(newline)
(print "sum2: " sum2)
; 362

(load "delay-memo.scm")
; ex-3.53
; 2,4,8,16,32

; ex-3.54
(define (add-streams s1 s2)
  (stream-map + s1 s2))
(define (mul-streams f1 f2)
  (stream-map * f1 f2))

(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))

(define factorials (cons-stream 1 (mul-streams factorials (add-streams ones integers))))

; gosh$ (stream-ref factorials 0)
; 1
; gosh$ (stream-ref factorials 1)
; 2
; gosh$ (stream-ref factorials 2)
; 6
; gosh$ (stream-ref factorials 3)
; 24
; gosh$ (stream-ref factorials 4)
; 120

; ex-3.55
(define (partial-sums s)
  (cons-stream (stream-car s)
               (add-streams
                 (stream-cdr s)
                 (partial-sums s))))

; gosh$ (stream-ref (partial-sums integers) 0)
; 1
; gosh$ (stream-ref (partial-sums integers) 1)
; 3
; gosh$ (stream-ref (partial-sums integers) 2)
; 6
; gosh$ (stream-ref (partial-sums integers) 3)
; 10
; gosh$ (stream-ref (partial-sums integers) 4)
; 15

; ex-3.56
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
          (let ((s1car (stream-car s1))
                (s2car (stream-car s2)))
            (cond ((< s1car s2car)
                   (cons-stream s1car (merge (stream-cdr s1) s2)))
                  ((> s1car s2car)
                   (cons-stream s2car (merge s1 (stream-cdr s2))))
                  (else
                    (cons-stream s1car
                                 (merge (stream-cdr s1)
                                        (stream-cdr s2)))))))))

(define S (cons-stream 1 (merge (scale-stream S 2)
                                (merge (scale-stream S 3)
                                       (scale-stream S 5)))))

; gosh$ (stream-head S 10)
; 1
; 2
; 3
; 4
; 5
; 6
; 8
; 9
; 10
; 12
; done

; ex-3.57
; memo: n-1
; メモ化されていないと計算を毎回行うので指数的に増えていく

; ex-3.58
(define (expand num den radix)
  (cons-stream
    (quotient (* num radix) den)
    (expand (remainder (* num radix) den) den radix)))

; gosh$ (stream-head (expand 1 7 10) 10)
; 1
; 4
; 2
; 8
; 5
; 7
; 1
; 4
; 2
; 8
; done
; gosh$ (stream-head (expand 3 8 10) 10)
; 3
; 7
; 5
; 0
; 0
; 0
; 0
; 0
; 0
; 0
; done

; ex-3.59 ~ 3.62
; skip

; ex-3.63
; (sqrt-stream x)が毎回実行されるので遅い.
; memo化されてなければ代わりはない.

; ex-3.64
(define (average x y)
  (/ (+ x y) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

(define (stream-limit st tolerance)
  (let ((s1 (stream-car st))
        (s2 (stream-car (stream-cdr st))))
    (if (< (abs (- s1 s2)) tolerance)
      s2
      (stream-limit (stream-cdr st) tolerance))))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

; gosh$ (sqrt 2 0.01)
; 1.4142156862745097

; ex-3.64
; skip
