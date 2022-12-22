; p.60, 1.2.6
(define (divides? a b)
  (= (remainder b a) 0))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))


(define (smallest-divisor n)
  (find-divisor n 2))

(define (prime? n)
  (= n (smallest-divisor n)))

; 3.5
(define the-empty-stream ())
(define (stream-null? stream) (null? stream))

(define (force delayed-object) (delayed-object))
(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (memo-proc proc)
  (let ((already-run? #f) (result #f))
    (lambda ()
      (if (not already-run?)
        (begin (set! result (proc))
               (set! already-run? #t)
               result)
        result))))
(define (delay exp) (memo-proc (lambda () exp)))

(define (cons-stream a b)
  (cons a (delay b)))

(define (stream-enumerate-interval low high)
  (if (> low high)
    the-empty-stream
    (cons-stream
      low
      (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else
          (stream-filter pred (stream-cdr stream)))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else
          (stream-filter pred (stream-cdr stream)))))

; SICPの通りの実装だとdelayが遅延評価されない(10000 ~ 1000000のリストが出来る)
; http://community.schemewiki.org/?sicp-ex-3.51
;(print (stream-car (stream-cdr (stream-filter prime? (stream-enumerate-interval 10000 1000000)))))

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
; 遅延評価されないので結果は以下
; gosh$ (define x (stream-map show (stream-enumerate-interval 0 10)))
;
; 0
; 1
; 2
; 3
; 4
; 5
; 6
; 7
; 8
; 9
; 10x
; gosh$ (stream-ref x 5)
; 5
; gosh$ (stream-ref x 7)
; 7

(define (display-line x)
  (display x)
  (newline))

(define (show x)
  (display-line x)
  x)

(define x (stream-map show (stream-enumerate-interval 0 10)))

(define (stream-ref s n)
  (if (= n 0)
    (stream-car s)
    (stream-ref (stream-cdr s) (- n 1))))
(stream-ref x 5)
(stream-ref x 7)

; ex-3.52
(define (stream-for-each proc s)
  (if (stream-null? s)
    'done
    (begin (proc (stream-car s))
           (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(print "********************************************************************* memo")

(define sum 0)
(define (accum x)
  (set! sum (+ x sum))
  sum)

(define seq (stream-map accum (stream-enumerate-interval 1 20)))
; gosh$ (display-stream seq)
; 1
; 3
; 6
; 10
; 15
; 21
; 28
; 36
; 45
; 55
; 66
; 78
; 91
; 105
; 120
; 136
; 153
; 171
; 190
; 210

; seqの偶数だけ
(define y (stream-filter even? seq))
; seqの5で割り切れる数だけ
(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq))
; yの8番目(0start)
(print "(stream-ref y 7): " (stream-ref y 7))
(display-stream z)

(print "********************************************************************* no memo")

(define (delay exp) (lambda () exp))

(define sum2 0)
(define (accum2 x)
  (set! sum2 (+ x sum2))
  sum2)

(define seq2 (stream-map accum2 (stream-enumerate-interval 1 20)))
(define y2 (stream-filter even? seq2))
(define z2 (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq2))
(stream-ref y2 7)
(print "(stream-ref y2 7): " (stream-ref y2 7))
(display-stream z2)
