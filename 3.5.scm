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

(print
  (stream-car (stream-cdr (stream-filter prime? (stream-enumerate-interval 10000 100000))))
)

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
    (<??>
      (apply proc (map stream-car argstreams))
      (apply stream-map
             (cons proc (map cdr argstreams))))))
