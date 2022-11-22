; ex-3.12
(define (append x y)
  (if (null? x)
    y
    (cons (car x) (append (cdr x) y))))

(define (append! x y)
  (set-cdr! (last-pair x) y))

(define (last-pair x)
  (if (null? (cdr x))
    x
    (last-pair (cdr x))))
;
; (define x (list 'a 'b))
; (define y (list 'c 'd))
; (define z (append x y))
;
; z
; (a b c d)
;
; (cdr x)
; (応答)
; (b)
; z->OO->OO--->OO
;    ↓   ↓　　 ↓
; x->OO->OO y->OO->OO
;    ↓   ↓     ↓   ↓
;    a   b     c   d
; 上記図のcdrなので(b)
;
; (define w (append! x y))
;
; w
; (a b c d)
;
; (cdr x)
; (応答)
; (b c d)
;    w       y
;    ↓       ↓
; x->OO->OO->OO->OO
;    ↓   ↓   ↓   ↓
;    a   b   c   d
; 上記図のcdrなので(b c d)

; ex-3.13
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

; (define z (make-cycle (list 'a 'b 'c)))
; gosh$ (define z (make-cycle (list 'a 'b 'c)))
;z
;gosh$ z
;#0=(a b c . #0#)
;

;   __________
;   ↓        |
; z→OO->OO->OO
;   ↓   ↓   ↓
;   a   b   c

; ex-3.14
(define (mystery x)
  (define (loop x y)
    (if (null? x)
      y
      (let ((temp (cdr x)))
        (set-cdr! x y)
        (print "x: " x)
        (loop temp x))))
  (loop x '()))

; 一般にmysteryが何をするか説明せよ.
; 1-loop(x: ('a 'b 'c 'd), y: ())
; temp: ('b 'c 'd)
; x: (a)
; (loop ('b 'c 'd) (a))
;
; 2-loop(x: ('b 'c 'd), y: (a))
; temp: ('c 'd)
; x: (b a)
; (loop (c d) (b a))
;
; 3-loop(x: (c d), y: (b a))
; temp: (d)
; x: (c b a)
; (loop (d) (c b a))
;
; 4-loop(x: (d), y: (c b a))
; temp: ()
; x: (d c b a)
; (loop () (d c b a))
;
; 5-loop
; return y(d c b a)
;
; listをreverseする、元のlistは壊れる.

(define v (list 'a 'b 'c 'd))
; vが束縛されているリストを表現する箱とポインタを描け.
;
; v→OO→OO→OO→OO
;   ↓  ↓  ↓  ↓
;   a  b  c  d

(define w (mystery v))
; この式を評価した後の構造vとwを示すポインタと箱を描け.
;
;            v
;            ↓
; w→OO→OO→OO→OO
;   ↓  ↓  ↓  ↓
;   d  c  b  a

; gosh$ (define v (list 'a 'b 'c 'd))
; v
; gosh$ (define w (mystery v))
; w
; gosh$ v
; (a)
; gosh$ w
; (d c b a)

