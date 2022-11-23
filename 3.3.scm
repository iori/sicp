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


(define x (list 'a 'b))
(define z1 (cons x x))
(define z2 (cons (list 'a 'b) (list 'a 'b)))

(define (set-to-wow! x)
  (set-car! (car x) 'wow)
  x)

; z1
; ((a b) a b)
;
; (set-to-wow! z1)
; ((wow b) wow b)
;
; z2
; ((a b) a b)
;
; (set-to-wow! z2)
; ((wow b) a b)

; ex-3.15
; z1
; z1→OO
;    ↓↓
;  x→OO→OO
;    ↓  ↓
;    a  b
;
; (set-to-wow! z1)
; z1→OO
;    ↓↓
;  x→OO→OO
;    ↓  ↓
; a wow b
;
; z2
; z2→OO→OO→OO
;    |  ↓  ↓
;    |  a  b
;    |  ↑  ↑
;    -→ OO→OO
;
; (set-to-wow! z2)
; z2→OO→OO→OO
;    |  ↓  ↓
;    |  a  b
;    |     ↑
;    -→ OO→OO
;       ↓
;      wow

; ex-3.16
(define (count-pairs x)
  (if (not (pair? x))
    0
    (+ (count-pairs (car x))
       (count-pairs (cdr x))
       1)))

(define x (cons 'd (cons 'a '())))
; x→OO→OO
;   ↓  ↓
;   d  a
(set-car! x (cons 'b (cdr x)))
;   b
;   ↑
;   OO--
;   ↑  ↓
; x→OO→OO
;      ↓
;      a
(count-pairs x)
; 4
x
; ((b a) a)

(define x (cons 'a (cons 'b (cons 'c '()))))
; x→OO→OO→OO
;   ↓  ↓  ↓
;   a  b  c
(set-car! (cdr x) (cdr (cdr x)))
;       ___
;      |  ↓
; x→OO→OO→OO
;   ↓     ↓
;   a  b  c
(set-car! x (cdr x))
;       ___
;      |  ↓
; x→OO→OO→OO
;   |--↑  ↓
;         c
;   a  b
;
; ※ a,bはポインタから外れている(この図だけ見にくいので注釈)
(count-pairs x)
; 7
x
; (((c) c) (c) c)

(define x (cons 'a (cons 'b (cons 'c '()))))
(make-cycle x)
; #0=(a b c . #0#)
;
;   _______
;   ↓      |
; x→OO→OO→OO
;   ↓  ↓  ↓
;   a  b  c
; (count-pairs x)
; 無限ループ
