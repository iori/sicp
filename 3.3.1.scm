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
        ; (print "x: " x)
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

; ex-3.17
(define (make-count-pairs walks)
  (define (count-pairs x)
    (print "walks: " walks)
    (print "x: " x)
    (print "memq: " (memq x walks))
    (cond ((not (pair? x)) 0)
          ((memq x walks) 0)
          (else
            (set! walks (cons x walks))
            (print "after set! walks: " walks)
            (+ (count-pairs (car x))
               (count-pairs (cdr x))
               1))))
  count-pairs)

(define CP (make-count-pairs '()))

(define x (cons 'a (cons 'b (cons 'c '()))))
; (print (CP x))

(define x (cons 'd (cons 'a '())))
(set-car! x (cons 'b (cdr x)))
; (print (CP x))

(define x (cons 'a (cons 'b (cons 'c '()))))
(set-car! (cdr x) (cdr (cdr x)))
(set-car! x (cdr x))
; (print (CP x))

; ex-3.18
(define (circulate? items)
  (define walks '())
  (define (has-circulate? x)
    (if (memq x walks)
      #t
      (begin (set! walks (cons x walks)) #f)))
  (define (circulate?-iter i)
    (if (not (pair? i))
      #f
      (if (has-circulate? (car i))
        #t
        (circulate?-iter (cdr i)))))
  (circulate?-iter items))

(define z (make-cycle (list 'a 'b 'c)))

(print (circulate? z))
(print (circulate? (list 'a 'b 'c)))
(print (circulate? 'a))

; ex-3.19
; pass

; ex-3.20
(define (cons x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define (dispatch m)
    (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          ((eq? m 'set-car!) set-x!)
          ((eq? m 'set-cdr!) set-y!)
          (else (error "Undefined operation -- CONS" m))))
  dispatch)

(define (car z) (z 'car))
(define (cdr z) (z 'cdr))
(define (set-car! z new-value)
  ((z 'set-car!) new-value)
  z)
(define (set-cdr! z new-value)
  ((z 'set-cdr!) new-value)
  z)

; 上の手続きを使って一連の式の評価を示す環境の図を描け
(define x (cons 1 2))
(define z (cons x x))
(set-car! (cdr z) 17)
(car x)
; 17

;(define x (cons 1 2))
; car, cdr, set-car!, set-cdrの図は省略
;
;             パラメタ:x y
;             本体: (define (set-x! ...
;             ↑
;             OO
;             ↓↑(cons)
;             _____________________________________________________________________
; 大域領域 -> |cons:
;             |car:
;             |cdr:
;             |set-car!:
;             |set-cdr!:
;             |x:
;             ---------------------------------------------------------------------
;             ↓(x)                    ↑
;             OO---------------------→E1->|x:1      |
;                                         |y:2      |
;             ↓                           |set-x!:  |←→OO→パラメタ:v, 本体:(set! x v)
;             パラメタ:x,y                |set-y!:  |←→OO→パラメタ:v, 本体:(set! y v)
;             本体: (cond ((eq? ...       |dispatch:|←→OO→パラメタ:m, 本体:(cond ...
;
; (define z (cons x x))
;             _____________________________________________________________________
; 大域領域 -> |cons:
;             |car:
;             |cdr:
;             |set-car!:
;             |set-cdr!:
;             |x:
;             |z:
;             ---------------------------------------------------------------------
;             ↓(x)                    ↑
;             OO---------------------→E1->|x:1      |
;                                         |y:2      |
;             ↓                           |set-x!:  |←→OO→パラメタ:v, 本体:(set! x v)
;             パラメタ:x,y                |set-y!:  |←→OO→パラメタ:v, 本体:(set! y v)
;             本体: (cond ((eq? ...       |dispatch:|←→OO→パラメタ:m, 本体:(cond ...
;
;             ↓(大域のz)              ↑(大域)
;             OO---------------------→E2->|x:x      |
;                                         |y:y      |
;             ↓                           |set-x!:  |←→OO→パラメタ:v, 本体:(set! x v)
;             パラメタ:x,y                |set-y!:  |←→OO→パラメタ:v, 本体:(set! y v)
;             本体: (cond ((eq? ...       |dispatch:|←→OO→パラメタ:m, 本体:(cond ...
;
; (set-car! (cdr z) 17)
;             _____________________________________________________________________
; 大域領域 -> |cons:
;             |car:
;             |cdr:
;             |set-car!:
;             |set-cdr!:
;             |x:
;             |z:
;             ---------------------------------------------------------------------
;             ↓(x)                    ↑
;             OO---------------------→E1->|x:1      |←E4→|m: 'set-car!|(dispatch)
;                                         |y:2      |
;             ↓                           |set-x!:  |←E5→|v: 17|(set-x!)
;             パラメタ:x,y                |set-y!:  |
;             本体: (cond ((eq? ...       |dispatch:|
;
;             ↓(大域のz)              ↑(大域)
;             OO---------------------→E2->|x:x      |
;                                         |y:x      |
;             ↓                           |set-x!:  |←→OO→パラメタ:v, 本体:(set! x v)
;             パラメタ:x,y                |set-y!:  |←→OO→パラメタ:v, 本体:(set! y v)
;             本体: (cond ((eq? ...       |dispatch:|←→OO→パラメタ:m, 本体:(cond ...
;
;                                      ↑(大域)
;                                     E3→|z:z          |
;                                        |new-value: 17|
;
; (car x)
;             _____________________________________________________________________
; 大域領域 -> |cons:
;             |car:
;             |cdr:
;             |set-car!:
;             |set-cdr!:
;             |x:
;             |z:
;             ---------------------------------------------------------------------
;             ↓(x)                    ↑
;             OO---------------------→E1->|x:1      |←E4→|m: 'set-car!|(dispatch)
;                                         |y:2      |
;             ↓                           |set-x!:  |←E5→|v: 17|(set-x!)
;             パラメタ:x,y                |set-y!:  |
;             本体: (cond ((eq? ...       |dispatch:|←E6→|m: 'car|(dispatch)
;
;             ↓(大域のz)              ↑(大域)
;             OO---------------------→E2->|x:x      |
;                                         |y:x      |
;             ↓                           |set-x!:  |←→OO→パラメタ:v, 本体:(set! x v)
;             パラメタ:x,y                |set-y!:  |←→OO→パラメタ:v, 本体:(set! y v)
;             本体: (cond ((eq? ...       |dispatch:|←→OO→パラメタ:m, 本体:(cond ...
;
;                                      ↑(大域)
;                                     E3→|z:z          |
;                                        |new-value: 17|
