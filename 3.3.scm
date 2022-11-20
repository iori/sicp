; ex-3.12
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
