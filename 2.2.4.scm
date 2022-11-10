; wave
;  線画を描く基本的なpainter
; beside
;  ２つのペインタをとり、第一のペインタ画像をフレームの左半分に描き、
;  第２のペインタ画像をフレームの右半分に描く新しい合成ペインタを作る
; below
;  ２つのペインタをとり、
;  第一のペインタ画像を第二のペインタ画像の下に描く新しい合成ペインタを作る
; flip-vert
;  ペインタを一つとり、その上下逆転の画像を描く
; flip-horiz
;  ペインタを一つとり、その左右逆転の画像を描く

(define wave2 (beside wave (flip-vert wave)))
; (define wave4 (below wave2 wave2))

; wave4のパターンを抽象化
#|
(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))
|#

; wave4のパターンを具体化(rogersじゃなくてwaveを渡している=具体的)
(define wave4 (flipped-pairs wave))

; 再帰的演算. ペインタを右の方へ分割,枝分かれさせるものである
#|
(define (right-split painter n)
  (if (= n 0)
    painter
    (let ((smaller (right-split painter (- n 1))))
      (beside painter (below smaller smaller)))))
|#

; 上へも枝分かれさせ、バランスの取れたパターンが作れる
(define (corner-split painter n)
  (if (= n 0)
    painter
    (let ((up (up-split painter (- n 1)))
          (right (right-split painter (- n 1))))
      (let ((top-left (beside up up))
            (bottom-right (below right right))
            (corner (corner-split painter (- n 1))))
        (beside (below painter top-left)
                (below bottom-right corner))))))

; 図2.9(きれいなやつ)
(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

; ex-2.44
#|
(define (up-split painter n)
  (if (= n 0)
    painter
    (let ((smaller (up-split painter (- n 1))))
      (below painter (beside smaller smaller)))))
|#
;;;;;;

; tl, tr, bl, br
; 上左コピー, 上右コピー, 下左コピー, 下右コピー
(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (flipped-pairs painter)
  (let ((combine4 (square-of-four identity flip-vert identity flip-vert)))
    (combine4 painter)))

; rotate180はex-2.50で出てくるらしい
(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

; ex-2.45
(define right-split (split beside below))
(define up-split (split below beside))

(define (split copy1 copy2)
  (if (= n 0)
    painter
    (let ((smaller (split painter (- n 1))))
      (copy1 painter (copy2 smaller smaller)))))

;;;;;;;;;

; v=(x,y)
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
      (origin-frame frame)
      (add-vect (scale-vect (xcor-vect v)
                            (edge1-frame frame))
                (scale-vect (ycor-vect v)
                            (edge2-frame frame))))))

; ((frame-coord-map a-frame) (make-vect 0 0))
; (origin-frame a-frame)
; ↑両者は同じベクタを返す

; ex-2.46
(define (make-vect x y) (cons x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2)) (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2)) (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v)) (* s (ycor-vect v))))

; ex-2.47
(define (make-frame origin edge1 edge2) (list origin edge1 edge2))
(define (origin-frame frame) (car frame))
(define (edge1-frame frame) (cadr frame))
(define (edge2-frame frame) (caddr frame))

(define (make-frame origin edge1 edge2) (cons origin (cons edge1 edge2)))
(define (origin-frame frame) (car frame))
(define (edge1-frame frame) (cadr frame))
(define (edge2-frame frame) (cddr frame))

; ex-2.48
(define (make-segment origin-to-start origin-to-end)
  (list origin-to-start origin-to-end))

(define (start-segment segment) (car segment))
(define (end-segment segment) (cadr segment))

; ex-2.49
(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
      (lambda (segment)
        (draw-line
          ((frame-coordmap frame) (start-segment segment))
          ((frame-coordmap frame) (end-segment segment))))
      (segment-list))))

; 実際に描画出来ないのでコピペしてふ〜んこうなんだーくらいの感じ
(define draw-frame-outline
  (let ((v0 (make-vect 0.0 0.0))
        (v1 (make-vect 1.0 0.0))
        (v2 (make-vect 1.0 1.0))
        (v3 (make-vect 0.0 1.0)))
    (segments->painter
      (list (make-segment v0 v1)
            (make-segment v1 v2)
            (make-segment v0 v3)
            (make-segment v3 v2)))))

(define draw-frame-cross
  (let ((v0 (make-vect 0 0))
        (v1 (make-vect 1 0))
        (v2 (make-vect 1 1))
        (v3 (make-vect 0 1)))
    (segments->painter
      (list (make-segment v0 v2)
            (make-segment v1 v3)))))

(define draw-frame-diamond
  (let ((v0 (make-vect 0.5 0.0))
        (v1 (make-vect 1.0 0.5))
        (v2 (make-vect 0.5 1.0))
        (v3 (make-vect 0.0 0.5)))
    (segments->painter
      (list (make-segment v0 v1)
            (make-segment v1 v2)
            (make-segment v2 v3)
            (make-segment v3 v0)))))

(define wave
  (segments->painter
    (list (make-segment (make-vect 0.35 0.85) (make-vect 0.40 1.00))
          (make-segment (make-vect 0.65 0.85) (make-vect 0.60 1.00))
          (make-segment (make-vect 0.35 0.85) (make-vect 0.40 0.65))
          (make-segment (make-vect 0.65 0.85) (make-vect 0.60 0.65))
          (make-segment (make-vect 0.60 0.65) (make-vect 0.75 0.65))
          (make-segment (make-vect 0.40 0.65) (make-vect 0.30 0.65))
          (make-segment (make-vect 0.75 0.65) (make-vect 1.00 0.35))
          (make-segment (make-vect 0.60 0.45) (make-vect 1.00 0.15))
          (make-segment (make-vect 0.60 0.45) (make-vect 0.75 0.00))
          (make-segment (make-vect 0.50 0.30) (make-vect 0.60 0.00))
          (make-segment (make-vect 0.30 0.65) (make-vect 0.15 0.60))
          (make-segment (make-vect 0.30 0.60) (make-vect 0.15 0.40))
          (make-segment (make-vect 0.15 0.60) (make-vect 0.00 0.85))
          (make-segment (make-vect 0.15 0.40) (make-vect 0.00 0.65))
          (make-segment (make-vect 0.30 0.60) (make-vect 0.35 0.50))
          (make-segment (make-vect 0.35 0.50) (make-vect 0.25 0.00))
          (make-segment (make-vect 0.50 0.30) (make-vect 0.40 0.00)))))

; ex-2.50
; r180とr270はコピペ.
; 実際に試せないとこの辺はやりにくなー
(define (flip-holiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

; ex-2.51
; below
;  ２つのペインタをとり、
;  第一のペインタ画像を第二のペインタ画像の下に描く新しい合成ペインタを作る
; これもコピペ.実際に(ry
  (define (below painter1 painter2)
    (let ((split-point (make-vect 0.0 0.5)))
      (let ((paint-bottom
              (transform-painter painter1
                                 (make-vect 0.0 0.0)
                                 (make-vect 1.0 0.0)
                                 split-point))
            (paint-top
              (transform-painter painter2
                                 split-point
                                 (make-vect 1.0 0.5)
                                 (make-vect 0.0 1.0))))
        (lambda (frame)
          (paint-bottom frame)
          (paint-top frame)))))

; ex-2.51
; これも実際に出力できないと厳しいかなぁ...
; ここでやりたい事は理解できた筈なのでパス
