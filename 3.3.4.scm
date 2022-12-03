; 3.3.4 ディジタル回路のシミュレーター

; インバーター: 入力を反転する(0->1, 1->0)
; アンドゲート: 2つの入力が共に1なら1. 1つでも0なら0.
; オアゲート:   2つの入力の内、1つでも1なら1. 両方0なら0.
; それぞれは遅延する

; ex-3.28
(define (logical-or s1 s2)
  (cond ((or (= s1 1) (= s2 1)) 1)
        ((and (= s1 0) (= s2 0)) 0)
        (else (error "Invalid signal" s1 s2))))

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
            (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)
