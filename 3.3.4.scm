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

; ex-3.29
; A-|I|-C--|
;         |AND|-E-|I|-out
; B-|I|-D--|
;   1+inverter-delay
;          1+and-gate-delay
;                  1+inverter-delay
; 遅延時間はinverter-delay + inverter-delay + and-gate-delay
(define (or-gate a b output)
  (let ((c (make-wire))
        (d (make-wire))
        (e (make-wire)))
    (inverter a c)
    (inverter b d)
    (and-gate c d e)
    (inverter e output)
    'ok))

; ex-3.30
(define (ripple-carry-adder list-a list-b list-s c)
  (define (iter list-a list-b list-sum c-in)
    (if (not (null? list-a))
      (let ((c-out (make-wire)))
        (full-adder (car list-a) (car list-b) c-in (car list-sum) c-out)
        (iter (cdr list-a) (cdr list-b) (cdr list-sum) c-out))
      'ok))
  (iter list-a list-b list-sum c-out))

; ex-3.31
; after-delayを実行してthe-agendaに手続きを追加したいから。
; after-delayが実行されないのでthe-agendaに手続きが追加されず、propagateが回らない.

; ex-3.32
; 次第書きのアクションが最後に入ったものが最初に出る（LIFO）の場合は、”回線bに登録されたアクション”、”回線aに登録されたアクション” の順で実行される。
; この場合は出力回線 c に変化は起こらず 0 のままとなりシミュレーションが正常に実行されない。
