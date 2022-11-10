; 3.1.1 局所状態変数

; withdraw以外のどこからでもbalanceにアクセス出来る.
; withdrawからだけアクセス出来るようにしたい.
(define balance 100)
(define (withdraw amount)
  (if (>= balance amount)
    ; beginは順次評価していって最後の式を返す
    ; set!は破壊的な再代入
    (begin (set! balance (- balance amount))
           balance)
    "Insufficient funds"))

; gosh$ (withdraw 25)
; 75
; gosh$ (withdraw 25)
; 50
; gosh$ (withdraw 100)
; "Insufficient funds"

; letを使って初期値100に束縛した局所変数balanceを持つ「環境」を作る.
; これはwithdrawとまったく一緒に振る舞うが、balanceにはnew-withdrawしかアクセス出来ない.
;
; set!と局所変数を組み合わせるのは一般的なプログラム技法だが、困ったことに重大な問題を惹き起こす。
; はじめに手続きを説明した時、手続き作用の意味の解釈の用意として、評価の置き換えモデルを説明した(1.1.5)
; 手続きの作用とは仮パラメタをその値で取替、手続きの本体を評価すること。
; 問題は言語に代入を取り入れると置き換えは最早手続き作用の適切なモデルにならない(なぜそうかは3.1.3節で説明する)。
;
; 上記で言いたいのはつまり「再代入という破壊的な操作を取り入れると、引数に対して関数の本体である手続きを作用させる時に問題が生じる。どんな問題が生じるかは後で説明する。取り敢えず駄目なんだよ」.
(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
    "Insufficient funds"))))

; gosh$ (new-withdraw 25)
; 75
; gosh$ (new-withdraw 25)
; 50
; gosh$ (new-withdraw 100)
; "Insufficient funds"

; balanceは局所変数として状態維持される(違和感ある)
(define (make-withdraw balance)
  (lambda (amount)
      (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))

(define W1 (make-withdraw 100))
(define W2 (make-withdraw 100))

; gosh$ (W1 50)
; 50
; gosh$ (W2 70)
; 30
; gosh$ (W2 40)
; "Insufficient funds"
; gosh$ (W1 40)
; 10

; balanceは局所変数として状態維持される(違和感ある(2回目))
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))

  (define (deposiot amount)
    (set! balance (+ balance amount))
    balance)

  ; メッセージパッシング流のプログラミング
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposiot)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)

; gosh$ (define acc (make-account 100))
; acc
; gosh$ ((acc 'withdraw) 50)
; 50
; gosh$ ((acc 'withdraw) 60)
; "Insufficient funds"
; gosh$ ((acc 'deposit) 40)
; 90
; gosh$ ((acc 'withdraw) 60)
; 30
; gosh$ (define acc2 (make-account 100))
; acc2
; gosh$ ((acc2 'withdraw) 10)
; 90
