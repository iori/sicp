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

; ex-3.1
(define (make-accumulator balance)
  (lambda (amount)
    (begin (set! balance (+ balance amount))
           balance)))

(define A (make-accumulator 5))
(define B (make-accumulator 5))
; (print (A 10))
; 15
; (print (A 10))
; 25
; (print (B 1))
; 6

; ex-3.2
; (define mfじゃなくてlambda使ってるけどこんな感じ
(define (make-monitored proc)
  (let ((counter 0))
    (lambda (x)
      (cond ((eq? x 'how-many-calls?) counter)
            ((eq? x 'reset-count) (set! counter 0))
            (else
              (begin (set! counter (+ counter 1))
                     (proc x)))))))

; 無理やりdispatchでやってみた
(define (make-monitored proc)
  (let ((counter 0))
    (define (do-proc x) (begin (set! counter (+ counter 1))
                               (proc x)))
    (define (return-counter x) counter)
    (define (reset-counter x) (set! counter 0))

    (define (dispatch m)
      (cond ((eq? m 'how-many-calls?) (return-counter m))
            ((eq? m 'reset-count) (reset-counter m))
            (else (do-proc m))))
    dispatch))

(define s (make-monitored sqrt))
; (print (s 100))
; 10
; (print (s 100))
; 10
; (print (s 'how-many-calls?))
; 2
; (print (s 'reset-count))
; 0
; (print (s 'how-many-calls?))
; 0

; ex-3.3
(define (make-account balance my-password)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))

  (define (deposiot amount)
    (set! balance (+ balance amount))
    balance)

  (define (dispatch password m)
    (if (eq? my-password password)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposiot)
            (else (error "Unknown request -- MAKE-ACCOUNT" m)))
      (error "Incorrect password")))
  dispatch)

; (define acc (make-account 100 'secret-password))
;
; (print ((acc 'secret-password 'withdraw) 40))
; 60
;
; (print ((acc 'some-other-password 'deposit) 50))
; "Incorrect password"

; ex-3.3
(define (make-account balance my-password)
  (let ((access-counter 0))
    (define (withdraw amount)
      (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))

    (define (deposiot amount)
      (set! balance (+ balance amount))
      balance)

    (define (call-the-cops)
      (error "call-the-cops"))

    (define (dispatch password m)
      (if (>= access-counter 7)
        (call-the-cops)
        (if (eq? my-password password)
          (begin (set! access-counter 0)
                 (cond ((eq? m 'withdraw) withdraw)
                       ((eq? m 'deposit) deposiot)
                       (else (error "Unknown request -- MAKE-ACCOUNT" m))))
          (begin (set! access-counter (+ access-counter 1))
                 (print "access-counter " access-counter)
                 (error "Incorrect password")))))
    dispatch))

;;;;;;; 7回連続で失敗、8回目で`call-the-cops`
; gosh$ (define acc (make-account 100 'pass))
; acc
; gosh$ ((acc 'some-other-password 'deposit) 50)
; access-counter 1
; *** ERROR: Incorrect password
; gosh$ ((acc 'some-other-password 'deposit) 50)
; access-counter 2
; *** ERROR: Incorrect password
; gosh$ ((acc 'some-other-password 'deposit) 50)
; access-counter 3
; *** ERROR: Incorrect password
; gosh$ ((acc 'some-other-password 'deposit) 50)
; access-counter 4
; gosh$ ((acc 'some-other-password 'deposit) 50)
; access-counter 5
; *** ERROR: Incorrect password
; gosh$ ((acc 'some-other-password 'deposit) 50)
; access-counter 6
; *** ERROR: Incorrect password
; gosh$ ((acc 'some-other-password 'deposit) 50)
; access-counter 7
; gosh$ ((acc 'some-other-password 'deposit) 50)
; *** ERROR: call-the-cops
;
;;;;;;; 途中で成功するとcounterは0に戻る
; gosh$ (define acc (make-account 100 'pass))
; acc
; gosh$ ((acc 'some-other-password 'deposit) 50)
; access-counter 1
; *** ERROR: Incorrect password
; gosh$ ((acc 'some-other-password 'deposit) 50)
; access-counter 2
; *** ERROR: Incorrect password
; gosh$ ((acc 'pass 'deposit) 50)
; 150
; gosh$ ((acc 'some-other-password 'deposit) 50)
; access-counter 1
; *** ERROR: Incorrect password
