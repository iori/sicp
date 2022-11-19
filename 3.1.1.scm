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

; 3.1.2 代入を取り入れた利点

; randの実装
; https://boxnos.hatenablog.com/entry/20080422/1208863688
(define random-init 12345)
(define (rand-update x)
   (modulo (+ (* 214013 x) 253011) 32767))
(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))

; πの近似値
; πの近似値を得るには多数回の実験を行う.
; 各実験で二つの整数をランダムに選び、そのGCDが1かどうかのテストを行う.
; テストが成功した回数の比率が6/π^2の推定を与え, これからπの近似を得る。
(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
            (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

; ex-3.5
(use srfi-27)
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random-integer range))))

(define (estimate-integral p x1 x2 y1 y2 trials)
  (*
    (monte-carlo trials (lambda () (p (random-in-range x1 x2) (random-in-range y1 y2))))
    (* (- x2 x1) (- y2 y1))))

; 面積と面積から算出した円周率piを表示する手続き
(define (pi-from-monte-carlo-simulation circle-area radius)
  ; (display circle-area)
  ; (newline)
  (/ circle-area radius))

; 中心(5, 7) 半径3 の円の場合
; テスト手続き
(define (p-test x y)
  (<= (+ (square (- x 5)) (square (- y 7))) (square 3)))

; (print (pi-from-monte-carlo-simulation (estimate-integral p-test 2 8 4 10 100000.0) (square 3)))
; 2.99488

; ex-3.6
(define rand
  (let ((x random-init))
    (define generate
      (lambda ()
        (set! x (rand-update x)) x))

    (define (reset new-value)
      (begin (set! x new-value) x))

    (define (dispatch m)
      ; (print "x " x)
      (cond ((eq? m 'generate) (generate))
            ((eq? m 'reset) reset)
            (else (error "Unknown requeset -- RAND" m))))
    dispatch))

; gosh$ (rand 'generate)
; x 12345
; 10917
; gosh$ (rand 'generate)
; x 10917
; 18162
; gosh$ ((rand 'reset) 100)
; x 18162
; 100
; gosh$ (rand 'generate)
; x 100
; 28091

; ex-3.7
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
            ((eq? m 'password?) #t)
            (else (error "Unknown request -- MAKE-ACCOUNT" m)))
      (error "Incorrect password")))
  dispatch)

(define (make-joint account account-password new-password)
  (define (dispatch password m)
    (if (and (account account-password 'password?) (eq? new-password password))
      (account account-password m)
      (error "Incorrect joint password")))
  dispatch)

(define peter-acc (make-account 100 'open-sesame))
(define paul-acc
 (make-joint peter-acc 'open-sesame 'rosebud))

; (print (peter-acc 'open-sesame 'password?))
; #t

; (print ((peter-acc 'open-sesame 'withdraw) 40))
; 60
; (print ((peter-acc 'open-sesame 'deposit) 50))
; 110
; (print ((paul-acc 'rosebud 'withdraw) 40))
; 70
; (print ((paul-acc 'rosebud 'deposit) 50))
; 120

; (print ((paul-acc 'rosebuda 'deposit) 50))
; Incorrect joint password
; (print ((peter-acc 'rosebuda 'withdraw) 40))
; Incorrect password

; ex-3.8
; 日本語が理解できぬ...

; ex-3.9
;
; 再帰版
;             _____________________________________
; 大域領域 -> |                                   |
;             -------------------------------------
;                 ↑        ↑        ↑
;             E1→|n:3| E2→|n:2| E3→|n:1|
;             (if (= n 1)
;               1
;               (* n (factorial (- n 1))))
;             E1 ~ E1同じ↑
;
; 反復版
;             _____________________________________
; 大域領域 -> |                                   |
;             -------------------------------------
;              ↑
;              E1→|n:3|
;              (fact-iter 1 1 n)
;                                ↑
;                                E2→|product:1  |
;                                   |counter:1  |
;                                   |max-count:3|
;                                   (fact-iter (* counter product)
;                                              (+ counter 1)
;                                              max-count)
;
; E3 product:1, counter:2, max-count:6
; E4 product:2, counter:3, max-count:6
; E5 product:6, counter:4, max-count:6
; E6 product:24, counter:5, max-count:6
; E6 product:120, counter:6, max-count:6
; E6 product:720, counter:7, max-count:6

; ex-3.10
;
; (define W1 (make-withdraw 100))
; (W1 50)
; (define W2 (make-withdraw 100))
;---------------------------------------------
;
; (define W1 (make-withdraw 100))
;             _____________________________________________________________________
; 大域領域 -> |make-withdraw:                                                     |
;             |W1:                                                                |
;             ---------------------------------------------------------------------
;             ↑                         ↓(W1)     ↑               ↑(make-withdraw:)
;             E1→|initial-balance:100|  OO--->E2→|balance:100|    OO
;                                       ↓                         ↓
;                                       パラメタ: amount          パラメタ:initial-amount
;                                       本体: (lambda (amount)... 本体: (lambda (balance)...
;
; (W1 50)
;             _____________________________________________________________________
; 大域領域 -> |make-withdraw:                                                     |
;             |W1:                                                                |
;             ---------------------------------------------------------------------
;              ↓(W1)      ↑
;              OO-------->E2→|balance:100|
;              ↓            ↑
;              パラメタ:   |amount: 50|
;              本体:
;
;             _____________________________________________________________________
; 大域領域 -> |make-withdraw:                                                     |
;             |W1:                                                                |
;             ---------------------------------------------------------------------
;              ↓(W1)      ↑
;              OO-------->E2→|balance:50|
;              ↓
;              パラメタ:
;              本体:
;
; (define W2 (make-withdraw 100))
;             _____________________________________________________________________
; 大域領域 -> |make-withdraw:                                                     |
;             |W1:                                                                |
;             |W2:                                                                |
;             ---------------------------------------------------------------------
;              ↓(W1)      ↑                ↑                       ↓(W2)    ↑
;              OO-------->E2→|balance:50|  E3|initial-balance:100| OO--->E4|balance:100|
;              ↓                                                   ↓
;              パラメタ:                                           パラメタ: amount
;              本体:                                               本体: (lambda (amount)...
; memo:
;   defineされた時にOO(対になるオブジェクト) が作られる.
;   評価された時に環境が作られるが作られる.


; ex-3.11
;
; (define acc (make-account 50))
;
; ((acc 'deposit) 40)
; 90
;
; ((acc 'withdraw) 60)
; 30
;
; (define acc2 (make-account 100))
;---------------------------------------------
;
; (define acc (make-account 50))
;             _____________________________________________________________________
; 大域領域 -> |make-account:
;             |acc:
;             ---------------------------------------------------------------------
;             ↑↓(make-account)               ↓(acc)                 ↑  ____________
;             OO                             OO--------------------→E1→|balance:50|
;             ↓                              ↓                         |withdraw: |<->OO->パラメタ:amount, 本体: (if (>= balance amount) ...
;             パラメタ: balance:             パラメタ: m               |deposit:  |<->OO->パラメタ:amount, 本体: (set! balance ...
;             本体: (define (withdraw ...    本体: (cond ((eq? ...     |dispatch: |<->OO->パラメタ:m, 本体: (cond ((eq? ...
;
;
; ((acc 'deposit) 40)
;             _____________________________________________________________________
; 大域領域 -> |make-account:
;             |acc:
;             ---------------------------------------------------------------------
;             ↑↓(make-account)               ↓(acc)                 ↑   ___________
;             OO                             OO--------------------→E1→|balance:90|
;             ↓                              ↓                         |withdraw: |<->OO->パラメタ:amount, 本体: (if (>= balance amount) ...
;             パラメタ: balance:             パラメタ: m               |deposit:  |<->OO->パラメタ:amount, 本体: (set! balance ...
;             本体: (define (withdraw ...    本体: (cond ((eq? ...     |dispatch: |<->OO->パラメタ:m, 本体: (cond ((eq? ...
;                                                                      ------------
;                                                                      ↑       ↑
;                                                         E2→|m: 'deposit| E3→|amount: 40|
; ((acc 'withdraw) 60)
;             _____________________________________________________________________
; 大域領域 -> |make-account:
;             |acc:
;             ---------------------------------------------------------------------
;             ↑↓(make-account)               ↓(acc)                 ↑   ___________
;             OO                             OO--------------------→E1→|balance:30|
;             ↓                              ↓                         |withdraw: |<->OO->パラメタ:amount, 本体: (if (>= balance amount) ...
;             パラメタ: balance:             パラメタ: m               |deposit:  |<->OO->パラメタ:amount, 本体: (set! balance ...
;             本体: (define (withdraw ...    本体: (cond ((eq? ...     |dispatch: |<->OO->パラメタ:m, 本体: (cond ((eq? ...
;                                                                      ------------
;                                                                      ↑       ↑
;                                                        E4→|m: 'withdraw| E5→|amount: 60|
;
;
; (define acc2 (make-account 100))
;             _____________________________________________________________________
; 大域領域 -> |make-account:
;             |acc:
;             |acc2:
;             ---------------------------------------------------------------------
;             ↑↓(make-account)               ↓(acc)                 ↑  ____________
;             OO                             OO--------------------→E1→|balance:50|
;             ↓                              ↓                         |withdraw: |<->OO->パラメタ:amount, 本体: (if (>= balance amount) ...
;             パラメタ: balance:             パラメタ: m               |deposit:  |<->OO->パラメタ:amount, 本体: (set! balance ...
;             本体: (define (withdraw ...    本体: (cond ((eq? ...     |dispatch: |<->OO->パラメタ:m, 本体: (cond ((eq? ...
;
;                                            ↓(acc2)                ↑(大域)
;                                            OO--------------------→E6→|balance:100|
;                                            ↓                         |withdraw:  |<->OO->パラメタ:amount, 本体: (if (>= balance amount) ...
;                                            パラメタ: m               |deposit:   |<->OO->パラメタ:amount, 本体: (set! balance ...
;                                            本体: (cond ((eq? ...     |dispatch:  |<->OO->パラメタ:m, 本体: (cond ((eq? ...

