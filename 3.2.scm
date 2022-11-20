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

