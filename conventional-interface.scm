; enumerate -> filter -> map -> accumulate
;
; enumerate: 信号を発生, 数え上げ
; filter: 不要な要素を除去
; map: 各要素に手続きを作用させる, 変換器
; accumulate: 0から+を使って組み合わせる

; even-fibでは与えられた範囲の整数を発生させる必要があり、次のようにする
; (enumerate-interval 2 7)
; (2 3 4 5 6 7)
(define (enumerate-interval low high)
  (if (> low high)
    ()
    (cons low (enumerate-interval (+ low 1) high))))

; 木の葉を数え上げるには
; (enumerate-tree (list 1 (list 2 (list 3 4)) 5))
; (1 2 3 4 5)
; が使える
(define (enumerate-tree tree)
  (cond ((null? tree) ())
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

; (filter odd? (list 1 2 3 4 5))
(define (filter predicate sequence)
  (cond ((null? sequence) ())
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

; (map square (list 1 2 3 4 5))

; (accumulate + 0 (list 1 2 3 4 5))
(define (accumulate op initial sequence)
  ; (print sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

(define (sum-odd-square tree)
  (accumulate + 0 (map square (filter odd? (enumerate-tree tree)))))

; ex-2.33
; p.60のmapの定義(参考用)
(define (d-map proc items)
  (if (null? items)
    ()
    (cons (proc (car items))
          (d-map proc (cdr items)))))

(define (my-map proc sequence)
  (accumulate (lambda (x y) (cons (proc x) y)) () sequence))

; (print (my-map (lambda(x) (* x x)) (list 1 2 3 4)))

(define (d-append l1 l2)
  (if (null? l1)
    l2
    (cons (car l1) (append (cdr l1) l2))))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

; (print (append (list 1 2) (list 2 3)))
(define (d-length items)
  (if (null? items)
    0
    (+ 1 (length (cdr items)))))

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

; (print (length (list 1 2 3)))


; ex-2.35
(define (d-count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (d-count-leaves (car x))
                 (d-count-leaves (cdr x))))))

; (accumulate ?? ?? (map ?? ??)))
(define (count-leaves t)
  (accumulate + 0 (map
                    (lambda (x) (if (not (pair? x))
                                  1
                                  (count-leaves x))) t)))
; (print (count-leaves (list 1 2 3)))

; ex-2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    ()
    (cons (accumulate op init (map car seqs))
          (accumulate-n op init (map cdr seqs)))))

; (print (accumulate-n + 0 (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12))))
; (22 26 30)

; ex-2.38
(define (fold-right op initial sequence)
  (accumulate op initial sequence))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
      result
      (iter (op result (car rest))
            (cdr rest))))
  (iter initial sequence))
; (print (fold-left / 1 (list 1 2 3)))

; ex-2.39
(define (d-reverse l)
  (define (reverse-iter l r)
    (if (null? l)
      r
      (reverse-iter (cdr l) (cons (car l) r))))
  (reverse-iter l ()))

(define (r-reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) () sequence))

; (print (r-reverse (list 1 2 3)))

(define (l-reverse sequence)
  (fold-left (lambda (x y) (append (list y) x)) () sequence))

; (print (l-reverse (list 1 2 3)))

; 写像の入れ子
(define (divides? a b)
  (= (remainder b a) 0))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (smallest-divisor n)
    (find-divisor n 2))

(define (prime? n)
  (= n (smallest-divisor n)))

;;;;;;;;;;;;;
(define (flatmap proc seq)
  (accumulate append () (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

; 1 <= j < i <= n
; ((2 1 3) (3 2 5) (4 1 5) (4 3 7) (5 2 7) (6 1 7) (6 5 11))
; ((i j i+j) ...)
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                 (lambda (i)
                   (map (lambda (j) (list i j))
                        (enumerate-interval 1 (- i 1))))
                 (enumerate-interval 1 n)))))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

(define (permutations s)
  (if (null? s)
    (list ())
    (flatmap (lambda (x)
               (map (lambda (p) (cons x p))
                    (permutations (remove x s))))
             s)))

; ex-2.40
(define (make-pair pair)
  (list (car pair) (cadr pair)))

(define (unique-pairs n)
  (map make-pair
       (filter prime-sum?
               (flatmap
                 (lambda (i)
                   (map (lambda (j) (list i j))
                        (enumerate-interval 1 (- i 1))))
                 (enumerate-interval 1 n)))))

; (print (prime-sum-pairs 6))
; (print (unique-pairs 6))

(define (my-prime-sum-pairs n)
  (map (lambda (pair) (list (car pair) (cadr pair) (+ (car pair) (cadr pair)))) (unique-pairs n)))

; (print (my-prime-sum-pairs 6))

; ex-2.41
; nより小さいか等しい相異なる整数i,j,kの順序付けられた三つの組で、和が与えられた整数sになる組み合わせ
; (equal-sum-of-unique-trio 10 20)
; ((8 7 5) (9 6 5) (9 7 4) (9 8 3) (10 6 4) (10 7 3) (10 8 2) (10 9 1))
(define (equal-sum-of-unique-trio n s)
  (filter (lambda (l) (= s (+ (car l) (cadr l) (caddr l))))
          (flatmap
            (lambda (i)
              (flatmap (lambda (j)
                         (map (lambda (k) (list i j k))
                              (enumerate-interval 1 (- j 1))))
                       (enumerate-interval 1 (- i 1))))
            (enumerate-interval 1 n))))

; (print (hoge 10 20))

; (map proc (map proc seq))
(map (lambda (x)
       (map (lambda (y)
              (* x y))
            (list 1 2 3)))
     (list 4 5 6))

#|
(print
(flatmap (lambda (i)
           (map (lambda (j) (list i j))
                (list 1 2 3)))
         (list 1 2 3))
)
|#



#|
(print
(flatmap (lambda (i)
           (flatmap (lambda (j)
                  (map (lambda (k) (list i j k))
                       (list 1 2 3)))
                (list 4 5 6)))
         (list 7 8 9))
)
|#
(define (hoge n s)
  (filter (lambda (l) (= s (+ (car l) (cadr l) (caddr l))))
          (flatmap
            (lambda (i)
              (print "****************************************i")
              (print i)
              (flatmap (lambda (j)
                         (print "*******j")
                         (print j)
                         (map (lambda (k)
                                (print "**k")
                                (print k)
                                (print (list i j k))
                                (list i j k))
                              (enumerate-interval 1 (- j 1))))
                       (enumerate-interval 1 (- i 1))))
            (enumerate-interval 1 n)))) ; 1 .. 10

; (print (hoge 10 20))

; ex-2.42
; rest-of-queensは最初のk-1列にk-1個のクイーンを置く方法である
; new-rowはk列目にクイーンの置ける行の案である
; - [x] adjoin-positionは盤上の位置の集合の表現を実装し、位置の集合に新しい場所の座標を連結する手続き
; - [x] empty-boardは場所のから集合を表現する
; - [x] safe?は他のクイーンに対してk番目のクイーンが安全な場所か?(他のクイーンは互いに安全である事が保証されているので新しいクイーンだけ調べれば良い)

; 参考 : https://www.serendip.ws/archives/776

; > (queens 4)
; (((2 . 1) (4 . 2) (1 . 3) (3 . 4)) ((3 . 1) (1 . 2) (4 . 3) (2 . 4)))
(define (queens board-size)
  (define (queen-cols k) ; 板の最初のk列にクイーンを置く全ての方法の並びを返す
    (if (= k 0)
      (list empty-board)
      (filter ; (fleter predicate sequence)
        (lambda (positions) (safe? k positions)) ; predicate
        ; (flatmap (lambda (rest-of-queens) (map (lambda (new-row) (adjoin-position new-row 2 rest-of-queens)) '(1 2 3 4))) '(((1 1))))
        ; (((1行目 2列目) (1行目 1列目)) ...)
        ; (((1 2) (1 1)) ((2 2) (1 1)) ((3 2) (1 1)) ((4 2) (1 1)))
        (flatmap ; sequence
          (lambda (rest-of-queens)
            ;                                    1,2,3,4    k        enumerate-intervalなnew-row(1 2 3 4)
            ; (map (lambda (new-row) (cons (list new-row 1) ())) (list 1 2 3 4))
            ; k = 1
            ;   行 列
            ; (((1 1)) ((2 1)) ((3 1)) ((4 1)))
            ; k = 2
            ; (((1 2)) ((2 2)) ((3 2)) ((4 2)))
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1  board-size))); new-row
          (queen-cols (- k 1)))))) ; rest-of-queens, queen-colsの再帰
  (queen-cols board-size)) ; k

(define empty-board ())

; rowはクイーンを置く行の案. 1,2,3,4
; kは引数board-sizeと-1されていく値. 4,3,2,1
; rest-of-queensは()
; (cons (list 1or2or3or4 1) ())
(define (adjoin-position new-row k rest-of-queens)
  (cons (list new-row k) rest-of-queens))

(define (safe? k positions)
  (print "poisitions " positions)
  ;  行 列
  ; ((1 1))
  ; ((2 1))
  ; ((3 3) (1 2) (3 1))
  ;   kth  : (3 3)
  ;   rest : ((1 2) (3 1))
  (let ((kth (car positions)))
    (define (iter rest)
      (print "kth " kth ", rest: " rest)

      ; conflictsするかnullになるまでiterを回す
      ; nullになった = (3行 3列)と(1 2), (3 3)と(3 1)すべて問題なし
      (cond ((null? rest) #t)
            ((conflicts? (car rest) kth) #f)
            (else (iter (cdr rest)))))
    (iter (cdr positions))))

; a (3 3)
; b (1 2)
; なんでこれでconflicts判定できるのかよくわかってない。
; ここはこういうロジックなんだなーくらいで良いかも。
; 2週目やる時はここもちゃんと理解したいな(別ロジックでも良い)。
(define (conflicts? a b)
  (let (
        ;   (abs (-      3       1))   => 2
        (dx (abs (- (car a) (car b))))
        ;   (abs (-       3        2)) => 1
        (dy (abs (- (cadr a) (cadr b)))))
    (cond ((= dx 0) #t)
          ((= dy 0) #t)
          ((= dx dy) #t)
          (else #f))))

; いつもflatmapの挙動を忘れるのでメモ
(flatmap
  (lambda (rest-of-queens)
    (map (lambda (new-row)
           (cons (list new-row 1) rest-of-queens))
         (list 1 2 3 4)))
  (list 1 2 3))
; (((1 1) . 1) ((2 1) . 1) ((3 1) . 1) ((4 1) . 1)
;  ((1 1) . 2) ((2 1) . 2) ((3 1) . 2) ((4 1) . 2)
;  ((1 1) . 3) ((2 1) . 3) ((3 1) . 3) ((4 1) . 3))

(print "------- rest-of-queens -> new-row")
(define count 0)
(flatmap
  (lambda (rest-of-queens)
    (print "lambda1")
    (map (lambda (new-row)
           (print "lambda2")
           (inc! count 1)
           (cons (list new-row 2) rest-of-queens))
         (list 1 2 3 4)))
  (list 1 1))
(print "count " count)

(print "------- new-row -> rest-of-queens")
(define count2 0)
(flatmap
  (lambda (new-row)
    (print "lambda1")
    (map (lambda (rest-of-queens)
           (print "lambda2")
           (inc! count2 1)
           (cons (list new-row 2) rest-of-queens))
         (list 1 1)))
  (list 1 2 3 4))
(print "count " count2)

#|
これだけ差がある。
------- rest-of-queens -> new-row
lambda1
lambda2
lambda2
lambda2
lambda2
lambda1
lambda2
lambda2
lambda2
lambda2
count 8
------- new-row -> rest-of-queens
lambda1
lambda2
lambda2
lambda1
lambda2
lambda2
lambda1
lambda2
lambda2
lambda1
lambda2
lambda2
count 8
|#
