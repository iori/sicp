; ex-2.29
; 二進モービルは左の枝と右の枝の2つの枝で出来てる。
; それぞれの枝はある長さの棒で、そこから錘か、別の二進モービルがぶら下がっている。
; 二進モービルを2つの枝から(例えばlistを使って)出来ている合成データで表現出来る。
(define (make-mobile left right)
  ; (list left right)
  (cons left right))

; 1つの枝はlength(数でなければならない)と、(単なる錘を表現する)数か別のモービルであるstructureで構成する.
(define (make-branch length structure)
  ; (list length structure)
  (cons length structure))

; a. これに対応する選択肢(モービルの枝を返す)left-branchとright-branchと,(枝の部品を返す)branch-lengthとbranch-structureを書け.
(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cdr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cdr branch))

; b. この選択肢を使い、モービルの全重量を返す手続きtotal-weightを定義せよ。
(define (total-weight mobile)
  (define (iter branch)
    (let ((structure (branch-structure branch)))
      ; (print structure)
      (if (pair? structure)
        (iter structure)
        structure)))
  (+ (iter (left-branch mobile)) (iter (right-branch mobile))))

#|
(print
  (total-weight
    (make-mobile
      (make-branch 2 2)
      (make-branch 3 (make-branch 1 (make-branch 1 3))))))
|#

; c. モービルは最上段左の枝による回転力と、最上段右の枝による回転力が等しく、
;    しかも枝にぶら下がっている部品モービルのそれぞれが釣り合っている時、釣り合っている(balanced)という.
;    回転力は左の棒の長さと棒の左にかかっている重さを掛けたものが右側の対応する積に等しい

(define (balanced? mobile)
  (define (iter branch)
    (let ((structure (branch-structure branch)))
      ; (print structure)
      (if (pair? structure)
        (+ (branch-length branch) (iter (right-branch branch)))
        (* (branch-length branch) structure))))
  ; (print mobile)
  ; (print (iter (left-branch mobile)))
  ; (print (iter (right-branch mobile)))
  (= (iter (left-branch mobile)) (iter (right-branch mobile))))

#|
(print
  (balanced?
    (make-mobile
      (make-branch 2 2)
      (make-branch 3 (make-branch 1 (make-branch 1 3))))))

(print
  (balanced?
    (make-mobile
      (make-branch 2 2)
      (make-branch 2 2))))

|#
(print
  (balanced?
    (make-mobile
      (make-branch 2 5)
      (make-branch 2 (make-branch 2 (make-branch 2 3))))))
