#|
; 要素xが集合setの構成要素か?
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

; setにxを追加する
(define (adjoin-set x set)
  (if (element-of-set? x set)
    set
    (cons x set)))

; ２つの集合の積集合, 両方の集合に現れる要素だけを含む集合
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) (cdr set2)))))

; ex-2.59
; 和集合, どちらかの集合に現れる要素を含んでいる集合
(define (union-set set1 set2)
  (if (null? set1)
    set2
    (union-set (cdr set1) (adjoin-set (car set1) set2))))
|#

; ex-2.60
; 重複可の集合

; そのまま
#|
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))
(define (adjoin-set x set)
  (cons x set))
|#

(define (union-set set1 set2)
  (append set1 set2))

; そのまま
#|
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) (cdr set2)))))
|#

; 順序づけられたリストとしての集合
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
    '()
    ((let ((x1 (car set1)) (x2 (car set2)))
       (cond ((= x1 x2)
              (cons x1
                    (intersection-set (cdr set1)
                                      (cdr set2))))
             ((< x1 x2)
              (intersection-set (cdr set1) set2))
             ((< x2 x1)
              (intersection-set set1 (cdr set2))))))))

; ex-2.61
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set)) (append (list x) set))
        (else (append (list (car set)) (adjoin-set x (cdr set))))))

; ex-2.62
; (union-set '(1 2) '(1 2 4 6 8 10))
; (1 2 4 6 8 10)
#|
(define (union-set set1 set2)
  (if (null? set1)
    set2
    (union-set (cdr set1) (adjoin-set (car set1) set2))))
|#
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
          (let ((x1 (car set1))
                (x2 (car set2)))
            (cond ((= x1 x2)
                   (cons x1 (union-set (cdr set1) (cdr set2))))
                  ((< x1 x2)
                   (cons x1 (union-set (cdr set1) set2)))
                  ((> x1 x2)
                   (cons x2 (union-set set1 (cdr set2)))))))))
