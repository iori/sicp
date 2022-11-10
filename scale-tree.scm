; ex-2.21
(define (square-list items)
  (map (lambda (x) (* x x)) items))


; ex-2.30
; (square-tree
;   (list 1
;         (list 2 (list 3 4) 5)
;         (list 6 7)))
;
; (1 (4 (9 16) 25) (36 49))

; mapでガーッと掘っていける & mapは(list (list (list ...)))みたいなのをそのままの構造で返してくれる(値は変えれる)
#|
(define (square-tree tree)
  (if (pair? tree)
    (map (lambda (x) (square-tree x)) tree)
    (* tree tree)))

(print (square-tree (list 1 2 3 (list 3))))
|#

; ex2.31
(define (tree-map fn tree)
  (if (pair? tree)
    (map (lambda (x) (tree-map fn x)) tree)
    (fn tree)))

(define (square-tree tree) (tree-map (lambda (x) (* x x)) tree))
; (print (square-tree (list 1 2 3 (list 3 (list 4 9)))))

; ex-2.32
; (subsets (list 1 2 3))
; (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))
(define (append l1 l2)
  (if (null? l1)
    l2
    (cons (car l1) (append (cdr l1) l2))))

(define (subsets s)
  (print s)
  (if (null? s)
    (list ())
    (let ((rest (subsets (cdr s))))
      (append rest (map (lambda (x) (cons (car s) x)) rest)))))

(print (subsets (list 1 2 3)))
