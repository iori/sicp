; 二進木としての集合

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

; ex-2.63
; a. 結果は同じ
; b. 1は再帰, 2は反復. 2の方が軽い.
;    けどprintしたらほぼ変わらなそう.
(define tree (make-tree
               7
               (make-tree 3 (make-tree 1 () ()) (make-tree 5 () ()))
               (make-tree 9 () (make-tree 11 () ()))))

(define (tree->list-1 tree)
  ; (print tree)
  (if (null? tree)
    ()
    (append (tree->list-1 (left-branch tree))
            (cons (entry tree)
                  (tree->list-1 (right-branch tree))))))

; (print (tree->list-1 tree))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    ; (print tree)
    (if (null? tree)
      result-list
      (copy-to-list (left-branch tree)
                    (cons (entry tree)
                          (copy-to-list (right-branch tree)
                                        result-list)))))
  (copy-to-list tree ()))

; (print (tree->list-2 tree))

; ex-2.64
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  ; (print elts)
  (if (= n 0)
    (cons '() elts)
    (let ((left-size (quotient (- n 1) 2)))
      (let ((left-result (partial-tree elts left-size)))
        (let ((left-tree (car left-result))
              (non-left-elts (cdr left-result))
              (right-size (- n (+ left-size 1))))
          (let ((this-entry (car non-left-elts))
                (right-result (partial-tree (cdr non-left-elts) right-size)))
            (let ((right-tree (car right-result))
                  (remaining-elts (cdr right-result)))
              (newline)
              (cons (make-tree this-entry left-tree right-tree)
                    remaining-elts))))))))

; (print (list->tree '(1 3 5 7 9 11)))
; (print "************************************")
; (print (list->tree '(1 3 5 7 9)))
; (5
;    (1 () (3 () ()))
;    (9 (7 () ()) (11 () ())))

; a.
;   5
; 1   9
;  3 7 11

; b. O(n)

; ex-2.65
; union-set 和集合
; intersection-set 積集合
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

(define (union-tree tree1 tree2)
  (list->tree
    (union-set
      (tree->list-1 tree1)
      (tree->list-1 tree2))))

; (print (union-tree (list->tree '(1 2 3 4 5)) (list->tree '(4 5 6 7 8))))

(define (element-of-set-old? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-set-old? x (cdr set)))))

(define (intersection-set set1 set2)
  (print set1)
  (print set2)
  (cond ((or (null? set1 ) (null? set2)) ())
        ((element-of-set-old? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (intersection-tree tree1 tree2)
  (list->tree
    (intersection-set
      (tree->list-1 tree1)
      (tree->list-1 tree2))))

; (print (intersection-tree (list->tree '(1 2 3 4 5)) (list->tree '(4 5 6 7 8))))

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) #f)
        ((equal? given-key (key (car set-of-records)))
         (car set-of-records))
        (else (lookup given-key (cdr set-of-records)))))

; ex-2.66
(define (key record) (car record))
(define (value record) (cadr record))
(define (make-record key value) (list key value))
(define record
  (list->tree (list
                (make-record 1 'hoge)
                (make-record 2 'fuga)
                (make-record 3 'piyo)
                (make-record 4 'piyora))))

(define (lookup-record given-key records)
  (print records)
  (if (null? records)
    #f
    (let ((record (car records)))
      (let ((take-key (key record)))
        (cond ((equal? given-key take-key)
               (value record))
              ((< given-key take-key)
               (lookup-record given-key (left-branch records)))
              ((> given-key take-key)
               (lookup-record given-key (right-branch records))))))))
