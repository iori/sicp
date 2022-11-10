; Huffman 木の表現
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
    (weight-leaf tree)
    (cadddr tree)))

; 復号化手続き
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
      ()
      (let ((next-branch
              (choose-branch (car bits) current-branch)))
        (if (leaf? next-branch)
          (cons (symbol-leaf next-branch)
                (decode-1 (cdr bits) tree))
          (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

; 重み付き要素の集合
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                     (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
    ()
    (let ((pair (car pairs)))
      (adjoin-set (make-leaf (car pair)   ; 記号
                             (cadr pair)) ; 頻度
                  (make-leaf-set (cdr pairs))))))

; ex-2.67
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                    (make-leaf 'B 2)
                    (make-code-tree (make-leaf 'D 1)
                                    (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

; (print (decode sample-message sample-tree))
; (A D A B B C A)

; ex-2.68
; message: (A D A B B C A)
; tree: sample-tree
;  ((leaf A 4) ((leaf B 2) ((leaf D 1) (leaf C 1) (D C) 2) (B D C) 4) (A B D C) 8)
; return: (0 1 1 0 0 1 0 1 0 1 1 1 0)
(define (encode message tree)
  (if (null? message)
    ()
    (append (encode-symbol (car message) tree)
            (encode (cdr message) tree))))


(define (encode-symbol symbol tree)
  ; (print tree)
  ; (print symbol)
  (cond ((leaf? tree) ())
        ((memq symbol (symbols (left-branch tree)))
         (cons 0 (encode-symbol symbol (left-branch tree))))
        ((memq symbol (symbols (right-branch tree)))
          (cons 1 (encode-symbol symbol (right-branch tree))))
        (else (error "not symbol in symbols" symbol))))

; (print (encode '(A D A B B C A) sample-tree))
; (0 1 1 0 0 1 0 1 0 1 1 1 0)
; (print (equal? sample-message (encode (decode sample-message sample-tree) sample-tree)))
; #t

; ex-2.69
; '((A 4) (B 2) (C 1) (D 1))
; '((leaf A 4) ((leaf B 2) ((leaf D 1) (leaf C 1) (D C) 2) (B D C) 4) (A B D C) 8)

; (make-leaf-set '((A 4) (B 2) (C 1) (D 1)))
; ((leaf D 1) (leaf C 1) (leaf B 2) (leaf A 4))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

; ((leaf D 1) (leaf C 1) (leaf B 2) (leaf A 4))
(define (successive-merge leafs)
  (print leafs)
  (cond ((null? leafs) ())
        ((null? (cdr leafs)) (car leafs))
        (else (successive-merge
                (adjoin-set (make-code-tree (car leafs) (cadr leafs))
                            (cddr leafs))))))
; (print (generate-huffman-tree '((A 4) (B 2) (C 1) (D 1))))
; ((leaf A 4) ((leaf B 2) ((leaf D 1) (leaf C 1) (D C) 2) (B D C) 4) (A B D C) 8)

; ex-2.70
(define song-tree (generate-huffman-tree '((A 2) (NA 16) (BOOM 1) (SHA 3) (GET 2) (YJP 9) (JOB 2) (WAH 1))))
(define song '(
               GET A JOB
               SHA NA NA NA NA NA NA NA NA
               GET A JOB
               SHA NA NA NA NA NA NA NA NA
               WAH YJP YJP YJP YJP YJP YJP YJP YJP YJP
               SHA BOOM))
; (print (encode song song-tree))
; (1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1 1 0 1 1 0 1 1)
; (print (length (encode song song-tree)))
; 84bit
