; 3.3.2 キューの表現
; 3.3.scmはconsを自前実装しているのでファイルを分ける(dispatchを返して意図したように動かないので)

(define (make-queue) (cons '() '()))

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (front-queue queue)
  (if (empty-queue? queue)
    (error "FRONT called with an empty queue" queue)
    (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
            (set-cdr! (rear-ptr queue) new-pair)
            (set-rear-ptr! queue new-pair)
            queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
          (set-front-ptr! queue (cdr (front-ptr queue)))
          queue)))

; ex-3.21
; queueは以下の様になっている。
;
; q→OO--------
;   ↓        ↓
;   OO→OO→OO→OO
;   ↓  ↓  ↓  ↓
;   a  b  c  d
;
; 最初の対で常に先頭と末尾を指し示している。
; よって印字した時に((a b c d) d)のようになってしまう。
(define (print-queue queue)
  (print (car queue)))

(define q1 (make-queue))
(print "************************** ex-3.21")
(print-queue q1)

(insert-queue! q1 'a)
(print-queue q1)
(insert-queue! q1 'b)
(print-queue q1)
(delete-queue! q1)
(print-queue q1)
(delete-queue! q1)
(print-queue q1)

; ex-3.22
(define (make-queue-dispatch)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (set-front-ptr! item) (set! front-ptr item))
    (define (set-rear-ptr! item) (set! rear-ptr item))
    (define (empty-queue?) (null? front-ptr))
    (define (front-queue)
      (if (empty-queue?)
        (error "FRONT called with an empty queue" front-ptr)
        (car front-ptr)))
    (define (rear-queue)
      (if (empty-queue?)
        (error "FRONT called with an empty queue" rear-ptr)
        (car rear-ptr)))
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
               (set-front-ptr! new-pair)
               (set-rear-ptr! new-pair)
               front-ptr)
              (else
                (set-cdr! rear-ptr new-pair)
                (set-rear-ptr! new-pair)
                front-ptr))))
    (define (delete-queue!)
      (cond ((empty-queue?)
             (error "DELETE! called with an empty queue" front-ptr))
            (else
              (set-front-ptr! (cdr front-ptr))
              front-ptr)))
    (define (print-queue)
      (print front-ptr))
    (define (dispatch m)
      (cond ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) delete-queue!)
            ((eq? m 'front) front-queue)
            ((eq? m 'rear) rear-queue)
            ((eq? m 'print-queue) (print-queue))
            (else
              (error "Undefined operation -- MAKE-QUEUE" m))))

    dispatch))

(define q (make-queue-dispatch))
(print "************************** ex-3.22")
((q 'insert-queue!) 'a)
(q 'print-queue)
((q 'insert-queue!) 'b)
(q 'print-queue)
((q 'insert-queue!) 'c)
(q 'print-queue)
((q 'delete-queue!))
(q 'print-queue)
((q 'delete-queue!))
(q 'print-queue)
((q 'insert-queue!) 'd)
(q 'print-queue)
((q 'delete-queue!))
(q 'print-queue)
((q 'delete-queue!))
(q 'print-queue)
; ((q 'delete-queue!))

; ex-3.23
;
; dq->OO
;     ↓↓
;     OO
;     ↓
;     aO
;
; dq->OO--
;     ↓  ↓
;   ->OO→OO
;   | ↓  ↓
;   | bO aO
;   |-----|(以下はb-ptrで代用)
;
; dq->OO-----
;     ↓     ↓
;     OO→OO→OO
;     ↓  ↓  ↓
;     cO bO aO
;         ↓  ↓
;         cp b-ptr

; debug
; https://wat-aro.hatenablog.com/entry/2015/11/20/184629
(define (value-ptr ptr) (caar ptr))
(define (prev-ptr ptr) (cdar ptr))
(define (next-ptr ptr) (cdr ptr))
(define (print-deque queue)
  (let recur ((deque (front-ptr queue)))
    (cond ((null? deque) '())
          (else
            (cons (value-ptr deque)
                  (recur (next-ptr deque)))))))

(define (make-deque) (cons '() '()))
(define (deque-front-ptr deque) (car deque))
(define (deque-rear-ptr deque) (cdr deque))
(define (empty-deque? deque) (null? (deque-front-ptr deque)))

(define (front-deque deque)
  (if (empty-deque? deque)
    (error "FRONT called with an empty deque" deque)
    (car (deque-front-ptr deque))))

(define (rear-deque deque)
  (if (empty-deque? deque)
    (error "REAR called with an empty deque" deque)
    (car (deque-rear-ptr deque))))

; (front-insert-deque! deque 'a)
(define (front-insert-deque! deque item)
  ; new-pair: (d)
  (let ((new-pair (list (list item))))
    (cond ((empty-deque? deque)
           ; 最初はfront,rear共に同じitemに向ける
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair)
           deque)
          (else
            (let ((before-front-item (car (front-ptr deque))))
              (set-cdr! new-pair (front-ptr deque))
              ; dequeのfrontをnew-pairに向ける.
              ; new-pairのcdrは既にdequeを向いているのでfrontにinsertした事になっている
              (set-front-ptr! deque new-pair)
              ;prev-item
              (set-cdr! before-front-item (front-ptr deque)))
            deque))))

(define (rear-insert-deque! deque item)
  (let ((new-pair (list (list item))))
    (cond ((empty-deque? deque)
           ; 最初はfront,rear共に同じitemに向ける
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair)
           deque)
          (else
            ; prev-item
            (set-cdr! (car new-pair) (rear-ptr deque))
            ; dequeのcdr(rear)のcdrをnew-pairに向ける(rearへのinsert)
            (set-cdr! (cdr deque) new-pair)
            ; dequeのrearをnew-pairに向ける
            (set-rear-ptr! deque new-pair)
            deque))))

(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "DELETE! called with an empty deque" deque))
        (else
          ; dequeのcar(先頭)のcdr(次のitem)をdequeのcar(front)にsetする(frontのdelete)
          (set-front-ptr! deque (cdr (car deque)))
          ; ゴミ掃除
          (set-cdr! (car (front-ptr deque)) '())
          deque)))

(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "DELETE! called with an empty deque" deque))
        (else
          (set-rear-ptr! deque (cdr (car (rear-ptr deque))))
          ; ゴミ掃除
          (set-cdr! (rear-ptr deque) '())
          deque)))

(print "************************** ex-3.23")
(define dq (make-deque))
;;;;;;;;;;;;;;;;;;; front-insert-deque!
(print "insert-queue!")
;
; dq->OO
;     ↓↓
;     OO
;     ↓
;     aO
(front-insert-deque! dq 'a)
(print (print-deque dq))

; dq->OO--
;     ↓  ↓
;   ->OO→OO
;   | ↓  ↓
;   | bO aO
;   |-----|(以下はb-ptrで代用)

(front-insert-deque! dq 'b)
(print (print-deque dq))

; dq->OO-----
;     ↓     ↓
;     OO→OO→OO
;     ↓  ↓  ↓
;     cO bO aO
;         ↓  ↓
;         cp b-ptr
(front-insert-deque! dq 'c)
(print (print-deque dq))

;;;;;;;;;;;;;;;;;;; rear-insert-deque!
(print "rear-insert-deque!")
; dq->OO--------
;     ↓        ↓
;     OO→OO→OO→OO
;     ↓  ↓  ↓  ↓
;     cO bO aO dO
;         ↓  ↓  ↓
;         cp bp ap
(rear-insert-deque! dq 'd)
(print (print-deque dq))

; dq->OO-----------
;     ↓           ↓
;     OO→OO→OO→OO→OO
;     ↓  ↓  ↓  ↓  ↓
;     cO bO aO dO eO
;         ↓  ↓  ↓  ↓
;         cp bp ap dp
(rear-insert-deque! dq 'e)
(print (print-deque dq))

;;;;;;;;;;;;;;;;;;; front-delete-deque!
(print "front-delete-deque!")
; dq->OO-----------
;        ↓        ↓
;     OO→OO→OO→OO→OO
;     ↓  ↓  ↓  ↓  ↓
;     cO bO aO dO eO
;            ↓  ↓  ↓
;            bp ap dp
(front-delete-deque! dq)
(print (print-deque dq))

; dq->OO-----------
;           ↓     ↓
;     OO→OO→OO→OO→OO
;     ↓  ↓  ↓  ↓  ↓
;     cO bO aO dO eO
;               ↓  ↓
;               ap dp
(front-delete-deque! dq)
(print (print-deque dq))

;;;;;;;;;;;;;;;;;;; rear-delete-deque!
(print "rear-delete-deque!")
; dq->OO--------
;           ↓  ↓
;     OO→OO→OO→OO OO
;     ↓  ↓  ↓  ↓  ↓
;     cO bO aO dO eO
;              ↓  ↓
;              ap dp
(rear-delete-deque! dq)
(print (print-deque dq))

; dq->OO------
;           ↓↓
;     OO→OO→OO OO OO
;     ↓  ↓  ↓  ↓  ↓
;     cO bO aO dO eO
;              ↓  ↓
;              ap dp
(rear-delete-deque! dq)
(print (print-deque dq))
