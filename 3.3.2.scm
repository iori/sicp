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
; このやり方は駄目なのでmake-deque2で実装する。
; 失敗も残しておきたいので消さないでおく。
;
; deque->OO-----
;        ↓     ↓
;        OO→OO→OO
;        ↓  ↓  ↓
;        a  b  c
;
; deque->OO--------
;        ↓        ↓
;        OO→OO→OO→OO
;        ↓  ↓  ↓  ↓
;        d  a  b  c

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
  (let ((new-pair (cons item '())))
    (cond ((empty-deque? deque)
           ; 最初はfront,rear共に同じitemに向ける
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair)
           deque)
          (else
            ; new-pairのcdrをdequeに向ける(frontへのinsert)
            (set-cdr! new-pair (car deque))
            ; dequeのfrontをnew-pairに向ける.
            ; new-pairのcdrは既にdequeを向いているのでfrontにinsertした事になっている
            (set-front-ptr! deque new-pair)
            deque))))

(define (rear-insert-deque! deque item)
  (let ((new-pair (cons item '())))
    (cond ((empty-deque? deque)
           ; 最初はfront,rear共に同じitemに向ける
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair)
           deque)
          (else
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
          deque)))

(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "DELETE! called with an empty deque" deque))
        (else
          ;(set-rear-ptr! deque (cadr (cdr deque)))
          (set-rear-ptr! deque (cdr deque))
          deque)))

(print "************************** ex-3.23")
(define dq (make-deque))
; dq->OO
;     ↓↓
;     OO
;     ↓
;     a
(front-insert-deque! dq 'a)
(print-queue dq)
; dq->OO--
;     |  |
;     ↓  ↓
;     OO→OO
;     ↓  ↓
;     b  a
(front-insert-deque! dq 'b)
(print-queue dq)
; dq->OO-----
;     |     |
;     ↓     ↓
;     OO→OO→OO
;     ↓  ↓  ↓
;     c  b  a
(front-insert-deque! dq 'c)
(print-queue dq)
; dq->OO--------
;     |        |
;     ↓        ↓
;     OO→OO→OO→OO
;     ↓  ↓  ↓  ↓
;     c  b  a  d
(rear-insert-deque! dq 'd)
(print-queue dq)
; dq->OO-----------
;     |           |
;     ↓           ↓
;     OO→OO→OO→OO→OO
;     ↓  ↓  ↓  ↓  ↓
;     c  b  a  d  e
(rear-insert-deque! dq 'e)
(print-queue dq)
; dq->OO-----------
;        |        |
;        ↓        ↓
;     OO→OO→OO→OO→OO
;     ↓  ↓  ↓  ↓  ↓
;     c  b  a  d  e
(front-delete-deque! dq)
(print-queue dq)
; dq->OO-----------
;           |     |
;           ↓     ↓
;     OO→OO→OO→OO→OO
;     ↓  ↓  ↓  ↓  ↓
;     c  b  a  d  e
(front-delete-deque! dq)
(print-queue dq)
; !!!!!!! このやり方だとrearの前のitemがΘ(1)ステップでわからない.
;         違う方法が必要.
(rear-delete-deque! dq)
(print-queue dq)
