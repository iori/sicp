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
(print-queue q1)

(insert-queue! q1 'a)
(print-queue q1)
(insert-queue! q1 'b)
(print-queue q1)
(delete-queue! q1)
(print-queue q1)
(delete-queue! q1)
(print-queue q1)
