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
((q 'delete-queue!))
