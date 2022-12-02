(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (same-key? key))

    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
          (let ((record (assoc key-2 (cdr subtable))))
            (if record
              (cdr record)
              false))
          false)))

    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
          (let ((record (assoc key-2 (cdr subtable))))
            (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
          (set-cdr! local-table
                    (cons (list key-1
                                (cons key-2 value))
                          (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation - - TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

; ex-3.24
(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (same-key? key-1 (cdr local-table))))
        (if subtable
          (let ((record (same-key? key-2 (cdr subtable))))
            (if record
              (cdr record)
              #f))
          #f)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (same-key? key-1 (cdr local-table))))
        (if subtable
          (let ((record (same-key? key-2 (cdr subtable))))
            (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
          (set-cdr! local-table
                    (cons (list key-1
                                (cons key-2 value))
                          (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define (search key records)
  (cond ((null? records) #f)
        ((equal? key (caar records)) (car records))
        ((and (number? key) (< (abs (- key (caar records))) 0.5)) (car records))
        (else (search key (cdr records)))))

; ex-3.26
(define (make-table)
  (let ((local-table '*table*))
    (define (key-tree tree)
      (car tree))
    (define (value-tree tree)
      (cadr tree))
    (define (left-branch tree)
      (caddr tree))
    (define (right-branch tree)
      (cadddr tree))
    (define (make-tree key value left right)
      (list key value left right))
    (define (set-value-tree! tree value)
      (set-car! (cdr tree) value))
    (define (set-left-branch-tree! tree left)
      (set-car! (cddr tree) left))
    (define (set-right-branch-tree! tree right)
      (set-car! (cdddr tree) right))

    (define (lookup key)
      (define (iter key tree)
        (cond ((null? key) #f)
              ((= key (key-tree tree)) (value-tree tree))
              ((< key (key-tree tree))
               (iter key (left-branch tree)))
              ((> key (key-tree tree))
               (iter key (right-branch tree)))))
      (iter key local-table))

    (define (insert! key value)
      (define (make-branch key value)
        (make-tree key value '() '()))
      (define (iter key value tree)
        (cond ((eq? tree '*table*)
               (set! local-table (make-branch key value)))
              ((= key (key-tree tree))
               (set-value-tree! tree value))
              ((< key (key-tree tree))
               (if (null? (left-branch tree))
                 (set-left-branch-tree! tree (make-branch key value))
                 (iter key value (left-branch tree))))
              ((> key (key-tree tree))
               (if (null? (right-branch tree))
                 (set-right-branch-tree! tree (make-branch key value))
                 (iter key value (right-branch tree))))))
      (iter key value local-table)
      'ok)

    (define (print-table)
      (display local-table)
      (newline))

    (define (dispatch m)
      (cond ((eq? m 'print-table) print-table)
            ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation TABLE" m))))
    dispatch))

(define tb (make-table))
(define lookup (tb 'lookup-proc))
(define insert! (tb 'insert-proc!))
(define print-table (tb 'print-table))

; ex-3.27
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result (lookup x table)))
        (or previously-computed-result
            (let ((result (f x)))
              (insert! x result table)
              result))))))

(define memo-fib
  (memoize (lambda (n)
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (memo-fib (- n 1))
                            (memo-fib (- n 2))))))))
