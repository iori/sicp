(define (memq item x)
  (cond ((null? x) #f)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

(print
  (memq 'apple '(x (apple sauce) y apple pear))
)

; ex-2.53

(print
  (list 'a 'b 'c)
)

(print
  (list (list 'george))
)

(print
  (cdr '((x1 x2) (y1 y2)))
)

(print
  (cadr '((x1 x2) (y1 y2)))
)

(print
  (pair? (car '(a short list)))
)

(print
  (memq 'red '((red shoes) (blue socks)))
)

(print
  (memq 'red '(red shoes blue socks))
)

; ex-2.54
(define (equal? a b)
  (cond ((and (not (pair? a)) (not (pair? b))) (eq? a b))
        ((and (pair? a) (pair? b))
         (and (equal? (car a) (car b)) (equal? (cdr a) (cdr b))))
        (else #f)))

; ex-2.55
; gosh$ (car (quote (quote abracadabra)))
; quote
