(load "./2.4.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; https://sicp.sourceacademy.org/chapters/2.5.1.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))

  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))

  ; ex-2.79追加
  (put 'equ? '(scheme-number scheme-number) =)
  ; ex-2.80追加
  (put 'zero? '(scheme-number)
       (lambda (x) (= x 0)))
  ; ex-2.81追加
  (put 'exp '(scheme-number scheme-number)
       (lambda (x y) (tag (expt x y))))

  ; ex-2.83追加
  (put 'raise '(scheme-number)
       (lambda (x) (make-rational x 1)))

  (put 'make 'scheme-number
       (lambda (x) (tag x)))

  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))

  (define (make-rat n d)
    (print "make-rat " n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))

  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))

  ; ex-2.79追加
  (define (equ? x y)
    (= (* (numer x) (denom y)) (* (numer y) (denom x))))
  ; ex-2.80追加
  (define (zero? x)
    (= 0 (numer x)))

  ; ex-2.83追加
  (define (raise-rat x)
    (make-real (/ (* (numer x) 1.0) (denom x))))

  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))

  ; ex-2.79追加
  (put 'equ? '(rational rational) equ?)
  ; ex-2.80追加
  (put 'zero? '(rational) zero?)
  ; ex-2.83追加
   (put 'raise '(rational)
       (lambda (x) (make-real (/ (* (numer x) 1.0) (denom x)))))
  ; (put 'raise '(rational)
  ;     (lambda (x) (raise-rat x)))

  ; ex-2.85追加
  (put 'project 'rational
       (lambda (x) (make-scheme-number (round (/ (numer x) (denom x))))))

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))


; ex-2.77
; (define z3 (make-complex-from-real-imag 1 2))
; (magnitude z3)
; apply-genericは2回呼び出される.
;
; * 最初にdefine z3が実行される
; * z3は(complex rectangular 1 . 2)になる
;
; * 2.4.scmのdefine magnitudeが実行される
;   * zは(complex rectangular 1 . 2)
; * apply-genericが呼び出される
; * proc実行されてcomplexタグのmagnitudeが呼び出される
; * complexのmagnitudeから2.4.scmで定義されたdefine magnitudeがもう一度呼び出される
;   * zは(rectangular 1 . 2)
; * apply-genericが呼び出される
; * proc実行されてrectangularタグのmagnitudeが呼び出される
; * rectangularのmagnitudeが実行されて計算される

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ; (print "complex make-from-real-imag")
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))

  ; ex-2.79追加
   (define (equ? x y)
     (and (= (real-part x) (real-part y)) (= (imag-part x) (imag-part y))))
  ; ex-2.80追加
   (define (zero? x)
     (and (= (real-part x) 0) (= (imag-part x) 0)))

  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))

  ; ex-2.77追加
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)

  ; ex-2.79追加
  (put 'equ? '(complex complex) equ?)

  ; ex-2.80追加
  (put 'zero? '(complex) zero?)

  ; ex-2.85追加
  (put 'project 'complex
       (lambda (x) (make-real (real-part x))))

  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(install-scheme-number-package)
(install-rational-package)
(install-complex-package)

; (define z3 (make-complex-from-real-imag 1 2))
; (print z3)
; (print "****************************")
; (print (magnitude z3))

; ex-2.78
(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number)
    contents
    (cons type-tag contents)))

(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else "Bad tagged datum - CONTENTS" datum)))

; ex-2.79
(define (equ? x y) (apply-generic 'equ? x y))

#|
(define sn1 (make-scheme-number 1))
(define sn2 (make-scheme-number 3))
(define sn3 (make-scheme-number 3))
(print (equ? sn1 sn2))
(print (equ? sn2 sn3))

(define r1 (make-rational 1 2))
(define r2 (make-rational 1 1))
(define r3 (make-rational 1 1))
(print (equ? r1 r2))
(print (equ? r2 r3))

(define c1 (make-complex-from-real-imag 1 2))
(define c2 (make-complex-from-real-imag 1 1))
(define c3 (make-complex-from-real-imag 1 1))
(print (equ? c1 c2))
(print (equ? c2 c3))

; ex-2.80

(define (zero? x) (apply-generic 'zero? x))
(define sn1 (make-scheme-number 1))
(define sn2 (make-scheme-number 0))
(print (zero? sn1))
(print (zero? sn2))

(define r1 (make-rational 1 0))
(define r2 (make-rational 0 2))
(print (zero? r1))
(print (zero? r2))

(define c1 (make-complex-from-real-imag 1 0))
(define c2 (make-complex-from-real-imag 0 1))
(define c3 (make-complex-from-real-imag 0 0))
(print (zero? c1))
(print (zero? c2))
(print (zero? c3))
|#

; 2.5.2 異なる型のデータの統合
(define (add-complex-to-schemenum z x)
  (make-from-real-imag (+ (real-part z) x)
                       (imag-part z)))

(put 'add '(complex scheme-number)
     (lambda (z x) (tag (add-complex-to-schemenum z x))))

(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; put-coercion,get-coercion for SICP 2.5.2
; https://gist.github.com/kinoshita-lab/b76a55759a0d0968cd97
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define coercion-list '())

(define (clear-coercion-list)
  (set! coercion-list '()))

(define (put-coercion type1 type2 item)
  (if (get-coercion type1 type2) coercion-list
    (set! coercion-list
      (cons (list type1 type2 item)
            coercion-list))))

(define (get-coercion type1 type2)
  (define (get-type1 listItem)
    (car listItem))
  (define (get-type2 listItem)
    (cadr listItem))
  (define (get-item listItem)
    (caddr listItem))
  (define (get-coercion-iter list type1 type2)
    (if (null? list) #f
      (let ((top (car list)))
        (if (and (equal? type1 (get-type1 top))
                 (equal? type2 (get-type2 top))) (get-item top)
          (get-coercion-iter (cdr list) type1 type2)))))
  (get-coercion-iter coercion-list type1 type2))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(put-coercion 'scheme-number 'complex scheme-number->complex)

(define (apply-generic op . args)
  ; (print op)
  ; (print args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      ; (print proc)
      (if proc
        (apply proc (map contents args))
        (if (= (length args) 2)
          (let ((type1 (car type-tags))
                (type2 (cadr type-tags))
                (a1 (car args))
                (a2 (cadr args)))
            (let ((t1->t2 (get-coercion type1 type2))
                  (t2->t1 (get-coercion type2 type1)))
              ; (print t1->t2)
              ; (print t2->t)
              (cond (t1->t2
                      ; (print "hoge")
                      (apply-generic op (t1->t2 a1) a2))
                    (t2->t1
                      ; (print "fuga")
                      (apply-generic op a1 (t2->t1 a2)))
                    (else
                      (error "No method for these types"
                             (list op type-tags))))))
          (error "No method for these types"
                 (list op type-tags)))))))

; ex-2.81
(define (scheme-number->scheme-number n ) n)
(define (complex->complex z) z)
(put-coercion 'scheme-number 'scheme-number scheme-number->scheme-number)
(put-coercion 'complex 'complex complex->complex)
(define (exp x y) (apply-generic 'exp x y))

; a.
(define c1 (make-complex-from-real-imag 1 2))
(define c2 (make-complex-from-real-imag 1 1))
; (exp c1 c2)
; procが常に#fなので無限ループに陥る

; b.
; 正しくない, このままだと動かない

; c.
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (if (= (length args) 2)
          (let ((type1 (car type-tags))
                (type2 (cadr type-tags))
                (a1 (car args))
                (a2 (cadr args)))
            (if (eq? type1 type2) ; 追加
              (error "eq type1 type2") ; 追加
              (let ((t1->t2 (get-coercion type1 type2))
                    (t2->t1 (get-coercion type2 type1)))
                (cond (t1->t2
                        (apply-generic op (t1->t2 a1) a2))
                      (t2->t1
                        (apply-generic op a1 (t2->t1 a2)))
                      (else
                        (error "No method for these types"
                               (list op type-tags)))))))
          (error "No method for these types"
                 (list op type-tags)))))))

; (exp c1 c2)
; *** ERROR: eq type1 type2

; ex-2.82
(define (apply-generic op . args)
  (define (coercion args my-type)
    (if (null? args)
      ()
      (let ((a1 (car args)))
        (let ((t2->t1 (get-coercion (type-tag a1) my-type)))
          ; (print "t2->t1 " t2->t1)
          (if t2->t1
            (cons (t2->t1 a1) (coercion (cdr args) my-type))
            (cons a1 (coercion (cdr args) my-type)))))))

  (define (search types)
    ; (print "---------------------------")
    (let ((my-type (car types)))
      ; (print my-type)
      (let ((my-args (coercion args my-type)))
        ; (print "my-args " my-args)
        (let ((proc (get op (map type-tag my-args))))
          ; (print "proc " proc)
          (if proc
            (apply proc (map contents my-args))
            (search (cdr types)))))))

  ; (print "types" (map type-tag args))
  (search (map type-tag args)))

(put 'add '(scheme-number scheme-number scheme-number)
     (lambda (x y z) (+ x y z)))

(put 'add '(complex complex complex)
     (lambda (x y z) (add (add (cons 'complex x)
                               (cons 'complex y))
                          (cons 'complex z))))

(define (add . args)
  (apply apply-generic (cons 'add args)))

(define z (make-complex-from-real-imag 3 4))

(put-coercion 'scheme-number 'complex scheme-number->complex)

; (print (add z 2 2))
; (complex rectangular 7 . 4)

; ex-2.83
(define (raise x) (apply-generic 'raise x))

;; 実数算術演算パッケージ
(define (install-real-package)
  (define (tag x)
    (print "real-tag")
    (attach-tag 'real x))
  (put 'add '(real real)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(real real)
       (lambda (x y) (tag (* x y))))
  (put 'div '(real real)
       (lambda (x y) (tag (/ x y))))
  (put 'equ? '(real real)
       (lambda (x y) (= x y)))
  (put '=zero? '(real)
       (lambda (x) (= x 0.0)))
  (put 'make 'real
       (lambda (x) (tag x)))


  (put 'raise '(real)
       (lambda (x) (make-complex-from-real-imag x 0)))

  ; ex-2.85追加
  (put 'project 'real
       (lambda (x)
         (let ((rat (rationalize
                      (inexact->exact x) 1/100)))
           (make-rational
             (numerator rat)
             (denominator rat)))))

  'done)

(define (make-real n)
  ((get 'make 'real) n))

(install-real-package)

; (print (raise 1))
; (print (raise (make-rational 1 2)))
; (print (raise (make-real 1.5)))

; (define i 2)
; (print (raise i))
; (print (raise (raise i)))
; (print (raise (raise (raise i))))

; ex-2.84
(define (higher-type x y)
  (let ((tower '(complex real rational scheme-number))) ;; 型の塔
    (define (iter twr)
      (if (null? twr)
        #f
        (cond ((eq? x (car twr)) x)
              ((eq? y (car twr)) y)
              (else (iter (cdr twr))))))
    (iter tower)))

(define (coerce-higher-type items)
  (let ((item1 (car items))
        (item2 (cadr items)))
    (let ((type1 (type-tag item1))
          (type2 (type-tag item2)))
      (if (eq? type1 type2)
        items
        (let ((tag (higher-type type1 type2)))
          (if (eq? tag type1)
            (coerce-higher-type (list item1 (raise item2)))
            (coerce-higher-type (list (raise item1) item2))))))))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (if (= (length args) 2)
          (let ((type1 (car type-tags))
                (type2 (cadr type-tags)))
            (if (eq? type1 type2)
              (error "E1. No method for these types" (list op type-tags))
              (let ((coerced-args (coerce-higher-type args)))
                (let ((proc (get op (map type-tag coerced-args))))
                  (if proc
                    (apply proc (map contents coerced-args))
                    (error "E2.No method for these types" (list op type-tags)))))))
          (error "E3. No method for these types" (list op type-tags)))))))

; ex-2.85
(define (drop x)
  (print "******************************")
  (print "*******drop " x)
  (let ((project-proc (get 'project (type-tag x))))
    (if project-proc
      (let ((project-number (project-proc (contents x))))
        (print "project-number " project-number)
        (print "raise-project-number " (raise project-number))

        (if (equ? project-number (raise project-number))
          (drop project-number)
          x))
      x)))

(define (apply-generic op . args)
  (print "**********apply " op args)

  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (print "proc " proc)
      (if proc
        ; (apply proc (map contents args)) ;; drop
        (drop (apply proc (map contents args))) ;; drop
        (if (= (length args) 2)
          (let ((type1 (car type-tags))
                (type2 (cadr type-tags)))
            (if (eq? type1 type2)
              (error "E1. No method for these types" (list op type-tags))
              (let ((coerced-args (coerce-higher-type args)))
                (let ((proc (get op (map type-tag coerced-args))))
                  (if proc
                    (drop (apply proc (map contents coerced-args))) ;; drop
                    (error "E2.No method for these types" (list op type-tags)))))))
          (error "E3. No method for these types" (list op type-tags)))))))

(define rat (make-rational 2 4))
(define rel (make-real 3.0))
(define cpx (make-complex-from-real-imag 2 0))

; (print (drop rat))
; (print (drop cpx))
; (print (add rat rel))
; (print (add cpx rel))

; the tower
; raise ↑
; project ↓
;
; 複素数(complex)
; ↑
; 実数(real)
; ↑
; 有理数(rational)
; ↑
; 整数(scheme-number)
