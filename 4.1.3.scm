(add-load-path "./packages/" :relative)
(load "repl.scm")

; ex-4.11
; repl.scmに記載

;; defaultのenv
; リストのペア
; (
;  (false true car cdr cons null? + - * =)
;  #f
;  #t
;  (primitive #<subr (car obj)>)
;  (primitive #<subr (cdr obj)>)
;  (primitive #<subr (cons obj1 obj2)>)
;  (primitive #<subr (null? obj)>)
;  (primitive #<subr (+ :rest args)>)
;  (primitive #<subr (- arg1 :rest args)>)
;  (primitive #<subr (* :rest args)>)
;  (primitive #<subr (= arg0 arg1 :rest args :optarray oarg)>)
; )
;
;; 4.11のenv
; 束縛のリスト
; (
;  (car primitive #<subr (car obj)>)
;  (false . #f)
;  (true . #t)
;  (cdr primitive #<subr (cdr obj)>)
;  (cons primitive #<subr (cons obj1 obj2)>)
;  (null? primitive #<subr (null? obj)>)
;  (+ primitive #<subr (+ :rest args)>)
;  (- primitive #<subr (- arg1 :rest args)>)
;  (* primitive #<subr (* :rest args)>)
;  (= primitive #<subr (= arg0 arg1 :rest args :optarray oarg)>)
; )

(driver-loop)
