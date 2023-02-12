(add-load-path "./packages/" :relative)
(load "analyze.scm")

; (define (factorial n) (if (= n 1) 1 (* (factorial (- n 1)) n)))

(driver-loop)
