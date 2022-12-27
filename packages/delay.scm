(define-macro (delay exp) `(lambda () ,exp))
(load "stream.scm")
