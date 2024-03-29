;;;; 5.2.1 計算機モデル
(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
       (for-each (lambda (register-name)
                         ((machine 'allocate-register) register-name))
                 register-names)
       ((machine 'install-operations) ops)
       ((machine 'install-instruction-sequence)
        (assemble controller-text machine))
       machine))

;; レジスタ
(define (make-register name)
  (let ((contents '*unassigned*))
       (define (dispatch message)
         (cond ((eq? message 'get) contents)
               ((eq? message 'set)
                (lambda (value) (set! contents value)))
               (else
                 (error "Unknown request -- REGISTER" message))))
       dispatch))

(define (get-contents register)
  (register 'get))

(define (set-contents! register value)
  ((register 'set) value))

;; スタック
(define (make-stack)
  (let ((s '())
        (number-pushes 0)
        (max-depth 0)
        (current-depth 0))
    (define (push x)
      (set! s (cons x s))
      (set! number-pushes (+ 1 number-pushes))
      (set! current-depth (+ 1 current-depth))
      (set! max-depth (max current-depth max-depth)))
    (define (pop)
      (if (null? s)
          (error "Empty stack -- POP")
          (let ((top (car s)))
            (set! s (cdr s))
            (set! current-depth (- current-depth 1))
            top)))    
    (define (initialize)
      (set! s '())
      (set! number-pushes 0)
      (set! max-depth 0)
      (set! current-depth 0)
      'done)
    (define (print-statistics)
      (newline)
      (display (list 'total-pushes  '= number-pushes
                     'maximum-depth '= max-depth)))
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialize) (initialize))
            ((eq? message 'print-statistics)
             (print-statistics))
            (else
             (error "Unknown request -- STACK" message))))
    dispatch))
(define (pop stack)
  (stack 'pop))

(define (push stack value)
  ((stack 'push) value))

;; 基本計算機
#|
(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '()))
       (let ((the-ops
               (list
                 (list 'initialize-stack
                       (lambda () (stack 'initialize)))))
             (register-table
               (list (list 'pc pc) (list 'flag flag))))
            (define (allocate-register name)
              (if (assoc name register-table)
                  (error "Multiply defined register: " name)
                  (set! register-table
                        (cons (list name (make-register name))
                              register-table)))
              'register-allocated)
            (define (lookup-register name)
              (let ((val (assoc name register-table)))
                   (if val
                       (cadr val)
                       (error "Unknown register:" name))))
            (define (execute)
              (let ((insts (get-contents pc)))
                   (if (null? insts)
                       'done
                       (begin
                         ((instruction-execution-proc (car insts)))
                         (execute)))))
            (define (dispatch message)
              (cond ((eq? message 'start)
                     (set-contents! pc the-instruction-sequence)
                     (execute))
                    ((eq? message 'install-instruction-sequence)
                     (lambda (seq) (set! the-instruction-sequence seq)))
                    ((eq? message 'allocate-register) allocate-register)
                    ((eq? message 'get-register) lookup-register)
                    ((eq? message 'install-operations)
                     (lambda (ops) (set! the-ops (append the-ops ops))))
                    ((eq? message 'stack) stack)
                    ((eq? message 'operations) the-ops)
                    (else (error "Unknown request -- MACHINE" message))))
            dispatch)))
|#

; https://rskmt.hateblo.jp/entry/20090725/1248513844
(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (instruction-count 0) ;; exercise 5.15
        (trace? #f) ;; exercise 5.16
        (label #f) ;; exercise5.17
        (breakpoint #f)
        (breakpoint-pairs '())
        (inst-count-for-label 0))
    (let ((the-operations
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))
                 (list 'print-stack-statistics
                       (lambda () (stack 'print-statistics)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply define register: " name)
            (set! register-table
                  (cons (list name (make-register name))
                        register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (error "Unknown register: " name))))
      ;; exercise5.19 ======================================
      (define (set-breakpoint label n)
        (if (not (null? breakpoint-pairs))
            (set-cdr! breakpoint-pairs (list (cons label n)))
            (set! breakpoint-pairs (list (cons label n))))
        (print "set-breakpoint"))
      (define (search-breakpoint label n)
        (let ((pair (assoc label breakpoint-pairs)))
          (if pair
              (if (eq? n (cdr pair))
                  pair
                  #f)
              #f)))
      (define (cancel-breakpoint label n)
        (let ((cancel-pair (cons label n)))
          (set! breakpoint-pairs
                (map (lambda (pair)
                       (if (equal? pair cancel-pair) '() pair))
                     breakpoint-pairs))
          (print "cancel-pair: "cancel-pair " breakpoint-pairs: " breakpoint-pairs)))
      (define (cancel-all-breakpoints)
        (set! breakpoint-pairs '())
        (set! breakpoint #f)
        'done)
      ;; ==================================================
      (define (execute)
        (let ((instructions (get-contents pc)))
          (if (null? instructions)
              'done
              (begin
                (if (not (eq? (caaar instructions) 'label)) ;; check-label-tag
                    (begin
                      (inc! inst-count-for-label)
                      (instruction-count-up)
                      (print-trace (caar instructions)))
                    (begin
                      (set! inst-count-for-label 0)
                      (set! label (cadr (caar instructions)))))
                (let ((break-pair (search-breakpoint label inst-count-for-label)))
                  (if break-pair
                      (call/cc (lambda (break)
                                 (set! breakpoint break)
                                 ;(error "***BREAK*** break-point: " break-pair  (caar instructions))))))
                                 (after-break)))))
                ((instruction-execution-proc (car instructions)))
                (execute)))))
      (define (print-trace trace) ;; exercise 5.16
        (if trace?
            (print "label: " label " instruction: " trace)))
      (define (print-stack-statistics) ;; exercise 5.14
        (stack 'print-statistics))
      (define (instruction-count-up) ;; exercise 5.15
        (set! instruction-count (+ instruction-count 1)))
      (define (print-inst-count) ;; exercise 5.15
        (let ((x instruction-count))
          (set! instruction-count 0)
          (print "instruction-count: " x)))
      (define (trace-on) ;; exercise 5.16
        (set! trace? #t))
      (define (trace-off) ;; exercise 5.16
        (set! trace? #f))
      (define (dispatch message)
        (cond [(eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute)]
              [(eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq))]
              [(eq? message 'allocate-register) allocate-register]
              [(eq? message 'get-register) lookup-register]
              [(eq? message 'install-operations)
               (lambda (operations) (set! the-operations
                                          (append the-operations operations)))]
              [(eq? message 'stack) stack]
              [(eq? message 'print-stack) (print-stack-statistics)] ;; exercise 5.14
              [(eq? message 'inst-count-init) (instruction-count-init)] ;; exercise 5.15
              [(eq? message 'print-inst-count) (print-inst-count)] ;; exercise 5.15
              [(eq? message 'trace-on) (trace-on)] ;; exercise 5.16
              [(eq? message 'trace-off) (trace-off)] ;; exercise 5.16
              [(eq? message 'set-label) set-label] ;; exercise 5.17
              [(eq? message 'set-breakpoint) set-breakpoint] ;; exercise5.19
              [(eq? message 'proceed-machine) (breakpoint)] ;; exercise5 5.19
              [(eq? message 'cancel-breakpoint) cancel-breakpoint] ;; exercise5.19
              [(eq? message 'cancel-all-breakpoints) (cancel-all-breakpoints)] ;;exercise5.19
              [(eq? message 'operations) the-operations]
              [else
               (error "Unknown request -- MACHINE" message)]))
      dispatch)))

(define (start machine)
  (machine 'start))

(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))

(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name) value)
  'done)

(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))

;;;; 5.2.2 アセンブラ
(define (assemble controller-text machine)
  (extract-labels controller-text
                  (lambda (insts labels)
                          (update-insts! insts labels machine)
                          insts)))

(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels (cdr text)
                      (lambda (insts labels)
                              (let ((next-inst (car text)))
                                   (if (symbol? next-inst)
                                       (receive insts
                                                (cons (make-label-entry next-inst
                                                                        insts)
                                                      labels))
                                       (receive (cons (make-instruction next-inst)
                                                      insts)
                                                labels)))))))

(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations)))
       (for-each
         (lambda (inst)
                 (set-instruction-execution-proc!
                   inst
                   (make-execution-procedure
                     (instruction-text inst) labels machine
                     pc flag stack ops)))
         insts)))

(define (make-instruction text)
  (cons text '()))

(define (instruction-text inst)
  (car inst))

(define (instruction-execution-proc inst)
  (cdr inst))

(define (set-instruction-execution-proc! inst proc)
  (set-cdr! inst proc))

(define (make-label-entry label-name insts)
  (cons label-name insts))

(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
       (if val
           (cdr val)
           (error "Undefined label -- ASSEMBLE" label-name))))

;;;; 5.2.3 命令の実行手続きの生成
(define (make-execution-procedure inst labels machine
                                  pc flag stack ops)
  (cond ((eq? (car inst) 'assign)
         (make-assign inst machine labels ops pc))
        ((eq? (car inst) 'test)
         (make-test inst machine labels ops flag pc))
        ((eq? (car inst) 'branch)
         (make-branch inst machine labels flag pc))
        ((eq? (car inst) 'goto)
         (make-goto inst machine labels pc))
        ((eq? (car inst) 'save)
         (make-save inst machine stack pc))
        ((eq? (car inst) 'restore)
         (make-restore inst machine stack pc))
        ((eq? (car inst) 'perform)
         (make-perform inst machine labels ops pc))
        (else (error "Unknown instruction type -- ASSEMBLE"
                     inst))))

;; assign 命令
(define (make-assign inst machine labels operations pc)
  (let ((target
          (get-register machine (assign-reg-name inst)))
        (value-exp (assign-value-exp inst)))
       (let ((value-proc
               (if (operation-exp? value-exp)
                   (make-operation-exp
                     value-exp machine labels operations)
                   (make-primitive-exp
                     (car value-exp) machine labels))))
            (lambda () ; assign の実行手続き
                    (set-contents! target (value-proc))
                    (advance-pc pc)))))

(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))

(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))

(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))

;; test 命令
(define (make-test inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
       (if (operation-exp? condition)
           (let ((condition-proc
                   (make-operation-exp
                     condition machine labels operations)))
                (lambda ()
                        (set-contents! flag (condition-proc))
                        (advance-pc pc)))
           (error "Bad TEST instruction -- ASSEMBLE" inst))))

(define (test-condition test-instruction)
  (cdr test-instruction))

;; branch 命令
(define (make-branch inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
       (if (label-exp? dest)
           (let ((insts
                   (lookup-label labels (label-exp-label dest))))
                (lambda ()
                        (if (get-contents flag)
                            (set-contents! pc insts)
                            (advance-pc pc))))
           (error "Bad BRANCH instruction -- ASSEMBLE" inst))))

(define (branch-dest branch-instruction)
  (cadr branch-instruction))

;; goto 命令
(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
       (cond ((label-exp? dest)
              (let ((insts
                      (lookup-label labels
                                    (label-exp-label dest))))
                   (lambda () (set-contents! pc insts))))
             ((register-exp? dest)
              (let ((reg
                      (get-register machine
                                    (register-exp-reg dest))))
                   (lambda ()
                           (set-contents! pc (get-contents reg)))))
             (else (error "Bad GOTO instruction -- ASSEMBLE"
                          inst)))))

(define (goto-dest goto-instruction)
  (cadr goto-instruction))

;; その他の命令
(define (make-save inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
       (lambda ()
               (push stack (get-contents reg))
               (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
       (lambda ()
               (set-contents! reg (pop stack))
               (advance-pc pc))))

(define (stack-inst-reg-name stack-instruction)
  (cadr stack-instruction))

(define (make-perform inst machine labels operations pc)
  (let ((action (perform-action inst)))
       (if (operation-exp? action)
           (let ((action-proc
                   (make-operation-exp
                     action machine labels operations)))
                (lambda ()
                        (action-proc)
                        (advance-pc pc)))
           (error "Bad PERFORM instruction -- ASSEMBLE" inst))))

(define (perform-action inst) (cdr inst))

;; 部分式の実行手続き
(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp)
         (let ((c (constant-exp-value exp)))
              (lambda () c)))
        ((label-exp? exp)
         (let ((insts
                 (lookup-label labels
                               (label-exp-label exp))))
              (lambda () insts)))
        ((register-exp? exp)
         (let ((r (get-register machine
                                (register-exp-reg exp))))
              (lambda () (get-contents r))))
        (else
          (error "Unknown expression type -- ASSEMBLE" exp))))

(define (register-exp? exp) (tagged-list? exp 'reg))

(define (register-exp-reg exp) (cadr exp))

(define (constant-exp? exp) (tagged-list? exp 'const))

(define (constant-exp-value exp) (cadr exp))

(define (label-exp? exp) (tagged-list? exp 'label))

(define (label-exp-label exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))

(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
        (aprocs
          (map (lambda (e)
                       (make-primitive-exp e machine labels))
               (operation-exp-operands exp))))
       (lambda ()
               (apply op (map (lambda (p) (p)) aprocs)))))

(define (operation-exp? exp)
  (and (pair? exp) (tagged-list? (car exp) 'op)))

(define (operation-exp-op operation-exp)
  (cadr (car operation-exp)))

(define (operation-exp-operands operation-exp)
  (cdr operation-exp))

(define (lookup-prim symbol operations)
  (let ((val (assoc symbol operations)))
       (if val
           (cadr val)
           (error "Unknown operation -- ASSEMBLE" symbol))))
