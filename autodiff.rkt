#lang racket
(provide (all-defined-out))
;; given
(struct num (value grad)
    #:property prop:custom-write
    (lambda (num port write?)
        (fprintf port (if write? "(num ~s ~s)" "(num ~a ~a)")
            (num-value num) (num-grad num))))

; 3.2 get-value
(
    define get-value (lambda (x) (
    cond [(num? x) (num-value x)]
         [(= (length x) 0) '()]
         (else (cons (cadr (car x)) (get-value (cdr x)) ))))
)

; 3.1 get-grad
(
    define get-grad (lambda (x) (
    cond [(num? x) (num-grad (x))]
         [(= (length x) 0) '()]
         (else (cons (caddr (car x)) (get-grad (cdr x)) ))))
)

; 4.1 add
(
    define add (lambda args (
        cond [(= (length args) 2) (num (+(num-value(car args)) (num-value(cadr args))) (+(num-grad(car args)) (num-grad(cadr args))))]
             [else (add (car args) (apply add (cdr args)))]
    ))
)

; 4.2 mul
(
    define mul (lambda args (
        cond [(= (length args) 2) (num (*(num-value(car args)) (num-value(cadr args))) (+(*(num-value(car args)) (num-grad(cadr args))) (*(num-value(cadr args)) (num-grad(car args)))))]
             [else (mul (car args) (apply mul (cdr args)))]
    ))
)

; 4.3 sub
(
    define sub (lambda args 
        (num (-(num-value(car args)) (num-value(cadr args))) (-(num-grad(car args)) (num-grad(cadr args))))
    )
)

; given
(define relu (lambda (x) (if (> (num-value x) 0) x (num 0.0 0.0))))
; given
(define mse (lambda (x y) (mul (sub x y) (sub x y))))

; 5.1 create-hash
(
    define create-hash (lambda (names values var) (
       apply hash (foldr append '() (concat names (make-nums names values var)))
    ))
)

(
    define concat (lambda (keys values) (map (lambda (k v) `(,k,v)) keys values))
)

(
    define make-nums (lambda (names values var) (
        cond [(= (length values) 0) '()]
             [(= (length names) 0) '()]
             [(equal? (car names) var) (cons (num (car values) 1.0) (make-nums(cdr names) (cdr values) var))]
             [else (cons (num (car values) 0.0) (make-nums(cdr names) (cdr values) var))]
    ))
)

; 5.2 parse
(
    define parse (lambda (hash expr) (
        cond [(equal? expr '+)  'add ]
             [(equal? expr '-)  'sub ]
             [(equal? expr '*)  'mul ]
             [(equal? expr 'mse)  'mse ]
             [(equal? expr 'relu)  'relu ]
             [(number? expr) (num expr 0.0) ]
             [(and (list? expr) (= (length expr) 0)) '()]
             [(and (list? expr) (> (length expr) 0)) (cons (parse hash (car expr)) (parse hash (cdr expr)))]
             [else (hash-ref hash expr)]
    ))
)

; 5.3 grad
(
    define grad (lambda (names values var expr)(
        num-grad (eval (parse (create-hash names values var) expr))
    ))
)

; 5.4 partial-grad
(
    define partial-grad (lambda (names values vars expr)(
        change-values (all-grads names values vars expr) names vars
    ))
)

(
    define all-grads (lambda (names values vars expr)(
        map (
            lambda (name) (
                grad names values name expr
            )
        ) names
    ))
)

(
    define change-values (lambda (grads names vars)(
        map (
            lambda (name grad) (
                cond [(member name vars) grad]
                     [else 0.0]
            )
        ) names grads
    ))
)

; cond [(= (length names) 0) '()]
;              [(member (car names) vars) (cons (grad names values (car(member (car names) vars)) expr) (partial-grad (cdr names) values vars expr))]
;              [else (cons 0.0 (partial-grad (cdr names) values vars expr))]