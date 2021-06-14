#lang racket
(provide (all-defined-out))
;; given
(struct num (value grad)
    #:property prop:custom-write
    (lambda (num port write?)
        (fprintf port (if write? "<num ~s ~s>" "<num ~a ~a>")
            (num-value num) (num-grad num))))

(
    define get-value (lambda (x) (
    cond [(num? x) (num-value x)]
         [(= (length x) 0) '()]
        (else (cons (cadr (car x)) (get-value (cdr x)) ))))
)

(
    define get-grad (lambda (x) (
    cond [(num? x) (num-grad x)]
         [(= (length x) 0) '()]
        (else (cons (caddr (car x)) (get-grad (cdr x)) ))))
)
;; given
; (define relu (lambda (x) (if (> (num-value x) 0) x (num 0.0 0.0))))
;; given
; (define mse (lambda (x y) (mul (sub x y) (sub x y))))
