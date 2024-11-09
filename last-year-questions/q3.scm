#lang sicp
(define (filter pred l)
  (cond ((null? l) '())
        ((pred (car l)) (cons (car l) (filter pred (cdr l))))
        (else (filter pred (cdr l)))))

(define-syntax same-parity
  (syntax-rules ()
    ((same-parity x) (list x))
    ((same-parity x y) 
     (if (= (remainder x 2) (remainder y 2)) 
         (list x y) 
         (list x)))
    ((same-parity x y . z) 
     (if (= (remainder x 2) (remainder y 2)) (cons x (cons y (cdr (same-parity x . z)))) (cons x (cdr (same-parity x . z))))))) 
