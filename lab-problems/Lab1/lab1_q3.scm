#lang sicp
(define (mod x y) (cond ((< x y) x)
                        (else (mod (- x y) y))))
(define (modexp x y n) (cond ((= y 0) 1)
                             (else (mod (* (modexp x (- y 1) n) x) n))))

(modexp 2 3 8)