#lang sicp
(define (gcd x y)
  (cond ((< x y) (gcd y x))
        ((= y 0) x)
        (else (gcd y (remainder x y)))))
(gcd 105 45)