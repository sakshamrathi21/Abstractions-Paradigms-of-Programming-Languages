#lang sicp
(define (cube x) (* x x x))
(define (cbrt x)
  (define (improve y)
    (/ (+ (/ x (* y y)) (* 2 y)) 3))
  (define (good-enough? guess)
    (< (abs (- (cube guess) x)) 0.001))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))
(cbrt 1000)