#lang sicp
(define (iterative-improve good-enough? improve)
  (define (iter guess)
    (if (good-enough? guess)
        guess
        (iter (improve guess))))
  iter)
(define (square x) (* x x))
(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (/ (+ (/ x guess) guess) 2))
  ((iterative-improve good-enough? improve) 1.0))

(define (product-series f a b next)
  (if (>  a b)
      1
      (* (f a) (product-series f (next a) b next))))
(define (factorial n)
  (product-series (lambda (x) x) 1 n (lambda (x) (+ x 1))))

(define (fact-iter n)
  (define (prod-ser-iter f a b v next)
    (if (>  a b)
        v
        (prod-ser-iter f (next a) b (* (f a) v) next)))
  (prod-ser-iter (lambda (x) x) 1 n 1 (lambda (x) (+ x 1))))

(define (compose f g)
  (lambda (x) (f (g x))))
(define (inc x) (+ x 1))

(define zero (lambda () '()))
(define (succ x) (lambda () x))
(define (pred x) (x))
(define one (succ zero))
(define two (succ one))
(define three (succ two))
(define four (succ three))
(define five (succ four))
(define six (succ five))
(define (is-zero? x) (null? (x)))
(define (is-equal? x y)
  (cond ((is-zero? x) (is-zero? y))
        ((is-zero? y) (is-zero? x))
        (else (is-equal? (pred x) (pred y)))))
(is-equal? zero zero)
(is-equal? zero one)
(is-equal? four (succ (succ (succ (succ zero)))))
(is-equal? two (pred (succ two)))
(define (add-church x y)
  (if (is-zero? y)
      x
      (add-church (succ x) (pred y))))
(define (subtract-church x y)
  (if (is-zero? y)
      x
      (subtract-church (pred x) (pred y))))
(define (multiply-church x y)
  (if (is-zero? y)
      zero
      (add-church x (multiply-church x (subtract-church y one)))))
        