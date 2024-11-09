#lang sicp

(define-syntax delay
  (syntax-rules ()
    ((delay expr) (lambda () expr))))

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream a b) (cons a (delay b)))))

(define (stream-car s)
  (car s))

(define (force promise) (promise))

(define (stream-cdr s) (force (cdr s)))

(define the-empty-stream '())

(define (stream-null? s) (eq? s the-empty-stream))

(define (stream-enumerate-interval a b)
  (if (> a b)
      the-empty-stream
      (cons-stream a (stream-enumerate-interval (+ a 1) b))))

(define (stream-filter pred? st)
  (cond ((stream-null? st) the-empty-stream)
        ((pred? (stream-car st)) (cons-stream (stream-car st) (stream-filter pred? (stream-cdr st))))
        (else (stream-filter pred? (stream-cdr st)))))

(define (stream-map f st)
  (if (stream-null? st) the-empty-stream
      (cons-stream (f (stream-car st)) (stream-map f (stream-cdr st)))))

(define (divisible? n a) (= (remainder n a) 0))

(define (integers-starting-from x) (cons-stream x (integers-starting-from (+ x 1))))

(define (square x) (* x x))
(define (prime2? x)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) x) true)
          ((divisible? x (stream-car ps)) false)
          (else (iter (stream-cdr ps)))))
  (iter primes))

(define (stream-ref st i)
  (if (= i 0) (stream-car st)
      (stream-ref (stream-cdr st) (- i 1))))

(define primes (cons-stream 2 (stream-filter prime2? (integers-starting-from 3))))

(define (sqrt-improve guess x)
  (/ (+ guess (/ x guess)) 2))

(define (sqrt-stream x)
  (define guesses (cons-stream 1.0 (stream-map (lambda (guess) (sqrt-improve guess x)) guesses)))
  guesses)
