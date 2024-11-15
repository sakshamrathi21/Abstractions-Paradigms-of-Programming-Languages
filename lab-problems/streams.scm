#lang sicp

; =============== CS339 Autumn 2024 =================

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))

(define-syntax delay
  (syntax-rules ()
    ((delay expr) (memo-proc (lambda () expr)))))

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream a b) (cons a (delay b)))))

(define (stream-car s)
  (car s))

(define (force promise) (promise))

(define (stream-cdr s) (force (cdr s)))

(define the-empty-stream '())

(define (stream-null? s) (eq? s the-empty-stream))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                         (stream-map proc (stream-cdr s)))))

(define (stream-filter pred s)
  (cond ((stream-null? s) the-empty-stream)
        ((pred (stream-car s))
         (cons-stream (stream-car s)
                      (stream-filter pred (stream-cdr s))))
        (else (stream-filter pred (stream-cdr s)))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low
                   (stream-enumerate-interval (+ low 1) high))))

(define (enumerate-interval low high)
  (if (> low high)
    nil
    (cons low (enumerate-interval (+ low 1) high))))

(define (map f l)
  (if (null? l)
    nil
    (cons (f (car l))
          (map f (cdr l)))))

(define (filter pred l)
  (cond ((null? l) nil)
        ((pred (car l)) (cons (car l) (filter pred (cdr l))))
        (else (filter pred (cdr l)))))

(define (square x)
  (* x x))

(define (prime? n)
  (define (divides? a b) (= (remainder b a) 0))
  (define (smallest-divisor n) (find-divisor n 2))
  (define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
  (= n (smallest-divisor n)))

;; (car (cdr
;;       (filter prime?
;;               (enumerate-interval 10000 1000000))))

;; (stream-car (stream-cdr
;;               (stream-filter
;;                 prime?
;;                   (stream-enumerate-interval 10000 1000000))))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))

(define (stream-ref n s)
  (if (= n 0)
      (stream-car s)
      (stream-ref (- n 1) (stream-cdr s))))

(define (divisible? x y) (= (remainder x y) 0))
   
(define nd5
  (stream-filter
   (lambda (x) (not (divisible? x 5)))
   integers))
   
(define (fibgen a b)
  (cons-stream a (fibgen b (+ a b))))
(define fibs (fibgen 0 1))

(define (sieve s)
  (cons-stream
    (stream-car s)
    (sieve (stream-filter
             (lambda (x) (not (divisible? x (stream-car s))))
           (stream-cdr s)))))

(define primes (sieve (integers-starting-from 2)))

(define primes2
  (cons-stream 2
               (stream-filter prime2? (integers-starting-from 3))))

(define (prime2? n)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n) true)
          ((divisible? n (stream-car ps)) false)
          (else (iter (stream-cdr ps)))))
  (iter primes2))

(define (average x y) (/ (+ x y) 2))
(define (sqrt-improve guess x)
    (average guess (/ x guess)))

(define (sqrt x tolerance)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) tolerance))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (sqrt-improve guess x))))
  (sqrt-iter 1.0))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

(define sqrt2s (sqrt-stream x))
