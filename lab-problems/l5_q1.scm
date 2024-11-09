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


(define (enumerate-interval a b)
  (if (> a b)
      nil
      (cons a (enumerate-interval (+ a 1) b))))

(define (filter pred? st)
  (cond ((null? st) nil)
        ((pred? (car st)) (cons (car st) (filter pred? (cdr st))))
        (else (filter pred? (cdr st)))))

(define (map f st)
  (if (null? st) nil
      (cons (f (car st)) (map f (cdr st)))))

(define (prime? n)
  (define (prime-iter i)
    (cond ((> (* i i) n) #t)
          ((= (remainder n i) 0) #f)
          (else (prime-iter (+ i 1)))))
  (prime-iter 2))

(define (stream-ref st i)
  (if (= i 0) (stream-car st)
      (stream-ref (stream-cdr st) (- i 1))))


(define (ref st i)
  (if (= i 0) (car st)
      (ref (cdr st) (- i 1))))

;(stream-ref (stream-filter prime? (stream-enumerate-interval 10000 1000000)) 1)
;(ref (filter prime? (enumerate-interval 10000 1000000)) 1)