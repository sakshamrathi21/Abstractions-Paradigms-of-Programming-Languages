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

(define (append-stream st1 st2)
  (if (stream-null? st1)
      st2
      (cons-stream (stream-car st1) (append-stream (stream-cdr st1) st2))))

(define (make-rational a b) (list a b))

(define (enumerate-rational sum curr)
  (if (= sum curr) the-empty-stream
      (cons-stream (make-rational curr (- sum curr)) (enumerate-rational sum (+ curr 1)))))

(define (rational-num-iter curr sum)
  (cond ((= curr sum) (rational-num-iter 1 (+ sum 1)))
        (else (cons-stream (make-rational curr (- sum curr)) (rational-num-iter (+ curr 1) sum)))))

(define rational-numbers (rational-num-iter 1 2))
(define (stream-ref st i)
  (if (= i 0) (stream-car st)
      (stream-ref (stream-cdr st) (- i 1))))

(define (print-rational rational-numbers i tot)
  (if (> i tot) (newline)
      ((display (car (stream-car rational-numbers)))
       (display "/")
       (display (cadr (stream-car rational-numbers)))
       (newline)
       (print-rational (stream-cdr rational-numbers) (+ i 1) tot))))

(stream-ref rational-numbers 200)
;(print-rational rational-numbers 0 19)
