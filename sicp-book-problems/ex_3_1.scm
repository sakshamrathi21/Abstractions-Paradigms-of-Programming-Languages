#lang sicp
(define (make-accumulator curr)
  (define balance curr)
  (lambda (x) (begin (set! balance (+ balance x)) balance)))

(define A (make-accumulator 5))
