#lang sicp
(define (make-monitored f)
  (define counter 0)
  (define (mf x)
    (cond ((eq? x `how-many-calls?) counter)
          ((eq? x `reset-count) (begin (set! counter 0) counter))
          (else (begin (set! counter (+ counter 1)) (f x)))))
  mf)

(define (square x) (* x x))
(define s (make-monitored square))