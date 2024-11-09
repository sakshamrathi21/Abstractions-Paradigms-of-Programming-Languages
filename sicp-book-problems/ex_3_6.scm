#lang sicp
(define (rand-m m)
  (define random-init 1)
  (define (rand)
    (set! random-init (modulo (+ (* random-init 1664525) 1013904223) 4294967296))
    random-init)
  (cond ((eq? m 'generate) (rand))
        ((eq? m 'reset) (lambda (new-value) (set! random-init new-value)))
        (else (error "Unknown operation" m))))

(rand-m 'generate)
((rand-m 'reset) 1000)
(rand-m 'generate)
