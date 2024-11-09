#lang sicp
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))
(define (square x) (* x x))
(define (experiment)
  (define xr (random-in-range 2 8))
  (define yr (random-in-range 4 10))
  (<= (+ (square (- xr 5)) (square (- yr 7))) 9))
(define (estimate-pi trials)
  (* 4 (monte-carlo trials experiment)))