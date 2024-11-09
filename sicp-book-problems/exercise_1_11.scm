#lang sicp
(define (recursive-f n)
  (if (<  n 3)
      n
      (+ (recursive-f (- n 1)) (* 2 (recursive-f (- n 2))) (* 3 (recursive-f (- n 3))))))
(recursive-f 20)


(define (iterative-f n)
  (define (iter-f a b c cnt)
    (if (< cnt 0)
       a
       (iter-f (+ a (* 2 b) (* 3 c)) a b (- cnt 1))))
  (iter-f 2 1 0 (- n 3)))

(iterative-f 20)