#lang sicp
(define (max a b c) (cond
                      ((and (>= a b) (>= a c)) a)
                      ((and (>= b c) (>= b a)) b)
                      ((and (>= c b) (>= c a)) c)))
(define (leastTwo a b c) (+ a
                            b
                            c
                            (- (max a b c))))
(leastTwo 8 2 3)