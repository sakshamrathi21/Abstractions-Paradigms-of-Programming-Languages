#lang sicp
(/ (+ 5
      4
      (- 2
         (- 3
            (+ 6
               (/ 4 5)))))
   (* 3
      (- 6 2)
      (- 2 7)))

(define (max a b)
  (if (>= a b) a b))
(define (leastTwo a b c)
  (- (+ a b c) (max a (max b c))))

(define (modexp x y n)
  (if (= y 0)
      1
      (remainder (* (modexp x (- y 1) n) x) n)))

(define (cube x)
  (* x x x))
(define (square x)
  (* x x))
(define (cbrt x)
  (define (improve guess)
    (/ (+ (/ x (square guess)) (* 2 guess)) 3))
  (define (good-enough? guess)
    (< (abs (- (cube guess) x)) 0.00001))
  (define (cbrt-iter guess)
    (if (good-enough? guess)
        guess
        (cbrt-iter (improve guess))))
  (cbrt-iter 1.0))

(define (count-change amount)
  (define (first-denomination kind-of-coins)
    (cond ((= kind-of-coins 1) 1)
          ((= kind-of-coins 2) 2)
          ((= kind-of-coins 3) 5)
          ((= kind-of-coins 4) 10)
          ((= kind-of-coins 5) 20)
          ((= kind-of-coins 6) 50)
          ((= kind-of-coins 7) 100)
          ((= kind-of-coins 8) 500)
          ((= kind-of-coins 9) 2000)
          (else 10000000)))
  (define (cc amount kind-of-coins)
    (cond ((= amount 0) 1)
          ((or (< amount 0) (= kind-of-coins 0)) 0)
          (else (+ (cc amount (- kind-of-coins 1))
             (cc (- amount (first-denomination kind-of-coins)) kind-of-coins)))))
  (cc amount 9)) 