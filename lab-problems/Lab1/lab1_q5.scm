#lang sicp
(define (count-change amount) (cc amount 9)) (define (cc amount kinds-of-coins)
(cond ((= amount 0) 1)
((or (< amount 0) (= kinds-of-coins 0)) 0) (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination
                         kinds-of-coins))
kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
      ((= kinds-of-coins 2) 2)
      ((= kinds-of-coins 3) 5)
      ((= kinds-of-coins 4) 10)
      ((= kinds-of-coins 5) 20)
      ((= kinds-of-coins 6) 50)
      ((= kinds-of-coins 7) 100)
      ((= kinds-of-coins 8) 500)
      ((= kinds-of-coins 9) 2000)))
                      
(count-change 50)