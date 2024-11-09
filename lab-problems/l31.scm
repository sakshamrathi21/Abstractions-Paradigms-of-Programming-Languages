#lang sicp
(define (myif test opt1 opt2)
  (cond (test opt1)
        (else opt2)))
(define-syntax myif2
  (syntax-rules ()
    ((myif2 test opt1 opt2)
     (cond (test opt1)
           (else opt2)))))
(define-syntax myor2
  (syntax-rules ()
    ((myor2 a b)
     (if a
         a
         b))))

(define-syntax myor3
  (syntax-rules ()
    ((myor3 a b)
     (let ((tmp a))
       (if tmp
           tmp
           b)))))

(define-syntax myorn
  (syntax-rules ()
    ((myorn) #f)
    ((myorn a) a)
    ((myorn a b . c)
     (let ((tmp a))
       (if tmp tmp (myorn b . c))))))

(define-syntax while
  (syntax-rules ()
    ((while condition . body)
     (let loop ()
       (cond (condition
              (begin . body)
              (loop)))))))


(define-syntax for
  (syntax-rules ()
    ((for e in l . body)
     (map (lambda (e) . body) l))))