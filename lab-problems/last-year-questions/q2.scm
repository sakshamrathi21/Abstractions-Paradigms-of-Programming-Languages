#lang sicp
(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))
(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))
(define (stream-for-each proc s)
  (if (stream-null? s)
      `done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))
(define (display-stream s)
  (stream-for-each display-line s))
(define (display-line x) (newline) (display x))

(define-syntax delay
  (syntax-rules ()
    ((delay expr) (memo-proc (lambda () expr)))))

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream a b) (cons a (delay b)))))

(define (stream-car s)
  (car s))

(define (force promise) (promise))

(define (stream-cdr s) (force (cdr s)))

(define (stream-filter pred s)
  (cond ((stream-null? s) the-empty-stream)
        ((pred (stream-car s)) (cons-stream (stream-car s) (stream-filter pred (stream-cdr s))))
        (else (stream-filter pred (stream-cdr s)))))

(define (stream-enumerate-interval a b)
  (if (> a b)
      the-empty-stream
      (cons-stream a (stream-enumerate-interval (+ a 1) b))))

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))

(define (stream-limit s tolerance)
  (if (< (abs (- (stream-car s) (stream-car (stream-cdr s)))) tolerance) (stream-car (stream-cdr s))
      (stream-limit (stream-cdr s) tolerance)))
(define (average x y) (/ (+ x y) 2))

(define (sqrt-improve guess x)
(average guess (/ x guess)))
(define (sqrt-stream x)
(define guesses
(cons-stream 1.0 (stream-map (lambda (guess) (sqrt-improve guess x)) guesses)))
guesses)
(define (sqrt x tolerance)
(stream-limit (sqrt-stream x) tolerance))
  
