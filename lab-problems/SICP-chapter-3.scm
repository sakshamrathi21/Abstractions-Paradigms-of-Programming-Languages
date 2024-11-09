#lang sicp
;; Theory
(define balance 100)
(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))

(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount)) balance)
          "Insufficient funds"))))

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount)) balance)
          "Insufficient funds")))

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m `withdraw) withdraw)
          ((eq? m `deposit) deposit)
          (else (error "Unknown request: MAKE-ACCOUNT"
                       m))))
  dispatch)


;; Exercise 3.1
(define (make-accumulator initial-value)
  (lambda (amount)
    (begin (set! initial-value (+ initial-value amount)) initial-value)))


;; Exercise 3.2
(define (make-monitored f)
  (define counter 0)
  (define (mf m)
    (cond ((eq? m `how-many-calls?) counter)
          ((eq? m `reset-count) (begin (set! counter 0) counter))
          (else (begin (set! counter (+ counter 1)) (f m)))))
  mf)


;; Exercise 3.3
(define (make-account-3 balance scpwd)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch  pwd m)
    (cond ((not (eq? pwd scpwd)) (error "Incorrect password"))
      ((eq? m `withdraw) withdraw)
          ((eq? m `deposit) deposit)
          (else (error "Unknown request: MAKE-ACCOUNT"
                       m))))
  dispatch)

;; Exercise 3.4
(define (make-account-4 balance scpwd)
  (define num-accessed 0)
  (define (call-the-cops) (error "Fake police called!"))
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! num-accessed 0) (set! balance (- balance amount))
             balance)
      "Insufficient funds"))
  (define (deposit amount)
    (begin (set! num-accessed 0) (set! balance (+ balance amount))
    balance))
  (define (dispatch  pwd m)
    (cond ((not (eq? pwd scpwd)) (begin (set! num-accessed (+ num-accessed 1)) (if (> num-accessed 7) (call-the-cops) (error "Incorrect password"))))
          ((eq? m `withdraw) withdraw)
          ((eq? m `deposit) deposit)
          (else (error "Unknown request: MAKE-ACCOUNT"
                       m))))
  dispatch)

;; Theory
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

(define (prime1? x)
  (define (prime-iter i)
    (cond ((> (* i i) x) #t)
          ((= (remainder x i) 0) #f)
          (else (prime-iter (+ i 1)))))
  (prime-iter 2))

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
;;(define (delay exp) (memo-proc (lambda () exp)))
;;(define (force delayed-object) (delayed-object))


;; Exercise 3.50
(define (stream-map2 proc . argstreams)
  (if (stream-null? (stream-car argstreams))
      the-empty-stream
      (cons-stream (apply proc (map stream-car argstreams))
                   (apply stream-map2 (cons proc (map stream-cdr argstreams))))))


;; Theory
(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))
(define (divisible? x y) (= (remainder x y) 0))
(define no-sevens (stream-filter (lambda (x) (not (divisible? x 7))) integers))
(define (fibgen a b) (cons-stream a (fibgen b (+ a b))))
(define fibs (fibgen 0 1))

(define (sieve stream)
  (cons-stream (stream-car stream) (sieve (stream-filter (lambda (x) (not (divisible? x (stream-car stream)))) (stream-cdr stream)))))

(define primes1 (sieve (integers-starting-from 2)))

(define ones (cons-stream 1 ones))

(define (add-streams s1 s2) (stream-map2 + s1 s2))
(define integers-r (cons-stream 1 (add-streams ones integers)))
(define fibs2 (cons-stream 0
                           (cons-stream 1
                                        (add-streams (stream-cdr fibs) fibs))))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))
(define double (cons-stream 1 (scale-stream double 2)))
(define primes (cons-stream 2
                            (stream-filter prime? (integers-starting-from 3))))
(define (square x) (* x x))
(define (prime? n)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n) true)
          ((divisible? n (stream-car ps)) false)
          (else (iter (stream-cdr ps)))))
  (iter primes))


