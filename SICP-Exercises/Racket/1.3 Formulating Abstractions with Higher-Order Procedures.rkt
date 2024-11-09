#lang racket

; 1.3 Formulating Abstractions with Higher-Order Procedures
; Procedures are abstractions that describe compound operations on numbers
; independent of the particular numbers. For example

(define (cube x) (* x x x))

; Above is a method for obtaining a cube of any number. We don't need it

(* 3 3 3)

; This would limit us to a languages primitive procedures. We wold be unable to
; express the concept of cubing.

; To go a step beyond this we also need the ability construct procedures that
; can accept procedures as arguments or return procedures as values. These are
; called higher-order procedures.

; 1.3.1 Procedures as Arguments
; Consider the following three procedures

(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))
(sum-integers 1 10)

(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a) (sum-cubes (+ a 1) b))))
(sum-cubes 1 10)

(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2))) (pi-sum(+ a 4) b))))
(pi-sum 1 10000)
; Approaches pi/8 slowly 0.39269908169872
(* (pi-sum 1 10000) 8)

; These three procedures share a common underlying pattern. They are identical except for
; - name of the procedure
; - the function of a used to compute the term to be added
; - the function that provides the next value of a
;
; Template
; (define (<name> a b)
;   (if (>a b)
;       0
;       ( + (<term> a)
;           (<name> (<next> a) b))))
;
; This is very similar to what mathematicians refer to as a summation of a series,
; "sigma notation". This allows mathematicians to deal with the concept of summation
; itself rather than only with particular sums. 
;
; We can do this by transforming the "slots" from above into formal parameters.

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

; Notice that sum takes as its arguments the lower and upper bounds a and b as well 
; as the procedures term and next.

(define (inc n) (+ n 1))
(define (sum-cubes2 a b)
  (sum cube a inc b))

(sum-cubes2 1 10)


(define (identity x) x)
(define (sum-integers2 a b)
  (sum identity a inc b))

(sum-integers2 1 10)


(define (pi-sum2 a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(* 8 (pi-sum2 1 1000))


; Once we have sum we can use it as a building block in formulating further concepts. For
; instance, the definite integral of a function f between the limits a and b can be
; approximated numerically using the formula ...
(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

(integral cube 0 1 0.01)
(integral cube 0 1 0.001)
(integral identity 0 1 0.01)
(integral identity 0 1 0.001)



; 1.3.2 Constructing Procedures Using Lambda
; In using sum it is awkward to have to define trivial procedures such as pi-term
; and pi-next just so that we can use them as arguments to our higher-order
; procedure. It would be mre convenient to have a way to directly specify "the
; procedure that returns its input incremented by 4" and "the procedure that
; returns the reciprocal of its input times its input plus 2". We can do this by
; the special for lambda, which creates procedures.

(lambda (x) (+ x 4))

; and

(lambda (x) (/ 1.0 (* x (+ x 2))))

; Then our pi-sum procedure can be expressed without defining any auxiliary 
; procedures as

(define (pi-sum3 a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a
       (lambda (x) (+ x 4))
       b))

; Again using lambda we can write the integral procedure without having to define 
; the auxiliary procedure add-dx

(define (integral2 f a b dx)
  (* (sum f
          (+ a (/ dx 2.0))
          (lambda (x) (+ x dx))
          b)
     dx))

; In general lambda is used to create procedures in the same way as define, except 
; that no name is specified for the procedure

; (lambda (<formal-parameters>) <body>)

; Using let to create local variables
; Often we would like to bind values as local variables above and beyond the 
; arguments that are passed into our function. We can do this by using an auxilary
; procedure like so
(define (square n) (* n n))

(define (f x y)
  (define (f-helper a b)
    (+ (* x (square a))
       (* y b)
       (* a b)))
  (f-helper (+ 1 (* x y))
            (- 1 y)))
(f 1 5)

; Or we could use a lambda expression

(define (f2 x y)
  ((lambda (a b)
    (+ (* x (square a))
       (* y b)
       (* a b)))
   (+ 1 (* x y))
   (- 1 y)))
(f2 1 5)
  
; This construct is so useful there is a special form called let to make its
; use more convenient. Using let, the f procedure could be written as

(define (f3 x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))
(f3 1 5)

; The general form of a let expression is
; (let ((<var1> <exp1>)
;       (<var2> <exp2>))
;      <body>)

; The let expression is interpreted as an alternative syntax for
; ((lambda (<var1> <var2>)
;       <body>)
;  <exp1>
;  <exp2>)
; A let expression is simply syntactic sugar for the underlying lambda application

; Let allows one to bind variables as locally as possible to where they are to be 
; used. For example, if the value of x is 5

(define (let-demo x)
  (+ (let ((x 3))
       (+ x (* x 10)))  ; Here x = 3
     x))                ; Here x = 5
(let-demo 5)

; The variables values are computed outside the let. This matters when expressions
; that provide the values for the local variables depend upon variables having
; the same names as the local variables themselves.

(define (let-demo2 x)
  (let ((x 3)
        (y (+ x 2)))  ; x = 2 here as the value is computed outside the let
    (* x y)))         ; x = 3 here, taking the value from the let
(let-demo2 2)



; Exercise 1.34
; Suppose we define the procedure

(define (f4 g)
  (g 2))

; Then we have 
(f4 square)                     ; 4
(f4 (lambda (z) (* z (+ z 1)))) ; 6

; What would happen if we ask the interpreter to evaluate (f4 f4)?
; I think it will go into an infinite loop trying to evaluate
; (f f)
; (f 2)
; (2 2) Which is an error



; 1.3.3 Procedures as General Methods
; Finding roots of equations by the half interval method
; is a technique for finding roots of an equation, f(x) = 0, where f is a continuous function.
; The idea is that, given points a and b where f(a) < 0 < f(b), then f must have at least one
; zero between a and b. To locate a zero, let x be the average of a and b and compute f(x). If
; f(x) > 0, then f must have a zero between a and x. If f(x) < 0, then f must have a zero between
; x and b. Continuing in this way, we can identify a smaller and smaller intervals on which f
; must have a zero. When we reach a point where the interval is small enough, the process stops.
; The number of steps required grows as O(log(L/T)) where L is the length of the original interval
; and T is the error tolerance.

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))
(define (average a b) (/ (+ a b) 2))
(define (abs x) (if (> x 0) x (* -1 x)))
(define (close-enough? a b) (< (abs (- b a)) 0.001))

(define (func1 x) (- (* x x) 9))
(search func1 1.0 10)
(search func1 -1.0 -10)

; Search is awkward because we will get the wrong value if we accidentally give it two 
; values that have the same sign.

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          (( and (negative? b-value) (positive? a-value))
           (search f b a))
          (else (error "Values are not of opposite sign" a b)))))
(half-interval-method func1 0 5)
(half-interval-method func1 5 0)
; (half-interval-method func1 10 5)
(half-interval-method sin 2.0 4.0)
(half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3))
                      1.0
                      2.0)

; Finding fixed points of functions
; A number is called a fixed point of a function f if x satifies the equation f(x) = x. For
; some functions we can locate a fixed point by begining with an initial guess and applying
; f repeatedly until the value doesn't change very much.

(define (fixed-point2 f guess)
  (if (close-enough? (f guess) (f (f guess)))
      guess
      (fixed-point2 f (f guess))))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

; We can use this method to approximate the fixed point of the cosine function, starting
; with 1 as an initial approximation

(fixed-point cos 1.0)

; Similarly, we can find a solution to the equation y = sin y + cos y

(fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0)

; The fixed-point process is reminiscent of the process we used for finding square roots. Both
; are based on the idea of repeatedly improving a guess until the result satisfies some criterion.
; In fact we can formulate the square-root computation as a fixed-point search. Computing the square
; root of some number x requires finding a y such that y^2 = x. Putting this equation into the
; equivilent form y = x/y we recognise that we are looking for a fixed point of the function.
; Therefore we can try

(define (sqrt-oscillating x)
  (fixed-point (lambda (y) (/ x y))
               1.0))

; (sqrt-oscillating 16.0)
; Unfortunatly this search does not converge. Consider
; - initial guess   y1
; - next guess      y2 = x/y1
; - next guess      y3 = x/y2
; - which is        y3 = x/(x/y1) = y1
; This results in an infinite loop with y1 and y2 oscillating about the answer.

; One way to control such oscillations is to prevent the guesses from changing so much. Since
; the answer is always between our guess y and x/y, we can make a new guess that is not as 
; far from y as x/y by averaging y with x/y so that the next guess after y is (1/2)(y + x/y).
; The process of making such a sequence of guesses is simply the process of looking for a 
; fixed point of y -> (1/2)(y + x/y)

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))
(sqrt 16.0)

; ((1/2)(y + x/y) comes from adding y to both sides of the equation and dividing by 2).

; With this modification, the square root procedure works. In fact, if we unravel the 
; definitions we can see that the sequence of approximations to the square root generated 
; here is the same as the one generated by our original square root procedure. This approach of 
; averaging successive approximations to a solution (average damping) often aids the convergence 
; of fixed-point searches.



; 1.3.4 Procedures as Returned Values
; We can acheive more expressive power by creating procedures whose returned values are themselves 
; procedures. As an example the average damping used above is a method that has general application.
; Namely, given a function f, we consider the function whose value at x is equal to the average of
; x and f(x).

(define (average-damp f)
  (lambda (x) (average x (f x))))

; Average-damp is a procedure that takes as its argument a procedure f and returns as its value a
; procedure (produced by the lambda) that, when applied to a number x, produces the average of x and 
; f(x). For example, applying the average-damp to the square procedure produces a procedure
; whose value at some number x is the average of x and x^2. Applying this resulting procedure to 10
; returns the average of 10 and 100, 55

((average-damp square) 10)

; Using average-damp we can reformulate the square-root procedure as follows

(define (sqrt2 x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0))

(sqrt2 16)

; Notice how this formulation makes explicit the three ideas on the method: fixed-point search, average
; damping, and the function y -> x/y. It is constructive to compare this formulation with the original
; version from section 1.1.7

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))
(define (improve guess x)
  (average guess (/ x guess)))
;(define (average x y)
;  (/ (+ x y) 2))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
(define (sqrt-1.1.7 x)
  (sqrt-iter 1.0 x))

; These procedures express the same process but the fixed-point version is much clearer. There are
; many ways to formulate a procedure. Look for the ways that allow reuse. As an example of reuse notice
; that the cube root of x is a fixed point of the function y -> x/y^2, so we can immediately generalise our
; square root procedure to one that extracts cube roots

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y)))) 1.0))

(cube-root 27)

; Newton's Method
; The square root procedure is a special case of Newton's method. If x -> g(x) is a differentiable function,
; then a solution of equation g(x) = 0 is a fixed point of the function x -> f(x) where
;
;              g(x)
; f(x) = x - -------
;             Dg(x)
;
; and Dg(x) is the derivitaive of g evaluated at x. Newton's method is the use of the fixed-point method
; we saw above to approximate a solution of the equation by finding a fixed point of the fuction f. For
; many functions g and for sufficiently good initial guesses for x, Newton's method converges very 
; rapidly to a solution of g(x) = 0

; In order to implement Newton's method as a procedure we must first express the idea of derivative.
; Note that "derivative" like average damping, is something that transforms a function into another
; function. For instance. the derivative of the function x -> x^3 is the function x -> 3x^2. In general
; if g is a function and dx is a small number, then the dervative Dg of g is the function whose value
; at any number x is given (in the limit of small dx) by
;
;         g(x + dx) - g(x)
; Dg(x) = ----------------
;                dx
;
; Thus we can express the idea of derivative by

(define dx 0.00001)
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x)) 
       dx)))

; Like average-damp deriv is a procedure that takes a procedure as argument and returns a procedure as
; value. For example to approximate the derivative of x -> x3 at 5 (answer is 75) we can evaluate

((deriv cube) 5)

; With the aid of deriv, we can express Newton's method as a fixed-point process.

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

; The newton-transform procedure expresses the formula at the beginning of this section, and 
; newtons-method is readily defined in terms of this. It takes as arguments a procedure that
; computes the function fo which we want to find a zero, together with an initial guess. For instance,
; to find the square root of x, we can use Newton's method to find a zero of the function y -> y^2 - x
; starting with an intial guess of 1. This provides yet another form of the square-root procedure

(define (sqrt3 x)
  (newtons-method (lambda (y) (- (square y) x))
                  1.0))
(sqrt3 64)


; Abstractions and first-class procedures
; We've seen two ways to express the square root computation as an instance of a more general method,
; once as a fixed-point search and once using Newton's method. Since Newton's method was itself
; expressed as a fixed-point process,we actuall saw two ways to compute square roots as fixed points.
; Each method begins with a function and finds a fixed point of some transformation of the function.
; We can express this general idea itself as a procedure

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

; This very general procedure takes as its arguments a procedure g that computes some function, a
; procedure that transforms g and an initial guess. The returned result is a fixed point of the
; transformed function.

; Using this abstraction, we can recast the first square-root computation from this section as
; an instance of this general method.

(define (sqrt4 x)
  (fixed-point-of-transform (lambda (y) (/ x y))
                            average-damp
                            1.0))
(sqrt4 16)

; Similarly we can express the second square-root computation as

(define (sqrt5 x)
  (fixed-point-of-transform (lambda (y) (- (square y) x))
                            newton-transform
                            1.0))
(sqrt5 16)

; We began section 1.3 with the observation that compound procedures are a crucial abstraction
; mechanism, because they permit us to express general methods of computing as explicit elements on
; our programming language. Now we've seen how higher-prder procedures permit us to manipulate these 
; general methods to create further abstracions.

; As programmers, we should be alert to opportunities to identify the underlying abstractions in our
; programs and to build upon them and generalize them to create more powerful abstractions. this is not
; to say that one should always write programs in the most abstract way possible; expert programmers 
; know how to choose the level of abstraction appropriate to their task. But it is important to be able to 
; think in terms of these abstractions, so that we can be ready to apply them in new contexts. The 
; significance of higher-order procesures is that they enable us to represent these abstractions explicitly
; as elements in our programming language, so that they can be handled just like other computational elements.

; In general, programming languages impose restrictions on the ways in which computational elements 
; can be manipulated. Elements with the fewest restrictions are said to have first-class status. Some of
; the "rights and privileges" of first-class elements are
; - They may be named by variables
; - They may be passed as arguments to procedures
; - They may be returned as the results of procedures
; - They may be included in data structures

; Lisp, unlike other common programming languages, awards procedures full first-class status. This
; poses challenges for efficient implementation, but the resulting gain in expressive power is enormous.


















