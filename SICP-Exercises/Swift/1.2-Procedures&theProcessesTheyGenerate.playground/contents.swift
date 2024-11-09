import Foundation // For NSDate


// 1.2 Procedures and the Processes they Generate
// 1.2.1 Linear Recursion and Iteration
// Factorial n!
// Factorial can be thought of by noticing that n! == n * (n - 1)! . This is a recursive process

/*
(factorial 3)
(* 3 (factorial 2))
(* 3 (* 2 (factorial 1)))
(* 3 (* 2 1)))
(* 3 2)
6
*/

func factorialRecursive(_ n: Int) -> Int {
    if (n == 1) {
        return 1
    } else {
        return n * factorialRecursive(n - 1)
    }
}
factorialRecursive(6)

// Note the ramped appearence of the calculation. This is due to the fact that the interpreter must keep track of a large amount of state as it progresses ???

// Lets take a different approach. We could maintain a running product along with a counter that counts from 1 to n

// product = counter * product
// counter = counter + a

/*
(factorial 3)
(fact-iter 1 1 3)
(fact-iter 1 2 3)
(fact-iter 2 3 3)
6
*/

func factIter(_ product: Int, _ counter: Int, _ maxCount: Int) -> Int {
    if (counter > maxCount) {
        return product
    } else {
        return factIter(product * counter, counter + 1, maxCount)
    }
}
func factorialIterative(_ n: Int) -> Int {
    return factIter(1, 1, n)
}
factorialIterative(6)

func factorialIterative2(_ n:Int) -> Int {
    
    var factIter2: (Int, Int, Int) -> Int = { _ in return 0 }
    factIter2 = { product, counter, maxCount in
        if (counter > maxCount) {
            return product
        } else {
            return factIter2(product * counter, counter + 1, maxCount)
        }
    }
    return factIter2(1, 1, n)
}
factorialIterative2(6)

// Both approaches compute the same mathematical function and require the same number of steps, which is proportional to n. The first approach has an expansion and then contraction. The sxpansion is due to a build up of deferred operations. The contraction is when the operations are performed. This is called a linear recursive process.
// The second approach doesn't shrink or grow. At each step all we need to keep track of for any n are the current values of product, counter and max-count. This is a linear iterative process.
// Most popular languages are designed in such a way that the interpretation  of any recursive process consumes an amount of memory that grows with the number of procedure calls. As such special looping constructs are required. Tail recursion can solve this defect though.


// Exercise 1.9 - Each of the following two procedures defines a method for adding two positive integers in terms of the procedures inc, which increments its argument by 1, and dec, which decrements its argument by 1.
func inc(_ value: Int) -> Int {
    return value + 1
}
func dec(_ value: Int) -> Int {
    return value - 1
}

func addition1(_ a: Int, _ b: Int) -> Int {
    if (a == 0) {
        return b
    } else {
        return inc(addition1(dec(a), b))
    }
}

addition1(4, 5)
/*
inc(addition1(3, 5))
inc(inc(addition1(2, 5)))
inc(inc(inc(addition1(1, 5))))
inc(inc(inc(inc(addition1(0, 5)))))
inc(inc(inc(inc(5))))
inc(inc(inc(6)))
inc(inc(7))
inc(8)
9
*/


func addition2(_ a: Int, _ b: Int) -> Int {
    if (a == 0) {
        return b
    } else  {
        return addition2(dec(a), inc(b))
    }
}

addition2(4, 5)
/*
addition2(3, 6)
addition2(2, 7)
addition2(1, 8)
addition2(0, 9)
9
*/
// addition1 is a recursive process. addition2 is an iterative process


// Exercise 1.10 - The following procedure computes a mathematical function called Ackermann's function.
func A(_ x:Int, _ y:Int) -> Int {
    print("x:\(x) y:\(y)")
    switch true {
    case y == 0:
        return 0
    case x == 0:
        return 2 * y
    case y == 1:
        return 2
    default:
        return A(x - 1, A(x, y - 1))
    }
}
// What are the values of the following expressions?
A(1, 10)
/*
x:1 y:10
x:1 y:9
x:1 y:8
x:1 y:7
x:1 y:6
x:1 y:5
x:1 y:4
x:1 y:3
x:1 y:2
x:1 y:1
x:0 y:2
x:0 y:4
x:0 y:8
x:0 y:16
x:0 y:32
x:0 y:64
x:0 y:128
x:0 y:256
x:0 y:512
Final output is 1024
*/
A(2, 4)
/*
x:2 y:4
x:2 y:3
x:2 y:2
x:2 y:1
x:1 y:2
x:1 y:1
x:0 y:2
x:1 y:4
x:1 y:3
x:1 y:2
x:1 y:1
x:0 y:2
x:0 y:4
x:0 y:8
x:1 y:16
x:1 y:15
x:1 y:14
x:1 y:13
x:1 y:12
x:1 y:11
x:1 y:10
x:1 y:9
x:1 y:8
x:1 y:7
x:1 y:6
x:1 y:5
x:1 y:4
x:1 y:3
x:1 y:2
x:1 y:1
x:0 y:2
x:0 y:4
x:0 y:8
x:0 y:16
x:0 y:32
x:0 y:64
x:0 y:128
x:0 y:256
x:0 y:512
x:0 y:1024
x:0 y:2048
x:0 y:4096
x:0 y:8192
x:0 y:16384
x:0 y:32768
Final output is 65536
*/
A(3, 3)
/*
x:3 y:3
x:3 y:2
x:3 y:1
x:2 y:2
x:2 y:1
x:1 y:2
x:1 y:1
x:0 y:2
x:2 y:4
x:2 y:3
x:2 y:2
x:2 y:1
x:1 y:2
x:1 y:1
x:0 y:2
x:1 y:4
x:1 y:3
x:1 y:2
x:1 y:1
x:0 y:2
x:0 y:4
x:0 y:8
x:1 y:16
x:1 y:15
x:1 y:14
x:1 y:13
x:1 y:12
x:1 y:11
x:1 y:10
x:1 y:9
x:1 y:8
x:1 y:7
x:1 y:6
x:1 y:5
x:1 y:4
x:1 y:3
x:1 y:2
x:1 y:1
x:0 y:2
x:0 y:4
x:0 y:8
x:0 y:16
x:0 y:32
x:0 y:64
x:0 y:128
x:0 y:256
x:0 y:512
x:0 y:1024
x:0 y:2048
x:0 y:4096
x:0 y:8192
x:0 y:16384
x:0 y:32768
Final output is 65536
*/

func f(_ n:Int) -> Int {
    return A(0, n)
}
// f(n) = 2n

func g(_ n:Int) -> Int {
    return A(1, n)
}
// g(n) = 2^n

func h(_ n: Int) -> Int {
    return A(3, n)
}
// h(n) = 2^(2^n)


// 1.2.2 Tree Recursion
// Fibonacci numbers 0, 1, 1, 2, 3, 5, 8, 13, 21

/*         / 0                       if n = 0
Fib(n) = | 1                       if n = 1
\ Fib(n - 1) + Fib(n - 2) otherwise
*/

func fib(_ n: Int) -> Int {
    switch true {
    case n == 0:
        return 0
    case n == 1:
        return 1
    default:
        return fib(n - 1) + fib(n - 2)
    }
}
fib(6)

// The fib procedure above involves a lot of redundant computation. It can be shown that the procedure will calculate fib 1 and fib 0 fib(n + 1) times. This is an exponential growth with respect to n. Memory grows linearly with the input.

// We can also process Fibonacci numbers in an iterative process.
// a = a + b
// b = a

func fib1(_ n: Int) -> Int {
    var fibIter: (Int, Int, Int) -> Int = { _ in return 0 }
    fibIter = { a, b, count in
        if (count == 0) {
            return b
        } else {
            return fibIter(a + b, a, count - 1)
        }
    }
    return fibIter(1, 0, n)
}

//fib1(64)  // Above fib1(91) Swift has a runtime error as the result is larger than 63 bits in size.
// Swift playgrounds aren't fast at this operation either. Dr Racket is significantly faster.


// Example: Counting change
// How many different ways can we make change of $1.00, given half-dollars, quarters, dimes, nickels and pennies? More generally, can we write a procedure to computer the number of ways to change any given amount of money?

// Recursive
// The number of ways to change amount a using n kinds of coins equals
// - the number of ways to change amount a using all but the first kind of coin, plus
// - the number of ways to change amount a - d using all n kinds of coins, where d is the denomination of the first kind of coin.

func countChange(_ amount: Int) -> Int {
    return cc(amount, kindsOfCoins: 5)
}
func cc(_ amount: Int, kindsOfCoins: Int) -> Int {
    switch true {
    case amount == 0:
        return 1
    case (amount < 0) || (kindsOfCoins == 0):
        return 0
    default:
        return cc(amount, kindsOfCoins: kindsOfCoins - 1) + cc(amount - firstDenomination(kindsOfCoins), kindsOfCoins: kindsOfCoins)
    }
}
func firstDenomination(_ kindsOfCoins: Int) -> Int {
    switch kindsOfCoins {
    case 1:
        return 1
    case 2:
        return 5
    case 3:
        return 10
    case 4:
        return 25
    case 5:
        return 50
    default:
        return 0
    }
}
countChange(25)

// The recursive approach used above is very inefficient. Can you come up with an iterative approach that has better performance?


// Exercise 1.11 - A function is defined by the rule that f(n) = n if n < 3 and f(n) = f(n -1) + 2f(n-2) + 3f(n - 3) if n >=3. Write a procedure that computes f by means of a recursive process.
/*
f(n) = | n                                if n  < 3
| f(n - 1) + 2f(n - 2) + 3f(n -3)  if n >= 3
*/

func function11(_ n: Int) -> Int {
    if (n < 3) {
        return n
    } else {
        return function11(n - 1) + 2*function11(n - 2) + 3*function11(n - 3)
    }
}
function11(8)

// Write a procedure that computes f by means of an iterative process

func function11Iter(_ three: Int, _ two: Int, _ one: Int, _ count: Int, _ total: Int) -> Int {
    if (count > total) {
        return one
    } else {
        return function11Iter(two, one, one + 2*two + 3*three, count + 1, total)
    }
}

func function11v2(_ n: Int) -> Int {
    if (n < 3) {
        return n
    } else {
        return function11Iter(0, 1, 2, 3, n)
    }
}

function11v2(10)

// Exercise 1.12 - Pascal's Triangle
// The following pattern of numbers is called Pascal's triangle
/*

1
1 1
1 2 1
1 3 3 1
1 4 6 4 1


*/
// The numbers at the edge of the triangle are all 1, and each number inside the triangle is the sum of the two numbers above it. Write a procedure that computes elements of Pascal's triangle by means of a recursive process.

// In other words, compute an entry on the Pascal triangle given the row and column. Rows start from 1, counting from above; columns start from 1 too, counting from left to right.

func pascalRecursive(_ row: Int, _ col: Int) -> Int {
    switch true {
    case col < 1 || row < 1 || col > row:
        return 0
    case col == row:
        return 1
    case col == 1:
        return 1
    default:
        return pascalRecursive(row - 1, col - 1) + pascalRecursive(row - 1, col)
    }
}
pascalRecursive(1, 1)
pascalRecursive(2, 1)
pascalRecursive(2, 2)
pascalRecursive(3, 1)
pascalRecursive(3, 2)
pascalRecursive(3, 3)
pascalRecursive(10, 5)


// Exercise 1.14

/*
func countChange(amount: Int) -> Int {
return cc(amount, 5)
}
func cc(amount: Int, kindsOfCoins: Int) -> Int {
switch true {
case amount == 0:
return 1
case (amount < 0) || (kindsOfCoins == 0):
return 0
default:
return cc(amount, kindsOfCoins - 1) + cc(amount - firstDenomination(kindsOfCoins), kindsOfCoins)
}
}
func firstDenomination(kindsOfCoins: Int) -> Int {
switch kindsOfCoins {
case 1:
return 1
case 2:
return 5
case 3:
return 10
case 4:
return 25
case 5:
return 50
default:
return 0
}
}
countChange(25)
*/



// Exercise 1.15
// The sine of an angle (specified in radians) can be computed by making use of the approximation sin x ~ x if x is sufficiently small, and the trigonometric identity

// sin r = 3 sin (r/3) - 4 sin^3 (r/3)

// to reduce the size of the argument of sin. (For purposes of this exercise an angle is considered "sufficiently small" if its magnitude is not greater than 0.1 radians. These ideas are incorporated in the following procedures:

func cube(_ x: Float) -> Float {
    return x * x * x
}
func p(_ x: Float) -> Float {
    return (3 * x) - (4 * cube(x))
}
func sine(_ angle: Float) -> Float {
    if (abs(angle) < 0.1) {
        return angle
    } else {
        return p(sine(angle / 3.0))
    }
}

// a) How many times is the procedure p2 applied when (sine 12.15) is evaluated?
// 12.15 / 3^n < 0.1
// 0.1 * 3^n = 12.15
// 3^n = 121.5
// n = 5
sine(12.15)

// b) What is the order of growth in space and number of steps (as a function of a) used by the process generated by the sine procedure when (sine a) is evaluated?

// The number of steps required to calculate sine a can be calculated as below
//        (ceiling(/ (log (/ a 0.1)) (log 3)))
// Which gives us an order of growth for size and space as
//        O(log(n))


// 1.2.4 Exponentiation
// Consider the problem of computing the exponential of a given number. We would like a procedure that takes as arguments a base b and a positive integer exponent n and computes b^n. One way to do this is via the recursive definition
/*
   b^n = b.b^(n-1)
   b^0 = 1
*/

// Which translates into the procedure
func expt1(_ b: Int, _ n:Int) -> Int {
    if (n == 0) {
        return 1
    } else {
        return b * expt1(b, n - 1)
    }
}
expt1(2, 10)

// This is a linear recursive process, which requires O(n) steps and space. Just as with factorial we can formulate an equivilent linear iteration

func exptIter(_ b:Int, _ count:Int, _ product: Int) -> Int {
    if (count == 0) {
        return product
    } else {
        return exptIter(b, count - 1, b * product)
    }
}
func expt2(_ b: Int, _ n:Int) -> Int {
    return exptIter(b, n, 1)
}
expt2(2, 10)

// The version above requires O(n) steps and O(1) space.

// We can compute exponentials in fewer steps by using successive squaring. For instance rather than computing b^8 as
//
// b * b * b * b * b * b * b * b
//
// We can compute it using three multiplications
/*
   b^2 = b * b
   b^4 = b^2 * b^2
   b^8 = b^4 * b^4
*/
// This method works fine for exponents that are powers of 2. We can also take advantage of successive squaring in computing exponentials in genearl if we use the rule
/*
    b^n = (b^2/2)^2     if n is even
    b^n = b*b^(n-1)     if n is odd
*/

func fastExpt(_ b: Int, _ n: Int) -> Int {
    switch true {
    case n == 0:
        return 1
    case isEven(n):
        return square(fastExpt(b, n / 2))
    default:
        return b * fastExpt(b, n - 1)
    }
}
func isEven(_ n: Int) -> Bool {
    return (n % 2) == 0
}
func square(_ x: Int) -> Int {
    return x * x
}
fastExpt(2, 10)

// fastExpt() grows logarithmically with n in both space and time. To see this observe that computing b^2n requires only one more multiplication than computing b^n. The size of the exponent we can computer therefor doubles with every new multiplication. Hence O(log(n)



// Exercise 1.16
// Design a procedure that evolves an iterative exponentiation process that uses successive squaring and uses a logarithmic number of steps, as does fast-expt.

// Hint: Using the observation that

// (b^(n/2))^2 = (b^2)^(n/2)

// keep, along with
// - the exponent n
// - the base b
// - an additional state variable a,

// and define the state transformation in such a way that the product a.b^n is unchanged from state to state.

// At the beginning of the process a is taken to be 1, and the answer is given by the value of a at the end of the process.

// In general, the technique of defining an invariant quantity that remains unchanged from state to state is a powerful way to thing about the design of iterative algorithms.

func fastExptIter(_ a: Int, _ b: Int, _ n: Int) -> Int {
    print("\(a), \(b), \(n), \(a * fastExpt(b, n))")
    switch true {
    case n == 0:
        return a
    case isEven(n):
        return fastExptIter(a, square(b), n / 2)
    default:
        return fastExptIter(a * b, b, n - 1)
    }
}
func fastExpt2(_ b: Int, _ n: Int) -> Int {
    return fastExptIter(1, b, n)
}
fastExpt2(2, 20)


// Exercise 1.17
// Using addition, double and halve design a multiplication procedure analoguous to fast-expt that uses a logarithmic number of steps.

func double(_ x: Int) -> Int {
    return x + x
}
func halve(_ x: Int) -> Int {
    return x / 2
}

func fastMutliplyRecursive(_ a: Int, _ b: Int) -> Int {
    switch true {
    case b == 0:
        return 0
    case isEven(b):
        return double(fastMutliplyRecursive(a, halve(b)))
    default:
        return a + fastMutliplyRecursive(a, b - 1)
    }
}
fastMutliplyRecursive(2, 4)


// Exercise 1.18
// Devise a procedure that generates an iterative process for multiplying two integers in terms of adding, doubling and halving that uses a logarithmic number of steps

func fastMultiplyIter(_ a: Int, _ b: Int, _ c: Int) -> Int {
    switch true {
    case b == 1:
        return a + c
    case isEven(b):
        return fastMultiplyIter(double(a), halve(b), c)
    default:
        return fastMultiplyIter(a, b - 1, c + a)
    }
}
func fastMultiply(_ a: Int, _ b: Int) -> Int {
    return fastMultiplyIter(a, b, 0)
}
//fastMultiply(2, 4)
fastMultiply(3, 1000000)


// Exercise 1.19
// From 1.2.2 recall the transformation of state variables a and b in fib-iter
/*
   a <- a + b
   b <- a
*/
//Call this transformation T. Applying T over and over n times starting with 1 and 0 produces the pair Fib(n + 1) and Fib(n). The Fibonaci numbers are generated by applying T^n with the starting pair (1, 0).

//Now consider Tpq
/*
   a' <- bq + aq + ap
   b' <- bp + aq
*/
// Show that if we apply such a transform twice, the effect is the same as using a single transform Tp`q` and compute p` and q` in terms of p and q. This gives us an explicit way to square these transformations and thus we can compute T^n using successive squaring.
/*
   T^2
   a' <- (q + p)a + qb
   b' <- qa + bp
   a" <- (p + q)(p + q)a + q(q
*/
/*
(define (fib3 n)
    (fib-iter3 1 0 0 1 n))

(define (fib-iter3 a b p q count)
    (cond   ((= count 0) b)
            ((even? count) (fib-iter3 a
                b
                (+ (square p) (square q))
                (+ (* 2 p q) (square q))
                (/ count 2)))
            (else (fib-iter3 (+ (* b q) (* a q) (* q p))
                (+ (* b p) (* a q))
                p q
                (- count 1)))))
(fib3 6)
*/

func fib3(_ n: Int) -> Int {
    return fibIter3(1, b: 0, p: 0, q: 1, count: n)
}

func fibIter3(_ a: Int, b: Int, p: Int, q: Int, count: Int) -> Int {
    switch true {
    case count == 0:
        return b
    case isEven(count):
        return fibIter3(a, b: b, p: square(p) + square(q), q: (2 * p * q) + square(q), count: count / 2)
    default:
        return fibIter3((b * q) + (a * q) + (q * p), b: (b * p) + (a * q), p:p, q:q, count:count - 1)
    }
}
fib3(30)



// 1.2.5 Greatest Common Divisors
// The greatest commong divisor of two integers is the largest integer that divides both a and b with no remainder. GCD of 16 and 28 is 4.

// Euclid's Algorithm
// If r is the remainder when a is divided by b, then the common divisors of a and b are the same as the common divisors of b and r.
/*
   GCD(a, b) == GCD(b, r)

   Example
   GCD(206,40) = GCD(40, 6)
               = GCD(6, 4)
               = GCD(4, 2)
               = GCD(2, 0)
               = 2
*/
func gcd(_ a: Int, _ b: Int) -> Int {
    if b == 0 {
        return a
    } else {
        return gcd(b, a % b)
    }
}

gcd(206, 40)
//gcd(28, 16)
//gcd(7, 300)

// This generates an iterative process whose number of steps grows at a logarithmic rate.

// Lame's Theorem
// If Euclid's Algorithm requires k steps to compute the GCD of some pair, then the smaller number in the pair must be greater than or equal to the kth Fibonacci number.

// Exercise 1.20
// The process that a procedure generates is dependent on the rules used by the interpreter. Using the substitution method (for normal order), illustrate the process generated in evaluating gcd(206, 40).
gcd(206, 40)
if 40 == 0 { 206 } else { gcd(40, 206 % 40) }
// if 40 == 0 { ... }
gcd(40, 206 % 40)
if 206 % 40 == 0 { 40 } else { gcd(206 % 40, 40 % (206 % 40)) }
// if 6 == 0 { ... }            // 1 remainder performed
gcd(206 % 40, 40 % (206 % 40))
if 40 % (206 % 40) == 0 { 206 % 40 } else { gcd(40 % (206 % 40), (206 % 40) % (40 % (206 % 40))) }
// if 4 == 0 { ... }            // 2 remainder performed (3 so far)
gcd(40 % (206 % 40), (206 % 40) % (40 % (206 % 40)))
if (206 % 40) % (40 % (206 % 40)) == 0 { 40 % (206 % 40) } else { gcd((206 % 40) % (40 % (206 % 40)), (40 % (206 % 40)) % (206 % 40) % (40 % (206 % 40))) }
// if 2 == 0 { ... }            // 4 remainder performed (7 so far)
gcd((206 % 40) % (40 % (206 % 40)), (40 % (206 % 40)) % (206 % 40) % (40 % (206 % 40)))
//if (40 % (206 % 40)) % (206 % 40) % (40 % (206 % 40)) == 0 { (206 % 40) % (40 % (206 % 40)) } else { gcd((40 % (206 % 40)) % (206 % 40) % (40 % (206 % 40)), (206 % 40) % (40 % (206 % 40)) % (40 % (206 % 40)) % (206 % 40) % (40 % (206 % 40)))
// if 0 == 0 { ... }            // 7 remainders performed (14 so far)
(206 % 40) % (40 % (206 % 40))  // 4 remainders performed (18 in total)
2

// Illustrate the applicative process
gcd(206, 40)
gcd(40, 206 % 40)
gcd(40, 6)
gcd(6, 40 % 6)
gcd(6, 4)
gcd(4, 6 % 4)
gcd(4, 2)
gcd(2, 4 % 2)
2
// 4 remainder procedures called.



// 1.2.6 Example: Testing for Primality
// Searching for divisors
// First up the straight forward method, testing n for divisibility by successive integers startng with 2.

func dividesWithNoRemainder(_ a: Int, _ b: Int) -> Bool {
    return  a % b == 0
}
dividesWithNoRemainder(10, 2)

func findDivisor(_ n: Int, _ testDivisor: Int) -> Int {
    switch true {
    case square(testDivisor) > n:
        return n
    case dividesWithNoRemainder(n, testDivisor):
        return testDivisor
    default:
        return findDivisor(n, testDivisor + 1)
    }
}

func smallestDivisor(_ n: Int) -> Int {
    return findDivisor(n, 2)
}

func isPrime(_ n: Int) -> Bool {
    return n == smallestDivisor(n)
}

isPrime(31123)


// Fermat's Little Theorem
// If n is a prime number and a is any positive integer less than n, then a raised to the nth power is congruent to a modulo n. (Not sure if you could make this any less comprehensible)
/* This can be restated as
   - Given a number n
   - pick a random number a < n
   - compute the remainder of a^n modulo n
   - If the result is not equal to a then n is certainly not prime
   - If it is then pick another value for a and try again.
   - The more values you try the more confident you are that n is prime
*/

func expMod(base: Int, exp: Int, m: Int) -> Int {
    switch true {
    case exp == 0:
        return 1
    case isEven(exp):
        return square(expMod(base: base, exp: exp / 2, m: m)) % m
    default:
        return (base * expMod(base: base, exp: exp - 1, m: m)) % m
    }
}
func fermatTest(_ n: Int) -> Bool {
    func tryIt(_ a: Int) -> Bool {
        return expMod(base:a, exp:n, m:n) == a
    }
    let randomA:Int = Int(arc4random_uniform(UInt32(n - 1)) + 1)
    return tryIt(randomA)
}
fermatTest(8)

func isPrimeFast(_ n: Int, times: Int) -> Bool {
    switch true {
    case times == 0:
        return true
    case fermatTest(n):
        return isPrimeFast(n, times: times - 1)
    default:
        return false
    }
}

isPrimeFast(31131, times: 10)


// Exercise 1.21
smallestDivisor(199)
smallestDivisor(1999)
smallestDivisor(19999)




// Exercise 1.23




