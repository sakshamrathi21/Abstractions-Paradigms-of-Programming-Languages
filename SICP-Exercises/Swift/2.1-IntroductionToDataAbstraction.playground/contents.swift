import Cocoa

// Chapter 2 - Building Abstractions with Data

// Chapter 1 was focused on computational processes and the role of procedures in program design. We saw how to use primitive data (numbers) and primitive operations (arithmetic operations), how to combine procedures to form compond procedures through composition, continionals adn the use of parameters.

// In chapter two we are going to look at more complex data. Chapter 1 was about building abstractions by combining procedures to form compound procedures, we turn in this chapter to another key aspect of any programming language: the means it provides for building abstractions by combining data objects to form combound data

// Why do we want combound data in a programming language?
// - to elevate the conceptual level at which we can design our programs
// - to increase the modularity of our designs
// - enhance expressive power


// Section 2.1 - Introduction to Data Abstraction
// In sections 1.1.8, we noted that a procedure used as an element in creating a more complex procedure could be regarded not only as a collection of particular operations but also as a procedural abstraction. So details of how the procedure was implemented could be suppressed. In other words, we could make an abstraction that would seperate the way the procedure would be used from the details of how the procedure was implemented. The analogous notion for compound data is called data abstraction. Which enables us to isolate how a compound data object is used from the details of how it is constracted from more primitive data objects.

// The basic idea of data abstraction is to structure the programs that are to use compound data objects so that they operate on "abstract data." Our programs should use data in such a way as to make no assumptions about the data that are not strictly nexessary for performing the task at hand. At the same time a "concrete" data representation is defined independent of the programs that use the data. The interface between these two parts of our system will be a set of procedures, called selectors and constructors, that implement the abstract data in terms of the concrete representations.

// 2.1.1 Example: Arithmetic Operations for Rational Numbers
// Pairs, To enable us to implement the concrete level of our data abstraction, our language provides a compound stracture called a pair, which can be constructed with the primitive procedure cons. This procedure takes two arguments and returns a compound data object that contains the two arguments as parts. Given a pair, we can extract the parts using the primitive procedures car and cdr. Thus we can use cons, car and cdr as follows

enum ConsPosition {
    case Left, Right
}

func cons<T>(_ a: T, _ b: T) -> ((ConsPosition) -> T) {
    func innerCons(i: ConsPosition) -> T {
        if i == .Left {
            return a;
        } else {
            return b;
        }
    }
    
    return innerCons;
}

func car<T>(_ innerCons: (ConsPosition) -> T) -> T {
    return innerCons(.Left);
}

func cdr<T>(_ innerCons: (ConsPosition) -> T) -> T {
    return innerCons(.Right);
}

// Above from https://gist.github.com/bcobb/70dac95ae10a39632e0b

let x = cons(1, 2)
car(x)
cdr(x)

let y = cons(3, 4)
let z = cons(x, y)
car(car(z))
car(cdr(z))

// Pairs can be used as general purpose building blocks to create all sorts of complex data structures. The single compount-data primitive pair, implemented by the procedures cons, car, and cdr, is the only glue we need. Data objects constructed from pairs are called list-structured data.

/*
(define (cons x y)
(lambda (m) (m x y)))
(define (car z)
(z (lambda (p q) p)))
(define (cdr z)
(z (lambda (p q) q)))
*/

// Representing rational numbers
// Pairs offer a natural way to complete the rational-number system. Simply represent a rational number as a pair of two integers: a numerator and a denominator. Then makeRat, numer and denom are readily implemented as follows

typealias Rational = ((ConsPosition) -> Int)

func makeRat1(_ n: Int, _ d:Int) -> Rational {
    return cons(n,d)
}
func numer(_ x: Rational) -> Int {
    return car(x)
}
func denom(_ x: Rational) -> Int {
    return cdr(x)
}

func printRat(_ x: Rational) {
    print("\(numer(x))/\(denom(x))")
}

func addRat1(_ x: Rational, _ y: Rational) -> Rational {
    return makeRat1((numer(x) * denom(y)) + (numer(y) * denom(x)), denom(x) * denom(y))
}
func subRat1(_ x: Rational, _ y: Rational) -> Rational {
    return makeRat1((numer(x) * denom(y)) - (numer(y) * denom(x)), denom(x) * denom(y))
}
func mulRat1(_ x: Rational, _ y: Rational) -> Rational {
    return makeRat1(numer(x) * numer(y), denom(x) * denom(y))
}
func divRat1(_ x: Rational, _ y: Rational) -> Rational {
    return makeRat1(numer(x) * denom(y), denom(x) * numer(y))
}
func isEqualRat(_ x: Rational, _ y: Rational) -> Bool {
    return (numer(x) * denom(y)) == (numer(y) * denom(x))
}

let oneHalf = makeRat1(1, 2)
printRat(oneHalf)
let oneThird = makeRat1(1, 3)
printRat(addRat1(oneHalf, oneThird))
printRat(mulRat1(oneHalf, oneThird))
printRat(addRat1(oneThird, oneThird))

// The final example shows that our rational-number implementation does not reduce numbers to lowest terms. We can remedy this by changing makeRat. 

func gcd(_ a: Int, _ b: Int) -> Int {
    if b == 0 {
        return abs(a)
    } else {
        return gcd(b, a % b)
    }
}

func makeRat2(_ n: Int, _ d:Int) -> Rational {
    let g = gcd(n, d)
    return cons(n/g, d/g)
}

func addRat2(_ x: Rational, _ y: Rational) -> Rational {
    return makeRat2((numer(x) * denom(y)) + (numer(y) * denom(x)), denom(x) * denom(y))
}

printRat(addRat2(oneThird, oneThird))


// 2.1.2 Abstraction Barriers
// In general, the underlying idea of data abstraction is to identify for each type of data object a basic set of operations in terms of which all manipulations of data objects of that type will be expressed, and then to use only those operations in manipulating the data.

// We can envision the structure of the rational-number system as shown in figure 2.1. The horizontal lines represent abstraction barriers that isolate different levels of the system. At each level the barrier separates the programs (abov) that use the data abstraction from the programs (below that implement the data abstraction.

// This simple idea has many advantages such as the fact that programs are easier to maintain and modify.

// For example an alternative way to address the problem of reducing rational numbers to lowest terms  is to perform the reduction whenever we access the parts of a rational number, rather than when we construct it. This leads to different constructor and selector procedures:

func makeRat(_ n: Int, d: Int) -> Rational {
    return cons(n, d)
}
func numer2(_ x: Rational) -> Int {
    let g = gcd(car(x), cdr(x))
    return car(x) / g
}
func denom2(_ x: Rational) -> Int {
    let g = gcd(car(x), cdr(x))
    return cdr(x) / g
}

// If our typical use of rational numberswas to access the numerators and denominators of the same rational numbers many times, it would be preferable to compute the gcd when the rational numbers are constructed. If not, we may be better off waiting until access time to compute the gcd. In any case, when we change from one representation to the other, the procedures addRat, subRat and so on do not have to be modified.


// 2.1.3 What is meant by Data?
// We began the rathional-number implementation in section 2.1.1 by implementing the rational-number operations addRat, subRat and so on in terms of three unspecified procedures: makeRat, numer and denom. At that point, we could think of the operations as being defined in terms of data objects -- numerators, denominators and rational numbers -- whose behavior was specified by the latter three procedures.

// But what is meant by data? It is not enough to say "whatever is implemented by the given selectors and constructors." Clearly, not every arbitrary set of three procedures can serve as an appropriate basis for the rational-number implementation. We need to guarantee that, if we construct a rational number x from a pair of integers n and d, then extracting the numer and the denom of x and dividing them should yield the same result as dividing n by d.

// In fact this is the only condition makeRat, numer, and denom must fulfill in order to form a suitable basis for a rational-number representation. In general, we can think of data as defined by some collection of selectors and constructors, together with specified conditions hat these procedures must fulfill in order to be a valid represntation.

// This point of view can serve to define not only "high-level" data objects, such as rational numbers, but lower-level objects as well. Consider the notion of a pair, which we used in order to define our rational numbers. We never actually saidwhat a pair was, only that the language supplied procedures conc, car, and cdr for operating on pairs. But the only thing we need to know about these three operations is that if we glue two objects together using cons we can retrieve the objects using car and cdr. Indeed, we mentioned that these three procedures are included as primitives in our language. However, any triple of procedures that satisfied the above condition can be used as the basis for implementing pairs. This point is illustrated strikingly by the fact that we could implement cons, car, and cdr without using any data structures at all but only using procedures. Here are the definitions

/*
enum ConsPosition {
    case Left, Right
}

func cons<T>(a: T, b: T) -> (ConsPosition -> T) {
    func innerCons(i: ConsPosition) -> T {
        if i == .Left {
            return a;
        } else {
            return b;
        }
    }
    
    return innerCons;
}

func car<T>(innerCons: ConsPosition -> T) -> T {
    return innerCons(.Left);
}

func cdr<T>(innerCons: ConsPosition -> T) -> T {
    return innerCons(.Right);
}
*/

// This use of procedures corresponds to nothing like our intuitive notion of what data should be. Nevertheless, all we need to do to show that this is a valid way to represent pairs is to verify that these procedures satisfy the condition given above.

// The subtle point to notice is that the value returned by cons(x, y) is a procedure -- namely the internally defined procedure dispatch, which takes one argument and returns either x or y depending on whether the argument is 0 or 1. Therefore this procedural implementation of pairs is a valid implmentation and if we access pairs using only cons, car, and cdr we cannot distinguish this implementation from one that uses "real" data structures.



// 2.1.4 Extended Exercise: Interval Arithmetic
// Alyssa P. Hacker is designing a system to help people solve engineering problems. One feature she wants to provide in her system is the ability to manipulate inexact quantities (such as measured parameters of physical devices). with known precision, so that when computations are done with such approcimate quantities the results will be numbers of known precision.

// Electrical engineers will be using Alyssa's system to compute electrical quantities. It is sometimes necessary for thm to compute the value of a parallel equivalent resistance Rp of two resistors R1 and R2 using the formula

//           1
// Rp = -----------
//      1/R1 + 1/R2

// Resistance values are usually known only up to some tolerance guaranteed by the manufacturer of the resistor. For example, if you buy a resistor labeled "6.8 ohms with 10% tolerance" you can only be sure that the resistor has a resistance between 
// 6.8 - 0.68 = 6.12 and
// 6.8 + 0.68 = 7.48 ohms.
// Thus, if you have a 6.8-ohm 10% resistor in parallel with a 4.7-ohm 5% resistor, the resistance of the combination can range from about 2.58 ohms (if the two resistors are at the lower bounds) to about 2.97 ohms (if the two resistors are at the upper bounds).

// Alyssa's idea is to implement "interval arithmetic" as a set of arithmetic operations for combining "intervals" (objects that represent the range of possible values of an inexact quantity). The result of adding, subtracting, multiplying, or dividing two intervals is itself an interval, representing a range of the result.

// Alyssa postulates the existence of an abstract object called an "interval" that has two endpoints: a lower bound and an upper bound. She also presumes that, given the endpoints of an interval, she can construct the interval using the data constructor make-interval().Alyssa first writes a procedure for adding two intervals. She reasons that the minimum value the sum could be is the sum of the two lower bounds and the maximum value it could be is the sum of the two upper bounds:

struct Interval {
    var lower: Double
    var upper: Double
    init(_ l: Double, _ u: Double) {
        self.lower = min(l,u)
        self.upper = max(l,u)
    }
}

let a = Interval(6.7, 6.9)
let b = Interval(2.1, 2.3)

func + (lhs: Interval, rhs: Interval) -> Interval {
    return Interval(lhs.lower + rhs.lower, lhs.upper + rhs.upper)
}
let c = a + b

// Alyssa also works out the product of two intervals by finding the minimum and the maximum of the products of the bounds using them as the bounds o fthe resulting interval.

func * (lhs: Interval, rhs: Interval) -> Interval {
    let p1 = lhs.lower * rhs.lower
    let p2 = lhs.lower * rhs.upper
    let p3 = lhs.upper * rhs.lower
    let p4 = lhs.upper * rhs.upper
    return Interval(min(p1, p2, p3, p4), max(p1, p2, p3, p4))
}
let d = a * b

// To divide two intervals, Alyssa multiplies the first by the reciprocal of the second. Note that the bounds of the reciprocal interval are the reciprocal of the upper bound and the reciprocal of the lower bound, in that order.

func / (lhs: Interval, rhs: Interval) -> Interval {
    return lhs * Interval(1 / rhs.upper, 1 / rhs.lower)
}
let e = d / b
a


// After debugging her program, Alyssa shows it to a potential user, who comlains that her program solves the wrong problem. He wants a program that can deal with numbers represented as a center value and an additive tolerance; for example, he wants to work with intervals such as 3.5 +- 0.15 rather than [3.35, 3.65]. Alyssa returns to her desk and fixes this problem by supplying an alternate constructor and alternate selectors:

extension Interval {
    var center: Double {
        get {
            return (lower + upper) / 2
        }
    }
    var width: Double {
        get {
            return (upper - lower) / 2
        }
    }
    init(center: Double, width: Double) {
        self.lower = center - width
        self.upper = center + width
    }
}

// Unfortunately, most of Alyssa's users are engineers. Real engineering situations usually involve measurements with only a small uncertainty, measured as the ratio of the width of the interval to the midpoint of the interval. Engineers usually specify percentage tolerances on the parameters of devices, as in the resistor specifications given earlier.

extension Interval {
    var percent: Double {
        get {
            return self.width / self.center
        }
    }
    init(center: Double, percent: Double) {
        self.init(center: center, width: center * percent)
    }
}

// After considerable work, Alyssa P. Hacker delivers her finished system. Several years later, after she has forgotten all about it, she gets a frenzied call from an irate user, Lem E. Tweakit. It seems that Lem has noticed that the formula for parallel resistors can be written in two algebraically equivalent ways

//  R1R2
// -------
// R1 + R2

//      1
// -----------
// 1/R1 + 1/R2

// He has written the following two programs, each of which computes the parallel-resistors formula differently

func parOne(_ a: Interval, _ b: Interval) -> Interval {
    return (a * b) / (a + b)
}

func parTwo(_ a: Interval, _ b: Interval) -> Interval {
    let one = Interval(1, 1)
    return one / ((one / a) + (one / b))
}

let r1 = Interval(center: 3.3, percent: 0.01)
let r2 = Interval(center: 4.7, percent: 0.05)

let pOne = parOne(r1, r2)
pOne.center
pOne.percent
let pTwo = parTwo(r1, r2)
pTwo.center
pTwo.percent







