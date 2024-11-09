import Cocoa

// Exercise 1.44
// The idea of smoothing a function is an important concept in signal processing. If f is a function and dx is some small number, then the smoothed version of f is the function whose value as a point x is the average of f(x - dx, f(x), and f(x + dx). 

// Write a procedure smooth that takes as input a procedure that computes f and returns a procedure that computes the smoothed f.

func smooth(_ f:@escaping (Double) -> Double) -> (Double) -> Double {
    let dx = 0.001
    return { (x: Double) -> Double in return (f(x - dx) + f(x) + f(x + dx)) / 3 }
}

// It is sometimes valuable to repeatedly smooth a function (that is, smooth the smoothed function) to obtain the n-fold smoothed function. Show how to generate the n-fold smoothed function of any given function using smooth and repeated from exersize 1.43

func compose<T>(_ f:@escaping (T) -> T, _ g:@escaping (T) -> T) -> (T) -> T {
    return { (x: T) -> T in return f(g(x)) }
}

func repeatIter<T>(_ f:@escaping (T) -> T, _ g:@escaping (T) -> T, _ step: Int) -> (T) -> T {
    if (step == 1) {
        return g
    } else {
        return repeatIter(f, compose(f, g), step - 1)
    }
}

func repeated<T>(_ f:@escaping (T) -> T , _ n: Int) -> (T) -> T {
    return repeatIter(f, f, n)
}

repeated(smooth, 3)(sin)(0.5)


func nFoldSmooth(_ f:@escaping (Double) -> Double, _ n: Int) -> (Double) -> Double {
    return repeated(smooth, n)(f)
}

sin(0.5)
smooth(sin)(0.5)
nFoldSmooth(sin, 1)(0.5)
nFoldSmooth(sin, 2)(0.5)


