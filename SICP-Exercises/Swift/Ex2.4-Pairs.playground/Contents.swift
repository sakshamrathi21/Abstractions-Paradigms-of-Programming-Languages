import Cocoa

// Exercise 2.4
// Here is an alternative procedural representation of pairs. For this representation, verify that car(cons(x, y)) yields x for any objects x and y.

typealias DRPFunction = (Int, Int) -> Int
typealias Pair = (DRPFunction) -> Int

func cons2(_ x: Int, _ y: Int) -> Pair {
    return { (m:DRPFunction) -> Int in
        return m(x, y)
    }
}

func car2(_ z:Pair) -> Int {
    return z({ (p:Int, q:Int) -> Int in return p })
}

let a = cons2(2, 3)
car2(a)

func cdr2(_ z:Pair) -> Int {
    return z({ (p:Int, q:Int) -> Int in return q })
}
cdr2(a)

print("\(a)")

