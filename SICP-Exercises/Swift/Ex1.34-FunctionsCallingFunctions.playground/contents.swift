// Playground - noun: a place where people can play

import Cocoa

func f(_ g: (Int) -> Int) -> Int {
    return g(2)
}

func square(_ x: Int) -> Int { return x * x }
f(square)

f({ (z:Int) -> Int in return z * (z + 1) })

// f(f)
// Swifts type system catches this one.

