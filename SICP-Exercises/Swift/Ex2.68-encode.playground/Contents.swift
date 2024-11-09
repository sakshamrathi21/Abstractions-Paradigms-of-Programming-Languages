import Cocoa

//: Exercise 2.68
//: The encode procedure takes as arguments a message and a tree and produces the list of bits that gives the encoded message.

extension Array {
    var match: (head: Element, tail: [Element])? {
        return (count > 0) ? (self[0], Array(self[1..<count])) : nil
    }
}

//: encodeSymbol is a procedure, which you must write, that returns the list of bits that encodes a given symbol according to a given tree. You should design encodeSymbol so that it signals an error if the symbol in not in the tree at all. Test your procedure by encoding the result you obtained in Exercise 2.67 with the sample tree and seeing whether it is the same as the original sample message.



func encodeSymbol(_ symbol: String, _ tree: Tree) -> [Int] {
    switch tree {
    case .Leaf(symbol: _, weight: _):
        return []
    case let .Branch(left: left, right: right, symbols: syms, weight: _):
        if syms.contains(symbol) {
            if symbols(left).contains(symbol) {
                return [0] + encodeSymbol(symbol, left)
            } else {
                return [1] + encodeSymbol(symbol, right)
            }
        } else {
            fatalError("The symbol:(\(symbol)) is not contained in the tree:(\(tree))")
        }
    }
}

func encode(_ message:[String], _ tree:Tree) -> [Int] {
    if let (head, tail) = message.match {
        return encodeSymbol(head, tree) + encode(tail, tree)
    } else {
        return []
    }
}

let sampleTree = makeCodeTree(makeLeaf("A", 4), makeCodeTree(makeLeaf("B", 2), makeCodeTree(makeLeaf("D", 1), makeLeaf("C", 1))))
let sampleMessage = ["A","D","A","B","B","C","A"]
let sampleEncoding = [0,1,1,0,0,1,0,1,0,1,1,1,0]

encode(sampleMessage, sampleTree)
