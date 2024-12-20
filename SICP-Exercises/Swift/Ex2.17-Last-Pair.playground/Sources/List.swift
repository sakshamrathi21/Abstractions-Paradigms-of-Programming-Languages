//
//  List.swift
//  swiftz_core
//
//  Created by Maxwell Swadling on 3/06/2014.
//  Copyright (c) 2014 Maxwell Swadling. All rights reserved.
//  Modified by Daniel Pink so that it could fit into one file for easy inclusion in these playground files.
//

public func error<A>(_ x : String) -> A {
    return fatalError(x) as! A
}

public func undefined<A>() -> A {
    return error("Undefined")
}

/*
/// Flip a function's arguments
public func flip<A, B, C>(f: ((A, B) -> C), _ b: B, _ a: A) -> C {
    return f(a, b)
}

/// Flip a function's arguments and return a function that takes
/// the arguments in flipped order.
public func flip<A, B, C>(f: (A, B) -> C, b: B, _ a: A) -> C {
    return f(a, b)
}

/// Flip a function's arguments and return a curried function that takes
/// the arguments in flipped order.
public func flip<A, B, C>(f: A -> B -> C) -> ((B) -> (A)) -> C {
    return { b in { a in f(a)(b) } }
}
*/

// cons(_ head: A, _ tail: List<A>) -> List<A>
public func flip<A, B>(f:((A, B) -> B)) -> ((B, A) -> B) {
    
}


/// An enum representing the possible values a list can match against.
public enum ListMatcher<A> {
    /// The empty list.
    case Nil
    /// A cons cell.
    case Cons(A, List<A>)
}

/// A lazy ordered sequence of homogenous values.
///
/// A List is typically constructed by two primitives: Nil and Cons.  Due to limitations of the
/// language, we instead provide a nullary constructor for Nil and a Cons Constructor and an actual
/// static function named `cons(: tail:)` for Cons.  Nonetheless this representation of a list is
/// isomorphic to the traditional inductive definition.  As such, the method `match()` is provided
/// that allows the list to be destructured into a more traditional `Nil | Cons(A, List<A>)` form
/// that is also compatible with switch-case blocks.
///
/// This kind of list is optimized for access to its length, which always occurs in O(1), and
/// modifications to its head, which always occur in O(1).  Access to the elements occurs in O(n).
///
/// Unlike an Array, a List can potentially represent an infinite sequence of values.  Because the
/// List holds these values in a lazy manner, certain primitives like iteration or reversing the
/// list will force evaluation of the entire list.  For infinite lists this can lead to a program
/// diverging.
extension Int {
    func successor() -> Int {
        return self + 1
    }
}

public struct List<A> {
    let len: Int
    let next: () -> (head: A, tail: List<A>)
    
    /// Constructs a potentially infinite list.
    init(_ next: @autoclosure @escaping () -> (head: A, tail: List<A>), isEmpty : Bool = false) {
        self.len = isEmpty ? 0 : -1
        self.next = next
    }
    
    /// Constructs the empty list.
    ///
    /// Attempts to access the head or tail of this list in an unsafe manner will throw an exception.
    public init() {
        self.init((error("Attempted to access the head of the empty list."), error("Attempted to access the tail of the empty list.")), isEmpty: true)
    }
    
    /// Construct a list with a given head and tail.
    public init(_ head: A, _ tail: List<A> = List<A>()) {
        if tail.len == -1 {
            self.len = -1
        } else {
            self.len = tail.len.successor()
        }
        self.next = { (head, tail) }
    }
    
    public init(_ array:[A]) {
        self.init()
        for value in array {
            self = cons(value, self)
        }
    }
    
    /// Appends an element onto the front of a list.
    public static func cons(_ head: A, _ tail: List<A>) -> List<A> {
        return List(head, tail)
    }
    
    /// Destructures a list.  
    /// If the list is empty, the result is Nil. If the list contains a value
    /// the result is Cons(head, tail).
    public func match() -> ListMatcher<A> {
        if self.len == 0 {
            return .Nil
        }
        let (hd, tl) = self.next()
        return .Cons(hd, tl)
    }
    
    /// Indexes into an array.
    /// Indexing into the empty list will throw an exception.
    public subscript(n : UInt) -> A {
        switch self.match() {
        case .Nil:
            return error("Cannot extract an element from an empty list.")
        case let .Cons(x, _) where n == 0:
            return x
        case let .Cons(_, xs):
            return xs[n - 1]
        }
    }
    
    /// Creates a list of n repeating values.
    public static func replicate(n : UInt, value : A) -> List<A> {
        var l = List<A>()
        for _ in 0..<n {
            l = List.cons(value, l)
        }
        return l
    }
    
    /// Returns the first element in the list, or None if the list is empty.
    public func head() -> Optional<A> {
        switch self.match() {
        case .Nil:
            return .none
        case let .Cons(head, _):
            return .some(head)
        }
    }
    public func car() -> Optional<A> {
        return head()
    }
    
    /// Returns the tail of the list, or None if the list is empty.
    public func tail() -> Optional<List<A>> {
        switch self.match() {
        case .Nil:
            return .none
        case let .Cons(_, tail):
            return .some(tail)
        }
    }
    public func cdr() -> Optional<List<A>> {
        return tail()
    }
    
    /// Returns whether or not the receiver is the empty list.
    public func isEmpty() -> Bool {
        return self.len == 0
    }
    
    /// Returns whether or not the receiver has a countable number of elements.
    ///
    /// It may be dangerous to attempt to iterate over an infinite list of values because the loop
    /// will never terminate.
    public func isFinite() -> Bool {
        return self.len != -1
    }
    
    /// Returns the length of the list.
    ///
    /// For infinite lists this function will throw an exception.
    public func length() -> UInt {
        if self.len == -1 {
            return error("Cannot take the length of an infinite list.")
        }
        return UInt(self.len)
    }
    
    /// Yields a new list by applying a function to each element of the receiver.
    public func map<B>(_ f:@escaping (A) -> B) -> List<B> {
        switch self.match() {
        case .Nil:
            return []
        case let .Cons(hd, tl):
            return List<B>(f(hd), tl.map(f))
        }
    }
    
    /// Appends two lists together.
    ///
    /// If the receiver is infinite, the result of this function will be the receiver itself.
    public func append(_ rhs : List<A>) -> List<A> {
        switch self.match() {
        case .Nil:
            return rhs
        case let .Cons(x, xs):
            return List.cons(x, xs.append(rhs))
        }
    }
    
    /// Maps a function over a list and concatenates the results.
    public func concatMap<B>(_ f: (A) -> List<B>) -> List<B> {
        return self.reduce({ l, r in l.append(f(r)) }, initial: List<B>())
    }
    
    /// Returns a list of elements satisfying a predicate.
    public func filter(_ p: (A) -> Bool) -> List<A> {
        switch self.match() {
        case .Nil:
            return []
        case let .Cons(x, xs):
            return p(x) ? List(x, xs.filter(p)) : xs.filter(p)
        }
    }
    
    /// Applies a binary operator to reduce the elements of the receiver to a single value.
    public func reduce<B>(_ f: (B) -> (A) -> B, initial : B) -> B {
        switch self.match() {
        case .Nil:
            return initial
        case let .Cons(x, xs):
            return xs.reduce(f, initial: f(initial)(x))
        }
    }
    
    /// Applies a binary operator to reduce the elements of the receiver to a single value.
    public func reduce<B>(_ f : (B, A) -> B, initial : B) -> B {
        switch self.match() {
        case .Nil:
            return initial
        case let .Cons(x, xs):
            return xs.reduce(f, initial: f(initial, x))
        }
    }
    
    /// Returns a list of successive applications of a function to the elements of the receiver.
    ///
    /// e.g.
    ///
    ///     [x0, x1, x2, ...].scanl(f, initial: z) == [z, f(z)(x0), f(f(z)(x0))(x1), f(f(f(z)(x2))(x1))(x0)]
    ///       [1, 2, 3, 4, 5].scanl(+, initial: 0) == [0, 1, 3, 6, 10, 15]
    ///
    public func scanl<B>(_ f: (B) -> (A) -> B, initial : B) -> List<B> {
        switch self.match() {
        case .Nil:
            return List<B>(initial)
        case let .Cons(x, xs):
            return List<B>.cons(initial, xs.scanl(f, initial: f(initial)(x)))
        }
    }
    
    /// Returns a list of successive applications of a function to the elements of the receiver.
    ///
    /// e.g.
    ///
    ///     [x0, x1, x2, ...].scanl(f, initial: z) == [z, f(z, x0), f(f(z, x0), x1), f(f(f(z, x2), x1), x0)]
    ///       [1, 2, 3, 4, 5].scanl(+, initial: 0) == [0, 1, 3, 6, 10, 15]
    ///
    public func scanl<B>(_ f: (B, A) -> B, initial : B) -> List<B> {
        switch self.match() {
        case .Nil:
            return List<B>(initial)
        case let .Cons(x, xs):
            return List<B>.cons(initial, xs.scanl(f, initial: f(initial, x)))
        }
    }
    
    /// Like scanl but draws its initial value from the first element of the receiver itself.
    public func scanl1(_ f: (A) -> (A) -> A) -> List<A> {
        switch self.match() {
        case .Nil:
            return []
        case let .Cons(x, xs):
            return xs.scanl(f, initial: x)
        }
    }
    
    /// Like scanl but draws its initial value from the first element of the receiver itself.
    public func scanl1(_ f: (A, A) -> A) -> List<A> {
        switch self.match() {
        case .Nil:
            return []
        case let .Cons(x, xs):
            return xs.scanl(f, initial: x)
        }
    }
    
    /// Returns the first n elements of the receiver.
    public func take(_ n: UInt) -> List<A> {
        if n == 0 {
            return []
        }
        
        switch self.match() {
        case .Nil:
            return []
        case let .Cons(x, xs):
            return List.cons(x, xs.take(n - 1))
        }
    }
    
    /// Returns the remaining list after dropping n elements from the receiver.
    public func drop(_ n: UInt) -> List<A> {
        if n == 0 {
            return self
        }
        
        switch self.match() {
        case .Nil:
            return []
        case let .Cons(_, xs):
            return xs.drop(n - 1)
        }
    }
    
    /// Returns a tuple of the first n elements and the remainder of the list.
    public func splitAt(_ n: UInt) -> (List<A>, List<A>) {
        return (self.take(n), self.drop(n))
    }
    
    /// Returns a list of the longest prefix of elements satisfying a predicate.
    public func takeWhile(_ p: (A) -> Bool) -> List<A> {
        switch self.match() {
        case .Nil:
            return []
        case .Cons(let x, let xs):
            if p(x) {
                return List.cons(x, xs.takeWhile(p))
            }
            return []
        }
    }
    
    /// Returns a list of the remaining elements after the longest prefix of elements satisfying a
    /// predicate has been removed.
    public func dropWhile(_ p: (A) -> Bool) -> List<A> {
        switch self.match() {
        case .Nil:
            return []
        case let .Cons(x, xs):
            if p(x) {
                return xs.dropWhile(p)
            }
            return self
        }
    }
    
    /// Returns the elements of the receiver in reverse order.
    ///
    /// For infinite lists this function will diverge.
    public func reverse() -> List<A> {
        return self.reduce(flip(List.cons), initial: [])
    }
    
    /// Given a predicate, searches the list until it find the first match, and returns that,
    /// or None if no match was found.
    ///
    /// For infinite lists this function will diverge.
    public func find(_ pred: (A) -> Bool) -> Optional<A> {
        for x in self {
            if pred(x) {
                return x
            }
        }
        return nil
    }
    
    /// For an associated list, such as [(1,"one"),(2,"two")], takes a function (pass the identity
    /// function) and a key and returns the value for the given key, if there is one, or None
    /// otherwise.
    //    public func lookup<K : Equatable, V>(ev : A -> (K, V), key : K) -> Optional<V> {
    //        return { ev($0).1 } <^> self.find({ ev($0).0 == key })
    //    }
    
    /// Returns a List of an infinite number of iteratations of applications of a function to an
    /// initial value.
    //public static func iterate(f : A -> A, initial : A) -> List<A> {
    //	return List((initial, iterate(f, initial: f(initial))))
    //}
    
    /// Cycles a finite list into an infinite list.
    public func cycle() -> List<A> {
        let (hd, tl) = self.next()
        return List((hd, (tl + [hd]).cycle()))
    }
}

public func cons<A>(_ left: A, _ right: List<A>) -> List<A> {
    return List(left, right)
}

/// Flattens a list of lists into a single lists.
public func concat<A>(_ xss : List<List<A>>) -> List<A> {
    return xss.reduce(+, initial: [])
}

/// Appends two lists together.
public func +<A>(lhs : List<A>, rhs : List<A>) -> List<A> {
    return lhs.append(rhs)
}

/// MARK: Equatable

public func ==<A : Equatable>(lhs : List<A>, rhs : List<A>) -> Bool {
    switch (lhs.match(), rhs.match()) {
    case (.Nil, .Nil):
        return true
    case let (.Cons(lHead, lTail), .Cons(rHead, rTail)):
        return lHead == rHead && lTail == rTail
    default:
        return false
    }
}

/// MARK: Collection Protocols

extension List : ExpressibleByArrayLiteral {
    public typealias Element = A
    
    public init(arrayLiteral s: Element...) {
        var xs : [A] = []
        var g = s.makeIterator()
        while let x : A = g.next() {
            xs.append(x)
        }
        
        var l = List()
        for x in xs.reversed() {
            l = List(x, l)
        }
        self = l
    }
}

public final class ListGenerator<A> : IteratorProtocol {
    var l : List<A>
    
    public func next() -> Optional<A> {
        if l.len == 0 {
            return nil
        }
        
        let (hd, tl) = l.next()
        l = tl
        return hd
    }
    
    public init(_ l : List<A>) {
        self.l = l
    }
}

extension List: Sequence {
    public func generate() -> ListGenerator<A> {
        return ListGenerator(self)
    }
}

extension List: Collection {
    /// Returns the position immediately after the given index.
    ///
    /// The successor of an index must be well defined. For an index `i` into a
    /// collection `c`, calling `c.index(after: i)` returns the same index every
    /// time.
    ///
    /// - Parameter i: A valid index of the collection. `i` must be less than
    ///   `endIndex`.
    /// - Returns: The index value immediately after `i`.
    public func index(after i: UInt) -> UInt {
        return i + 1
    }

    public typealias Index = UInt
    
    public var startIndex: UInt { get { return 0 } }
    
    public var endIndex: UInt {
        get {
            return self.isFinite() ? self.length() : error("An infinite list has no end index.")
        }
    }
}



extension List : CustomStringConvertible {
    public func fmap<B>(_ f: @escaping ((A) -> B)) -> List<B> {
        return self.map(f)
    }
    
    public var description : String {
        let x = self.fmap({ "\($0)" }).joined(separator: ", ")
        return "[\(x)]"
    }
}

public func car<A>(list: List<A>) -> Optional<A> {
    return list.head()
}
public func cdr<A>(list: List<A>) -> Optional<List<A>> {
    return list.tail()
}

/// MARK: Control.*
/*
extension List : Functor {
typealias B = Any
typealias FB = List<B>

public func fmap<B>(f : (A -> B)) -> List<B> {
return self.map(f)
}
}

extension List : Pointed {
public static func pure(a: A) -> List<A> {
return List(a, [])
}
}

extension List : Applicative {
typealias FA = List<A>
typealias FAB = List<A -> B>

public func ap<B>(f : List<A -> B>) -> List<B> {
return concat(f.map({ self.map($0) }))
}
}

extension List : Monad {
public func bind<B>(f: A -> List<B>) -> List<B> {
return self.concatMap(f)
}
}
*/
