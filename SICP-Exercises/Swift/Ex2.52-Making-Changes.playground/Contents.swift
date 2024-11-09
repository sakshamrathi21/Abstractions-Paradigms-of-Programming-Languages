import Cocoa

// Exercise 2.52
// Make changes to the square limit of wave shown in Figure 2.9 by working at each of the levels described above. In particular:

// a. Add some segments to the primitive wave painter of Exercise 2.49 (to add a smile, for example)

public func wave(frame: Frame) {
    segmentsToPainter([Segment(startPoint: Point(x: 0.165, y: 0.945), endPoint: Point(x: 0.465, y: 0.665)),
        Segment(startPoint: Point(x: 0.465, y: 0.665), endPoint: Point(x: 0.465, y: 0.285)),
        Segment(startPoint: Point(x: 0.465, y: 0.455), endPoint: Point(x: 0.745, y: 0.585)),
        Segment(startPoint: Point(x: 0.465, y: 0.665), endPoint: Point(x: 0.755, y: 0.925)),
        Segment(startPoint: Point(x: 0.475, y: 0.455), endPoint: Point(x: 0.185, y: 0.615)),
        Segment(startPoint: Point(x: 0.245, y: 0.265), endPoint: Point(x: 0.685, y: 0.295)),
        Segment(startPoint: Point(x: 0.685, y: 0.295), endPoint: Point(x: 0.686, y: 0.035)),
        Segment(startPoint: Point(x: 0.685, y: 0.035), endPoint: Point(x: 0.245, y: 0.065)),
        Segment(startPoint: Point(x: 0.245, y: 0.065), endPoint: Point(x: 0.245, y: 0.265)),
        Segment(startPoint: Point(x: 0.5, y: 0.135), endPoint: Point(x: 0.245, y: 0.065)),
        Segment(startPoint: Point(x: 0.5, y: 0.135), endPoint: Point(x: 0.686, y: 0.035))])(frame)
}
draw(wave)


// b. Change the pattern constructed by cornerSplit (for example, by using only one copy of the upSplit and rightSplit images instead of two).

func cornerSplit(_ painter: @escaping Painter, n: Int) -> Painter {
    if n == 0 {
        return painter
    } else {
        let up = upSplit(painter, n:n - 1)
        let right = rightSplit(painter, n:n - 1)
        let topLeft = beside(left: up, right: wave)
        let bottomRight = below(top: right, bottom:wave)
        let corner = cornerSplit(painter, n:n - 1)
        
        return beside(left: below(top: topLeft, bottom: painter),
                      right: below(top: corner, bottom: bottomRight))
    }
}
draw(cornerSplit(wave, n:3))

// c. Modify the version of squareLimit that uses squareOfFour so as to assemble the corners in a different pattern. (For example you might make the big Mr. Rogers look outward from each corner of the square.)


func squareOfFour(_ tl: @escaping Transformer,
                  _ tr: @escaping Transformer,
                  bl: @escaping Transformer,
                  br: @escaping Transformer) -> Transformer {
    return { painter in
        let top = beside(left: tl(painter), right: tr(painter))
        let bottom = beside(left: bl(painter), right: br(painter))
        return below(top: bottom, bottom: top)
    }
}

func squareLimit(_ painter: @escaping Painter, n: Int) -> Painter {
    let combine4 = squareOfFour(rotate180, flipVert, bl:flipHoriz, br:flipHoriz)
    return combine4(cornerSplit(painter, n:n))
}

draw(squareLimit(wave, n:4))





