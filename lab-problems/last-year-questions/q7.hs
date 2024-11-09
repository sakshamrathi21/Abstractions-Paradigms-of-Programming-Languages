grid m n = [(x,y) | x <- [0..m], y <- [0..n], x /= y]
square n = grid n n