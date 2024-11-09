myLength([], 0) :- !.
myLength([_ | T], N) :- myLength(T, M), N is M + 1.