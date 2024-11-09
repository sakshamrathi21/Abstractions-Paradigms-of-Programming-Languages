letter_count([], 0, 0, 0) :- !.
letter_count([r], 1, 0, 0) :- !.
letter_count([r], 0, 1, 0) :- !.
letter_count([w], 0, 0, 1) :- !.
letter_count([Head | Tail], X, Y, Z) :- Head == 'r', letter_count(Tail, X1, Y, Z), X is X1 + 1, !.
letter_count([Head | Tail], X, Y, Z) :- Head == 'b', letter_count(Tail, X, Y1, Z), Y is Y1 + 1, !.
letter_count([Head | Tail], X, Y, Z) :- Head == 'w', letter_count(Tail, X, Y, Z1), Z is Z1 + 1, !.
string_sequence(0, 0, 0, []) :- !.
string_sequence(1, 0, 0, [r]) :- !.
string_sequence(0, 1, 0, [b]) :- !.
string_sequence(0, 0, 1, [w]) :- !.
string_sequence(X, Y, Z, S) :- X > 0, X1 is X - 1, string_sequence(X1, Y, Z, S1), append([r], S1, S), !.
string_sequence(0, Y, Z, S) :- Y > 0, Y1 is Y - 1, string_sequence(0, Y1, Z, S1), append([b], S1, S), !.
string_sequence(0, 0, Z, S) :- Z > 0, Z1 is Z - 1, string_sequence(0, 0, Z1, S1), append([w], S1, S), !.
arrange(L, M) :- letter_count(L, X, Y, Z), string_sequence(X, Y, Z, M).