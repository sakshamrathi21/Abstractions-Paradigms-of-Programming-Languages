rooms([room(_, 5), room(_, 4), room(_, 3), room(_, 2), room(_, 1)]).
hostel(Rooms) :- rooms(Rooms),
member(room(anirudh, A), Rooms), A\=5,
member(room(krish, K), Rooms), K \= 1,
member(room(mrinal, M), Rooms), M \= 1, M \= 5,
member(room(parthiv, P), Rooms),
not(adjacent(M, P)), not(adjacent(M, K)),
member(room(nilabha, N), Rooms), N > K,
print_rooms(Rooms).

adjacent(X, Y) :- X =:= Y+1.
adjacent(X, Y) :- X =:= Y-1.
print_rooms([A | B]) :- write(A), nl, print_rooms(B).
print_rooms([]).
