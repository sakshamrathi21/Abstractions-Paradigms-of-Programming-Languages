last_but_one([], nil) :- !.
last_but_one([_], nil) :- !.
last_but_one([X, _], X) :- !.    
last_but_one([_|T], X) :- last_but_one(T, X).