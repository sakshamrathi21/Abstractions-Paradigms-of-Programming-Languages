palindrome([]) :- !.
palindrome([_]) :- !.
palindrome([H | T]) :- append(Mid, [H], T),
                       palindrome(Mid), !.