sum_hexagons_value([], 0):-!.
sum_hexagons_value([H|T], S):-
    sum_hexagons_value(T, S1),
    S is H + S1.