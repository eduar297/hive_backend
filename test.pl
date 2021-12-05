s(a,b).
s(a,c).
s(b,d).
s(b,e).
s(d,j).
s(e,f).
s(f,i).
s(c,h).
s(c,g).
s(g,k).
s(k,l).
s(k,m).
s(b,l).

:-dynamic node/2.

adj([U, Lvl], A):-
    Lvl1 is Lvl + 1,
    findall([V, Lvl1], s(U,V), A).

bfs([], _):-!.

bfs([[U, _]|Q], Visited):-
    member(U, Visited),
    bfs(Q, Visited).

bfs([[U, Lvl]|Q], Visited):-
    not(member(U, Visited)),
    assert(node(U, Lvl)),
    adj([U, Lvl], A),
    append([U], Visited, Visited1),
    append(Q, A, Q1),
    bfs(Q1, Visited1).