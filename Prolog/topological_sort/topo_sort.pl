can_topo_sort(Edges) :-
    retractall(edge(_,_)),
    assert_edges(Edges),
    (   cycle
    ->  writeln('can not make a topological sort - a cycle found'),
        fail 
    ;   writeln('can make a topological sort')
    ).

assert_edges([]).
assert_edges([X-Y | Es]) :-
    assert(edge(X,Y)),
    assert_edges(Es).

cycle :-
    findall(N, (edge(N,_); edge(_,N)), Ns0),
    sort(Ns0, Ns),
    member(N, Ns),
    dfs_cycle(N, []),
    !. 

% Исправленный DFS для поиска циклов
dfs_cycle(N, Path) :-
    member(N, Path), !. 
dfs_cycle(N, Path) :-
    edge(N, M),      
    dfs_cycle(M, [N|Path]). 