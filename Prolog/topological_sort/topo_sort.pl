can_topo_sort(Edges) :-
    retractall(edge(_,_)),
    assert_edges(Edges),
    (   cycle
    ->  writeln('can not make a topological sort - a cycle found')
    ;   writeln('can make a topological sort')
    ).
