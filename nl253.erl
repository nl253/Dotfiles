-module(nl253).
-compile(export_all).

-spec star_worker(function(), any(), any()) -> any().

star_worker(F, X, Parent) -> 
    Parent ! {self(), F(X)}.

-spec map_star(function(), [any()]) -> [any()].

map_star(_F, []) -> [];
map_star(F, [X|XS]) -> 
    WID = spawn(?MODULE, star_worker, [F, X, self()]),
    YS = map_star(F, XS),
    receive {WID, Y} -> [Y|YS] end.

ring_worker(F, X, Parent) -> 
    Y = F(X),
    receive YS -> Parent ! [Y|YS] end.

-spec map_ringInner(function(), [any()], pid()) -> pid().

map_ringInner(_F, [], Left) -> Left;
map_ringInner(F, [X|XS], _Left) ->
    Current = spawn(?MODULE, worker_alt, [F, X, self()]),
    map_ringInner(F, XS, Current).

-spec map_ring(function(), [any()]) -> [any()].

map_ring(F, XS) ->
    Last = map_ringInner(F, XS, self()),
    Last ! [],
    receive YS -> YS end.
