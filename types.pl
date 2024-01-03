% unify Expr is T given Ctx

unify(X, int, _Ctx) :- integer(X).

unify(+(E1, E2), int, Ctx) :- unify(E1, int, Ctx), unify(E2, int, Ctx).
unify(-(E1, E2), int, Ctx) :- unify(E1, int, Ctx), unify(E2, int, Ctx).
unify(=:=(E1, E2), bool, Ctx) :- unify(E1, T, Ctx), unify(E2, T, Ctx).
unify(=\=(E1, E2), bool, Ctx) :- unify(E1, T, Ctx), unify(E2, T, Ctx).

unify(X, T, Ctx) :- atomic(X), member(type(X, T), Ctx).
unify(abstract(X, E), fn(S, T), Ctx) :-
	unify(E, T, [type(X, S)|Ctx]).
unify(apply(E1, E2), T, Ctx) :-
	unify(E2, S, Ctx),
	unify(E1, fn(S, T), Ctx).

typeof(X, T) :- unify(X, T, []).


test(unify) :-
	findall(TL, typeof(abstract(x, x + x), TL), TLs),
	TLs = [fn(int, int)],
	findall(TA, typeof(apply(abstract(x, x + x), 1), TA), TAs),
	TAs = [int],
	findall(TR, typeof(abstract(f, apply(f, 1) + 1), TR), TRs),
	TRs = [fn(fn(int, int), int)],
	findall(TC, typeof(abstract(x, abstract(y, x + y)), TC), TCs),
	TCs = [fn(int, fn(int, int))],
	true.

