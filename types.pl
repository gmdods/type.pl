% Simply Typed Lambda Calculus
% https://en.wikipedia.org/wiki/Simply_typed_lambda_calculus#Typing_rules
%
%! unify(?Ctx:list, +X, -T) is semidet.

% (STLC-1)
unify(Ctx, X, T) :- atomic(X), member(type(X, T), Ctx).

% (STLC-2)
unify(_Ctx, X, int) :- integer(X).
unify(Ctx, +(E1, E2), int) :- unify(Ctx, E1, int), unify(Ctx, E2, int).
unify(Ctx, -(E1, E2), int) :- unify(Ctx, E1, int), unify(Ctx, E2, int).
unify(Ctx, =:=(E1, E2), bool) :- unify(Ctx, E1, T), unify(Ctx, E2, T).
unify(Ctx, =\=(E1, E2), bool) :- unify(Ctx, E1, T), unify(Ctx, E2, T).

% (STLC-3)
unify(Ctx, abstract(X, E), fn(S, T)) :-
	unify([type(X, S)|Ctx], E, T).

% (STLC-4)
unify(Ctx, apply(E1, E2), T) :-
	unify(Ctx, E2, S),
	unify(Ctx, E1, fn(S, T)).

typeof(X, T) :- unify([], X, T).


test(unify) :-
	findall(TL, typeof(abstract(x, x + x), TL), TLs),
	TLs = [fn(int, int)],
	findall(TA, typeof(apply(abstract(x, x + x), 1), TA), TAs),
	TAs = [int],
	findall(TR, typeof(abstract(f, apply(f, 1) + 1), TR), TRs),
	TRs = [fn(fn(int, int), int)],
	findall(TC, typeof(abstract(x, abstract(y, x =:= y)), TC), TCs),
	TCs = [fn(int, fn(int, bool))],
	true.

test(systemf) :-
	findall(TF, typeof(abstract(f, apply(f, 1)), TF), TFs),
	TFs = [fn(fn(int, A), A)], var(A),
	true.

