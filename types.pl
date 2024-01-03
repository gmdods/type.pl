% type.pl
%! unify(?Ctx:list, +X, -T) is semidet.
%! typeof(+X, -T) is semidet.

typeof(X, T) :- lambda(X), !, unify([], X, T).

% Simply Typed Lambda Calculus
% https://en.wikipedia.org/wiki/Simply_typed_lambda_calculus#Typing_rules

%% Syntax
lambda(abstract(X, E)) :- atomic(X), lambda(E).
lambda(apply(E1, E2)) :- lambda(E1), lambda(E2).

lambda(+(E1, E2)) :- lambda(E1), lambda(E2).
lambda(-(E1, E2)) :- lambda(E1), lambda(E2).
lambda(=:=(E1, E2)) :- lambda(E1), lambda(E2).
lambda(=\=(E1, E2)) :- lambda(E1), lambda(E2).
lambda(X) :- atomic(X).
lambda(X) :- integer(X).

%% (STLC-1)
unify(Ctx, X, T) :- atomic(X), member(typeis(X, T), Ctx).

%% (STLC-2)
unify(_Ctx, X, int) :- integer(X).
unify(Ctx, +(E1, E2), int) :- unify(Ctx, E1, int), unify(Ctx, E2, int).
unify(Ctx, -(E1, E2), int) :- unify(Ctx, E1, int), unify(Ctx, E2, int).
unify(Ctx, =:=(E1, E2), bool) :- unify(Ctx, E1, T), unify(Ctx, E2, T).
unify(Ctx, =\=(E1, E2), bool) :- unify(Ctx, E1, T), unify(Ctx, E2, T).

%% (STLC-3)
unify(Ctx, abstract(X, E), fn(S, T)) :-
	unify([typeis(X, S)|Ctx], E, T).

%% (STLC-4)
unify(Ctx, apply(E1, E2), T) :-
	unify(Ctx, E2, S),
	unify(Ctx, E1, fn(S, T)).

% Tests
% test(Pass).

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

test(systemf) :-
	findall(TE, typeof(abstract(x, abstract(y, x =:= y)), TE), TEs),
	TEs = [fn(A, fn(A, bool))], var(A),
	findall(TF, typeof(abstract(f, apply(f, 1)), TF), TFs),
	TFs = [fn(fn(int, B), B)], var(B),
	true.

