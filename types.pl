% type.pl
%! unify(?Ctx:list, +X, -T) is semidet.
%! typeof(+X, -T) is semidet.

typeof(X, T) :- calculus(X), !, unify([], X, T).

%% Syntax
calculus(lambda(X, E)) :- atomic(X), calculus(E).
calculus(apply(E1, E2)) :- calculus(E1), calculus(E2).

calculus(+(E1, E2)) :- calculus(E1), calculus(E2).
calculus(-(E1, E2)) :- calculus(E1), calculus(E2).
calculus(=:=(E1, E2)) :- calculus(E1), calculus(E2).
calculus(=\=(E1, E2)) :- calculus(E1), calculus(E2).
calculus(X) :- atomic(X).
calculus(X) :- integer(X).

%% Types
type(int).
type(bool).
type(fn(T, S)) :- type(T), type(S).

% Simply Typed Lambda Calculus
% https://en.wikipedia.org/wiki/Simply_typed_lambda_calculus#Typing_rules

%% (STLC-1)
unify(Ctx, X, T) :- atomic(X), member(typeis(X, T), Ctx).

%% (STLC-2)
unify(_Ctx, X, int) :- integer(X).
unify(Ctx, +(E1, E2), int) :- unify(Ctx, E1, int), unify(Ctx, E2, int).
unify(Ctx, -(E1, E2), int) :- unify(Ctx, E1, int), unify(Ctx, E2, int).
unify(Ctx, =:=(E1, E2), bool) :- unify(Ctx, E1, T), unify(Ctx, E2, T).
unify(Ctx, =\=(E1, E2), bool) :- unify(Ctx, E1, T), unify(Ctx, E2, T).

%% (STLC-3)
unify(Ctx, lambda(X, E), fn(S, T)) :-
	unify([typeis(X, S)|Ctx], E, T).

%% (STLC-4)
unify(Ctx, apply(E1, E2), T) :-
	unify(Ctx, E2, S),
	unify(Ctx, E1, fn(S, T)).

% Tests
% test(Pass).

test(unify) :-
	findall(TL, typeof(lambda(x, x + x), TL), TLs),
	TLs = [fn(int, int)],
	findall(TA, typeof(apply(lambda(x, x + x), 1), TA), TAs),
	TAs = [int],
	findall(TR, typeof(lambda(f, apply(f, 1) + 1), TR), TRs),
	TRs = [fn(fn(int, int), int)],
	findall(TC, typeof(lambda(x, lambda(y, x + y)), TC), TCs),
	TCs = [fn(int, fn(int, int))],
	true.

test(systemf) :-
	findall(TE, typeof(lambda(x, lambda(y, x =:= y)), TE), TEs),
	TEs = [fn(A, fn(A, bool))], var(A),
	findall(TF, typeof(lambda(f, apply(f, 1)), TF), TFs),
	TFs = [fn(fn(int, B), B)], var(B),
	true.

