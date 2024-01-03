% type.pl
%! unify(?Ctx:list, +X, -T) is semidet.
%! typeof(+X, -T) is semidet.

typeof(X, T) :- calculus(X), unify([], X, T), !.

% Simply Typed Lambda Calculus

%% Syntax
calculus(lambda(X, _, E)) :- atomic(X), calculus(E).
calculus(apply(E1, E2)) :- calculus(E1), calculus(E2).
calculus(X) :- atomic(X).
calculus(forall(A, E)) :- atomic(A), calculus(E).
calculus(at(E, _)) :- calculus(E).

calculus(+(E1, E2)) :- calculus(E1), calculus(E2).
calculus(-(E1, E2)) :- calculus(E1), calculus(E2).
calculus(=:=(E1, E2)) :- calculus(E1), calculus(E2).
calculus(=\=(E1, E2)) :- calculus(E1), calculus(E2).
calculus(X) :- integer(X).

%% Type Substitution
typesub(A, F, A, F) :- atomic(A).
typesub(A, F, fn(T, R), fn(T1, R1)) :-
	atomic(A), typesub(A, F, T, T1), typesub(A, F, R, R1).
typesub(A, _F, forall(A, T), forall(A, T)) :- atomic(A).
typesub(A, F, forall(B, T), forall(B, T1)) :-
	atomic(A), typesub(A, F, T, T1).

% Typing rules
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
unify(Ctx, lambda(X, T, E), fn(T, R)) :-
	unify([typeis(X, T)|Ctx], E, R).

%% (STLC-4)
unify(Ctx, apply(E1, E2), R) :-
	unify(Ctx, E2, T),
	unify(Ctx, E1, fn(T, R)).

% System F
% https://en.wikipedia.org/wiki/System_F#Typing_rules

%% (SF-1)
unify(Ctx, at(E, T), F) :-
	unify(Ctx, E, forall(A, S)),
	typesub(A, T, S, F).

%% (SF-2)
unify(Ctx, forall(A, E), forall(A, T)) :-
	unify([typeis(A, type)|Ctx], E, T).

% Tests
% test(Pass).

tests(Pass) :- findall(T, test(T), Pass).

test(unify) :-
	findall(TL, typeof(lambda(x, int, x + x), TL), TLs),
	TLs = [fn(int, int)],
	findall(TA, typeof(apply(lambda(x, int, x + x), 1), TA), TAs),
	TAs = [int],
	findall(TR, typeof(lambda(f, _, apply(f, 1) + 1), TR), TRs),
	TRs = [fn(fn(int, int), int)],
	findall(TC, typeof(lambda(x, int, lambda(y, int, x =:= y)), TC), TCs),
	TCs = [fn(int, fn(int, bool))],
	true.

test(forall) :-
	findall(TE, typeof(lambda(x, _, lambda(y, _, x =:= y)), TE), TEs),
	TEs = [fn(A, fn(A, bool))], var(A),
	findall(TF, typeof(lambda(f, _, apply(f, 1)), TF), TFs),
	TFs = [fn(fn(int, B), B)], var(B),
	true.

test(systemf) :-
	church(true, True), church(false, False),
	findall(TT, typeof(True, TT), TTs),
	TTs = [forall(a, fn(a, fn(a, a)))],
	typeof(True, Bool),
	findall(TN, typeof(lambda(x, Bool, lambda(y, Bool,
			apply(apply(at(x, Bool), y), False))), TN), TNs),
	TNs = [fn(Bool, fn(Bool, Bool))],
	true.

% Constants

church(true, forall(a, lambda(x, a, lambda(y, a, x)))).
church(false, forall(a, lambda(x, a, lambda(y, a, y)))).

