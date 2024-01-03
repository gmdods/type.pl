% type.pl
%! typeof(+X, -T) is semidet.
%! generalize(+X, -G) is semidet.

typeof(X, T) :- calculus(X), unify([], X, T), !.
generalize(X, G) :- typeof(X, T), typevar(T, L), typeforall(L, T, G), !.

% Simply Typed Lambda Calculus

%% Syntax
calculus(lambda(X, _, E)) :- atom(X), calculus(E).
calculus(apply(E1, E2)) :- calculus(E1), calculus(E2).
calculus(X) :- atom(X).
calculus(forall(A, E)) :- atom(A), calculus(E).
calculus(at(E, _)) :- calculus(E).

calculus(+(E1, E2)) :- calculus(E1), calculus(E2).
calculus(-(E1, E2)) :- calculus(E1), calculus(E2).
calculus(=:=(E1, E2)) :- calculus(E1), calculus(E2).
calculus(=\=(E1, E2)) :- calculus(E1), calculus(E2).
calculus(X) :- integer(X).

%% Type Substitution
typesub(A, F, A, F).
typesub(A, F, fn(T, R), fn(U, V)) :- typesub(A, F, T, U), typesub(A, F, R, V).
typesub(A, _F, forall(A, T), forall(A, T)).
typesub(A, F, forall(B, T), forall(B, T1)) :- typesub(A, F, T, T1).

%% Generalize
typevar(T, [A|_As]) :- var(T), new_atom(a, A), !, T = A.
typevar(fn(T, R), As) :- typevar(T, At), typevar(R, Ar), append(At, Ar, As).
typevar(forall(A, T), As) :- atom(A), typevar(T, As).
typevar(T, []) :- ground(T).

typeforall([], T, T).
typeforall([A|As], T, forall(A, G)) :- atom(A), typeforall(As, T, G).
typeforall(_As, T, T).

% Typing rules
% https://en.wikipedia.org/wiki/Simply_typed_lambda_calculus#Typing_rules

%% (STLC-1)
unify(Ctx, X, T) :- atom(X), member(typeis(X, T), Ctx).

%% (STLC-2)
unify(_Ctx, X, int) :- integer(X).
unify(Ctx, +(E1, E2), int) :- unify(Ctx, E1, int), unify(Ctx, E2, int).
unify(Ctx, -(E1, E2), int) :- unify(Ctx, E1, int), unify(Ctx, E2, int).
unify(Ctx, =:=(E1, E2), bool) :- unify(Ctx, E1, T), unify(Ctx, E2, T).
unify(Ctx, =\=(E1, E2), bool) :- unify(Ctx, E1, T), unify(Ctx, E2, T).

%% (STLC-3)
unify(Ctx, lambda(X, T, E), fn(T, R)) :- unify([typeis(X, T)|Ctx], E, R).

%% (STLC-4)
unify(Ctx, apply(E1, E2), R) :- unify(Ctx, E2, T), unify(Ctx, E1, fn(T, R)).

% System F
% https://en.wikipedia.org/wiki/System_F#Typing_rules

%% (SF-1)
unify(Ctx, at(E, T), F) :- unify(Ctx, E, forall(A, S)), typesub(A, T, S, F).

%% (SF-2)
unify(Ctx, forall(A, E), forall(A, T)) :- unify([typeis(A, type)|Ctx], E, T).

% Tests

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
	findall(TG, generalize(lambda(x, _, lambda(y, _, x =:= y)), TG), TGs),
	TGs = [forall(C, fn(C, fn(C, bool)))], atom(C),
	true.

test(systemf) :-
	True = forall(a, lambda(x, a, lambda(y, a, x))),
	False = forall(a, lambda(x, a, lambda(y, a, y))),
	findall(TT, typeof(True, TT), TTs),
	TTs = [forall(a, fn(a, fn(a, a)))], [Bool] = TTs,
	findall(TN, typeof(lambda(x, Bool, lambda(y, Bool,
			apply(apply(at(x, Bool), y), False))), TN), TNs),
	TNs = [fn(Bool, fn(Bool, Bool))],
	true.

