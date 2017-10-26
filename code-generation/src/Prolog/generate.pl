lit(-1).
lit(0).
lit(1).
lit(2).

valid_depth(N) :- N < 3.

variable(x).
variable(y).

expression(X, N, E) :-
    valid_depth(N),
    app_expression(X, N, E).

expression(X, _, E) :-
    var_expression(X, 0, E).

expression(X, _, _) :-
    lit_expression(X, 0).

expression(X, N, E) :-
    valid_depth(N),
    lambda_expression(X, N, E).

lit_expression(X, N) :-
    valid_depth(N),
    lit(X).

var_expression(X, N, E) :-
    valid_depth(N),
    variable(X),
    member(X, E).

operator('+').
operator('*').
operator('/').
operator('-').

operand(X, N, _) :- lit_expression(X, N).
operand(X, N, E) :- var_expression(X, N, E).
operand(X, N, E) :- app_expression(X, N, E).

app_expression([O, X, Y], N, E) :-
    valid_depth(N),
    K is N+1,
    operator(O),
    operand(X, K, E),
    operand(Y, K, E).

app_expression([O, X], N, E) :-
    valid_depth(N),
    K is N+1,
    lambda_expression(O, K, E),
    operand(X, K, E).

lambda_expression([F, B], N, E) :-
    K is N+1,
    variable(F),
    union(E, [F], En),
    expression(B, K, En).

display_expression(X, E, X) :-
    var_expression(X, 0, E).

display_expression(X, _, X) :-
    lit_expression(X, 0).

display_expression([F, B], E, S) :-
    lambda_expression([F, B], 0, E),
    union(E, [F], En),
    display_expression(B, En, Bd),
    atomic_list_concat(['(lambda (', F, ') ', Bd, ')'], S).

%% display_operator(X, S) :-
%%     expression(X, 0), !,
%%     display_expression(X, S).

%% display_operator(X, X).

display_expression([O, X, Y], E, S) :-
    app_expression([O, X, Y], 0, E),
    display_expression(X, E, Xd),
    display_expression(Y, E, Yd),
    atomic_list_concat(['(', O, ' ', Xd, ' ', Yd, ')'], S).

display_expression([O, X], E, S) :-
    app_expression([O, X], 0, E),
    display_expression(O, E, Od),
    display_expression(X, E, Xd),
    atomic_list_concat(['(', Od, ' ', Xd, ')'], S).

program(X) :- app_expression(X, 0, []).


display_program(S) :-
    program(X),
    display_expression(X, [], Ex),
    atomic_list_concat([Ex, "\n"], S).

generate_programs :-
    open('../../data/output.txt', write, Stream),
    forall(display_program(P), write(Stream, P)),
    close(Stream).
