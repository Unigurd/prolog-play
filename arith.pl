:- op(920,fy, *).
*_.
?- use_module(library(clpfd)).

parse(T,S) :- top(T,S,[]).

top(L) --> let(L).
top(I) --> if(I).
top(E) --> expr(E).

if(if(E0,E1,E2)) -->
    [if],   top(E0),
    [then], top(E1),
    [else], top(E2).

let(let(V,E0,E1)) -->
    [let], var(var(V)),
    [be],   top(E0),
    [in],  top(E1).

expr(T) --> simple_expr(T).
expr(R) -->
    simple_expr(X),binop(O0),expr(Y),
    {expr_help(X,O0,Y,R)}.

expr_help(X,O0,binop(O1,Y,Z),R) :-
    binopdict(O0,_,Prec0),
    binopdict(O1,_,Prec1),
    Prec0 < Prec1,
    R = binop(O0, X, binop(O1, Y, Z)).
expr_help(X,O0,binop(O1,Y,Z),R) :-
    binopdict(O0,_,Prec0),
    binopdict(O1,_,Prec1),
    Prec0 > Prec1,
    R = binop(O1, binop(O0, X, Y), Z).
expr_help(X,O0,binop(O1,Y,Z),R) :-
    binopdict(O0,left,Prec0),
    binopdict(O1,left,Prec1),
    Prec0 == Prec1,
    R = binop(O1, binop(O0, X, Y), Z).
expr_help(X,O0,binop(O1,Y,Z),R) :-
    binopdict(O0,right,Prec0),
    binopdict(O1,right,Prec1),
    Prec0 == Prec1,
    R = binop(O0, X, binop(O1, Y, Z)).
expr_help(X,O,Y,binop(O,X,Y)) :-
    Y \= binop(_,_,_).

term(X) --> simple_expr(X).
term(binop(O1,binop(O0,X,Y),Z)) -->
    simple_expr(X), prodOp(O0), term(binop(O1,Y,Z)).
term(binop(O,X,Y)) -->
    simple_expr(X), prodOp(O), term(Y),
    {Y \= binop(_,_,_)}.

simple_expr(V) --> var(V).
simple_expr(N) --> num(N).
simple_expr(B) --> bool(B).
simple_expr(unit(L)) --> [<],top(L),[>].

bool(bool(true))  --> [true].
bool(bool(false)) --> [false].

var(var(a)) --> [a].
var(var(b)) --> [b].
var(var(c)) --> [c].
var(var(x)) --> [x].
var(var(y)) --> [y].

%% number/1 cannot be used to generate.
num(num(X),[X|Y],Y) :- number(X).

binop(+)   --> [+].
binop(-)   --> [-].
binop(*)   --> [*].
binop(/)   --> [/].
binop(=)   --> [=].
binop(and) --> [and].
binop(or)  --> [or].

binopdict(*,   left, 5).
binopdict(/,   left, 5).
binopdict(+,   left, 4).
binopdict(-,   left, 4).
binopdict(=,   left, 3).
binopdict(and, left, 2).
binopdict(or,  left, 1).


lookup([(Var-Val)|_], Var, Val).
lookup([_|Vtab], Var, Val) :-
    lookup(Vtab,Var,Val).

arith(+,num(X),num(Y),num(R)) :- R is X + Y.
arith(-,num(X),num(Y),num(R)) :- R is X - Y.
arith(*,num(X),num(Y),num(R)) :- R is X * Y.
arith(/,num(X),num(Y),num(R)) :- R is X / Y.
arith(and, bool(X), bool(Y), bool(R)) :- and(X,Y,R).
arith(or,  bool(X), bool(Y), bool(R)) :- or(X,Y,R).
arith(=,   bool(X), bool(Y), bool(R)) :- eq(bool(X),bool(Y),bool(R)).
arith(=,   num(X),  num(Y),  bool(R))  :- eq(num(X),num(Y),bool(R)).

and(true,  true,  true).
and(true,  false, false).
and(false, true,  false).
and(false, false, false).

or(true,  true,  true).
or(true,  false, true).
or(false, true,  true).
or(false, false, false).

eq(num(X), num(X), bool(true)).
eq(num(X), num(Y), bool(false)) :-
    X #\= Y.

eq(bool(X), bool(X), bool(true)).
eq(bool(X), bool(Y), bool(false)) :-
    X \= Y.

eval(E,Res) :- eval([],E,Res).

eval(Vtab, let(V,E0,E1),Res) :-
    eval(Vtab,E0,VRes),
    eval([(V-VRes)|Vtab], E1, Res).


eval(Vtab, binop(Op,X0,Y0),Res) :-
    eval(Vtab, X0, X1),
    eval(Vtab, Y0, Y1),
    arith(Op,X1,Y1,Res).

eval(_, num(X),num(X)).

eval(_, bool(X),bool(X)).

eval(Vtab, var(V), Res) :-
    lookup(Vtab,V,Res).

eval(Vtab, unit(E), R) :-
    eval(Vtab, E, R).

eval(Vtab, if(E0,E1,E2), Res) :-
    eval(Vtab,E0,B),
    eval_if(Vtab,B,E1,E2,Res).

eval_if(Vtab, bool(true), E1, _, Res) :-
    eval(Vtab, E1, Res).

eval_if(Vtab, bool(false), _, E2, Res) :-
    eval(Vtab, E2, Res).

interpret(I,R) :-
    parse(T,I),
    eval(T,R).
