parse(T,S) :- let(T,S,[]).

let(let(V,E0,E1)) -->
    [let], var(var(V)),
    [=],   let(E0),
    [in],  let(E1).
let(E) --> expr(E).
expr(T) --> term(T).
expr(binop(O1,binop(O0,X,Y),Z)) -->
    term(X), termop(O0), expr(binop(O1,Y,Z)).
expr(binop(O,X,Y)) -->
    term(X), termop(O), expr(Y),
    {Y \= binop(_,_,_)}.

term(X) --> simple_expr(X).
term(binop(O1,binop(O0,X,Y),Z)) -->
    simple_expr(X), prodOp(O0), term(binop(O1,Y,Z)).
term(binop(O,X,Y)) -->
    simple_expr(X), prodOp(O), term(Y),
    {Y \= binop(_,_,_)}.

simple_expr(V) --> var(V).
simple_expr(N) --> num(N).
simple_expr(unit(L)) --> [<],let(L),[>].

var(var(a)) --> [a].
var(var(b)) --> [b].
var(var(c)) --> [c].
var(var(x)) --> [x].
var(var(y)) --> [y].

%% number/1 cannot be used to generate.
num(num(X),[X|Y],Y) :- number(X).

termop(+) --> [+].
termop(-) --> [-].

prodOp(*) --> [*].
prodOp(/) --> [/].

lookup([(Var-Val)|_], Var, Val).
lookup([_|Vtab], Var, Val) :-
    lookup(Vtab,Var,Val).

arith(+,X,Y,R) :- R is X + Y.
arith(-,X,Y,R) :- R is X - Y.
arith(*,X,Y,R) :- R is X * Y.
arith(/,X,Y,R) :- R is X / Y.

eval(E,Res) :- eval([],E,Res).

eval(Vtab, let(V,E0,E1),Res) :-
    eval(Vtab,E0,VRes),
    eval([(V-VRes)|Vtab], E1, Res).

eval(Vtab, binop(Op,X0,Y0),Res) :-
    eval(Vtab, X0, X1),
    eval(Vtab, Y0, Y1),
    arith(Op,X1,Y1,Res).

eval(_, num(X),X).

eval(Vtab, unit(E), R) :-
    eval(Vtab, E, R).


eval(Vtab, var(V), Res) :-
    lookup(Vtab,V,Res).
