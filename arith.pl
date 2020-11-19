parse(T,S) :- let(T,S,[]).

let(let(V,E0,E1)) -->
    [let], var(var(V)),
    [=],   let(E0),
    [in],  let(E1).
let(E) --> expr(E).

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
simple_expr(unit(L)) --> [<],let(L),[>].

var(var(a)) --> [a].
var(var(b)) --> [b].
var(var(c)) --> [c].
var(var(x)) --> [x].
var(var(y)) --> [y].

%% number/1 cannot be used to generate.
num(num(X),[X|Y],Y) :- number(X).

binop(+) --> [+].
binop(-) --> [-].
binop(*) --> [*].
binop(/) --> [/].

binopdict(*,left,2).
binopdict(/,left,2).
binopdict(+,left,1).
binopdict(-,left,1).



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
