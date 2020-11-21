nat(zero).
nat(succ(N)) :-
    nat(N).

nat2num(zero,0).
nat2num(succ(Nat),Num) :-
    nat2num(Nat,PrevNum),
    Num is PrevNum+1.

runNat(F,NumArg0,NumArg1) :-
    nat2num(NatArg0,NumArg0),
    nat2num(NatArg1,NumArg1),
    call(F, NatArg0,NatArg1).

runNat(F,NumArg0,NumArg1,NumArg2) :-
    nat2num(NatArg0,NumArg0),
    nat2num(NatArg1,NumArg1),
    nat2num(NatArg2,NumArg2),
    call(F, NatArg0,NatArg1,NatArg2).

eq(zero,zero).
eq(succ(N),succ(M)) :-
    eq(N,M).

g(succ(_),zero).
g(succ(N),succ(M)) :-
    g(N,M).

ge(N,M) :-
    g(N,M) ; eq(N,M).

l(zero,succ(_)).
l(succ(N),succ(M)) :-
    l(N,M).

le(N,M) :-
    l(N,M) ; eq(N,M).

add(zero,M,M).
add(N,zero,N).
add(succ(N),M,R) :-
    add(N,succ(M),R).

sub(N,zero,N).
sub(succ(N),succ(M),R) :-
    sub(N,M,R).

multRec(Acc,_,zero,Acc).
multRec(Acc,N,succ(M),R) :-
    add(Acc,N,Acc2),
    multRec(Acc2,N,M,R).

mult(zero,_,zero).
mult(_,zero,zero).
mult(N,M,R) :-
    multRec(zero,N,M,R).

divRec(N,M,Acc,R) :-
    sub(N,M,NewN),
    divRec(NewN,M,suc(Acc),R).
divRec(N,M,Acc,Acc) :-
    l(N,M).

div(N,succ(M),R) :-
    divRec(N,succ(M),zero,R).
