find_path(Graph,Root,Result) :-
    find_path(Graph,Root,[],Result).
find_path(_,Root,Explored,[]) :-
    member(Root,Explored),!.
find_path(Graph,Root,_,[]) :-
    \+select(edge(Root,_),Graph,_),!.
find_path([],_,_,[]).
find_path(Graph,Root,Explored,Result):-
    (   \+member(Root,Explored),
    select(edge(Root,B),Graph,NewGraph),
    append(Explored,[Root],NewExplored),
    Result = [edge(Root,B) | NewRes],
    find_path(NewGraph,B,NewExplored,NewRes)
    ).

is_palindrome(L) :- reverse(L,L).
arg(Func,Res):-
   Func=..List,
   List=[_|Res].
symmetric([]).
symmetric(T):-
    arg(T,Res),
    is_palindrome(Res),
    symmetrics(Res).
symmetrics([]):-!.
symmetrics(T):-
    T=[Head|Tail],
    symmetric(Head),
    symmetrics(Tail).

long_conc([ ],[ ]).
long_conc([[ ]|Others],L):-
      long_conc(Others,L).
long_conc([[X|L1]|Others],[X|L2]):-
      long_conc([L1|Others],L2).

tree2complete(Tree,CompleteTree):-
    Tree=t(L,_,R),
    getDepth(L,ResL),
    getDepth(R,ResR),
    Min is min(ResL,ResR)+1,
    makeComplete(Tree,Min,CompleteTree).

getDepth(nil,0):-!.
getDepth(Tree,Res):-
    Tree=t(L,_,R),
    getDepth(L,Res1),
    getDepth(R,Res2),
    Res is 1+min(Res1,Res2).

makeComplete(_,0,nil):-!.
makeComplete(Tree,Value,Result):-
    Tree=t(L,X,R),
    Val is Value-1,
    makeComplete(L,Val,Left),
    makeComplete(R,Val,Right),
    Result = t(Left,X,Right).
