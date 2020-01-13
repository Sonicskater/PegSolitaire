


%solve(Board, Goal)

% Devon Hockley - 30045270


% validate a board state.

validState([]).
validState([S|Ss]):-
    (middleRows(S);topRows(S);bottomRows(S)),
    validState(Ss).

middleRows(S):-
    X is mod(S,10),
    X =< 6,
    X >= 0,
    S >= 20,
    S =< 46.

topRows(S):-
    X is mod(S,10),
    X =< 4 ,
    X >= 2 ,
    S >= 0 ,
    S =< 14.

bottomRows(S):-
    X is mod(S,10),
    X =< 4,
    X >= 2,
    S >= 52,
    S =< 64.

% problem definitions

% MostlyDead
mostlydeadBoard([
  22,23,24,
        34,35,
  42,43,44
  ]).

% HalfDead
halfdeadBoard([
  20,     22, 23, 24, 
  30,             34, 35,
  40, 41, 42, 43, 44, 45,
          52,     54,
          62,     64
  ]).

mostlydeadGoal([33]).

halfdeadGoal([33]).

% NotQuiteDead
notquitedeadBoard([
        2,  3,  4,
        12,     14,
20, 21, 22, 23, 24, 25, 26,
30,     32,         35, 36,
40, 41, 42, 43, 44, 45, 46,
        52,     54,
        62,     64
  ]).

notquitedeadGoal([33]).

% Full Game
fullgameBoard([
         2,  3,  4,
        12, 13, 14,
20, 21, 22, 23, 24, 25, 26,
30, 31, 32,     34, 35, 36,
40, 41, 42, 43, 44, 45, 46,
        52, 53, 54,
        62, 63, 64
  ]).

fullgameGoal([33]).

crossbowBoard([
  31, 32,     34, 35,
  41, 42, 43, 44, 45,
          53
  ]).
crossbowGoal([3]).

longbowBoard([
  20,                     26,
  30, 31,     33,     35, 36,
      41,     43,     45,
          52, 53, 54,
              63
  ]).

longbowGoal([3]).

% utils

jump(Start, Middle, End):-
    ((End is Start - 20,
    Middle is Start - 10);
    (End is Start + 20,
    Middle is Start + 10);
    (End is Start + 2,
    Middle is Start + 1);
    (End is Start - 2,
    Middle is Start - 1)),
    validState([Start, Middle, End]).

remove(X, [X|Ys], Ys).
remove(X, [Y|Ys], [Y|Zs]):-
    remove(X, Ys, Zs).

move(Pred,(Start,End),[End | Rest]):-
    remove(Start, Pred,  PredPrime),
    jump(Start, Middle, End),
    remove( Middle, PredPrime,  Rest),
    not(member(End,Rest)).

% pagodas

pagoda(simple,13,1).
pagoda(simple,31,1).
pagoda(simple,33,1).
pagoda(simple,35,1).
pagoda(simple,43,1).

pagoda(stronger,13,1).
pagoda(stronger,23,1).
pagoda(stronger,33,2).
pagoda(stronger,43,1).
pagoda(stronger,53,1).

pagoda(stronger,20,-1).

pagoda(stronger,40,-1).

pagoda(stronger,21,1).
pagoda(stronger,31,2).
pagoda(stronger,41,1).

pagoda(stronger,25,1).
pagoda(stronger,35,2).
pagoda(stronger,45,1).

pagoda(stronger,26,-1).

pagoda(stronger,46,-1).

pagoda(asym,2,-1).
pagoda(asym,4,-1).
pagoda(asym,12,1).
pagoda(asym,14,1).
pagoda(asym,32,1).
pagoda(asym,34,1).
pagoda(asym,31,1).
pagoda(asym,36,1).
pagoda(asym,52,1).
pagoda(asym,56,1).
pagoda(asym,62,-1).
pagoda(asym,64,-1).

% weight function

wgt(P, B, Wgt).
wgt(_,[], 0).
wgt(P,[Pos | Rest], Wgt) :-
    (pagoda(P,Pos,PWgt) ; PWgt = 0), !,
    wgt(P,Rest,WgtRest),
    Wgt is WgtRest + PWgt.

% goal weights

goal_wgt(full,simple,1).
goal_wgt(full,stronger,2).

goal_wgt(crossbow,simple,0).
goal_wgt(crossbow,stronger,0).
goal_wgt(halfdead,simple,1).
goal_wgt(mostlydead,simple,1).
goal_wgt(halfdead,stronger,2).
goal_wgt(mostlydead,stronger,2).

goal_wgt(notquitedead,simple,1).
goal_wgt(fullgame,simple,1).
goal_wgt(notquitedead,stronger,2).
goal_wgt(fullgame,stronger,2).

% independence checking

independence(_, []).
independence(Potential,[Mv|_]):-
    intersects(Potential,Mv),!.
independence(Potential, [Mv|T]):-
    lexographical(Potential,Mv),
    independence(Potential,T).

% main search function

steps(Name,Pred,[Mv|Moves],GB,History):-
    move(Pred,Mv,PredPrime),
    independence(Mv,History),
    findall((Name,W),
        (member(Name,[simple,stronger]), %list of pagoda functions
        wgt(Name,PredPrime,W)), Wgts),
    check_wgts(Name,Wgts),
    steps(Name,PredPrime,Moves,GB,[Mv|History]).

steps(_,A, [], A, _).

%weights for pagaodas

check_wgts(G,[]).
check_wgts(G,[(P,WgtP) | Rest]) :-
    goal_wgt(G,P,WgtGoal),
    WgtP >= WgtGoal,
    check_wgts(G,Rest). 

% run various configurations

showCrossbow():-
    crossbowSolver(X),
    crossbowBoard(Start),
    crossbowGoal(Goal),
    !,
    showStep(Start,X,Goal).

showLongbow():-
    longbowSolver(X),
    longbowBoard(Start),
    longbowGoal(Goal),
    !,
    showStep(Start,X,Goal).

showMostlydead():-
    mostlydeadSolver(X),
    mostlydeadBoard(Start),
    mostlydeadGoal(Goal),
    !,
    showStep(Start,X,Goal).


showHalfdead():-
    halfdeadSolver(X),
    halfdeadBoard(Start),
    halfdeadGoal(Goal),
    !,
    showStep(Start,X,Goal).

shownotquitedead():-
    notquitedeadSolver(X),
    notquitedeadBoard(Start),
    notquitedeadGoal(Goal),
    !,
    showStep(Start,X,Goal).

showfullgame():-
    fullgameSolver(X),
    fullgameBoard(Start),
    fullgameGoal(Goal),
    !,
    showStep(Start,X,Goal).

% dispaly step

showStep(Pred,[Mv|Moves],Goal):-
    move(Pred, Mv, X),
    printBoard(X,Goal),
    nl,
    !,
    showStep(X,Moves,Goal).

showStep(_,[],_).

% ordering

lexographical((A,C),(X,Z)):-
    X > A.
    
lexographical((A,C),(X,Z)):-
    X == A,
    Z > C.

% move util

expandMove((Start,End),(Start,Middle,End)):-
    ((End is Start - 20,
    Middle is Start - 10);
    (End is Start + 20,
    Middle is Start + 10);
    (End is Start + 2,
    Middle is Start + 1);
    (End is Start - 2,
    Middle is Start - 1)).

% intersection

intersects((A,C),(X,Z)):-
    expandMove((A,C),(A,B,C)),
    expandMove((X,Z),(X,Y,Z)),
    intersects((A,B,C),(X,Y,Z)).

intersects((A,B,C),(X,Y,Z)):-
    A == X;
    A == Z;
    A == Y;
    B == X;
    B == Y;
    B == Z;
    C == X;
    C == Z;
    C == Y.

% solvers

crossbowSolver(X):-
    crossbowBoard(Start),
    crossbowGoal(Goal),
    steps(crossbow,Start,X,Goal,[]),
    printBoard(Start,Goal).

longbowSolver(X):-
    longbowBoard(Start),
    longbowGoal(Goal),
    steps(longbow,Start,X,Goal,[]),
    printBoard(Start,Goal).

mostlydeadSolver(X):-
    mostlydeadBoard(Start),
    mostlydeadGoal(Goal),
    steps(mostlydead,Start,X,Goal,[]),
    printBoard(Start,Goal).

halfdeadSolver(X):-
    halfdeadBoard(Start),
    halfdeadGoal(Goal),
    steps(halfdead,Start,X,Goal,[]),
    printBoard(Start,Goal).


notquitedeadSolver(X):-
    notquitedeadBoard(Start),
    notquitedeadGoal(Goal),
    steps(notquitedead,Start,X,Goal,[]),
    printBoard(Start,Goal).


fullgameSolver(X):-
    fullgameBoard(Start),
    fullgameGoal(Goal),
    steps(fullgame,Start,X,Goal,[]),
    printBoard(Start,Goal).

% print stuff

printBoard(State,Goal):-
    nl,
    printBoardLine([0,1,2,3,4,5,6],State,Goal),
    nl,
    printBoardLine([10,11,12,13,14,15,16],State,Goal),
    nl,
    printBoardLine([20,21,22,23,24,25,26],State,Goal),
    nl,
    printBoardLine([30,31,32,33,34,35,36],State,Goal),
    nl,
    printBoardLine([40,41,42,43,44,45,46],State,Goal),
    nl,
    printBoardLine([50,51,52,53,54,55,56],State,Goal),
    nl,
    printBoardLine([60,61,62,63,64,65,66],State,Goal).

printBoardLine([A,B,C,D,E,F,G],State,Goal):-
    printItem(A,State,Goal),
    printItem(B,State,Goal),
    printItem(C,State,Goal),
    printItem(D,State,Goal),
    printItem(E,State,Goal),
    printItem(F,State,Goal),
    printItem(G,State,Goal).

printItem(I,State,Goal):-
    member(I,State),
    !,
    format(' ~w',['x']).

printItem(I,State,Goal):-
    member(I,Goal),
    !,
    format(' ~w',['o']).

printItem(I,State,Goal):-
    validState([I]),
    !,
    format(' ~w',['-']).

printItem(I,State,Goal):-
    !,
    format(' ~w',['#']).
    