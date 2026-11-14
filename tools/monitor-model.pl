% monitor-model.pl — explicit-state model of the object monitor's owner word
% (host/chez/java/concurrency.ss, "the owner word").
%
%   swipl -q -g main -t halt tools/monitor-model.pl     (or `make monitormodel`)
%
% Every interleaving of N contexts entering and leaving one monitor, each step of
% the protocol one atomic transition:
%
%   idle      fast enter: CAS free -> own(T), else go take bk
%   lock      take bk
%   check     (bk held) free -> claim it, already marked cont(T) when another
%             context is waiting or signalled; own(X) -> mark cont(X) and wait;
%             cont(X) -> wait; mine -> reentrant (not modelled: owner-only)
%   waitnow   condition-wait: join the wait set and release bk atomically
%   woken     after the signal, take bk and decide again
%   crit      fast exit: CAS own(T) -> free; a contended word, or any release
%             that is not the fast one (Object.wait, a dead fiber's monitor,
%             a weak CAS that failed spuriously) takes the slow exit
%   exitslow  (bk held) free the word, signal ONE waiter (any), release bk
%
% There are no spurious wakeups, so a lost wakeup shows up as a context stuck in
% `waiting` rather than being rescued by a retry. A weak CAS that fails spuriously
% only moves a context onto its slow path, which the model allows everywhere.
%
% Checked: mutual exclusion, no deadlock, every waiter can always leave its wait,
% every waiter is covered by the contended mark or a pending signal,
% only a context on its way out ever holds a word it is not running under, and
% (with two contexts) each context can always get back into the critical section.
% Two mutants must be caught: a model that passes its mutant proves nothing.
% The first frees a CONTENDED word on the fast exit, the bug the contended mark
% exists to prevent. The second claims a free word unmarked while others still
% wait, which with a one-waiter signal leaves them for an exit that never wakes.

:- dynamic mutant/0.
:- dynamic mutant_claim/0.
:- dynamic nthreads/1.

threads(Ts) :- nthreads(N), numlist(1, N, Ts).
init(s(free, none, [], Ps)) :- nthreads(N), length(Ps, N), maplist(=(idle), Ps).

pc(s(_,_,_,Ps), T, P) :- nth1(T, Ps, P).
setpc(s(W,M,C,Ps), T, P, s(W,M,C,Ps2)) :- setnth(Ps, T, P, Ps2).
setnth([_|Xs], 1, Y, [Y|Xs]) :- !.
setnth([X|Xs], N, Y, [X|Ys]) :- N1 is N-1, setnth(Xs, N1, Y, Ys).

step(S, T, S2) :- pc(S,T,idle), S = s(free,M,C,Ps), setpc(s(own(T),M,C,Ps), T, crit, S2).
step(S, T, S2) :- pc(S,T,idle), S = s(W,_,_,_), W \== free, setpc(S, T, lock, S2).
step(S, T, S2) :- pc(S,T,lock), S = s(W,none,C,Ps), setpc(s(W,T,C,Ps), T, check, S2).
step(S, T, S2) :- pc(S,T,check), S = s(free,T,C,Ps), claim_word(S, T, Wd), setpc(s(Wd,T,C,Ps), T, acquired, S2).
step(S, T, S2) :- pc(S,T,check), S = s(own(X),T,C,Ps), X \== T, setpc(s(cont(X),T,C,Ps), T, waitnow, S2).
step(S, T, S2) :- pc(S,T,check), S = s(cont(X),T,_,_), X \== T, setpc(S, T, waitnow, S2).
step(S, T, S2) :- pc(S,T,waitnow), S = s(W,T,C,Ps), ord_add_element(C, T, C2), setpc(s(W,none,C2,Ps), T, waiting, S2).
step(S, T, S2) :- pc(S,T,woken), S = s(W,none,C,Ps), setpc(s(W,T,C,Ps), T, check, S2).
step(S, T, S2) :- pc(S,T,acquired), S = s(W,T,C,Ps), setpc(s(W,none,C,Ps), T, crit, S2).
step(S, T, S2) :- pc(S,T,crit), S = s(own(T),M,C,Ps), setpc(s(free,M,C,Ps), T, idle, S2).
step(S, T, S2) :- pc(S,T,crit), S = s(cont(T),M,C,Ps), mutant, setpc(s(free,M,C,Ps), T, idle, S2).
step(S, T, S2) :- pc(S,T,crit), S = s(cont(T),_,_,_), \+ mutant, setpc(S, T, exitlock, S2).
step(S, T, S2) :- pc(S,T,crit), S = s(own(T),_,_,_), setpc(S, T, exitlock, S2).
step(S, T, S2) :- pc(S,T,exitlock), S = s(W,none,C,Ps), setpc(s(W,T,C,Ps), T, exitslow, S2).
step(S, T, S2) :- pc(S,T,exitslow), S = s(_,T,[],Ps), setpc(s(free,none,[],Ps), T, idle, S2).
step(S, T, S2) :- pc(S,T,exitslow), S = s(_,T,C,Ps), C \== [], member(X, C), ord_del_element(C, X, C1),
                  setnth(Ps, X, woken, Ps1), setpc(s(free,none,C1,Ps1), T, idle, S2).

% The slow claim marks the word when another context is in the wait set or has
% been signalled and not yet retaken bk: the runtime reads both as the entry
% condition's thread count, which a signalled waiter leaves only once it holds bk.
claim_word(s(_,_,C,Ps), T, cont(T)) :- \+ mutant_claim, others_waiting(C, Ps, T), !.
claim_word(_, T, own(T)).
others_waiting(C, _, _) :- C \== [], !.
others_waiting(_, Ps, T) :- nth1(X, Ps, woken), X \== T, !.

next(S, S2) :- threads(Ts), member(T, Ts), step(S, T, S2).
succs(S, L) :- findall(S2, next(S, S2), L0), sort(L0, L).

bfs([], Seen, Seen).
bfs([S|Q], Seen, Out) :- succs(S, N0), ord_subtract(N0, Seen, N), ord_union(Seen, N, Seen2), append(Q, N, Q2), bfs(Q2, Seen2, Out).
reachable(R) :- init(I), bfs([I], [I], R).

% backward closure: the states of R from which some state in Good is reachable
closure(R, Good, Final) :-
    findall(S, (member(S, R), \+ ord_memberchk(S, Good), succs(S, L), member(S2, L), ord_memberchk(S2, Good)), N0),
    sort(N0, New),
    ( New == [] -> Final = Good ; ord_union(Good, New, G2), closure(R, G2, Final) ).

in_crit(S, T) :- threads(Ts), member(T, Ts), pc(S, T, crit).
exiting(S, X) :- pc(S, X, P), memberchk(P, [crit, acquired, exitlock, exitslow]).

mutex_violation(R) :- member(S, R), in_crit(S, A), in_crit(S, B), A < B.
deadlock(R) :- member(S, R), \+ next(S, _).
wrong_owner(R) :- member(S, R), S = s(W,_,_,_), (W = own(X) ; W = cont(X)), \+ exiting(S, X).
% Every waiter is covered: while the wait set is non-empty the word is marked
% contended, or a slow exit (which will signal) is already under way, or a
% signalled context, or one deciding under bk, is about to mark it. A waiter
% outside that is one whose wake depends on luck — some later context happening
% to release through the slow path — which is what a lost wakeup is.
uncovered_waiter(R) :-
    member(S, R), S = s(W,_,C,_), C \== [], \+ W = cont(_),
    \+ (threads(Ts), member(X, Ts), pc(S, X, P), memberchk(P, [exitlock, exitslow, woken])),
    \+ (S = s(_,M,_,_), M \== none, pc(S, M, check)).
% T is stuck: in `waiting`, and no continuation lets it out
stuck_waiter(R, T) :-
    threads(Ts), member(T, Ts),
    findall(S, (member(S, R), \+ pc(S, T, waiting)), G0), sort(G0, G),
    closure(R, G, CanLeave),
    member(S, R), pc(S, T, waiting), \+ ord_memberchk(S, CanLeave), !.
starvable(R, T) :-
    threads(Ts), member(T, Ts),
    findall(S, (member(S, R), pc(S, T, crit)), G0), sort(G0, G),
    closure(R, G, CanEnter),
    member(S, R), \+ ord_memberchk(S, CanEnter), !.

check(Name, Goal) :-
    ( call(Goal) -> format("  FAIL ~w~n", [Name]), nb_setval(failed, true)
    ; format("  ok   ~w~n", [Name]) ).

run(N, Checks) :-
    retractall(nthreads(_)), assertz(nthreads(N)),
    reachable(R), length(R, Count),
    format("~w contexts, ~w reachable states~n", [N, Count]),
    forall(member(Name-G, Checks), check(Name, call(G, R))).

main :-
    nb_setval(failed, false),
    run(3, ['mutual exclusion'-mutex_violation, 'no deadlock'-deadlock,
            'no stuck waiter'-[R]>>stuck_waiter(R, _), 'every waiter covered by the mark'-uncovered_waiter,
            'word held only by its owner'-wrong_owner]),
    run(2, ['every context can re-enter'-[R]>>starvable(R, _)]),
    assertz(mutant),
    retractall(nthreads(_)), assertz(nthreads(3)), reachable(MR),
    ( uncovered_waiter(MR) -> format("  ok   mutant (fast exit frees a contended word) is caught~n")
    ; format("  FAIL mutant was not caught: the model cannot see a lost wakeup~n"), nb_setval(failed, true) ),
    retract(mutant),
    assertz(mutant_claim), reachable(CR),
    ( uncovered_waiter(CR) -> format("  ok   mutant (slow claim leaves the word unmarked) is caught~n")
    ; format("  FAIL claim mutant was not caught~n"), nb_setval(failed, true) ),
    retract(mutant_claim),
    nb_getval(failed, F),
    ( F == true -> halt(1) ; true ).
