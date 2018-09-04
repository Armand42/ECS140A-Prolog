
% PART I

np_names(N) :- 
        np(N,_,_).       % returns first name

np_names_not_yosemite(N) :-     % all names but yosemite
        np(N,_,_),              % iteratively checks all names  
        N \= yosemite.          % skips yosemite


np_activities_yosemite(A) :-     % yosemite and activity
    np(yosemite,_,A).

np_states_yosemite(S) :-
    np(yosemite,S,_).

np_states_grandcanyon(S) :-
    np(grandcanyon,S,_).

np_states(N, S) :-
    np(N,S,_).


np_sorted_activities_yosemite(SA) :-
    np_activities_yosemite(A),
    sort(A,SA).

np_single_state(N) :-       % wildcard for single element
    np(N,[_],_).

np_multi_state(N) :-
    np(N,S,_),
    S=[_|X],
    X=[_|_].

np_pair_names([N1, N2]) :-
    np(N1,[X],_),
    np(N2,[X],_),
    N1 @< N2.

np_2_state_2_activities(N) :-
    np(N,[_,_],[_,_]).

np_12_states_1or(N) :-
    np(N,[_],_);
    np(N,[_,_],_).

np_12_states_2wo(N) :- np(N,[_],_).
np_12_states_2wo(N) :- np(N,[_,_],_).

np_camping_hiking_1or(N) :-
    np(N,_,[camping,hiking]);
    np(N,_,[hiking,camping]).

np_camping_hiking_2wo(N) :- np(N,_,[camping,hiking]).
np_camping_hiking_2wo(N) :- np(N,_,[hiking,camping]).

np_camping_hiking_sort(N) :-
    np(N,_,A),
    sort(A,SA),
    SA = [camping,hiking].







% PART 2

% | ?- insert([1,5,8],6,Z). 
% Z = [1,5,6,8]
 
 % append list containing E to L, called that L1    E needs to be turned into a list
 % Z is sort of L1
 insert(L,E,Z) :-
    append(L,[E],L1),
    sort(L1,Z).

% given a list, remove last element

% | ?- butlast([1,2,3],Z).
% Z = [1,2]


butlast([_],[]).     % base case if only 1 element
butlast(L,Z) :-
    L = [H|L1],
    Z = [H|Z1],
    butlast(L1,Z1).


% Z is butlast of L if 
% the first element of L is the same as the first element of Z
% and the tail of Z is butlast of the tail of L.


    
%| ?- naaa([1,a,2,3,b,c,d,e,4],NAL,AL). 
% NAL = [1,2,3,4]
% AL = [a,b,c,d,e]

naaa([],[],[]).         % return if they all are empty  % AL -> AL1 has to be different when used for multiple parts
naaa(L,NAL,AL) :-
    L = [H|L1],
    (atom(H) -> 
        (AL = [H|AL1],naaa(L1,NAL,AL1))     ;       % recurse invdividually for if and else
        (NAL = [H|NAL1],naaa(L1,NAL1,AL))
    ).



% given list L -> all non-atoms and append to NAL
% atoms appended to AL
% L = [H|T] check if head is an atom, then append to AL
% else append to NAL
% recursively do it on T
%


% PART 3


% | ?- splitlist([1,2,3,4,5,6],Left,3,Right).
% Left = [1,2]
% Right = [4,5,6]

% splitlist([H],[],H,[]).             % if list contains exactly the pivot

splitlist(L, Left, Pivot, Right) :-
    L = [H|L1],                             % Left1 is unknown, we just extract the head until we hit pivot
    ((Pivot = H) -> (Left = [], Right = L1) ; (Left = [H|Left1], splitlist(L1,Left1,Pivot,Right))).      

% if head = pivot then we finish, and right is the rest
% else H is the first one of the left and recursively call function.




% | ?- split3list([[0,9,a],[12,15,b],[22,25,c],[33,5,d],[41,2,e]],c,Left,Pivot,Right).
% Left = [[0,9,a],[12,15,b]]
% Pivot = [22,25,c]
% Right = [[33,5,d],[41,2,e]]

split3list(L, Owner, Left, Pivot, Right) :-
    L = [H|L1],            % if we already found c, return   else recurse until found owner c
    (H = [_,_,Owner] -> (Left = [], Right = L1, Pivot = H) ; (Left = [H|Left1], split3list(L1,Owner,Left1,Pivot,Right))).
                                                % Pivot = H to contain everything in H



% PART 4
% delete(H,List,Rest),perm(Rest,Perm).

perm([],[]).
perm(L, PermL) :- 
    PermL=[H|PermL1],
    select(H,L,R),
    perm(R,PermL1).

permsub([],[]).
permsub(L, PermL) :-
    PermL=[H|PermL1],
    select(H,L,R),
    permsub(R,PermL1),
    naaa(PermL,NAPermL,_),          % extract the non atoms from the permutation
    naaa(L,NAL,_),                  % extract the non atoms from original list
    NAL = NAPermL.                  % compare to see if they are equal


% PART 5

% PART 5
                                % A = addr, S = size, O = owner     = [H|T] ,TM is tail of M
fit1stRequest([Owner, Size], [[A,S,O]|TM], N) :-
    ( O = z, Size == S -> 
        N = [[A,S,Owner]|TM]
        ; % else   
        ( O = z, Size @< S ->
            NewS is S - Size,
            NewA is A + Size,
            N =[[A,Size,Owner]|[[NewA, NewS, z]|TM]]
            ;
            N = [[A,S,O]|TN],
            fit1stRequest([Owner, Size], TM, TN)
        )
    ).


% M Mem
% N newmem 
% fitRelease(Owner, M, N) :-
%    split3list(M, Owner, L, [Addr, Size, Owner], R),

%    (last(L, [A1,S1,z]) ->
%            butlast(L, L1),
%            NewS is S1 + Size,
%            append(L1, [[A1, NewS, z]], L2)
%    )
%    
%    append(L, [[Addr,Size,z]], L1),
%    append(L1, R, M1),
%    coalesces(M1, N).


fitRelease(Owner, M, N) :-
    split3list(M, Owner, L, [Addr, Size, Owner], R),
    append(L, [[Addr,Size,z]], L1),
    append(L1, R, M1),
    mergeallconsecutivezblocks(M1, N).

    
mergeallconsecutivezblocks(M1, N).([],[]).
mergeallconsecutivezblocks([H], [H]).
mergeallconsecutivezblocks([[A,S,O]|TM], N) :-
    TM = [[_,S1,O1]|TTM],
    ((O = z, O1 = z) ->             % if head and one after = z merge
        S2 is S + S1,
        mergeallconsecutivezblocks([[A, S2, z]|TTM], N)
        ;
        N = [[A,S,O]|TN],
        mergeallconsecutivezblocks(TM, TN)
    ).


















% fit1stRequest([Owner, Size], MemList, NewMemList) :-
 %   MemList = [[A, S, O]|T1],
  %  ((O = z, Size @=< S) ->
  %      (Size == S ->
   %         NewMemList = [[A,S,Owner]|T1]
    %        ;
     %       S1 is S - Size,
      %%
      %     NewMemList =[[A,S,Owner]|([A1, S1, z]|T1)]
       % )
        %;
        %(NewMemList = [[A,S,O]|T2],
        % fit1stRequest([Owner, Size], T1, T2))
    %).


%| ?- fit1stRequest([d,2],[[0,2,a],[2,2,z],[4,2,b],[6,2,z],[8,2,c],[10,2,z]],M). 
% M = [[0,2,a],[2,2,d],[4,2,b],[6,2,z],[8,2,c],[10,2,z]]

% split3list([[0,2,a],[2,2,z],[4,2,b],[6,2,z],[8,2,c],[10,2,z]], d, Left, [Addr, Size, Owner], Right)



% coalesces([H], [H]).
% coalesces([[A,S,O]|T1], NewMemList) :-
%    T1 = [[_,S1,O1]|T2],
%    ((O = z, O1 = z) ->
%        (S2 is S + S1,
%        coalesces([[A, S2, z]|T2], NewMemList))
%        ;
%        (NewMemList = [[A,S,O]|T2],
%        coalesces(T1, T2))
%    ).


% fitRelease(Owner, MemList, NewMemList) :-
 %   split3list(MemList, Owner, Left, [A, S, Owner], Right),
  %  append(Left, [[A,S,z]], MidLeft),
  %  append(MidLeft, Right, Mem),
  %  coalesces(Mem, NewMemList).


% PART 6

fitanyRequest([Owner, Size], [[A,S,O]|TM], N) :-
    ( O = z, Size == S -> 
        N = [[A,S,Owner]|TM]
        ; % else   
        ( O = z, Size @< S ->
            NewS is S - Size,
            NewA is A + Size,
            N =[[A,Size,Owner]|[[NewA, NewS, z]|TM]]
            ;
            N = [[A,S,O]|TN],
            fitanyRequest([Owner, Size], TM, TN)
        )
    );                              % or to search for all other possible options skips first part
    ( O = z, Size @=< S ->
        N = [[A,S,O]|TN],           % if option available, not taking the free space, skip
        fitanyRequest([Owner, Size], TM, TN)
    ).


% PART 7
fit1st([],M,M).
fit1st([H|T],M,N) :-  
    (H = [_,_]  ->              % if Head is a request or release
        fit1stRequest(H,M,N1)  % call request to get the new memory after the head 
        ;
        fitRelease(H,M,N1)   % else we Head is an atom so we release the memory to get N1
    ),
    fit1st(T,N1,N).   % recursively call the tail, the rest of the list

fitany([],M,M).
fitany([H|T],M,N) :-
    (H = [_,_]  ->              % if Head is a request or release
        fitanyRequest(H,M,N1)  % call request to get the new memory after the head 
        ;
        fitRelease(H,M,N1)   % else we Head is an atom so we release the memory to get N1
    ),
    fitany(T,N1,N).   % recur
    

% PART 8


fit1stTryHarder(R,M,NewR,NM) :-     
        \+ fit1st(R,M,NM),      % checking if failing to run the RList 
        permsub(R,NewR),        % find a new order for the R List if full to try new combinations
        fit1st(NewR,M,NM).      % run on the new RList on the original memory 

fitanyTryHarder(R,M,NewR,NM) :-
        \+ fitany(R,M,NM),
        permsub(R,NewR),
        fitany(NewR,M,NM).




% consult('file.ext').