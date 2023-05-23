initPos(left).
initPos(right).

abs_val(X, X) :- X >= 0.
abs_val(X, Y) :- X < 0, Y is -X.

legalMove(CL, ML, CR, MR) :-
	% Check if this  state is allowed
	ML>=0, CL>=0, MR>=0, CR>=0,
	(ML>=CL ; ML=0),
	(MR>=CR ; MR=0).

move([left_shore(CL,ML),right_shore(CR,MR)],initPos(left), [left_shore(CL,ML2),right_shore(CR,MR2)]):-
     % Two missionaries cross left to right.
     MR2 is MR+2,
	 ML2 is ML-2,
	 legalMove(CL,ML2,CR,MR2).
     

move([left_shore(CL,ML),right_shore(CR,MR)],initPos(left), [left_shore(CL2,ML),right_shore(CR2,MR)]):-
	% Two cannibals cross left to right.
	CR2 is CR+2,
	CL2 is CL-2,
	legalMove(CL2,ML,CR2,MR).

move([left_shore(CL,ML),right_shore(CR,MR)],initPos(left), [left_shore(CL2,ML2),right_shore(CR2,MR2)]):-
	%  One missionary and one cannibal cross left to right.
	CR2 is CR+1,
	CL2 is CL-1,
	MR2 is MR+1,
	ML2 is ML-1,
	legalMove(CL2,ML2,CR2,MR2).

move([left_shore(CL,ML),right_shore(CR,MR)],initPos(left), [left_shore(CL,ML2),right_shore(CR,MR2)]):-
	% One missionary crosses left to right.
	MR2 is MR+1,
	ML2 is ML-1,
	legalMove(CL,ML2,CR,MR2).

move([left_shore(CL,ML),right_shore(CR,MR)],initPos(left), [left_shore(CL2,ML),right_shore(CR2,MR)]):-
	% One cannibal crosses left to right.
	CR2 is CR+1,
	CL2 is CL-1,
	legalMove(CL2,ML,CR2,MR).


move([left_shore(CL,ML),right_shore(CR,MR)],initPos(right), [left_shore(CL,ML2),right_shore(CR,MR2)]):-
     % Two missionaries cross right to left.
     MR2 is MR+2,
	 ML2 is ML-2,
	 legalMove(CL,ML2,CR,MR2).
     

move([left_shore(CL,ML),right_shore(CR,MR)],initPos(right), [left_shore(CL2,ML),right_shore(CR2,MR)]):-
	% Two cannibals cross right to left.
	CR2 is CR+2,
	CL2 is CL-2,
	legalMove(CL2,ML,CR2,MR).

move([left_shore(CL,ML),right_shore(CR,MR)],initPos(right), [left_shore(CL2,ML2),right_shore(CR2,MR2)]):-
	%  One missionary and one cannibal cross right to left.
	CR2 is CR+1,
	CL2 is CL-1,
	MR2 is MR+1,
	ML2 is ML-1,
	legalMove(CL2,ML2,CR2,MR2).

move([left_shore(CL,ML),right_shore(CR,MR)],initPos(right), [left_shore(CL,ML2),right_shore(CR,MR2)]):-
	% One missionary crosses right to left.
	MR2 is MR+1,
	ML2 is ML-1,
	legalMove(CL,ML2,CR,MR2).

move([left_shore(CL,ML),right_shore(CR,MR)],initPos(right), [left_shore(CL2,ML),right_shore(CR2,MR)]):-
	% One cannibal crosses right to left.
	CR2 is CR+1,
	CL2 is CL-1,
	legalMove(CL2,ML,CR2,MR).

get_moves(left_shore(CL1,ML1), right_shore(CR1,MR1), Moves,
          left_shore(CL_Final,ML_Final), right_shore(CR_Final,MR_Final)) :-
          missionaryCannibal([left_shore(CL1,ML1), initPos(left), right_shore(CR1,MR1)],
                         [left_shore(CL_Final,ML_Final), initPos(right), right_shore(CR_Final,MR_Final)],
                         [],[],Moves).
    
missionaryCannibal([left_shore(CL1,ML1), initPos(B1), right_shore(CR1,MR1)],
                         [left_shore(CL_Final,ML_Final), initPos(B_Final), right_shore(CR_Final,MR_Final)],
                         ExploredStates,MovesList,Moves):-
                       
        [left_shore(CL1,ML1), initPos(B1), right_shore(CR1,MR1)] \= 
        [left_shore(CL_Final,ML_Final), initPos(B_Final), right_shore(CR_Final,MR_Final)],
          move([left_shore(CL1,ML1), initPos(B1), right_shore(CR1,MR1)], initPos(B1),
                 [left_shore(CL2,ML2), initPos(B2), right_shore(CR2,MR2)]), % make an allowed move
   \+member([left_shore(CL2,ML2), initPos(B2), right_shore(CR2,MR2)],ExploredStates),
         missionaryCannibal([left_shore(CL2,ML2), initPos(B2), right_shore(CR2,MR2)],
                         [left_shore(CL_Final,ML_Final), initPos(B_Final), right_shore(CR_Final,MR_Final)],
                         ExploredStates,MovesList,Moves),
         [[left_shore(CL2,ML2), initPos(B2), right_shore(CR2,MR2)]|ExploredStates],
          [[[left_shore(CL2,ML2), initPos(B2), right_shore(CR2,MR2)], initPos(B1),
             [left_shore(CL1,ML1), initPos(B1), right_shore(CR1,MR1)]] | MovesList ],
             
           C1 is CL2 - CL1,
           M1 is ML2 - ML1,
             abs_val(C1, C2),
             abs_val(M1, M2),
             [boat(C2,M2) | Moves].


missionaryCannibal([left_shore(CL_Final,ML_Final), initPos(B_Final), right_shore(CR_Final,MR_Final)]
                   [left_shore(CL_Final,ML_Final), initPos(B_Final), right_shore(CR_Final,MR_Final)]
                   ,_,_, Moves):- reverse(Moves,Movesr),
writeList(Movesr).
        
writeList([]) :- nl. 
writeList([X|T]) :- 
   	write(X), nl,
    writeList(T).
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        