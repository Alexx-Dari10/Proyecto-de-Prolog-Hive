
module(ia, [
    find_move_more_near_to_queen/5, find_move_more_far_to_queen/5,get_all_possible_moves/3
]).

axial_distance([R1,Q1], [R2,Q2], Dist):-
    R_abs is abs(R1-R2),
    Q_abs is abs(Q1-Q2),
    S_abs is abs(-R1 - Q1 + R2 + Q2),
    Dist is (R_abs + Q_abs + S_abs)/2.


% encontrando los movimientos q estan mas cerca de la reina del contrario
find_move_more_near_to_queen(Hex_queen, [], Distance, Moves_near, Moves_near).

find_move_more_near_to_queen(Hex_queen, [Hex|Moves], Distance, Moves_near,Moves_near_resp):-
    axial_distance(Hex_queen, Hex, Dist),

    Dist < Distance,!,

    find_move_more_near_to_queen(Hex_queen, Moves, Dist, [Hex],Moves_near_resp).

find_move_more_near_to_queen(Hex_queen, [Hex|Moves], Distance, Moves_near,Moves_near_resp):-
    axial_distance(Hex_queen, Hex, Dist),

    Dist == Distance,!,

    append(Moves_near, [Hex], Moves_near1),

    find_move_more_near_to_queen(Hex_queen, Moves, Dist, Moves_near1,Moves_near_resp).

find_move_more_near_to_queen(Hex_queen, [Hex|Moves], Distance, Moves_near,Moves_near_resp):-
    axial_distance(Hex_queen, Hex, Dist),
    Dist > Distance,!,
    find_move_more_near_to_queen(Hex_queen, Moves, Dist, Moves_near,Moves_near_resp).




% encontrando los movimientos q estan mas lejos de la reina de la IA

find_move_more_far_to_queen(Hex_queen, [], Distance, Moves_far, Moves_far).

find_move_more_far_to_queen(Hex_queen, [Hex|Moves], Distance, Moves_far,Moves_far_resp):-
    axial_distance(Hex_queen, Hex, Dist),

    Dist > Distance, !, 

    find_move_more_far_to_queen(Hex_queen, Moves, Dist, [Hex], Moves_far_resp).


find_move_more_far_to_queen(Hex_queen, [Hex|Moves], Distance, Moves_far,Moves_far_resp):-
    axial_distance(Hex_queen, Hex, Dist),

    Dist == Distance,!,

    append(Moves_far, [Hex], Moves_far1),

    find_move_more_far_to_queen(Hex_queen, Moves, Dist, Moves_far1, Moves_far_resp).

find_move_more_far_to_queen(Hex_queen, [Hex|Moves], Distance, Moves_far,Moves_far_resp):-
    axial_distance(Hex_queen, Hex, Dist),
    Dist < Distance, 
    find_move_more_far_to_queen(Hex_queen, Moves, Dist, Moves_far,Moves_far_resp).



get_all_possible_moves([], All_moves_next_to_one, All_moves_next_to_one).
get_all_possible_moves([[Val,Type, Id, Player_id,Hex, Level, Moves]|All_moves], 
                        All_moves_next_to_one, All_moves_next_to_one_resp):-

    append(All_moves_next_to_one, Moves, All_moves_next_to_one_concat),
    get_all_possible_moves(All_moves, All_moves_next_to_one_concat, All_moves_next_to_one_resp).
       