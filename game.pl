:- consult(insects), import(insects).
:- consult(hexagon), import(hexagon).

:- dynamic player/3, hive/1, add_queen/1.

% player(Id, Moves, Current, Init).
% insect(Type, Id, Player_id, Hex=[Q,R], Placed, Lvl)

start_game():-
    assert(hive([])),
    assert(player(p1, 0, true, true)),
    assert(player(p2, 0, false, true)),
    insects:start_insects(p1),
    insects:start_insects(p2).




move_insect(Val, Type, Id, Player_id, Hex, Level, Hex_fin, L_hive):-
    (
        Val == init,
        insects:possible_moves(Val, Type, Id, Player_id, Hex, Level,Moves, L_hive),
        member(Hex_fin, Moves),
        insects:move_insect_db(Type, Id, Player_id, Hex, Level, Hex_fin, L_hive),
        change_player_turn(Type, Id, Player_id, Hex, Level, Hex_fin, L_hive)
    );
    (
        Val == add,
        not(must_add_queen(Player_id)),
        insects:possible_moves(Val, Type, Id, Player_id, Hex, Level,Moves, L_hive),
        member(Hex_fin, Moves),
        insects:move_insect_db(Type, Id, Player_id, Hex, Level, Hex_fin, L_hive),
        change_player_turn(Type, Id, Player_id, Hex, Level, Hex_fin, L_hive)
    );
    (
        Val == add,
        must_add_queen(Player_id),
        not(Type == abejaReina),
        writeln("Debe agregar la abeja reina")
        
    );
    (
        not(Val == init),
        not(Val == add),
        not(queen_in_game(Player_id)),
        writeln("No puede moverse ninguna ficha hasta que la reina este en juego")
    );
    (
        not(Val == init),
        not(Val == add),
        queen_in_game(Player_id),
        insects:possible_moves(Val, Type, Id, Player_id, Hex, Level,Moves, L_hive),
        member(Hex_fin, Moves),
        insects:move_insect_db(Type, Id, Player_id, Hex, Level, Hex_fin, L_hive),
        change_player_turn(Type, Id, Player_id, Hex, Level, Hex_fin, L_hive)
    ).
        
    



change_player_turn(Type, Id, Player_id, Hex, Level, Hex_fin, L):-
    % elimina el jugador que es current ahora 
    retract(player(Player_id, Moves, Current,Init)),

    % busca el otro jugador que hay en la base de datos y lo pone como current
    retract(player(Id, Moves_other, false, Init_other)),
    assert(player(Id, Moves_other, true, Init_other)),
    
    % agrega al jugador que hizo la jugada con un movimiento mas
    Moves_new is Moves + 1,
    assert(player(Player_id, Moves_new, false, false)).


queen_in_game(Player_id):-
    insect(abejaReina, Id, Player_id, Hex, Level),
    bigger(Level, -1).

% mira si ya es el cuarto movimiento a realizar y la reina no ha jugado
must_add_queen(Player_id):-
    not(queen_in_game(Player_id)),
    player(Id, Moves, Current),
    player(Player_id, Moves, true),
    Moves == 3.


game():-
    start_game().


restart_game():-
    retractall(player(_,_,_,_)),
    retractall(insects:insect(_,_,_,_,_)),
    start_game().


