:- consult(insects), import(insects).
:- consult(hexagon), import(hexagon).

:- dynamic player/4, hive/1, add_queen/1.

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
        
    




change_player_turn(Type, Id, Player_id, Hex, Level, Hex_fin, L_hive):-
    (
        % elimina el jugador que es current ahora 
        retract(player(Player_id, Moves, Current,Init)),
        % busca el otro jugador que hay en la base de datos y lo pone como current si este puede jugar
        player(Id, Moves_other, false, Init_other),
        player_can_play(Id, L_hive),
        retract(player(Id, Moves_other, false, Init_other)),
        assert(player(Id, Moves_other, true, Init_other)),
        
        % agrega al jugador que hizo la jugada con un movimiento mas
        Moves_new is Moves + 1,
        assert(player(Player_id, Moves_new, false, false))
    );
    (
        % elimina el jugador que es current ahora con su cantidad de movimientos 
        retract(player(Player_id, Moves, Current,Init)),
        % busca el otro jugador que hay en la base de datos y lo pone como current si este puede jugar
        player(Id, Moves_other, false, Init_other),
        not(player_can_play(Id, L_hive)),

        % agrega al jugador que hizo la jugada con un movimiento mas
        Moves_new is Moves + 1,
        assert(player(Player_id, Moves_new, true, false))
    ).

queen_in_game(Player_id):-
    insect(abejaReina, Id, Player_id, Hex, Level),
    bigger(Level, -1).

% mira si ya es el cuarto movimiento a realizar y la reina no ha jugado
must_add_queen(Player_id):-
    not(queen_in_game(Player_id)),
    player(Id, Moves, Current,_),
    player(Player_id, Moves, true, _),
    Moves == 3.


player_can_play(Player_id, L_hive):-
    findall([Type, Id, Player_id,Hex,Level], 
            (hexagon:insect(Type, Id, Player_id,Hex, Level), member(Hex, L_hive)), 
            insects_hive),

    findall([Type, Id, Player_id,Hex,Level], 
    (hexagon:insect(Type, Id, Player_id,Hex, Level), not(member(Hex, L_hive))), 
    insects_hand),
    
    % for para recorrer todos los insectos de la colmena, del jugador
    (
        member([Type, Id, Player_id,Hex,Level], insects_hive), 
        insects:possible_moves(Type, Type, Id, Player_id , Hex, Level, Moves, L_hive),
        length(Moves, Length),
        hexagon:bigger(Length, 0), ! % si encuentro uno q se mueva mover entonces ya esta
    );
    % for para recorrer todos los insectos de la mano, del jugador 
    (
        member([Type, Id, Player_id,Hex,Level], insects_hand), 
        insects:possible_moves(add, Type, Id, Player_id , Hex, Level, Moves, L_hive),
        length(Moves, Length),
        hexagon:bigger(Length, 0), ! % si encuentro uno q se mueva mover entonces ya esta
    ).

% juego terminado 
end_game(Player_id, L_hive):-
    insect(abejaReina, Id, Player_id, Hex, Level),
    member(Hex, L_hive),
    findall(Hex_neighbor, 
            (hexagon:are_neighbors(Hex,Hex_neighbor), member(Hex_neighbor,L_hive)), 
            L_neighbors),
    
    length(L_neighbors, Length),
    Length == 6,
    writeln("Fin de la partida. Perdio: "),
    writeln(Player_id).
    

% juego empatado  
tie_game(Player_id1, Player_id2, L_hive):-
    end_game(Player_id1, L_hive),
    end_game(Player_id2, L_hive),
    writeln("Juego empatado").   
    

game():-
    start_game().


restart_game():-
    retractall(player(_,_,_,_)),
    retractall(insects:insect(_,_,_,_,_)),
    start_game().


