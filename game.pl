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

:- consult(insects), import(insects).
:- consult(hexagon), import(hexagon).


add_init_insect(Type, Id, Player_id, Hex, Level, Hex_fin):-
    hive(L), 
    Level == -1, % el insecto esta en la mano todavia
    move_insect(Type, Id, Player_id, Hex, Level, Hex_fin, L).

%add_insect_to_hive(insect)
add_insect_to_hive(Type, Id, Player_id, Hex, Level, Hex_fin):-
    (
        hive(L), 
        Level == -1, % el insecto esta en la mano todavia
        not(must_add_queen(Player_id)),
        check_Hex_same_color(Player_id, Hex, L),
        move_insect(Type, Id, Player_id, Hex, Level, Hex_fin, L)
    ),!;
    (
        must_add_queen(Player_id),
        writeln("Debe poner la abeja reina o unida al grafo")
    ),!;
    (
        not(check_Hex_same_color(Player_id, Hex, L)),
        writeln("No puede tener fichas del color del enemigo al alrededor (primera vez)")
    ),!;
    (
        not(move_insect(Type, Id, Player_id, Hex, Level, Hex_fin, L)),
        writeln("La ficha debe ser adyacente a la colmena")    
    ).

% este metodo es para cuando se agregue una ficha, no este al lado de ninguna del adversario
check_Hex_same_color(Player_id, Hex, L_hive):-

    findall(Neighbor, 
            (hexagon:are_neighbors(Hex, Neighbor), % para ir por cada vecino
            is_same_color_or_white(Player_id, Player_id2, Neighbor, L_hive)), 
            List_neighbor),
    length(List_neighbor, 6). % si todas las casillas alrededor son del mismo color o estan vacias
    
    
    
    

% analizo si la casilla es un insecto del mismo color o la casilla no esta en la colmena(o sea esta en blanco)
is_same_color_or_white(Player_id, Player_id2, Neighbor, L_hive):-
    (insect(_, _, Player_id2, Neighbor, _), Player_id == Player_id2), ! ; 
    not(member(Neighbor, L_hive)).


% mover insecto de la casilla Hex a la Hex_fin
move_insect(Type, Id, Player_id, Hex, Level, Hex_fin, L):-
    %agrega el insecto a la colmena poniendole nivel 0
    hexagon:is_connected_hex_to_hive(Hex_fin, L),
    retract(insect(Type, Id, Player_id,Hex, Level)),
    assert(insect(Type, Id, Player_id, Hex_fin, 0)),
    
    %elimina la posicion vieja del insecto de la lista de las casillas y agrega la nueva
    delete(L, Hex, L_1),
    append(L, [Hex_fin], L_2),
    retract(hive(L)),
    assert(hive(L_2)),

    % elimina el jugador que es current ahora 
    retract(player(Player_id, Moves, Current,Init)),

    % busca el otro jugador que hay en la base de datos y lo pone como current
    retract(player(Id, Moves_other, false, Init_other)),
    assert(player(Id, Moves_other, true, Init_other)),
    
    % agrega al jugador que hizo la jugada con un movimiento mas
    Moves_new is Moves + 1,
    assert(player(Player_id, Moves_new, false, false)).
    


queen_in_game(Player_id):-
    insect(abejaReina, Id, Player_Id, Hex, Level),
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


