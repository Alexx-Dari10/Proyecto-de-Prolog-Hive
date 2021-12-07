module(insects, [
        start_insects/3, insect/5, possible_moves/8, move_insect_db/7
    ]).

:- consult(hexagon), import(hexagon).
:- consult(utils), import(utils).

% si level = -1 entonces la ficha no ha sido puesta en juego
% insect(Type, Id, Player_id, Hex=[R,Q], Level)

:- dynamic insect/5.

start_insects(Player,[X1,X2,X3,X4,X5,X6,X7,X8], [Y1,Y2,Y3]):- 
    
    assert(insect(hormiga, 1, Player, [X1,Y1], -1)),
    assert(insect(hormiga, 2, Player, [X1, Y2], -1)),
    assert(insect(hormiga, 3, Player, [X1, Y3], -1)),

    assert(insect(escarabajo, 1, Player, [X2, Y1], -1)),
    assert(insect(escarabajo, 2, Player, [X2, Y2], -1)),

    assert(insect(saltamonte, 1, Player, [X3, Y1], -1)),
    assert(insect(saltamonte, 2, Player, [X3, Y2], -1)),
    assert(insect(saltamonte, 3, Player, [X3, Y3], -1)),

    assert(insect(abejaReina, 1, Player, [X4, Y1], -1)),

    assert(insect(aranha, 1, Player, [X5, Y1], -1)),
    assert(insect(aranha, 2, Player, [X5, Y2], -1)),

    assert(insect(mariquita, 1, Player, [X6, Y1], -1)),

    assert(insect(mosquito, 1, Player, [X7, Y1], -1)),

    assert(insect(bichoBola, 1, Player, [X8, Y1], -1)).



% actualizar los movimientos del insecto en la base de datos

move_insect_db(Type, Id, Player_id, Hex, Level, Hex_fin, L_hive):-
    
    (
        (Type = escarabajo,!; Type = mosquito),
        member(Hex_fin, L_hive), % ya hay otra ficha en esta posicion
        %quito la ficha con el nivel que tenga y la pongo en un nivel mas
        %NOTA: ver los niveles como las posiciones de una pila (el q esta en el nivel 0 esta en el tope de la pila) 
        retract(insect(Type2, Id2, Player_id2,Hex_fin, Level1)),
        Level2 is Level1 + 1,
        assert(insect(Type2, Id2, Player_id2,Hex_fin, Level2)),

        % agrega el insecto a la colmena poniendole nivel 0
        retract(insect(Type, Id, Player_id,Hex, _)),
        assert(insect(Type, Id, Player_id, Hex_fin, Level)),
        
        
        %elimina la posicion vieja del insecto de la lista de las casillas y agrega la nueva
        delete(L_hive, Hex, L_1),
        append(L_1, [Hex_fin], L_2),
        retract(hive(L_hive)),
        assert(hive(L_2))
    );
    (
        % agrega el insecto a la colmena poniendole nivel 0
        retract(insect(Type, Id, Player_id,Hex, _)),
        assert(insect(Type, Id, Player_id, Hex_fin, Level)),
        
        %elimina la posicion vieja del insecto de la lista de las casillas y agrega la nueva
        delete(L_hive, Hex, L_1),
        append(L_1, [Hex_fin], L_2),
        retract(hive(L_hive)),
        assert(hive(L_2))
    ).


    

% busca el insecto que esta en la casilla Hex
find_insect_by_hex(Hex, Type, Id, Player_id, Level):- 
    insect(Type, Id, Player_id, Hex, Level).


% devuelve una lista de los insectos que no estan en la colmena todavia
find_insects_in_hand(Player,L):-
    findall([Type, Id, Player, Hex, -1], insect(Type, Id, Player, Hex, -1), L).


% devuelve una lista de los insectos que estan en la colmena
find_insects_in_hive(Player,L):-
    findall([Type, Id, Player, Hex, Level], (insect(Type, Id, Player, Hex,Level), bigger(Level,-1)), L).

insect_blocked(Type, Id, Player_id , Hex, Level):-
    not(Level == 0).


cell_in_center(Hex_center, L_hive):-
    findall(Hex_neighbor, 
            (hexagon:are_neighbors(Hex_center, Hex_neighbor), member(Hex_neighbor, L_hive)), 
        L_neighbors),
    
    length(L_neighbors, Length),
    Length >= 5.
    
    


%~~~~~~~~~~~~~ POSIBLES MOVIMIENTOS ~~~~~~~~~~~~~

possible_moves(Val, Type, Id, Player_id , Hex, Level, Moves, L_hive):-
    hexagon:switch(Val,
        [
            init: init_possible_moves(Type, Id, Player_id , Hex, Level, Moves, L_hive),
            add: add_possible_moves(Type, Id, Player_id , Hex, Level, Moves, L_hive),

            abejaReina: abejaReina_possible_moves(Type, Id, Player_id , Hex, Level, Moves, L_hive),
            hormiga: hormiga_possible_moves(Type, Id, Player_id , Hex, Level, Moves, L_hive),
            saltamonte: saltamonte_possible_moves(Type, Id, Player_id , Hex, Level, Moves, L_hive),
            escarabajo: escarabajo_possible_moves(Type, Id, Player_id , Hex, Level, Moves, L_hive),
            aranha: aranha_possible_moves(Type, Id, Player_id , Hex, Level, Moves, L_hive),
            mosquito: mosquito_possible_moves(Type, Id, Player_id , Hex, Level, Moves, L_hive),
            mariquita: mariquita_possible_moves(Type, Id, Player_id , Hex, Level, Moves, L_hive),
            bichoBola: bichoBola_possible_moves(Type, Id, Player_id , Hex, Level, Moves, L_hive)
        ]).


%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  

%~~~~ Init ~~~~

% Dos primeros movimientos
init_possible_moves(Type, Id, Player_id , Hex, Level, Moves, L_hive):-
    (
        Player_id == p1,
        Moves = [0, 0]
    );
    (
        Player_id == p2,
        findall(Hex_neighbor, hexagon:are_neighbors([0, 0],Hex_neighbor), Moves)
    ).


%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  

%~~~~ ADD ~~~~

% Fichas para agregar
add_possible_moves(Type, Id, Player_id , Hex, Level, Moves, L_hive):-
    findall(
        Hex, 
        (
            findall(Hex, insect(_,_,Player_id,Hex,_), L_insects_same_color),
            member(Insect, L_insects_same_color),
            not(cell_in_center(Hex,L_hive)),
            hexagon:are_neighbors(Insect, Hex_neighbor),
            check_Hex_same_color(Player_id, Hex_neighbor, L_hive)
        ),
        Moves
    ).

% este predicado es para revisar q ninguna ficha alrededor es del adversario
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


%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



%~~~~ ABEJA REINA ~~~~

abejaReina_possible_moves(Type, Id, Player_id , Hex, Level, Moves, L_hive):-
    (
        not(insect_blocked(Type, Id, Player_id , Hex, Level)),
        not(hexagon:articulation_point(Hex,L_hive)),
        delete(L_hive, Hex, L_hive1), % esto es para analizar si esta conectada una casilla a la colmena sin la casilla Hex 
        findall(Hex1, 
                (
                    hexagon:are_neighbors(Hex,Hex1), 
                    not(member(Hex1, L_hive1)),
                    not(cell_in_center(Hex1,L_hive)), 
                    hexagon:is_connected_hex_to_hive(Hex1,L_hive1)
                ),
                Moves)
    );
    (
        insect_blocked(Type, Id, Player_id , Hex, Level),
        Moves = []
    ).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



% ~~~~ HORMIGA ~~~~

hormiga_possible_moves(Type, Id, Player_id , Hex, Level, Moves, L_hive):-
    (
        not(insect_blocked(Type, Id, Player_id , Hex, Level)),
        not(hexagon:articulation_point(Hex,L_hive)),
        delete(L_hive, Hex, L_hive1), % esto es para analizar si esta conectada una casilla a la colmena sin la casilla Hex 
        % esto nos da todos los hexagonos en blanco q son adyacentes a los de la colmena
        % (pueden dar valores repetidos)
        findall(Hex2, 
                (
                    member(Hex1, L_hive1),
                    hexagon:are_neighbors(Hex1,Hex2),
                    not(cell_in_center(Hex2,L_hive1)), 
                    not(member(Hex2, L_hive1)),
                    hexagon:is_connected_hex_to_hive(Hex2,L_hive1)), 
                Moves_rep),
        
        %quitando los valores repetidos
        utils:remove_repeated(Moves_rep,Moves)
    );
    (
        insect_blocked(Type, Id, Player_id , Hex, Level),
        Moves = []
    ).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



%~~~~ SALTAMONTE ~~~~

saltamonte_possible_moves(Type, Id, Player_id , Hex, Level, Moves, L_hive):-
    (
        not(insect_blocked(Type, Id, Player_id , Hex, Level)),
        not(hexagon:articulation_point(Hex,L_hive)),
        delete(L_hive, Hex, L_hive1), % esto es para analizar si esta conectada una casilla a la colmena sin la casilla Hex 
        saltamonte_move(Hex, L_hive1, Moves)
    );
    (
        insect_blocked(Type, Id, Player_id , Hex, Level),
        Moves = []
    ).

% para saber la posible casilla en una direccion
saltamonte_move_dir([R,Q],Dir,L_hive1, Steps, Move):- 
    
    (
        hexagon:direction(Dir,[R_dir,Q_dir]), 
        R2 is R+R_dir, Q2 is Q+Q_dir,
        not(member([R2,Q2], L_hive1)), 
        Steps > 0, 
        Move = [R2,Q2]
    );
    (
        hexagon:direction(Dir,[R_dir,Q_dir]), 
        R2 is R+R_dir, Q2 is Q+Q_dir,
        not(member([R2,Q2], L_hive1)), 
        Steps == 0, 
        Move = none % devolvemos este valor y luego lo quitamos
        
    );
    (
        hexagon:direction(Dir,[R_dir,Q_dir]), 
        R2 is R+R_dir, Q2 is Q+Q_dir,
        member([R2,Q2], L_hive1), 
        Steps1 is Steps + 1, 
        saltamonte_move_dir([R2,Q2],Dir,L_hive1, Steps1, Move)
    ).

saltamonte_move([R,Q],L_hive1, Moves):-
    saltamonte_move_dir([R,Q],1,L_hive1, 0, Move_1),
    saltamonte_move_dir([R,Q],2,L_hive1, 0, Move_2),
    saltamonte_move_dir([R,Q],3,L_hive1, 0, Move_3),
    saltamonte_move_dir([R,Q],4,L_hive1, 0, Move_4),
    saltamonte_move_dir([R,Q],5,L_hive1, 0, Move_5),
    saltamonte_move_dir([R,Q],6,L_hive1, 0, Move_6),

    Moves1 = [Move_1, Move_2,Move_3, Move_4, Move_5, Move_6],
    delete(Moves1, none, Moves).


%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



%~~~~ ESCARABAJO ~~~~

escarabajo_possible_moves(Type, Id, Player_id , Hex, Level, Moves, L_hive):-
    (
        not(insect_blocked(Type, Id, Player_id , Hex, Level)),
        not(hexagon:articulation_point(Hex,L_hive)),
        delete(L_hive, Hex, L_hive1), % esto es para analizar si esta conectada una casilla a la colmena sin la casilla Hex 
        findall(Hex1, 
                (hexagon:are_neighbors(Hex,Hex1),hexagon:is_connected_hex_to_hive(Hex1,L_hive1)), 
                Moves)
    );
    (
        insect_blocked(Type, Id, Player_id , Hex, Level),
        Moves = []
    ).

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


%~~~~ ARANHA ~~~~
aranha_possible_moves(Type, Id, Player_id , Hex, Level, Moves, L_hive):-
    (
        not(insect_blocked(Type, Id, Player_id , Hex, Level)),
        not(hexagon:articulation_point(Hex,L_hive)),
        delete(L_hive, Hex, L_hive1), % esto es para analizar si esta conectada una casilla a la colmena sin la casilla Hex 
        findall(Move, (aranha_move(Hex,L_hive1,[],0,Move)), Moves1),
        delete(Moves1, none, Moves)
    );
    (
        insect_blocked(Type, Id, Player_id , Hex, Level),
        Moves = []
    ).

aranha_move(Hex1, L_hive1, Visited, Level_rec, Move):- 
    (Level_rec == 3, not(member(Hex1, L_hive1)), not(cell_in_center(Hex1,L_hive1)), Move = Hex1);
    (cell_in_center(Hex,L_hive), Move = none);
    (Level_rec > 3, not(member(Hex1, L_hive1)), Move = none);
    (member(Hex1, L_hive1), Move = none),!.

aranha_move(Hex1, L_hive1, Visited,Level_rec, Move):-
    append(Visited,[Hex1], Visited_1),
    hexagon:are_neighbors(Hex1, Adj_hex1),
    not(member(Adj_hex1, L_hive1)), % esta en blanco
    not(member(Adj_hex1, Visited_1)), % para saber si un nodo no esta visitado
    hexagon:is_connected_hex_to_hive(Adj_hex1,L_hive1),
    Level_rec1 is Level_rec + 1,
    aranha_move(Adj_hex1, L_hive1, Visited_1,Level_rec1,Move).

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


%~~~~ MOSQUITO ~~~~~

mosquito_possible_moves(Type, Id, Player_id , Hex, Level, Moves, L_hive):-
    (
        not(insect_blocked(Type, Id, Player_id , Hex, Level)),
        not(hexagon:articulation_point(Hex,L_hive)),
        delete(L_hive, Hex, L_hive1), % esto es para analizar si esta conectada una casilla a la colmena sin la casilla Hex 

        neighbor_dir(Hex,1,neighbor_1),
        neighbor_dir(Hex,2,neighbor_2),
        neighbor_dir(Hex,3,neighbor_3),
        neighbor_dir(Hex,4,neighbor_4),
        neighbor_dir(Hex,5,neighbor_5),
        neighbor_dir(Hex,6,neighbor_6),

        mosquito_move(neighbor_1, Moves_1,L_hive),
        mosquito_move(neighbor_2, Moves_2,L_hive),
        mosquito_move(neighbor_3, Moves_3,L_hive),
        mosquito_move(neighbor_4, Moves_4,L_hive),
        mosquito_move(neighbor_5, Moves_5,L_hive),
        mosquito_move(neighbor_6, Moves_6,L_hive),

        append(Moves_1, Moves_2, Moves_temp1),
        append(Moves_temp1, Moves_3, Moves_temp2),
        append(Moves_temp2, Moves_4, Moves_temp3),
        append(Moves_temp3, Moves_5, Moves_temp4),
        append(Moves_temp4, Moves_6, Moves_temp5),

        utils:remove_repeated(Moves_temp5,Moves)
    );
    (
        insect_blocked(Type, Id, Player_id , Hex, Level),
        Moves = []
    ).
    
mosquito_move(Hex_neighbor, Moves,L_hive):-
    (
        insect(Type,_,_,Hex_neighbor,Level),
        possible_moves(Val, Type, Id, Player_id , Hex_neighbor, Level, Moves, L_hive)
    );
    (
        not(insect(Type,_,_,Hex_neighbor,Level)),
        Moves=[]
    ).

neighbor_dir([R1,Q1],Dir,[R2,Q2]):- hexagon:direction(Dir,[R_dir,Q_dir]), R2 is R1+R_dir, Q2 is Q1+Q_dir.

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



%~~~~ MARIQUITA ~~~~~

mariquita_possible_moves(Type, Id, Player_id , Hex, Level, Moves, L_hive):-
    (
        not(insect_blocked(Type, Id, Player_id , Hex, Level)),
        not(hexagon:articulation_point(Hex,L_hive)),
        delete(L_hive, Hex, L_hive1), % esto es para analizar si esta conectada una casilla a la colmena sin la casilla Hex 
        findall(Move, (mariquita_move(Hex,L_hive1,[],0,Move)), Moves1),
        delete(Moves1, none, Moves2),
        utils:remove_repeated(Moves2,Moves)
    );
    (
        insect_blocked(Type, Id, Player_id , Hex, Level),
        Moves = []
    ).

mariquita_move(Hex1, L_hive1, Visited, Level_rec, Move):- 
    (
        Level_rec == 3, 
        not(member(Hex1, L_hive1)), 
        Move = Hex1
    );
    (
        Level_rec > 3, 
        not(member(Hex1, L_hive1)), 
        Move = none
    );
    (
        append(Visited,[Hex1], Visited_1),
        hexagon:are_neighbors(Hex1, Adj_hex1),
        Level_rec < 2 , 
        member(Adj_hex1, L_hive1), % esta ocupada
        not(member(Adj_hex1, Visited_1)), % para saber si un nodo no esta visitado
        Level_rec1 is Level_rec + 1,
        mariquita_move(Adj_hex1, L_hive1, Visited_1,Level_rec1,Move)
    );
    (
        append(Visited,[Hex1], Visited_1),
        hexagon:are_neighbors(Hex1, Adj_hex1),
        Level_rec == 2 , 
        not(member(Adj_hex1, L_hive1)), % esta en blanco la casilla
        not(member(Adj_hex1, Visited_1)), % para saber si un nodo no esta visitado
        Level_rec1 is Level_rec + 1,
        mariquita_move(Adj_hex1, L_hive1, Visited_1,Level_rec1,Move)
    ).

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



%~~~~~ BICHO BOLA ~~~~~

bichoBola_possible_moves(Type, Id, Player_id , Hex, Level, Moves, L_hive):-
    (
        not(insect_blocked(Type, Id, Player_id , Hex, Level)),
        not(hexagon:articulation_point(Hex,L_hive)),
        delete(L_hive, Hex, L_hive1), % esto es para analizar si esta conectada una casilla a la colmena sin la casilla Hex 
        findall(Hex1, 
                (hexagon:are_neighbors(Hex,Hex1), not(member(Hex1, L_hive1)), hexagon:is_connected_hex_to_hive(Hex1,L_hive1)),
                Moves)
    );
    (
        insect_blocked(Type, Id, Player_id , Hex, Level),
        Moves = []
    ).
    % falta hacer la habilidad especial
    % preguntar si es dar click sobre ella y la ficha q se va a mover
