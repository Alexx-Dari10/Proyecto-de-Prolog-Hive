module(insects, [
        start_insects/3, insect/5, possible_moves/8, move_insect_db/6, move_insect/8,start_game/0,
        set_hex_to_type/5, select_in_hand/6,find_insect_high_level/2
    ]).

:- consult(hexagon), import(hexagon).
:- consult(utils), import(utils).

% si level = -1 entonces la ficha no ha sido puesta en juego
% insect(Type, Id, Player_id, Hex=[R,Q], Level)

:- dynamic insect/5,  hive/1 .
:- dynamic player/4, add_queen/1.

start_game():-
    assert(hive([])),
    assert(player(p1, 0, true, true)),
    assert(player(p2, 0, false, true)).




move_insect(Val, Type, Id, Player_id, Hex, Level, Hex_fin, L_hive, Msg):-
    (
        Val == init,
        
        possible_moves(Val, Type, Id, Player_id, Hex, Level,Moves, L_hive),
        
        member(Hex_fin, Moves),
        move_insect_db(Type, Id, Player_id, Hex, 0, Hex_fin),
        Msg = "",!
        
    );
    (
        Val == add,
        not(must_add_queen(Player_id)),
        possible_moves(Val, Type, Id, Player_id, Hex, Level,Moves, L_hive),
        member(Hex_fin, Moves),
        write("Type: "), writeln([Type, Id, Player_id, Hex, 0, Hex_fin]),
        
        move_insect_db(Type, Id, Player_id, Hex, 0, Hex_fin),
        Msg = "",!
    );
    (
        Val == add,
        must_add_queen(Player_id),
        not(Type == abejaReina),
        Msg = "Must add queen. 4 movement",!
        
    );
    (
        Val == add,
        must_add_queen(Player_id),
        Type == abejaReina,
        possible_moves(Val, Type, Id, Player_id, Hex, Level,Moves, L_hive),
        member(Hex_fin, Moves),
        move_insect_db(Type, Id, Player_id, Hex, 0, Hex_fin),
        Msg = "",!
        
    );
    (
        not(Val == init),
        not(Val == add),
        
        
        not(queen_in_game(Player_id)),
        Msg = "Add queen to move any piece",!
        
    );
    (
        not(Val == init),
        not(Val == add),
        queen_in_game(Player_id),

        possible_moves(Val, Type, Id, Player_id, Hex, Level,Moves, L_hive),
/*  
Chequear si es mosquito o escarabajo ponerle 1 nivel mas
        member(Hex_fin, L_hive),!, % ya hay otra ficha en esta posicion
        %quito la ficha con el nivel que tenga y la pongo en un nivel mas
        %NOTA: ver los niveles como las posiciones de una pila (el q esta en el nivel 0 esta en el tope de la pila) 
 */     
        
        member(Hex_fin, Moves),
        
        move_insect_db(Type, Id, Player_id, Hex, Level, Hex_fin),
        Msg = "",
        writeln(Msg),!
    ).

%% encontrar el insecto de mayor nivel en la colmena      
find_high_level([],Lev,Insect, Insect):-!.

find_high_level([[Type, Id,Player_id, [Axial_x, Axial_y], Level]|R_L_insects],Lev, Insect, Insect_keeped):-
    bigger(Level,Lev),!,
    find_high_level(R_L_insects, Level, [Type, Id,Player_id, [Axial_x, Axial_y], Level], Insect_keeped).

find_high_level([[Type, Id,Player_id, [Axial_x, Axial_y], Level]|R_L_insects],Lev, Insect, Insect_keeped):-
    not(bigger(Level,Lev)),
    find_high_level(R_L_insects, Lev, Insect, Insect_keeped).


find_insect_high_level([Axial_x, Axial_y], Insect_keeped):-
    findall([Type, Id,Player_id, [Axial_x, Axial_y], Level], 
            insects:insect(Type, Id,Player_id, [Axial_x, Axial_y], Level), 
            L_insects),

    find_high_level(L_insects, -2, _, Insect_keeped).

%%%%%%%%%%%%%%%%%%%%%%%%

insect_blocked(Type, Id, Player_id , Hex, Level):-
    find_insect_high_level(Hex, [Type1, Id1, Player_id1 , Hex1, Level1 ]),
    Level1 > Level.



change_player_turn(Type, Player_id, Hex, Level, Hex_fin, L_hive):-
    (
        % elimina el jugador que es current ahora 
        writeln("change_player_turn"),
        retract(player(Player_id, Moves, Current,Init)),
        % busca el otro jugador que hay en la base de datos y lo pone como current si este puede jugar
        
        player(Id_other, Moves_other, false, Init_other),
        
        

        player_can_play(Id_other, L_hive),
        writeln("can play"),
        retract(player(Id_other, Moves_other, _, Init_other)),
        assert(player(Id_other, Moves_other, true, Init_other)),
        
        % agrega al jugador que hizo la jugada con un movimiento mas
        Moves_new is Moves + 1,
        assert(player(Player_id, Moves_new, false, false))
    );
    (
        writeln("aqui entra alguna vez?"),
        % elimina el jugador que es current ahora con su cantidad de movimientos 
        retract(player(Player_id, Moves, Current,Init)),
        % busca el otro jugador que hay en la base de datos y lo pone como current si este puede jugar
        player(Id, Moves_other, false, Init_other),
        not(player_can_play(Id, L_hive)),

        
        assert(player(Player_id, Moves, Current, Init))
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
    
     % for para recorrer todos los insectos de la mano, del jugador 
     (
        findall([Type, Id, Player_id,Hex,-1], 
                insect(Type, Id, Player_id,Hex, -1), 
                Insects_hand),
        
       
        member([Type, Id, Player_id_memb ,Hex,Level], Insects_hand),
        possible_moves(init, Type, Id, Player_id_memb , Hex, Level, Moves, L_hive),
    
        length(Moves, Length),

        hexagon:bigger(Length, 0),
         ! % si encuentro uno q se mueva mover entonces ya esta
    );

    % para recorrer todos los insectos de la mano, del jugador 
    (
        findall([Type, Id, Player_id,Hex,-1], 
                insect(Type, Id, Player_id,Hex, -1), 
                Insects_hand),
        
       
        member([Type, Id, Player_id_memb ,Hex,Level], Insects_hand),
        possible_moves(add, Type, Id, Player_id_memb , Hex, Level, Moves, L_hive),
        length(Moves, Length),

        hexagon:bigger(Length, 0),
         ! % si encuentro uno q se mueva mover entonces ya esta
    );
    % para recorrer todos los insectos de la colmena, del jugador
    (
        findall([Type, Id, Player_id,Hex,Level], 
            ((Type, Id, Player_id,Hex, Level), bigger(Level,-1)), 
            Insects_hive),

        member([Type, Player_id_memb, Player_id,Hex,Level], Insects_hive), 
        insects:possible_moves(Type, Type, Id, Player_id_memb , Hex, Level, Moves, L_hive),
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

move_insect_db(Type, Id, Player_id, Hex, Level, Hex_fin):-
    
    (
        hive(L_hive),
        
        % agrega el insecto a la colmena poniendole nivel 0
        retract(insect(Type, Id, Player_id, Hex, _)),
        assert(insect(Type, Id, Player_id, Hex_fin, Level)),
        
        write("se hizo el cambio"),
        
        %elimina la posicion vieja del insecto de la lista de las casillas y agrega la nueva
        delete(L_hive, Hex, L_1),
        append(L_1, [Hex_fin], L_2),
        retract(hive(L_hive)),
        assert(hive(L_2))
        
       
    );
    (
        writeln("Estoy en el metodo db"),
        hive(L_hive),
        

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




%revisar esto

give_adj(Index, Index1,Index2):-
    (Index == 6, Index1 = 5, Index2 = 1, !);
    (Index == 5, Index1 = 4, Index2 = 6, !);
    (Index == 4, Index1 = 3, Index2 = 5, !);
    (Index == 3, Index1 = 2, Index2 = 4, !);
    (Index == 2, Index1 = 1, Index2 = 3, !);
    (Index == 1, Index1 = 6, Index2 = 2, !).


cell_in_center(Hex,Index, L_hive):-

    give_adj(Index, Index1, Index2),

    hexagon:neighbor_dir(Hex, Adj1, Index1),
    hexagon:neighbor_dir(Hex, Adj2, Index2),

    member(Adj1, L_hive),
    member(Adj2, L_hive),
    write_ln("es en centro").


    


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
        Moves = [[0, 0]]
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
        Hex_neighbor, 
        (
            insect(_,_,Player_id, Hex_insect, Level1),
            bigger(Level1,-1),
            are_neighbors(Hex_insect, Hex_neighbor),
            not(member(Hex_neighbor, L_hive)),
            valid_hex(Player_id, Hex_neighbor, L_hive)
        ),
        Moves1
    ),
    utils:remove_repeated(Moves1, Moves).

% este predicado es para revisar q ninguna ficha alrededor es del adversario
valid_hex(Player_id, Hex_neighbor, L_hive):-
   
    findall(Neighbor, 
            (
                are_neighbors(Hex_neighbor, Neighbor), % para ir por cada vecino
                
                is_same_color_or_empty(Player_id, Neighbor, L_hive)
            ), 
            List_neighbor),
    
    length(List_neighbor, 6). % si todas las casillas alrededor son del mismo color o estan vacias
    
    
    
% analizo si la casilla es un insecto del mismo color o la casilla no esta en la colmena(o sea esta en blanco)
is_same_color_or_empty(Player_id, Neighbor, L_hive):-
    insect(_, _, Player_id, Neighbor,_), ! ; 
    not(member(Neighbor, L_hive)).
    


%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



%~~~~ ABEJA REINA ~~~~

abejaReina_possible_moves(Type, Id, Player_id , Hex, Level, Moves, L_hive):-
    (
        not(insect_blocked(Type, Id, Player_id , Hex, Level)),
        hexagon:not_articulation_point(Hex,L_hive),
        delete(L_hive, Hex, L_hive1), % esto es para analizar si esta conectada una casilla a la colmena sin la casilla Hex 
        findall(Hex1, 
                (
                    rang(1,6,Index),
                    hexagon:neighbor_dir(Hex,Hex1,Index),

                    not(member(Hex1, L_hive1)),
                    not(cell_in_center(Hex,Index,L_hive)), 

                    hexagon:connected(Hex1,L_hive1)
                ),
                Moves),!
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
        hexagon:not_articulation_point(Hex,L_hive),
        delete(L_hive, Hex, L_hive1), % esto es para analizar si esta conectada una casilla a la colmena sin la casilla Hex 
        % esto nos da todos los hexagonos en blanco q son adyacentes a los de la colmena
        % (pueden dar valores repetidos)
        findall(Hex1, 
                (
                    hormiga_move(Hex, Hex1, L_hive1,[]),
                    not(Hex1 == Hex)
                ), 
                Moves_rep),
        
        %quitando los valores repetidos
        utils:remove_repeated(Moves_rep,Moves)
    );
    (
        insect_blocked(Type, Id, Player_id , Hex, Level),
        Moves = []
    );
    (
        not(hexagon:not_articulation_point(Hex,L_hive)),
        writeln("Es de articulacion"),
        Moves = []
    ).



hormiga_move(Hex, Hex1, Hive, Visited):- Hex1 = Hex.
hormiga_move(Hex,Hex1, Hive, Visited):-
    append(Visited,[Hex], Visited_1),

    rang(1,6,Index),
    hexagon:neighbor_dir(Hex,Adj_hex,Index),

    not(cell_in_center(Hex,Index,Hive)), 

    not(member(Adj_hex, Hive)),
    not(member(Adj_hex, Visited_1)), % para saber si un nodo no esta visitado
    connected(Adj_hex, Hive),
    

    hormiga_move(Adj_hex, Hex1, Hive, Visited_1).


%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



%~~~~ SALTAMONTE ~~~~

saltamonte_possible_moves(Type, Id, Player_id , Hex, Level, Moves, L_hive):-
    (
        not(insect_blocked(Type, Id, Player_id , Hex, Level)),
        hexagon:not_articulation_point(Hex,L_hive),
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
        hexagon:not_articulation_point(Hex,L_hive),
        delete(L_hive, Hex, L_hive1), % esto es para analizar si esta conectada una casilla a la colmena sin la casilla Hex 
        findall(Hex1, 
                (hexagon:are_neighbors(Hex,Hex1),hexagon:connected(Hex1,L_hive1)), 
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
        hexagon:not_articulation_point(Hex,L_hive),
        delete(L_hive, Hex, L_hive1), % esto es para analizar si esta conectada una casilla a la colmena sin la casilla Hex 
        
        findall(Move, (aranha_move(Hex,L_hive1,[],0,Move)), Moves)
    );
    (
        insect_blocked(Type, Id, Player_id , Hex, Level),
        Moves = []
    ).

aranha_move(Hex, L_hive1, Visited, Level_rec, Move):- 
    (Level_rec == 3, Move = Hex).

aranha_move(Hex, L_hive1, Visited,Level_rec, Move):-

    Level_rec < 3,

    append(Visited,[Hex], Visited_1),

    rang(1,6,Index),
    hexagon:neighbor_dir(Hex,Adj_hex,Index),

    
    not(member(Adj_hex, L_hive1)), % esta en blanco
    not(cell_in_center(Hex,Index,L_hive1)),
    not(member(Adj_hex, Visited_1)), % para saber si un nodo no esta visitado
    
    hexagon:is_connected_hex_to_hive(Adj_hex,L_hive1),


    Level_rec1 is Level_rec + 1,
    aranha_move(Adj_hex, L_hive1, Visited_1, Level_rec1, Move).

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


%~~~~ MOSQUITO ~~~~~

mosquito_possible_moves(Type, Id, Player_id , Hex, Level, Moves, L_hive):-
    (
        not(insect_blocked(Type, Id, Player_id , Hex, Level)),
        hexagon:not_articulation_point(Hex,L_hive),
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
        hexagon:not_articulation_point(Hex,L_hive),
        delete(L_hive, Hex, L_hive1), % esto es para analizar si esta conectada una casilla a la colmena sin la casilla Hex 
        findall(Move, (mariquita_move(Hex,L_hive1,[],0,Move)), Moves1),
        delete(Moves1, none, Moves)
        
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
        hexagon:not_articulation_point(Hex,L_hive),
        delete(L_hive, Hex, L_hive1), % esto es para analizar si esta conectada una casilla a la colmena sin la casilla Hex 
        findall(Hex1, 
                (hexagon:are_neighbors(Hex,Hex1), not(member(Hex1, L_hive1)), hexagon:connected(Hex1,L_hive1)),
                Moves)
    );
    (
        insect_blocked(Type, Id, Player_id , Hex, Level),
        Moves = []
    ).
    % falta hacer la habilidad especial
    % preguntar si es dar click sobre ella y la ficha q se va a mover







select_in_hand(Type, Player_id , Hex_select, Id,[X1,X2,X3,X4,X5,X6,X7,X8], [Y1,Y2,Y3]):-
        (
            findall(Type, insect(Type,_,Player_id, _, -1), L_insects),
            length(L_insects, Length),
            Length == 3,
            Id = 3,
            writeln("3"),
            
            set_hex_to_type(Type, 3, Hex_select,[X1,X2,X3,X4,X5,X6,X7,X8], [Y1,Y2,Y3])
        ),!;
        (
            
            findall(Type, insect(Type,_,Player_id, _, -1), L_insects),
            length(L_insects, Length),
            Length == 2,
            Id = 2,
            writeln("2"),
            set_hex_to_type(Type, 2, Hex_select,[X1,X2,X3,X4,X5,X6,X7,X8], [Y1,Y2,Y3])
        ),!;
        (
            findall(Type, insect(Type,_,Player_id, _, -1), L_insects),
            length(L_insects, Length),
            Length == 1,
            Id = 1,
            writeln("1"),
            set_hex_to_type(Type, 1, Hex_select,[X1,X2,X3,X4,X5,X6,X7,X8], [Y1,Y2,Y3])
        ).


set_hex_to_type(Type, Id, Hex_select,[X1,X2,X3,X4,X5,X6,X7,X8], [Y1,Y2,Y3]):-
        (
            Id == 3,
            (
                ( Type == hormiga, Hex_select = [X1,Y3] );
                ( Type == escarabajo, Hex_select = [X2,Y3] );
                ( Type == saltamonte, Hex_select = [X3,Y3] );
                ( Type == abejaReina, Hex_select = [X4,Y3] );
                ( Type == aranha, Hex_select = [X5,Y3] );
                ( Type == mariquita, Hex_select = [X6,Y3] );
                ( Type == mosquito, Hex_select = [X7,Y3] );
                ( Type == bichoBola, Hex_select = [X8,Y3] )
            )
        );
        (
            Id == 2,
            (
                (Type == hormiga,Hex_select = [X1,Y2]);
                (Type == escarabajo,Hex_select = [X2,Y2]);
                (Type == saltamonte,Hex_select = [X3,Y2]);
                (Type == abejaReina,Hex_select = [X4,Y2]);
                (Type == aranha,Hex_select = [X5,Y2]);
                (Type == mariquita,Hex_select = [X6,Y2]);
                (Type == mosquito,Hex_select = [X7,Y2]);
                (Type == bichoBola,Hex_select = [X8,Y2])
            )
        );
        (
            Id == 1,
            (
                (Type == hormiga,Hex_select = [X1,Y1]);
                (Type == escarabajo,Hex_select = [X2,Y1]);
                (Type == saltamonte,Hex_select = [X3,Y1]);
                (Type == abejaReina,Hex_select = [X4,Y1]);
                (Type == aranha,Hex_select = [X5,Y1]);
                (Type == mariquita,Hex_select = [X6,Y1]);
                (Type == mosquito,Hex_select = [X7,Y1]);
                (Type == bichoBola,Hex_select = [X8,Y1] )   
            ) 
        ).
    
    
    
        
    