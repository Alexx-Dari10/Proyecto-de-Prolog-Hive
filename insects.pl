module(insects, [
        start_insects/1, insect/5
    ]).

:- consult(hexagon), import(hexagon).
:- consult(utils), import(utils).

% si level = -1 entonces la ficha no ha sido puesta en juego
% insect(Type, Id, Player_id, Hex=[R,Q], Level)

:- dynamic insect/5.

start_insects(Player):- 
    assert(insect(abejaReina, 1, Player, none, -1)),

    assert(insect(hormiga, 1, Player, none, -1)),
    assert(insect(hormiga, 2, Player, none, -1)),
    assert(insect(hormiga, 3, Player, none, -1)),

    assert(insect(saltamonte, 1, Player, none, -1)),
    assert(insect(saltamonte, 2, Player, none, -1)),
    assert(insect(saltamonte, 3, Player, none, -1)),

    assert(insect(escarabajo, 1, Player, none, -1)),
    assert(insect(escarabajo, 2, Player, none, -1)),

    assert(insect(aranha, 1, Player, none, -1)),
    assert(insect(aranha, 2, Player, none, -1)),

    assert(insect(mosquito, 1, Player, none, -1)),
    assert(insect(mariquita, 1, Player, none, -1)),
    assert(insect(bichoBola, 1, Player, none, -1)).




% busca el insecto que esta en la casilla Hex
find_insect_by_hex(Hex, Type, Id, Player_id, Level):- 
    insect(Type, Id, Player_id, Hex, Level).


% devuelve una lista de los insectos que no estan en la colmena todavia
find_insects_in_hand(Player,L):-
    findall([Type, Id, Player, Hex, -1], insect(Type, Id, Player, Hex, -1), L).


% devuelve una lista de los insectos que estan en la colmena
find_insects_in_hive(Player,L):-
    findall([Type, Id, Player, Hex, Level], (insect(Type, Id, Player, Hex,Level), bigger(Level,-1)), L).



celd_empty(Hex):- not(insect(Type, Id, Player_id, Hex, Level)).


%~~~~~~~~~~~~~posibles movimientos

possible_moves(Player_id, Type, Hex, Moves, L_hive):-
    switch(Type,
        [
            abejaReina: abejaReina_possible_moves(Player_id, Hex, Moves,L_hive),
            hormiga: hormiga_possible_moves(Player_id, Hex, Moves,L_hive),
            saltamonte: saltamonte_possible_moves(Player_id, Hex, Moves,L_hive),
            escarabajo: escarabajo_possible_moves(Player_id, Hex, Moves,L_hive),
            aranha: aranha_ant_possible_moves(Player_id, Hex, Moves,L_hive),
            mosquito: mosquito_possible_moves(Player_id, Hex, Moves,L_hive),
            mariquita: mariquita_possible_moves(Player_id, Hex, Moves,L_hive),
            bichoBola: bichoBola_possible_moves(Player_id, Hex, Moves,L_hive)
        ]).

% switch case 
switch(Type, [T:Goal|Cases]) :-
    ( Type=T ->
        call(Goal)
    ;
        switch(Type, Cases)
    ).



% ~abeja reina
abejaReina_possible_moves(Player_id, Hex, Moves, L_hive):-
    %si no esta bloqueada
    %ver si es punto de articulacion y no se puede mover
    delete(L_hive, Hex, L_hive1), % esto es para analizar si esta conectada una casilla a la colmena sin la casilla Hex 
    findall(Hex1, 
            (are_neighbors(Hex,Hex1), not(member(Hex1, L_hive1)), hexagon:is_connected_hex_to_hive(Hex1,L_hive1)),
            Moves).
    
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

% hormiga
hormiga_possible_moves(Player_id, Hex, Moves,L_hive):-
    %si no esta bloqueada
    %ver si es punto de articulacion y no se puede mover
    delete(L_hive, Hex, L_hive1), % esto es para analizar si esta conectada una casilla a la colmena sin la casilla Hex 
    % esto nos da todos los hexagonos en blanco q son adyacentes a los de la colmena
    % (pueden dar valores repetidos)
    findall(Hex2, 
            (member(Hex1, L_hive1),are_neighbors(Hex1,Hex2), not(member(Hex2, L_hive1)),
            hexagon:is_connected_hex_to_hive(Hex2,L_hive1)), 
            Moves_rep),
    
    %quitando los valores repetidos
    utils:remove_repeated(Moves_rep,Moves).

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


%saltamonte
saltamonte_possible_moves(Player_id, Hex, Moves,L_hive):-
    %si no esta bloqueada
    %ver si es punto de articulacion y no se puede mover
    delete(L_hive, Hex, L_hive1), % esto es para analizar si esta conectada una casilla a la colmena sin la casilla Hex 
    saltamonte_move(Hex, L_hive1, Moves).

% para saber la posible casilla en una direccion
saltamonte_move_dir([R,Q],Dir,L_hive1, Steps, Move):- 
    hexagon:direction(Dir,[R_dir,Q_dir]), 
    R2 is R1+R_dir, Q2 is Q1+Q_dir,
    (
        not(member([R2,Q2], L_hive1)), 
        Steps>0, 
        Move is [R2,Q2]
    ), !;
    (
        not(member([R2,Q2], L_hive1)), 
        Steps == 0, 
        Move is none % devolvemos este valor y luego lo quitamos
    ),!;
    (
        member([R2,Q2], L_hive1), 
        Steps1 is Steps + 1, 
        saltamonte_move_dir([R2,Q2],Dir,L_hive1, Steps1, Move)
    ).

saltamonte_move([R,Q],L_hive1, Moves):-
    saltamonte_move_dir([R,Q],1,L_hive1, Steps_1, Move_1),
    saltamonte_move_dir([R,Q],2,L_hive1, Steps_2, Move_2),
    saltamonte_move_dir([R,Q],3,L_hive1, Steps_3, Move_3),
    saltamonte_move_dir([R,Q],4,L_hive1, Steps_4, Move_4),
    saltamonte_move_dir([R,Q],5,L_hive1, Steps_5, Move_5),
    saltamonte_move_dir([R,Q],6,L_hive1, Steps_6, Move_6),

    Moves1 = [Move_1, Move_2,Move_3, Move_4, Move_5, Move_6],
    delete(Moves1, none, Moves).
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



%escarabajo
escarabajo_possible_moves(Player_id, Hex, Moves,L_hive):-
    %si no esta bloqueada
    %ver si es punto de articulacion y no se puede mover
    delete(L_hive, Hex, L_hive1), % esto es para analizar si esta conectada una casilla a la colmena sin la casilla Hex 
    findall(Hex1, 
            (are_neighbors(Hex,Hex1), not(member(Hex1, L_hive1)),hexagon:is_connected_hex_to_hive(Hex1,L_hive1)), 
            Moves).
    

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


%aranha
aranha_ant_possible_moves(Player_id, Hex, Moves,L_hive):-
    %si no esta bloqueada
    %ver si es punto de articulacion y no se puede mover
    delete(L_hive, Hex, L_hive1). % esto es para analizar si esta conectada una casilla a la colmena sin la casilla Hex 


%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


%mosquito
mosquito_possible_moves(Player_id, Hex, Moves,L_hive):-
    %si no esta bloqueada
    %ver si es punto de articulacion y no se puede mover
    delete(L_hive, Hex, L_hive1). % esto es para analizar si esta conectada una casilla a la colmena sin la casilla Hex 

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



%mariquita
mariquita_possible_moves(Player_id, Hex, Moves,L_hive):-
    %si no esta bloqueada
    %ver si es punto de articulacion y no se puede mover
    delete(L_hive, Hex, L_hive1). % esto es para analizar si esta conectada una casilla a la colmena sin la casilla Hex 


%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



%bicho bola
bichoBola_possible_moves(Player_id, Hex, Moves,L_hive):-
    %si no esta bloqueada
    %ver si es punto de articulacion y no se puede mover
    delete(L_hive, Hex, L_hive1). % esto es para analizar si esta conectada una casilla a la colmena sin la casilla Hex 

