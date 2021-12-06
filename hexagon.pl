% exports

module(hexagon,
    [
        is_connected_hex_to_hive/2, direction/2, articulation_point/2,hive/1,
        flat_hex_to_pixel/4, pixel_to_flat_hex/4

    ]).

%import
:-consult(utils), import(utils).



% tomaremos las direcciones axiales
% S = -R-Q
% aqui usamos el para <fila;columna> ver las direcciones en forma horaria
direction(1, [-1, 1]). 
direction(2, [0, 1]).
direction(3, [1, 0]).
direction(4, [1, -1]).
direction(5, [0, -1]).
direction(6, [-1, 0]).


are_neighbors(Hex1, Hex2):- are_neighbors_dir(Hex1,Hex2,1).

%are_neighbors_dir(Hex1,Hex2,Dir).
are_neighbors_dir([R1,Q1],[R2,Q2],Dir):- direction(Dir,[R_dir,Q_dir]), R2 is R1+R_dir, Q2 is Q1+Q_dir.
are_neighbors_dir([R1,Q1],[R2,Q2],Dir):- Dir1 is Dir + 1, Dir1 =< 6, are_neighbors_dir([R1,Q1],[R2,Q2],Dir1).

% esta colmena es de ejemplo. Pero va a hacer asi. Contiene las posiciones de las casillas

hive([[0,0], [1,0], [2,0], [3,0], [0,1], [1,1], [2,1] , [1,2], [0,3]]).
% este metodo nos dice si una casilla esta en la colmena o no 
in_Hive(Cas):- hive(L), member(Cas, L),!.

% para saber si dos hexagonos son adyacentes en la colmena
adj(H1,H2):-
    in_Hive(H1), in_Hive(H2),
    are_neighbors(H1,H2).

% para saber si el nodo q se quiere agregar esta conectado a la colmena
is_connected_hex_to_hive(Hex, Hive):-
    member(Hex_memb, Hive),
    not(Hex_memb == Hex),
    dfs(Hex, Hex_memb, Hive, []),!.

add_to_hive(Hex):- not(in_Hive(Hex)), hive(L), is_connected_hex_to_hive(Hex, L).

% devuelve true si hay camino entre dos casillas de la colmena
dfs(Hex1, Hex2, Hive, Visited):- Hex1 == Hex2, writeln("esta conectado"),!.
dfs(Hex1, Hex2, Hive, Visited):-
    append(Visited,[Hex1], Visited_1),
    are_neighbors(Hex1, Adj_hex1),
    member(Adj_hex1, Hive),
    not(member(Adj_hex1, Visited_1)), % para saber si un nodo no esta visitado
    dfs(Adj_hex1, Hex2, Hive, Visited_1).


all_neighbors(Hex, L_Neighbors):- findall(Neighbor_hex, are_neighbors(Hex,Neighbor_hex), L_Neighbors).


% saber si es un punto de articulacion
articulation_point(Hex_old, L_hive):-
    delete(L_hive, Hex_old, L_hive1),
    findall(Hex_neighbor, 
               (
                    are_neighbors(Hex_old, Hex_neighbor), 
                    member(Hex_neighbor, L_hive1),
                    is_connected_hex_to_hive(Hex_neighbor, L_hive1)
                );
                (
                    are_neighbors(Hex_old, Hex_neighbor), 
                    not(member(Hex_neighbor, L_hive1))
                ),
            L_neighbors),
    
    length(L_neighbors, Length),
    
    Length < 6,
    writeln("Articulation point").
    
    

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% esto es para tener los hexagonos de la forma del juego
% axial_to_oddq(Hex, OffsetCoord)
axial_to_oddq([R, Q], OffsetCoord):-
    number(R),
    number(Q),
    var(OffsetCoord),
    Row is R + (Q - (Q mod 2)) / 2,
    Col = Q,
    OffsetCoord = [Row, Col].

% oddq_to_axial(OffsetCoord, Hex)
oddq_to_axial([Row, Col], Hex):-
    number(Row),
    number(Col),
    var(Hex),
    R = Row - (Col - (Col mod 2)) / 2,
    Q = Col,
    Hex = [R, Q].

% estas funciones son para dado un pixel convertirlo a un hexagono y viceversa

flat_hex_to_pixel(R, Q, Size, Pixel):-
    Row = Size * ( 3/2 * Q),
    Col = Size * (sqrt(3)/2 * Q + sqrt(3)*R),
    Pixel= [Row,Col].

pixel_to_flat_hex(Row,Col,Size,Hex):-
    R = (-1/3 * Row + sqrt(3)/3 * Col)/Size,
    Q = (2/3 * Row),
    axial_round(R, Q, Round_R, Round_Q),
    Hex = [Round_R,Round_Q].

% estos metodos son x si los valores de los pixeles no son enteros entonces hay q redondearlos
axial_round(R, Q, Round_R, Round_Q):-
    Rgrid = round(R), 
    Qgrid = round(Q),
    X = R - Rgrid, 
    Y = Q - Qgrid,
    axial_round_2(X, Y, Rgrid, Qgrid, Round_R, Round_Q).

axial_round_2(X, Y, Rgrid, Qgrid, Round_R, Round_Q):- 
    abs(X) >= abs(Y),
    Round_R = Rgrid + round(X + 0.5*Y),
    Round_Q = Qgrid, !.

axial_round_2(X, Y, Rgrid, Qgrid, Round_R, Round_Q):- 
    abs(X) < abs(Y),
    Round_R = Rgrid,
    Round_Q = Qgrid + round(Y + 0.5 *X), !.

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
