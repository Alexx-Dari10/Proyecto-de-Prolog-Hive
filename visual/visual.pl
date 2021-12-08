:-use_module(library(pce)).
:- pce_image_directory('./images').

:-consult('../hexagon'), import('../hexagon').
:-consult('../insects'), import('../insects').

:-consult('draw_visual'), import('draw_visual').

resource(cero, image,image('0.jpg')).
resource(uno, image,image('1.jpg')).
resource(dos, image,image('2.jpg')).
resource(abejaReina, image,image('abejaReina.jpg')).
resource(hormiga, image,image('hormiga.jpg')).
resource(aranha, image,image('aranha.jpg')).
resource(saltamonte, image,image('saltamonte.jpg')).
resource(escarabajo, image,image('escarabajo.jpg')).
resource(mosquito, image,image('mosquito.jpg')).
resource(mariquita, image,image('mariquita.jpg')).
resource(bichoBola, image,image('bichoBola.jpg')).

main:-
    Size_hex is 35,
    Size_x is 800, 
    Size_y is 800,
    new(W, window("window", size(Size_x, Size_y))),
    send(W, background, colour(@default, 52000, 40000, 8000)),
    event_click(W),

    insects:start_insects(p1, [40,120,200,280,360,440,520,600], [40,70,100]),
    insects:start_insects(p2, [40,120,200,280,360,440,520,600], [700,730,760]),
         
    
    draw_initials_pieces(W, Size_hex, colour(white), [40,120,200,280,360,440,520,600], [40,70,100]),
    draw_initials_pieces(W, Size_hex, colour(black), [40,120,200,280,360,440,520,600], [700,730,760]),
    
    draw_possible_movements(W, [[0,0],[1,0],[2,0]], Size_hex,Size_x,Size_y, colour(blue)),
    send(W, open).



event_click(W):-
    send(W, recogniser, click_gesture(
        left, '', single, 
        message(@prolog, select_piece, W, @event?position))).

select_piece(W, Position):-
    (
        %Player 1
        Size_hex is 35,
        Size_x is 800, 
        Size_y is 800,
        get(Position, x, Click_X),
        get(Position, y, Click_Y),
        check_positions_in_hand([Click_X,Click_Y], Size_hex, 
                                [40,120,200,280,360,440,520,600], [40,100], Type,In_hand),
        In_hand,
        
        check_type_insect(true,false,Type, p1 , Hex_select, [],[40,120,200,280,360,440,520,600], [40,70,100]),
        writeln(Hex_select),
        
        draw_hexagon_pixel_empty(W, Hex_select, Size_hex, red)
    ),!;
    (
        % Player 2
        Size_hex is 35,
        Size_x is 800, 
        Size_y is 800,
        get(Position, x, Click_X),
        get(Position, y, Click_Y),
        check_positions_in_hand([Click_X,Click_Y], Size_hex, 
                                [40,120,200,280,360,440,520,600], [700,760], Type,In_hand),
        In_hand,
        writeln(Type),
        writeln("Player 2")
        
    ),!;
    (
        Size_hex is 35,
        Size_x is 800, 
        Size_y is 800,
        get(Position, x, Click_X),
        get(Position, y, Click_Y),
        
        check_position_in_hive([Click_X,Click_Y], Size_hex, [40,600], [100,700]),
        
        draw_hexagon_pixel_axial(W, [Click_X, Click_Y], Size_hex,Size_x, Size_y, aranha, colour(white))
    ).
   

check_type_insect(Init, Add, Type, Player_id, Hex_select,Moves, [X1,X2,X3,X4,X5,X6,X7,X8], [Y1,Y2,Y3]):-
    hexagon:switch(Type,
        [
            hormiga: hormiga_select(Init,Add,Type, Player_id , Hex_select, Moves, [X1,X2,X3,X4,X5,X6,X7,X8], [Y1,Y2,Y3]),
            escarabajo: escarabajo_select(Init,Add,Type, Player_id , Hex_select, Moves, [X1,X2,X3,X4,X5,X6,X7,X8], [Y1,Y2,Y3]),
            saltamonte: saltamonte_select(Init,Add,Type, Player_id , Hex_select, Moves, [X1,X2,X3,X4,X5,X6,X7,X8], [Y1,Y2,Y3]),
            abejaReina: abejaReina_select(Init,Add,Type, Player_id , Hex_select, Moves, [X1,X2,X3,X4,X5,X6,X7,X8], [Y1,Y2,Y3]),
            aranha: aranha_select(Init,Add,Type, Player_id , Hex_select, Moves, [X1,X2,X3,X4,X5,X6,X7,X8], [Y1,Y2,Y3]),
            mariquita: mariquita_select(Init,Add,Type, Player_id , Hex_select, Moves, [X1,X2,X3,X4,X5,X6,X7,X8], [Y1,Y2,Y3]),
            mosquito: mosquito_select(Init,Add,Type, Player_id , Hex_select, Moves, [X1,X2,X3,X4,X5,X6,X7,X8], [Y1,Y2,Y3]),
            bichoBola: bichoBola_select(Init,Add,Type, Player_id , Hex_select, Moves, [X1,X2,X3,X4,X5,X6,X7,X8], [Y1,Y2,Y3])
        ]).

hormiga_select(Init,Add,Type, Player_id , Hex_select, Moves,[X1,X2,X3,X4,X5,X6,X7,X8], [Y1,Y2,Y3]):-
    (
        (
            Init,
            
            findall(Type, insect(Type,_,Player_id, _, -1), L_insects),
            length(L_insects, Length),
            Length == 3,
            Hex_select = [X1,Y3],
            writeln("hear")
            
        ),!;
        (
            Init,
            findall(Type, insect(Type,_,Player_id, _, -1), L_insects),
            length(L_insects, Length),
            Length == 2,
            Hex_select = [X1,Y2]
        ),!;
        (
            Init,
            findall(Type, insect(Type,_,Player_id, _, -1), L_insects),
            length(L_insects, Length),
            Length == 1,
            Hex_select = [X1,Y1]    
        )
    );
    (
        (
            Add,
            findall(Type, insect(Type,_,Player_id, _, -1), L_insects),
            length(L_insects, Length),
            Length == 3,
            Hex_select = [X1,Y3]
        ),!;
        (
            Add,
            findall(Type, insect(Type,_,Player_id, _, -1), L_insects),
            length(L_insects, Length),
            Length == 2,
            Hex_select = [X1,Y2]
        ),!;
        (
            Add,
            findall(Type, insect(Type,_,Player_id, _, -1), L_insects),
            length(L_insects, Length),
            Length == 1,
            Hex_select = [X1,Y1]    
        )
    );
    (
        not(Init), not(Add)
        
    ).


escarabajo_select(Init,Add,Type, Id, Player_id , Hex, Level, Moves, L_hive).
saltamonte_select(Init,Add,Type, Id, Player_id , Hex, Level, Moves, L_hive).
abejaReina_select(Init,Add,Type, Id, Player_id , Hex, Level, Moves, L_hive).
aranha_select(Init,Add,Type, Id, Player_id , Hex, Level, Moves, L_hive).
mariquita_select(Init,Add,Type, Id, Player_id , Hex, Level, Moves, L_hive).
mosquito_select(Init,Add,Type, Id, Player_id , Hex, Level, Moves, L_hive).
bichoBola_select(Init,Add,Type, Id, Player_id , Hex, Level, Moves, L_hive).