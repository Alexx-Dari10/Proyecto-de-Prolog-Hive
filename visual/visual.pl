:-use_module(library(pce)).
:- pce_image_directory('./images').

:-consult('../hexagon'), import('../hexagon').
:-consult('../insects'), import('../insects').

:-consult('utils_visual'), import('utils_visual').

:-consult('draw_visual'), import('draw_visual').

:-dynamic size_hex/1, size_y/1, size_x/1, bool_selected/1, piece_selected/5,pieces/4, 
        init_player/2, move_state/1, current_player/1, arrowLeft/3.


change_player(p1,p2).
change_player(p2,p1).

% move_state/1 es para llevar el estado de la ficha seleccionada: init, add, move

% pieces(Player_id, Type, Initial_pieces, In_hive_pieces)

%fichas blancas
resource(abejaReinaBlanca, image,image('abejaReinaBlanca.jpg')).
resource(hormigaBlanca, image,image('hormigaBlanca.jpg')).
resource(aranhaBlanca, image,image('aranhaBlanca.jpg')).
resource(saltamonteBlanca, image,image('saltamonteBlanca.jpg')).
resource(escarabajoBlanca, image,image('escarabajoBlanca.jpg')).
resource(mosquitoBlanca, image,image('mosquitoBlanca.jpg')).
resource(mariquitaBlanca, image,image('mariquitaBlanca.jpg')).
resource(bichoBolaBlanca, image,image('bichoBolaBlanca.jpg')).

%fichas negras
resource(abejaReinaNegra, image,image('abejaReinaNegra.jpg')).
resource(hormigaNegra, image,image('hormigaNegra.jpg')).
resource(aranhaNegra, image,image('aranhaNegra.jpg')).
resource(saltamonteNegra, image,image('saltamonteNegra.jpg')).
resource(escarabajoNegra, image,image('escarabajoNegra.jpg')).
resource(mosquitoNegra, image,image('mosquitoNegra.jpg')).
resource(mariquitaNegra, image,image('mariquitaNegra.jpg')).
resource(bichoBolaNegra, image,image('bichoBolaNegra.jpg')).

resource(arrow_blanca, image,image('blanca.jpg')).
resource(arrow_negra, image,image('negra.jpg')).

arrow_left(p1, [700,40], arrow_blanca).
arrow_left(p2, [700,700],arrow_negra).


main:-
    insects:start_game(),

    assert(bool_selected(false)),
    assert(piece_selected(_,_,_,_,_)),
    assert(size_hex(40)),
    assert(size_x(800)),
    assert(size_y(800)),

    assert(init_player(p1,true)),
    assert(init_player(p2,true)),
    assert(current_player(p1)),
    

    assert(move_state(init)),
    

    %Variables
    assert(pieces(p1, hormiga    ,[[40,40],  [40,70],  [40, 100]], [])),
    assert(pieces(p1, escarabajo ,[[120,40], [120,70]           ], [])),
    assert(pieces(p1, saltamonte ,[[200,40], [200,70], [200,100]], [])),
    assert(pieces(p1, abejaReina ,[[280,40]                     ], [])),
    assert(pieces(p1, aranha     ,[[360,40], [360,70]           ], [])),
    assert(pieces(p1, mariquita  ,[[440,40]                     ], [])),
    assert(pieces(p1, mosquito  ,[[520,40]                     ], [])),
    assert(pieces(p1, bichoBola  ,[[600,40]                     ], [])),

    assert(pieces(p2, hormiga    ,[[40,700 ], [40,730 ],[40, 760]], [])),
    assert(pieces(p2, escarabajo ,[[120,700], [120,730]          ], [])),
    assert(pieces(p2, saltamonte ,[[200,700], [200,730],[200,760]], [])),
    assert(pieces(p2, abejaReina ,[[280,700]                     ], [])),
    assert(pieces(p2, aranha     ,[[360,700], [360,730]          ], [])),
    assert(pieces(p2, mariquita  ,[[440,700]                     ], [])),
    assert(pieces(p2, mosquito   ,[[520,700]                     ], [])),
    assert(pieces(p2, bichoBola  ,[[600,700]                     ], [])),


    
    size_hex(Size_hex),
    size_x(Size_x),
    size_y(Size_y),

    new(W, window("window", size(Size_x, Size_y))),
    send(W, background, colour(@default, 52000, 40000, 8000)),
    
    

    event_click(W),

    insects:start_insects(p1, [40,120,200,280,360,440,520,600], [40,70,100]),
    insects:start_insects(p2, [40,120,200,280,360,440,520,600], [700,730,760]),
         
    draw_game(W, Size_hex),

    send(W, open).


draw_game(W, Size_hex):-
    clear_game(W), 


    %pinta las piezas del jugador 1
    findall(_, 
            (
                pieces(p1, Type, Initials, In_hive),
                (
                    (
                        draw_pieces(W, p1,Size_hex,Type, Initials),
                        draw_pieces(W, p1,Size_hex,Type, In_hive)
                    ); 
                    (
                        draw_pieces(W, p1,Size_hex,Type, In_hive),
                        draw_pieces(W, p1,Size_hex,Type, Initials)
                    )
                )
            ), _),
    
    %pinta las piezas del jugador 2

    findall(_, 
        (
            pieces(p2, Type, Initials2, In_hive2),
            (
                (
                    draw_pieces(W, p2,Size_hex,Type, Initials2),
                    draw_pieces(W, p2,Size_hex,Type, In_hive2)
                ); 
                (
                    draw_pieces(W, p2,Size_hex,Type, In_hive2),
                    draw_pieces(W, p2,Size_hex,Type, Initials2)
                )
            )
        ), _),

    
    current_player(Current),
    arrow_left(Current,Hex_arrow, Arrow_name),
    
    draw_image_hexagon(W,Arrow_name,Hex_arrow).

clear_game(W):- send(W, clear).



event_click(W):-
    send(W, recogniser, click_gesture(
        left, '', single, 
        message(@prolog, select_piece,W, @event?position))).




check_init_add(W,Position,Player_id, [X1,X2,X3,X4,X5,X6,X7,X8], [Y1,Y2,Y3]):-
    (
        %init
        bool_selected(false),
        current_player(Player_id),

        init_player(Player_id, true),
        writeln("init"),
        writeln(Player_id),
        
        
        Size_hex is 40,
        Size_x is 800, 
        Size_y is 800,
        get(Position, x, Click_X),
        get(Position, y, Click_Y),
        check_positions_in_hand([Click_X,Click_Y], Size_hex, 
                                [X1,X2,X3,X4,X5,X6,X7,X8], [Y1,Y3], Type),
        
        
        select_in_hand(Type, Player_id, Hex_select, Id, [X1,X2,X3,X4,X5,X6,X7,X8], [Y1,Y2,Y3]),
        
        
        draw_hexagon_pixel_empty(W, Hex_select, Size_hex, colour(red)),
        
        hive(L_hive),

        possible_moves(init, Type, Id, Player_id , Hex_select, -1, Moves, L_hive),

        

        draw_possible_movements(W, Moves, Size_hex,Size_x,Size_y, colour(blue)),


        !, % este corte es para q no entre a las otras partes
        retract(move_state(_)), % ya se movio la ficha o no se pudo mover
        assert(move_state(init)),

        retract(bool_selected(false)),
        assert(bool_selected(true)),

        retract(piece_selected(_,_,_,_,_)),
        assert(piece_selected(Type,Id,Player_id,Hex_select,-1)),!
    );
    (
        %add
        bool_selected(false),
        current_player(Player_id),

        init_player(Player_id, false),

        writeln("addddddd"),


        Size_hex is 40,
        Size_x is 800, 
        Size_y is 800,
        get(Position, x, Click_X),
        get(Position, y, Click_Y),
        check_positions_in_hand([Click_X,Click_Y], Size_hex, 
                [X1,X2,X3,X4,X5,X6,X7,X8], [Y1,Y3], Type),
        
        
        select_in_hand(Type, Player_id, Hex_select, Id, [X1,X2,X3,X4,X5,X6,X7,X8], [Y1,Y2, Y3]),
        
        

        draw_hexagon_pixel_empty(W, Hex_select, Size_hex, colour(red)),   
        
        hive(L_hive),
        possible_moves(add, Type, Id, Player_id , Hex_select, -1, Moves, L_hive),
        
        draw_possible_movements(W, Moves, Size_hex,Size_x,Size_y, colour(blue)),

        !, % este corte es para q no entre a las otras partes

        retract(move_state(_)), % ya se movio la ficha o no se pudo mover
        assert(move_state(add)),

        retract(bool_selected(_)),
        assert(bool_selected(true)),

        retract(piece_selected(_,_,_,_,_)),
        assert(piece_selected(Type,Id,Player_id,Hex_select,-1)),!
        
    ).


select_piece(W, Position):-
        (
            check_init_add(W,Position, p1, [40,120,200,280,360,440,520,600], [40,70,100]),!
        );
        (
            check_init_add(W,Position, p2, [40,120,200,280,360,440,520,600], [700,730,760]),!
        );
        (
            bool_selected(false),

            not(check_init_add(W,Position, p1, [40,120,200,280,360,440,520,600], [40,70,100])),
            not(check_init_add(W,Position, p2, [40,120,200,280,360,440,520,600], [700,730,760])),

            Size_hex is 40,
            Size_x is 800, 
            Size_y is 800,
            get(Position, x, Click_X),
            get(Position, y, Click_Y),
            
            check_position_in_hive([Click_X,Click_Y], Size_hex, [40,600], [100,700]),
            
            
            %arreglar esto aqui pa q me de bien los valores de piece-selected
            draw_hexagon_pixel_axial(W, [Click_X, Click_Y], Size_hex,Size_x, Size_y, aranhaBlanca, colour(white)),
            


            !, % este corte es para q no entre a las otras partes
            retract(move_state(_)), % ya se movio la ficha o no se pudo mover
            assert(move_state(move)), % aqui en move debe ir la ficha q es

            retract(bool_selected(false)),
            assert(bool_selected(true)),

            retract(piece_selected(_,_,_,_,_)),
            assert(piece_selected(Type,Id,p1,Hex_select,Level))
        );
        (
            
            % mover la ficha seleccionada
            
            move_state(Type_state),
            write(Type_state),
            
            (
                (
                    Type_state == init,
                    make_move_state(W, Position,init),!
                );
                (
                    Type_state == add,
                    make_move_state(W, Position,add),!
                );
                (
                    not(Type_state == init), not(Type_state == add),

                    make_move_state(W, Position,Type_state),!
                )
            )
         
        ).

   
make_move_state_part1(W, Position, Type_move,Size_hex,Size_x,Size_y,L_hive,Type, Id, Player_id, Hex_ini, 
                        Level, [X_axial,Y_axial]):-
    writeln("4to"),
    bool_selected(true),
    write("Esto es "), writeln(Type_move),
    move_state(Type_move),

    Size_hex is 40,
    Size_x is 800, 
    Size_y is 800,
    

    get(Position, x, Click_X),
    get(Position, y, Click_Y),

    %buscar insecto para q esta marcado
    pixel_to_axial([Click_X,Click_Y], [X_axial,Y_axial], Size_hex,Size_x, Size_y),
    
    piece_selected(Type, Id, Player_id, Hex_ini, -1), % selected in hand
    
    hive(L_hive).


    
    

unclick(W, Size_hex, Player_id):-
    
    draw_game(W, Size_hex),

    retract(init_player(Player_id, _)),
    assert(init_player(Player_id, false)),

    retract(bool_selected(_)),
    assert(bool_selected(false)),

    retract(piece_selected(_,_,_,_,_)),
    assert(piece_selected(_,_,_,_,_)).
    
    


make_move_state(W, Position, Type_move):-
    (
        make_move_state_part1(W, Position, Type_move,Size_hex,Size_x,Size_y,L_hive,Type, Id, Player_id, Hex_ini, 
                            -1,[X_axial,Y_axial]),

        
        move_insect(Type_move, Type, Id, Player_id, Hex_ini, -1, [X_axial,Y_axial],L_hive, Msg),
        Msg == "ok",
        
        color_player(Player_id,Col),
    

        axial_to_pixel([X_axial,Y_axial], [X_pixel,Y_pixel], Size_hex, Size_x, Size_y),
    
    
        move_piece(W, [X_pixel,Y_pixel], Type, Col), 
        
       
        change_player_turn(Type, Player_id, Hex_ini, Level, Hex_fin, L_hive), % ver cuando esto no se cumple
        
        
        change_player(Player_id, Other_player),
        retract(current_player(_)),
        assert(current_player(Other_player)),
       

        unclick(W, Size_hex, Player_id), !
    );

    (
        make_move_state_part1(W, Position, Type_move,Size_hex,Size_x,Size_y,L_hive,Type, Id, Player_id, Hex_ini, 
            -1,[X_axial,Y_axial]),


        not(move_insect(Type_move, Type, Id, Player_id, Hex_ini, -1, [X_axial,Y_axial],L_hive, Msg)),

        writeln("invalid move"),
        unclick(W, Size_hex, Player_id),!
    );
    (
        make_move_state_part1(W, Position, Type_move,Size_hex,Size_x,Size_y,L_hive,Type, Id, Player_id, Hex_ini, 
            -1,[X_axial,Y_axial]),
        move_insect(Type_move, Type, Id, Player_id, Hex_ini, -1, [X_axial,Y_axial],L_hive, Msg),
        not(Msg == "ok"),
        
        writeln(Msg),
        unclick(W, Size_hex, Player_id),!
    ).
                

% este predicado recibe las coordenadas en pixeles
move_piece(W,Hex_end,Type, Color):-
    (
        size_hex(Size_hex),
        size_x(Size_x),
        size_y(Size_y),
        
        piece_selected(Type,Id,Player_id,Hex_select,Level),

        pieces(Player_id, Type, Initial_pieces, In_hive_pieces),


        member(Hex_select, Initial_pieces),
        
        delete(Initial_pieces, Hex_select, Initial_pieces_1),


        append(In_hive_pieces, [Hex_end], In_hive_pieces_1),

        retract(pieces(Player_id, Type, Initial_pieces, In_hive_pieces)),
        assert(pieces(Player_id, Type, Initial_pieces_1, In_hive_pieces_1))
    );
    (
        size_hex(Size_hex),
        size_x(Size_x),
        size_y(Size_y),
        
        piece_selected(Type,Id,Player_id,Hex_select,Level),

        pieces(Player_id, Type, Initial_pieces, In_hive_pieces),

        not(member(Hex_select, Initial_pieces)),
        
        member(Hex_select, In_hive_pieces),
        delete(In_hive_pieces, Hex_select, In_hive_pieces_1),
        append(In_hive_pieces_1, [Hex_end], In_hive_pieces_2),!,

        retract(pieces(Player_id, Type, Initial_pieces, In_hive_pieces)),
        assert(pieces(Player_id, Type, Initial_pieces, In_hive_pieces_2))
    ).




