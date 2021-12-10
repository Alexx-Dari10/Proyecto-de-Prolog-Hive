:-use_module(library(pce)).
:- pce_image_directory('./images').

:-consult('../hexagon'), import('../hexagon').
:-consult('../insects'), import('../insects').

:-consult('utils_visual'), import('utils_visual').

:-consult('draw_visual'), import('draw_visual').

:-dynamic bool_selected/1, piece_selected/5,pieces/4, 
        init_player/2, move_state/1, current_player/1, arrowLeft/3.


change_player(p1,p2).
change_player(p2,p1).

dimensions(40,1000,800).
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


    dimensions(Size_hex,Size_x,Size_y),

    new(W, window("window", size(Size_x, Size_y))),
    send(W, background, colour(@default, 52000, 40000, 8000)),
    
    

    event_click(W),
    new(Lbl2, label),
    insects:start_insects(p1, [40,120,200,280,360,440,520,600], [40,70,100]),
    insects:start_insects(p2, [40,120,200,280,360,440,520,600], [700,730,760]),
         
    draw_game(W),

   
    send(W, open).




draw_game(W):-
    clear_game(W), 

    
    dimensions(Size_hex,Size_x,Size_y),

    
    %pinta las piezas del jugador 1
    findall(_, 
            (
                pieces(p1, Type, Initials, In_hive),
                (
                    (
                        draw_pieces(W, p1,Size_hex,Size_x, Size_y, Type, Initials, false),
                        draw_pieces(W, p1,Size_hex,Size_x, Size_y,Type, In_hive, true)
                    ); 
                    (
                        draw_pieces(W, p1,Size_hex,Size_x, Size_y,Type, In_hive, true),
                        draw_pieces(W, p1,Size_hex,Size_x, Size_y, Type, Initials, false)
                    )
                )
            ), _),
    
    %pinta las piezas del jugador 2

    findall(_, 
        (
            pieces(p2, Type, Initials2, In_hive2),
            (
                (
                    draw_pieces(W, p2,Size_hex,Size_x, Size_y, Type, Initials2, false),
                    draw_pieces(W, p2,Size_hex,Size_x, Size_y,Type, In_hive2, true)
                ); 
                (
                    draw_pieces(W, p2,Size_hex,Size_x, Size_y,Type, In_hive2, true),
                    draw_pieces(W, p2,Size_hex,Size_x, Size_y, Type, Initials2, false)
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
        message(@prolog, click,W, @event?position))).




check_move_init_add(Type_move,W,Position,Player_id, [X1,X2,X3,X4,X5,X6,X7,X8], [Y1,Y2,Y3]):-
    (
        bool_selected(false),
        current_player(Player_id),
        
        dimensions(Size_hex,Size_x,Size_y),


        get(Position, x, Click_X),
        get(Position, y, Click_Y),
        check_positions_in_hand([Click_X,Click_Y], Size_hex, 
                                [X1,X2,X3,X4,X5,X6,X7,X8], [Y1,Y3], Type),
        
        
        select_in_hand(Type, Player_id, Hex_select, Id, [X1,X2,X3,X4,X5,X6,X7,X8], [Y1,Y2,Y3]),
        
        
        
        
        hive(L_hive),
        write_ln(Type_move),

        possible_moves(Type_move, Type, Id, Player_id , Hex_select, -1, Moves, L_hive),

       
        length(Moves, Length_moves),
        write_ln(Length_moves),
        Length_moves > 0,
        
        draw_hexagon_pixel_empty(W, Hex_select, Size_hex, colour(red)),

        draw_possible_movements(W, Moves, Size_hex,Size_x,Size_y, colour(blue)),


        !, % este corte es para q no entre a las otras partes
        retract(move_state(_)), % ya se movio la ficha o no se pudo mover
        assert(move_state(Type_move)),

        retract(bool_selected(false)),
        assert(bool_selected(true)),

        retract(piece_selected(_,_,_,_,_)),
        assert(piece_selected(Type,Id,Player_id,Hex_select,-1)),!
    ).


select_init(W, Position, Player_id, [X1,X2,X3,X4,X5,X6,X7,X8],[Y1,Y2,Y3]):-
    init_player(Player_id, true),  
    check_move_init_add(init, W,Position, Player_id, [X1,X2,X3,X4,X5,X6,X7,X8],[Y1,Y2,Y3]),!.

select_add(W, Position, Player_id, [X1,X2,X3,X4,X5,X6,X7,X8],[Y1,Y2,Y3]):-
    not(init_player(Player_id, true)),  
    check_move_init_add(add, W,Position, Player_id, [X1,X2,X3,X4,X5,X6,X7,X8],[Y1,Y2,Y3]),!.

select_move(W, Position, Current_Player_id):-
    %player 1 y player 2 
    bool_selected(false),

    dimensions(Size_hex,Size_x,Size_y),


    get(Position, x, Click_X),
    get(Position, y, Click_Y),
    
    check_position_in_hive([Click_X,Click_Y], Size_hex, [40,600], [100,700]),
    

    
    %convertimos a pixeles para poder pintar y guardar el punto en el visual en piece selected
    pixel_to_axial([Click_X,Click_Y], [X_axial,Y_axial], Size_hex,Size_x, Size_y),
    
    insects:find_insect_high_level([X_axial,Y_axial], [Type, Id, Player_id, Hex_select, Level]),
    
    Player_id ==Current_Player_id, %para no tocar sobre una ficha del contrario
   

    axial_to_pixel([X_axial,Y_axial], [Pixel_x,Pixel_y],Size_hex,Size_x,Size_y),
    piece_selected(Type, Id, Player_id, [Pixel_x,Pixel_y], Level),
    

    hive(L_hive),
    possible_moves(Type, Type, Id, Player_id , [X_axial,Y_axial], Level, Moves, L_hive),
    
    length(Moves, Length_moves),
    Length_moves > 0,

    draw_hexagon_pixel_empty(W, [Pixel_x,Pixel_y], Size_hex, colour(red)),

    draw_possible_movements(W, Moves, Size_hex,Size_x,Size_y, colour(blue)),

   
    !, % este corte es para q no entre a las otras partes
    retract(move_state(_)), % ya se movio la ficha o no se pudo mover
    assert(move_state(Type)), % aqui en move debe ir la ficha q es

    retract(bool_selected(false)),
    assert(bool_selected(true)),

    retract(piece_selected(_,_,_,_,_)),
    current_player(Current),
    
    assert(piece_selected(Type,Id,Current,Hex_select,Level)).


click(W, Position):-
        (
            % player 1 init or add
            select_init(W, Position, p1, [40,120,200,280,360,440,520,600], [40,70,100]);
            select_add(W, Position, p1, [40,120,200,280,360,440,520,600], [40,70,100]),!
        );
        (
            % player 2 init or add
            select_init(W, Position, p2, [40,120,200,280,360,440,520,600], [700,730,760]);
            select_add(W, Position, p2, [40,120,200,280,360,440,520,600], [700,730,760]),!
        );
        (
            current_player(Current_Player_id),
            select_move(W, Position, Current_Player_id),!
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
        
        
        dimensions(Size_hex,Size_x,Size_y), %duda aqui si volver a ponerlo
        

        get(Position, x, Click_X),
        get(Position, y, Click_Y),

        pixel_to_axial([Click_X,Click_Y], [X_axial,Y_axial], Size_hex,Size_x, Size_y),
        
        piece_selected(Type, Id, Player_id, Hex_ini, Level), % selected in hand
        
        
        hive(L_hive).

    
    

unclick(W, Player_id, Msg):-
    
    draw_game(W),

    write_message(W,Msg),

    

    retract(bool_selected(_)),
    assert(bool_selected(false)),

    retract(piece_selected(_,_,_,_,_)),
    assert(piece_selected(_,_,_,_,_)).
    
    


make_move_state(W, Position, Type_move):-
    (
        write_ln("primero"),
        make_move_state_part1(W, Position, Type_move,Size_hex,Size_x,Size_y,L_hive,Type, Id, Player_id, Hex_ini, 
                            Level,[X_axial,Y_axial]),

        
        move_insect(Type_move, Type, Id, Player_id, Hex_ini, Level, [X_axial,Y_axial],L_hive, Msg),
        Msg == "",
        
        color_player(Player_id,Col),
        

        axial_to_pixel([X_axial,Y_axial], [X_pixel,Y_pixel], Size_hex, Size_x, Size_y),
        
        
    
        move_piece(W, [X_axial,Y_axial], Type, Col), 
        
        retract(init_player(Player_id, _)),
        assert(init_player(Player_id, false)),

        change_player_turn(Type, Player_id, Hex_ini, Level, Hex_fin, L_hive), % ver cuando esto no se cumple
        
        
        change_player(Player_id, Other_player),
        retract(current_player(_)),
        assert(current_player(Other_player)),
       

        unclick(W, Player_id,Msg),
        !
    ),!;       

    (
        
        make_move_state_part1(W, Position, Type_move,Size_hex,Size_x,Size_y,L_hive,Type, Id, Player_id, Hex_ini, 
            Level,[X_axial,Y_axial]),

        
        not(move_insect(Type_move, Type, Id, Player_id, Hex_ini, Level, [X_axial,Y_axial],L_hive, Msg)),

        
        writeln("Invalid move"),

        unclick(W, Player_id, "Invalid move")
        
        
    ),!;
    (
        make_move_state_part1(W, Position, Type_move,Size_hex,Size_x,Size_y,L_hive,Type, Id, Player_id, Hex_ini, 
            Level,[X_axial,Y_axial]),
        move_insect(Type_move, Type, Id, Player_id, Hex_ini, Level, [X_axial,Y_axial],L_hive, Msg),
        not(Msg == ""),
        
        writeln(Msg),
        
        unclick(W, Player_id, Msg),!
    ).
                

% este predicado recibe las coordenadas en pixeles
move_piece(W,Hex_end,Type, Color):-
    (
        dimensions(Size_hex,Size_x,Size_y),
        
        piece_selected(Type,Id,Player_id,Hex_select,Level),

        pieces(Player_id, Type, Initial_pieces, In_hive_pieces),

        

        member(Hex_select, Initial_pieces),

        
        
        delete(Initial_pieces, Hex_select, Initial_pieces_1),


        append(In_hive_pieces, [Hex_end], In_hive_pieces_1),

        retract(pieces(Player_id, Type, Initial_pieces, In_hive_pieces)),
        assert(pieces(Player_id, Type, Initial_pieces_1, In_hive_pieces_1))
    );
    (
        dimensions(Size_hex,Size_x,Size_y),
        
        piece_selected(Type,Id,Player_id,Hex_select,Level),

        pieces(Player_id, Type, Initial_pieces, In_hive_pieces),

        not(member(Hex_select, Initial_pieces)),
        
        member(Hex_select, In_hive_pieces),
        delete(In_hive_pieces, Hex_select, In_hive_pieces_1),
        append(In_hive_pieces_1, [Hex_end], In_hive_pieces_2),!,

        retract(pieces(Player_id, Type, Initial_pieces, In_hive_pieces)),
        assert(pieces(Player_id, Type, Initial_pieces, In_hive_pieces_2))
    ).




