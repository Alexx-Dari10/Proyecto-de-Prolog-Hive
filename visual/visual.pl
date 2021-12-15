:-use_module(library(pce)).
:- pce_image_directory('./images').

:-consult('../hexagon'), import('../hexagon').
:-consult('../insects'), import('../insects').

:-consult('utils_visual'), import('utils_visual').

:-consult('draw_visual'), import('draw_visual').

:-dynamic bool_selected/1, piece_selected/5,pieces/4, 
        init_player/2, move_state/1, current_player/1, arrowLeft/3, dimensions/3, dimensions_hive/4,
        dimensions_static/3, arrow_left/3, game_over/1, tie/1, win/1, message_end_game/1.


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




main:-

    insects:start_game(),

    assert(game_over(false)),
    assert(message_end_game('')),
    assert(dimensions(40,1000,800)),

    dimensions(Size_hex, Size_x, Size_y),
    
    assert(dimensions_static(Size_hex, Size_x, Size_y)),

    dimensions_static(Size_hex_static, Size_x_static, Size_y_static),

    assert(bool_selected(false)),
    assert(piece_selected(_,_,_,_,_)),
    

    assert(init_player(p1,true)),
    assert(init_player(p2,true)),
    assert(current_player(p1)),
    

    assert(move_state(init)),
    
    Static_Y1 is Size_y_static - 100,
    Static_Y2 is Size_y_static - 70,
    Static_Y3 is Size_y_static - 40,

    assert(arrow_left(p1, [700,40], arrow_blanca)),
    assert(arrow_left(p2, [700,Static_Y1],arrow_negra)),
    %Variables
    assert(pieces(p1, hormiga    ,[[40,40],  [40,70],  [40, 100]], [])),
    assert(pieces(p1, escarabajo ,[[120,40], [120,70]           ], [])),
    assert(pieces(p1, saltamonte ,[[200,40], [200,70], [200,100]], [])),
    assert(pieces(p1, abejaReina ,[[280,40]                     ], [])),
    assert(pieces(p1, aranha     ,[[360,40], [360,70]           ], [])),
    assert(pieces(p1, mariquita  ,[[440,40]                     ], [])),
    assert(pieces(p1, mosquito  ,[[520,40]                     ], [])),
    assert(pieces(p1, bichoBola  ,[[600,40]                     ], [])),

    assert(pieces(p2, hormiga    ,[[40,Static_Y1 ], [40,Static_Y2 ],[40, Static_Y3]], [])),
    assert(pieces(p2, escarabajo ,[[120,Static_Y1], [120,Static_Y2]          ], [])),
    assert(pieces(p2, saltamonte ,[[200,Static_Y1], [200,Static_Y2],[200,Static_Y3]], [])),
    assert(pieces(p2, abejaReina ,[[280,Static_Y1]                     ], [])),
    assert(pieces(p2, aranha     ,[[360,Static_Y1], [360,Static_Y2]          ], [])),
    assert(pieces(p2, mariquita  ,[[440,Static_Y1]                     ], [])),
    assert(pieces(p2, mosquito   ,[[520,Static_Y1]                     ], [])),
    assert(pieces(p2, bichoBola  ,[[600,Static_Y1]                     ], [])),


   

    assert(dimensions_hive(0, Size_x, 100, Static_Y1)),

    new(W, window("window", size(Size_x, Size_y))),
    send(W, background, colour(@default, 52000, 40000, 8000)),
    
    

    event_click(W),
    new(Lbl2, label),
    insects:start_insects(p1, [40,120,200,280,360,440,520,600], [40,70,100]),
    insects:start_insects(p2, [40,120,200,280,360,440,520,600], [Static_Y1,Static_Y2,Static_Y3]),
         
    draw_game(W),
   
    send(W, open).


restart(W):-
    
    
    clear_buttons(W),
    clear_game(W),

    insects:restart_game(),

    retractall(win(_)),
    retractall(tie(_)),
    retractall(message_end_game(_)),
    retractall(game_over(_)),
    retractall(dimensions(_,_,_)),
    retractall(dimensions_static(_,_,_)),
    retractall(bool_selected(_)),
    retractall(piece_selected(_,_,_,_,_)),
    retractall(init_player(_,_)),
    retractall(current_player(_)),
    retractall(move_state(_)),
    retractall(arrow_left(_,_,_)),
    retractall(pieces(_,_,_,_)),
    retractall(dimensions_hive(_,_,_,_)),

    insects:start_game(),

    assert(game_over(false)),
    assert(message_end_game('')),
    assert(dimensions(40,1000,800)),

    dimensions(Size_hex, Size_x, Size_y),
    
    assert(dimensions_static(Size_hex, Size_x, Size_y)),

    dimensions_static(Size_hex_static, Size_x_static, Size_y_static),

    assert(bool_selected(false)),
    assert(piece_selected(_,_,_,_,_)),
    

    assert(init_player(p1,true)),
    assert(init_player(p2,true)),
    assert(current_player(p1)),
    

    assert(move_state(init)),
    
    Static_Y1 is Size_y_static - 100,
    Static_Y2 is Size_y_static - 70,
    Static_Y3 is Size_y_static - 40,

    assert(arrow_left(p1, [700,40], arrow_blanca)),
    assert(arrow_left(p2, [700,Static_Y1],arrow_negra)),
    %Variables
    assert(pieces(p1, hormiga    ,[[40,40],  [40,70],  [40, 100]], [])),
    assert(pieces(p1, escarabajo ,[[120,40], [120,70]           ], [])),
    assert(pieces(p1, saltamonte ,[[200,40], [200,70], [200,100]], [])),
    assert(pieces(p1, abejaReina ,[[280,40]                     ], [])),
    assert(pieces(p1, aranha     ,[[360,40], [360,70]           ], [])),
    assert(pieces(p1, mariquita  ,[[440,40]                     ], [])),
    assert(pieces(p1, mosquito  ,[[520,40]                     ], [])),
    assert(pieces(p1, bichoBola  ,[[600,40]                     ], [])),

    assert(pieces(p2, hormiga    ,[[40,Static_Y1 ], [40,Static_Y2 ],[40, Static_Y3]], [])),
    assert(pieces(p2, escarabajo ,[[120,Static_Y1], [120,Static_Y2]          ], [])),
    assert(pieces(p2, saltamonte ,[[200,Static_Y1], [200,Static_Y2],[200,Static_Y3]], [])),
    assert(pieces(p2, abejaReina ,[[280,Static_Y1]                     ], [])),
    assert(pieces(p2, aranha     ,[[360,Static_Y1], [360,Static_Y2]          ], [])),
    assert(pieces(p2, mariquita  ,[[440,Static_Y1]                     ], [])),
    assert(pieces(p2, mosquito   ,[[520,Static_Y1]                     ], [])),
    assert(pieces(p2, bichoBola  ,[[600,Static_Y1]                     ], [])),


   

    assert(dimensions_hive(0, Size_x, 100, Static_Y1)),

    
    new(Lbl2, label),
    insects:start_insects(p1, [40,120,200,280,360,440,520,600], [40,70,100]),
    insects:start_insects(p2, [40,120,200,280,360,440,520,600], [Static_Y1,Static_Y2,Static_Y3]),
         
    draw_game(W).

   

check_end_game(W):-
    (
        insects:end_game(p1),
        assert(win(p2))
        
    );
    (
        insects:end_game(p2),
        assert(win(p1))
        
    );
    ( 
        insects:tie_game(p1, p2),
        assert(tie(true))
    ).


draw_board(W):-
        clear_game(W), 

        
        dimensions(Size_hex,Size_x,Size_y),

        dimensions_static(Size_hex_static, Size_x_static, Size_y_static),
        
        %pinta las piezas del jugador 1
        findall(_, 
                (
                    pieces(p1, Type, Initials, In_hive),
                    (
                        (
                            draw_pieces(W, p1,Size_hex,Size_x_static, Size_y_static, Type, Initials, false),
                            draw_pieces(W, p1,Size_hex,Size_x, Size_y,Type, In_hive, true)
                        ); 
                        (
                            draw_pieces(W, p1,Size_hex,Size_x, Size_y,Type, In_hive, true),
                            draw_pieces(W, p1,Size_hex,Size_x_static, Size_y_static, Type, Initials, false)
                        )
                    )
                ), _),
        
        %pinta las piezas del jugador 2

        findall(_, 
            (
                pieces(p2, Type, Initials2, In_hive2),
                (
                    (
                        draw_pieces(W, p2,Size_hex,Size_x_static, Size_y_static, Type, Initials2, false),
                        draw_pieces(W, p2,Size_hex,Size_x, Size_y,Type, In_hive2, true)
                    ); 
                    (
                        draw_pieces(W, p2,Size_hex,Size_x, Size_y,Type, In_hive2, true),
                        draw_pieces(W, p2,Size_hex,Size_x_static, Size_y_static, Type, Initials2, false)
                    )
                )
            ), _),

        
        current_player(Current),
        arrow_left(Current,Hex_arrow, Arrow_name),
        
        draw_image_hexagon(W, Arrow_name,Hex_arrow),
        
        dimensions_hive(Dim_hiveX1, Dim_hiveX2, Dim_hiveY1, Dim_hiveY2),

        draw_visual:draw_line(W, [Dim_hiveX1,Dim_hiveX2,Dim_hiveY1 + Size_hex], white),

        draw_visual:draw_line(W, [Dim_hiveX1,Dim_hiveX2,Dim_hiveY2 - Size_hex], black),
        
        draw_buttons(W),
        message_end_game(Message),
        write_message(W,Message,[400, 400]).


draw_game(W):-
    (
        not(check_end_game(W)),
        draw_board(W)
    );
    (
        check_end_game(W),
        win(p1),
        
        retract(message_end_game(_)),
        assert(message_end_game("Winner Player 1")),
        

        draw_board(W),
       
        
        retract(game_over(false)),
        assert(game_over(true))
        
        
        
    ),!;
    (
        check_end_game(W),
        win(p2),
        
        retract(message_end_game(_)),
        assert(message_end_game("Winner Player 2")),
        
        draw_board(W),

        retract(game_over(false)),
        assert(game_over(true))
        
       
        
    ),!;
    (
        check_end_game(W),
        tie(true),
        
        retract(message_end_game(_)),
        assert(message_end_game("Tie game")),
        
        draw_board(W),
       
        retract(game_over(false)),
        assert(game_over(true))
        
        
       
    ).



clear_game(W):- 
    send(W, clear).


clear_buttons(W):-
    send(@buttonup, free),
    send(@buttondown, free ),
    send(@buttonleft, free),
    send(@buttonright, free),
    send(@buttonrestart, free),
    send(@buttonIA_1, free),
    send(@buttonIA_2, free).




draw_buttons(W):-

    new(@buttonup, button("up", message(@prolog, move_dir,W,1))),
    new(@buttondown, button("down", message(@prolog, move_dir,W,2))),
    new(@buttonleft, button("left", message(@prolog, move_dir,W,3))),
    new(@buttonright, button("right", message(@prolog, move_dir,W,4))),

    new(@buttonrestart, button("restart", message(@prolog, restart, W))),


    new(@buttonIA_1, button("IA 1", message(@prolog, ia, W, p1))),
    new(@buttonIA_2, button("IA 2", message(@prolog, ia, W, p2))),

    dimensions_static(Size_hex, Size_x, Size_y),

    send(W,display, @buttonup, point(Size_x-100,Size_y - 50)),
    send(W,display, @buttondown, point(Size_x-100,Size_y - 30)),
    send(W,display, @buttonleft, point(Size_x-150,Size_y - 40)),
    send(W,display, @buttonright, point(Size_x - 50,Size_y - 40)),
    send(W,display, @buttonrestart, point(Size_x-110,100)),
    send(W,display, @buttonIA_1, point(Size_x-200, 30)),
    send(W,display, @buttonIA_2, point(Size_x-200,Size_y-135)).





event_click(W):-
    send(W, recogniser, click_gesture(
        left, '', single, 
        message(@prolog, click,W, @event?position))).



move_dir(W, Dir):-
    (
        Dir == 1,
        retract(dimensions(Size_hex,Size_x,Size_y)),
        assert(dimensions(Size_hex,Size_x, Size_y - 40)),
        
        clear_buttons(W),
        draw_board(W)
    );
    (
        Dir == 2,
        retract(dimensions(Size_hex,Size_x,Size_y)),
        assert(dimensions(Size_hex,Size_x , Size_y + 40 )),
        
        clear_buttons(W),
        draw_board(W)
    );
    (
        Dir == 3,
        retract(dimensions(Size_hex,Size_x,Size_y)),
        assert(dimensions(Size_hex,Size_x - 40, Size_y)),
        
        clear_buttons(W),
        draw_board(W)
    );
    (
        Dir == 4,
        retract(dimensions(Size_hex,Size_x,Size_y)),
        assert(dimensions(Size_hex,Size_x + 40, Size_y)),
        
        clear_buttons(W),
        draw_board(W)
    ).


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
        writeln(Type_move),

        possible_moves(Type_move, Type, Id, Player_id , Hex_select, -1, Moves, L_hive),

       
        length(Moves, Length_moves),
        writeln(Length_moves),
        Length_moves > 0,
        
        

        draw_possible_movements(W, Moves, Size_hex,Size_x,Size_y, colour(blue)),

        draw_hexagon_pixel_empty(W, Hex_select, Size_hex, colour(red)),

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
    
    dimensions_hive(Dim_hiveX1, Dim_hiveX2, Dim_hiveY1, Dim_hiveY2),

    check_position_in_hive([Click_X,Click_Y], Size_hex, [Dim_hiveX1,Dim_hiveX2], [Dim_hiveY1,Dim_hiveY2]),
    
    
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

    

    draw_possible_movements(W, Moves, Size_hex,Size_x,Size_y, colour(blue)),

    draw_hexagon_pixel_empty(W, [Pixel_x,Pixel_y], Size_hex, colour(red)),
   
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
            game_over(false),
            % player 1 init or add
            select_init(W, Position, p1, [40,120,200,280,360,440,520,600], [40,70,100]);
            select_add(W, Position, p1, [40,120,200,280,360,440,520,600], [40,70,100]),!
        );
        (
            % player 2 init or add
            (
                game_over(false),
                dimensions_static(_,_,Size_y),
                Static_Y1 is Size_y - 100,
                Static_Y2 is Size_y - 70,
                Static_Y3 is Size_y - 40,
                
                select_init(W, Position, p2, [40,120,200,280,360,440,520,600], [Static_Y1,Static_Y2,Static_Y3])
            );
            (
                game_over(false),
                dimensions_static(_,_,Size_y),
                Static_Y1 is Size_y - 100,
                Static_Y2 is Size_y - 70,
                Static_Y3 is Size_y - 40,
                
                select_add(W, Position, p2, [40,120,200,280,360,440,520,600], [Static_Y1,Static_Y2,Static_Y3]),!
            )
            
        );
        (
            game_over(false),
            current_player(Current_Player_id),
            
            select_move(W, Position, Current_Player_id),!
        );

        (
            game_over(false),
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
    
        
        bool_selected(true),
        writeln("4to"),
        write("Esto es "), writeln(Type_move),
        move_state(Type_move),
        
        
        dimensions(Size_hex,Size_x,Size_y), %duda aqui si volver a ponerlo
        

        get(Position, x, Click_X),
        get(Position, y, Click_Y),

        pixel_to_axial([Click_X,Click_Y], [X_axial,Y_axial], Size_hex,Size_x, Size_y),
        
        piece_selected(Type, Id, Player_id, Hex_ini, Level), % selected in hand
        
        
        hive(L_hive).

    
    

unclick(W, Player_id, Msg):-
    
    clear_buttons(W),
    draw_game(W),


    dimensions_static(Size_hex_static, Size_x_static, Size_y_static),
    write_message(W,Msg,[Size_x_static, Size_y_static]),

    

    retract(bool_selected(_)),
    assert(bool_selected(false)),

    retract(piece_selected(_,_,_,_,_)),
    assert(piece_selected(_,_,_,_,_)).
    
    


make_move_state(W, Position, Type_move):-
    (
        writeln("primero"),
        make_move_state_part1(W, Position, Type_move,Size_hex,Size_x,Size_y,L_hive,Type, Id, Player_id, Hex_ini, 
                            Level,[X_axial,Y_axial]),

        
        move_insect(Type_move, Type, Id, Player_id, Hex_ini, Level, [X_axial,Y_axial],L_hive, Msg,
            [Type2, Id2, Player_id2, Hex2, Level2, Hex_select2]),
        
        write("mensaje"),writeln(Msg),
        Msg == "",

        writeln([Type2, Id2, Player_id2, Hex2, Level2]),
        
        color_player(Player_id,Col),
        
        
        
        retract(piece_selected(_,_,_,_,_)),
        assert(piece_selected(Type2,Id2,Player_id2,Hex_select2,Level2)),
        

        move_piece(W, Hex2, Type2, Col), 
        
        retract(init_player(Player_id, _)),
        assert(init_player(Player_id, false)),

        change_player_turn(Type, Player_id, Hex_ini, Level, Hex_fin, L_hive), % ver cuando esto no se cumple
        
        
        change_player(Player_id, Other_player),
        retract(current_player(_)),
        assert(current_player(Other_player)),
       

        unclick(W, Player_id,Msg),
        !
    );       
    
    (
        
        make_move_state_part1(W, Position, Type_move,Size_hex,Size_x,Size_y,L_hive,Type, Id, Player_id, Hex_ini, 
            Level,[X_axial,Y_axial]),

        
        not(move_insect(Type_move, Type, Id, Player_id, Hex_ini, Level, [X_axial,Y_axial],L_hive, Msg,
            [Type2, Id2, Player_id2, Hex2, Level2, Hex_select2])),

        
        writeln("Invalid move"),

        unclick(W, Player_id, "Invalid move")
        
        
    ),!;
    (
        make_move_state_part1(W, Position, Type_move,Size_hex,Size_x,Size_y,L_hive,Type, Id, Player_id, Hex_ini, 
            Level,[X_axial,Y_axial]),
        
        write_ln([Type_move, Type, Id, Player_id, Hex_ini, Level, [X_axial,Y_axial]]),
        move_insect(Type_move, Type, Id, Player_id, Hex_ini, Level, [X_axial,Y_axial],L_hive, Msg,
            [Type2, Id2, Player_id2, Hex2, Level2, Hex_select2]),
        writeln("helllooooooo"),
        not(Msg == ""),
        
        writeln(Msg),
        
        unclick(W, Player_id, Msg),!
    ).
                
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



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% IA %%

get_all_pieces_player(W, Player_id, Pieces_player):-
    findall([Type, Initials, In_hive], pieces(Player_id, Type, Initials, In_hive), Pieces_player).

get_possible_moves_initials(W,Val, Player_id, Pieces_player, All_moves):-
    hive(L_hive),

    findall([Val,Type, Id, Player_id,Hex, Level, Moves], 
        (
            member([Type, Initials, In_hive], Pieces_player),

            member(Hex, Initials),

            insect(Type, Id, Player_id,Hex, Level),

            possible_moves(Val, Type, Id, Player_id , Hex, Level, Moves, L_hive)
        ), 
        All_moves).

get_possible_moves_in_hive(W, Val, Player_id, Pieces_player, All_moves):-
    hive(L_hive),

    findall([Val,Type, Id, Player_id,Hex, Level, Moves], 
        (
            member([Type, Initials, In_hive], Pieces_player),

            member(Hex, In_hive),

            insect(Type, Id, Player_id,Hex, Level),

            Val = Type,
            possible_moves(Type, Type, Id, Player_id , Hex, Level, Moves, L_hive)
        ), 
        All_moves).

axial_distance([R1,Q1], [R2,Q2], Dist):-
    R_abs = abs(R1-R2),
    Q_abs = abs(Q1-Q2),
    S_abs = abs(-R1 - Q1 + R2 + Q2),
    Dist = (R_abs + Q_abs + S_abs)/2.



ia(W, Player_id):-
    (
        get_all_pieces_player(W, Player_id, Pieces_player),

        init_player(Player_id,true),
        get_possible_moves_initials(W, init, Player_id, Pieces_player, All_moves_init),

        %heuristica para seleccionar el valor
        %despues poner aqui un random
        
        member([init, escarabajo, 2, Player_id, Hex_select, Level, Moves], All_moves_init),

        length(Moves, Length_mov),
        
        
        New_Length_mov is Length_mov + 1,
        writeln(New_Length_mov),
        random(0,  New_Length_mov, Rnd),
        writeln(Rnd),
        insects:utils:element_at(Moves, Rnd , Hex_move),

        retract(piece_selected(_,_,_,_,_)),
        assert(piece_selected(escarabajo,2,Player_id,Hex_select,Level)),

        hive(L_hive),

        move_insect(init, escarabajo, 2, Player_id, Hex_select, Level, Hex_move,L_hive, Msg,
            [Type2, Id2, Player_id2, Hex2, Level2, Hex_select2]),
        
        Msg == "",

        writeln([Type2, Id2, Player_id2, Hex2, Level2]),
        
        color_player(Player_id,Col),
        
        
        
        retract(piece_selected(_,_,_,_,_)),
        assert(piece_selected(Type2,Id2,Player_id2,Hex_select2,Level2)),
        

        move_piece(W, Hex2, Type2, Col), 
        
        retract(init_player(Player_id, _)),
        assert(init_player(Player_id, false)),

        change_player_turn(Type2, Player_id, Hex_select2, Level2, Hex2, L_hive), % ver cuando esto no se cumple
        
        
        change_player(Player_id, Other_player),
        retract(current_player(_)),
        assert(current_player(Other_player)),
       

        unclick(W, Player_id,Msg),

        
        writeln(Hex_move)

    );
    (
        get_all_pieces_player(W, Player_id, Pieces_player),

        init_player(Player_id,false),
        get_possible_moves_initials(W, add, Player_id, Pieces_player, All_moves_add),

        get_possible_moves_in_hive(W, Val, Player_id, Pieces_player, All_moves_in_hive),


        append(All_moves_add, All_moves_in_hive, All_moves),

        length(All_moves, Length_all_mov),
        
        New_Length_all_mov is Length_all_mov +1,
        random(0, New_Length_all_mov, Rnd_all_mov),
        insects:utils:element_at(All_moves, Rnd_all_mov , [Val ,Type, Id, Player_id, Hex_select, Level, Moves]), % aqui devolver el ultimo id
        %heuristica para seleccionar el valor
        %despues poner aqui un random
       
        length(Moves, Length_mov),
        
        New_Length_mov is Length_mov + 1,
        random(0, New_Length_mov, Rnd),
        insects:utils:element_at(Moves, Rnd , Hex_move),

        retract(piece_selected(_,_,_,_,_)),
        assert(piece_selected(Type,Id,Player_id,Hex_select,Level)),

        hive(L_hive),

        move_insect(Val, Type, Id, Player_id, Hex_select, Level, Hex_move,L_hive, Msg,
            [Type2, Id2, Player_id2, Hex2, Level2, Hex_select2]),
        
        writeln(Msg),

        Msg == "",

        writeln([Type2, Id2, Player_id2, Hex2, Level2]),
        
        color_player(Player_id,Col),
        
        
        
        retract(piece_selected(_,_,_,_,_)),
        assert(piece_selected(Type2,Id2,Player_id2,Hex_select2,Level2)),
        

        move_piece(W, Hex2, Type2, Col), 
        
        retract(init_player(Player_id, _)),
        assert(init_player(Player_id, false)),

        change_player_turn(Type2, Player_id, Hex_select2, Level2, Hex2, L_hive), % ver cuando esto no se cumple
        
        
        change_player(Player_id, Other_player),
        retract(current_player(_)),
        assert(current_player(Other_player)),
       

        unclick(W, Player_id,Msg),

        
        writeln(Hex_move)


    )
   
    .
    
