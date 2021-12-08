:-use_module(library(pce)).
:- pce_image_directory('./images').

:-consult('../hexagon'), import('../hexagon').
:-consult('../insects'), import('../insects').

:-consult('draw_visual'), import('draw_visual').

:-dynamic size_hex/1, size_x/1, size_y/1.
:-dynamic bool_selected/1, piece_selected/5.


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
    assert(bool_selected(false)),
    assert(piece_selected(_,_,_,_,_)),
    assert(size_hex(35)),
    assert(size_x(800)),
    assert(size_y(800)),
    
    size_hex(Size_hex),
    size_x(Size_x),
    size_y(Size_y),

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
        message(@prolog, select_piece,W, @event?position))).



select_piece(W, Position):-
        (
            writeln("1ero"),
            bool_selected(false),
            %Player 1
            Size_hex is 35,
            Size_x is 800, 
            Size_y is 800,
            get(Position, x, Click_X),
            get(Position, y, Click_Y),
            check_positions_in_hand([Click_X,Click_Y], Size_hex, 
                                    [40,120,200,280,360,440,520,600], [40,100], Type),
            
            
            select_in_hand(Type, p1, Hex_select, Id, [40,120,200,280,360,440,520,600], [40,70,100]),
            
            
            draw_hexagon_pixel_empty(W, Hex_select, Size_hex, colour(red)),



            !, % este corte es para q no entre a las otras partes

            retract(bool_selected(false)),
            assert(bool_selected(true)),

            retract(piece_selected(_,_,_,_,_)),
            assert(piece_selected(Type,Id,p1,Hex_select,-1)),!
            
        );
        (
            writeln("2do"),
            bool_selected(false),
            %Player 2

            Size_hex is 35,
            Size_x is 800, 
            Size_y is 800,
            get(Position, x, Click_X),
            get(Position, y, Click_Y),
            check_positions_in_hand([Click_X,Click_Y], Size_hex, 
                                    [40,120,200,280,360,440,520,600], [700,760], Type),
            

            select_in_hand(Type, p2, Hex_select, Id, [40,120,200,280,360,440,520,600], [700,730,760]),

            draw_hexagon_pixel_empty(W, Hex_select, Size_hex, colour(red)),
            
            !, % este corte es para q no entre a las otras partes

            retract(bool_selected(false)),
            assert(bool_selected(true)),

            retract(piece_selected(_,_,_,_,_)),
            assert(piece_selected(Type,Id,p2,Hex_select,-1))
            
        );
        (
            bool_selected(false),
            Size_hex is 35,
            Size_x is 800, 
            Size_y is 800,
            get(Position, x, Click_X),
            get(Position, y, Click_Y),
            
            check_position_in_hive([Click_X,Click_Y], Size_hex, [40,600], [100,700]),
            
            %arreglar esto aqui pa q me de bien los vlaores de piece-selected
            draw_hexagon_pixel_axial(W, [Click_X, Click_Y], Size_hex,Size_x, Size_y, aranha, colour(white)),
            


            !, % este corte es para q no entre a las otras partes
            retract(bool_selected(false)),
            assert(bool_selected(true)),

            retract(piece_selected(_,_,_,_,_)),
            assert(piece_selected(Type,Id,p1,Hex_select,Level))
        );

        (
            writeln("4to"),
            bool_selected(true),
            
            % ver xq se traba pa moverse
            get(Position, x, Click_X),
            get(Position, y, Click_Y),

            %buscar insecto para q esta marcado
            move_piece(W, [Click_X,Click_Y], abejaReina, black),

            retract(bool_selected(true)),
            assert(bool_selected(false)),

            retract(piece_selected(_,_,_,_,_)),
            assert(piece_selected(_,_,_,_,_)),!
            
        ).
   
erase_piece(W,Hex_select, Size_hex,Color):-
    
    (
        piece_selected(Type,Id,Player_id,Hex_select,Level),
        Id > 1 ,
        draw_hexagon_pixel_filling(W,Hex_select, Size_hex, Color),
        writeln("Id"),
        findall(Hex, (insect(Type,_,Player_id,Hex,-1),
                draw_hexagon_pixel(W, Hex, Size_hex, Type, Color)), 
                List_hex)
    );
    (
        
        piece_selected(Type,Id,Player_id,Hex_select,Level),
        Id =< 1, 
        draw_hexagon_pixel_filling(W,Hex_select, Size_hex, Color),
        writeln("No hay mas insectos de este tipo en mano")
    ).

    


move_piece(W,Hex_end,Type, Color):-
    
    size_hex(Size_hex),
    size_x(Size_x),
    size_y(Size_y),
    
    piece_selected(Type,Id,Player_id,Hex_select,Level),

    %erase_piece(W,Hex_select,Size_hex, colour(@default, 52000, 40000, 8000)),

   
    pixel_to_axial(Hex_end, [R, Q], Size_hex,Size_x, Size_y),
    draw_hexagon_axial(W, [R,Q], Size_hex,Size_x, Size_y, Type, Color, true).



