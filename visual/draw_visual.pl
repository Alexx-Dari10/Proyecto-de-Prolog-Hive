:-use_module(library(pce)).
:- pce_image_directory('./images').


module(draw_visual,[
        draw_image_hexagon/4, draw_hexagon_axial/8, draw_hexagon_pixel/5, 
        draw_hexagon_pixel_axial/5, draw_possible_movements/4, draw_pieces/4,
        draw_hexagon_pixel_empty/4, draw_hexagon_pixel_filling/4, color_player/2
    ]).

:-consult('../hexagon'), import('../hexagon').
:-consult('../insects'), import('../insects').
:-consult('../utils'), import('../utils').
:-consult('utils_visual'), import('utils_visual').


color_player(p1, colour(white)).
color_player(p2, colour(black)).




draw_image_hexagon(Window,Image,[Point_x, Point_y]):-
    X_center is Point_x - 23,
    Y_center is Point_y -23,
    new_image(Window,_, Image, point(X_center, Y_center)).



draw_background_hex(W, [Corner_1_x, Corner_1_y],[Corner_2_x, Corner_2_y],[Corner_3_x, Corner_3_y],[Corner_4_x, Corner_4_y],
            [Corner_5_x, Corner_5_y],[Corner_6_x, Corner_6_y], Color):-
        
            new(Pa, path),
            send(Pa, append, point(Corner_1_x, Corner_1_y)),
            send(Pa, append, point(Corner_2_x, Corner_2_y)),
            send(Pa, append, point(Corner_3_x, Corner_3_y)),
            send(Pa, append, point(Corner_4_x, Corner_4_y)),
            send(Pa, append, point(Corner_5_x, Corner_5_y)),
            send(Pa, append, point(Corner_6_x, Corner_6_y)),

            send(Pa, colour, Color),
            send(Pa, closed, @on),
            send(W, display, Pa),
            send(Pa, fill_pattern, Color).



draw_foreground_hex(W, [Corner_1_x, Corner_1_y],[Corner_2_x, Corner_2_y],[Corner_3_x, Corner_3_y],[Corner_4_x, Corner_4_y],
            [Corner_5_x, Corner_5_y],[Corner_6_x, Corner_6_y], Color):-
            
            
            new(Pa1, path),
            send(Pa1, append, point(Corner_1_x, Corner_1_y)),
            send(Pa1, append, point(Corner_2_x, Corner_2_y)),
            send(Pa1, append, point(Corner_3_x, Corner_3_y)),
            send(Pa1, append, point(Corner_4_x, Corner_4_y)),
            send(Pa1, append, point(Corner_5_x, Corner_5_y)),
            send(Pa1, append, point(Corner_6_x, Corner_6_y)),
           
            send(Pa1, colour, Color),
            send(Pa1, closed, @on),
            send(W, display, Pa1).



draw_hexagon_axial(W, [R, Q], Size_hex, Size_x, Size_y, Image, Color, Empty):-
    (
        axial_to_pixel([R,Q], [Point_x,Point_y], Size_hex, Size_x, Size_y),


        flat_hex_corner([Point_x, Point_y], Size_hex, 1, [Corner_1_x, Corner_1_y]),
        flat_hex_corner([Point_x, Point_y], Size_hex, 2, [Corner_2_x, Corner_2_y]),
        flat_hex_corner([Point_x, Point_y], Size_hex, 3, [Corner_3_x, Corner_3_y]),
        flat_hex_corner([Point_x, Point_y], Size_hex, 4, [Corner_4_x, Corner_4_y]),
        flat_hex_corner([Point_x, Point_y], Size_hex, 5, [Corner_5_x, Corner_5_y]),
        flat_hex_corner([Point_x, Point_y], Size_hex, 6, [Corner_6_x, Corner_6_y]),

        Empty,

        draw_background_hex(W, [Corner_1_x, Corner_1_y],[Corner_2_x, Corner_2_y],[Corner_3_x, Corner_3_y],
        [Corner_4_x, Corner_4_y],[Corner_5_x, Corner_5_y],[Corner_6_x, Corner_6_y], Color),

        draw_image_hexagon(W, Image, [Point_x, Point_y]),!
    );
    (
        axial_to_pixel([R,Q], [Point_x,Point_y], Size_hex,Size_x, Size_y),

        flat_hex_corner([Point_x, Point_y], Size_hex, 1, [Corner_1_x, Corner_1_y]),
        flat_hex_corner([Point_x, Point_y], Size_hex, 2, [Corner_2_x, Corner_2_y]),
        flat_hex_corner([Point_x, Point_y], Size_hex, 3, [Corner_3_x, Corner_3_y]),
        flat_hex_corner([Point_x, Point_y], Size_hex, 4, [Corner_4_x, Corner_4_y]),
        flat_hex_corner([Point_x, Point_y], Size_hex, 5, [Corner_5_x, Corner_5_y]),
        flat_hex_corner([Point_x, Point_y], Size_hex, 6, [Corner_6_x, Corner_6_y]),

        not(Empty),
        draw_foreground_hex(W, [Corner_1_x, Corner_1_y],[Corner_2_x, Corner_2_y],[Corner_3_x, Corner_3_y],
        [Corner_4_x, Corner_4_y],[Corner_5_x, Corner_5_y],[Corner_6_x, Corner_6_y], Color)
    ).



draw_hexagon_pixel(W, [Point_x, Point_y], Size_hex, Image, Color):-
    
    flat_hex_corner([Point_x, Point_y], Size_hex, 1, [Corner_1_x, Corner_1_y]),
    flat_hex_corner([Point_x, Point_y], Size_hex, 2, [Corner_2_x, Corner_2_y]),
    flat_hex_corner([Point_x, Point_y], Size_hex, 3, [Corner_3_x, Corner_3_y]),
    flat_hex_corner([Point_x, Point_y], Size_hex, 4, [Corner_4_x, Corner_4_y]),
    flat_hex_corner([Point_x, Point_y], Size_hex, 5, [Corner_5_x, Corner_5_y]),
    flat_hex_corner([Point_x, Point_y], Size_hex, 6, [Corner_6_x, Corner_6_y]),

    draw_background_hex(W, [Corner_1_x, Corner_1_y],[Corner_2_x, Corner_2_y],[Corner_3_x, Corner_3_y],
    [Corner_4_x, Corner_4_y],[Corner_5_x, Corner_5_y],[Corner_6_x, Corner_6_y], Color),

    draw_image_hexagon(W, Image, [Point_x, Point_y]).


draw_hexagon_pixel_empty(W, [Point_x,Point_y], Size_hex, Color):-
    
    
    
    flat_hex_corner([Point_x,Point_y], Size_hex, 1, [Corner_1_x,Corner_1_y]),
    flat_hex_corner([Point_x,Point_y], Size_hex, 2, [Corner_2_x,Corner_2_y]),
    flat_hex_corner([Point_x,Point_y], Size_hex, 3, [Corner_3_x,Corner_3_y]),
    flat_hex_corner([Point_x,Point_y], Size_hex, 4, [Corner_4_x,Corner_4_y]),
    flat_hex_corner([Point_x,Point_y], Size_hex, 5, [Corner_5_x,Corner_5_y]),
    flat_hex_corner([Point_x,Point_y], Size_hex, 6, [Corner_6_x,Corner_6_y]),

   
    draw_foreground_hex(W, [Corner_1_x, Corner_1_y],[Corner_2_x, Corner_2_y],[Corner_3_x, Corner_3_y],
    [Corner_4_x, Corner_4_y],[Corner_5_x, Corner_5_y],[Corner_6_x, Corner_6_y], Color).
    

draw_hexagon_pixel_filling(W, [Point_x,Point_y], Size_hex, Color):-
    
    flat_hex_corner([Point_x,Point_y], Size_hex, 1, [Corner_1_x,Corner_1_y]),
    flat_hex_corner([Point_x,Point_y], Size_hex, 2, [Corner_2_x,Corner_2_y]),
    flat_hex_corner([Point_x,Point_y], Size_hex, 3, [Corner_3_x,Corner_3_y]),
    flat_hex_corner([Point_x,Point_y], Size_hex, 4, [Corner_4_x,Corner_4_y]),
    flat_hex_corner([Point_x,Point_y], Size_hex, 5, [Corner_5_x,Corner_5_y]),
    flat_hex_corner([Point_x,Point_y], Size_hex, 6, [Corner_6_x,Corner_6_y]),

    
   
    draw_background_hex(W, [Corner_1_x, Corner_1_y],[Corner_2_x, Corner_2_y],[Corner_3_x, Corner_3_y],
    [Corner_4_x, Corner_4_y],[Corner_5_x, Corner_5_y],[Corner_6_x, Corner_6_y], Color).


draw_hexagon_pixel_axial(W, [Pixel_x, Pixel_y], Size_hex,Size_x, Size_y, Image, Color):-
    
    pixel_to_axial([Pixel_x, Pixel_y], [R, Q], Size_hex,Size_x, Size_y),

    draw_hexagon_axial(W, [R, Q], Size_hex,Size_x, Size_y, Image, Color, true),
    draw_hexagon_axial(W, [R, Q], Size_hex,Size_x, Size_y, Image, colour(red), false).
    


draw_possible_movements(W,L_hive,Size_hex,Size_x, Size_y, Color):-
    findall(_, 
        (
            member([R,Q], L_hive),
            draw_hexagon_axial(W, [R,Q], Size_hex,Size_x, Size_y, Image,Color, false)
        )
        ,_).


draw_pieces(W, Player_id,Size_hex, Type, Pieces):-

    color_player(Player_id, Col),
    length(Pieces, Length_pieces),
    Length_pieces > 0,
    member(Insect_pos, Pieces),
    draw_hexagon_pixel(W, Insect_pos, Size_hex, Type, Col).

