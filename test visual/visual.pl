:-use_module(library(pce)).
:- pce_image_directory('./images').

:-consult('../hexagon'), import('../hexagon').

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
    new(W, window("window", size(800, 800))),
    event_click(W),

    send(W, background, colour(brown)),

    draw_initials_pieces(W, 35, black, [40,120,200,280,360,440,520,600], [700,730,760]),
    draw_initials_pieces(W, 35, white, [40,120,200,280,360,440,520,600], [40,70,100]),
    draw_hexagon_axial(W, [0, 1], 35, saltamonte, black, true),
    
    draw_possible_movements(W, [[0,0],[1,0],[2,0]], 35, blue),
    send(W, open).


event_click(W):-
    send(W, recogniser, click_gesture(
        left, '', single, message(@prolog, select_piece, W, @event?position))).

select_piece(W, Position):-
    get(Position, x, Click_X),
    get(Position, y, Click_Y),

    writeln(Click_X), writeln(Click_Y).

new_image(Window, Figure, Image, Position) :-
    new(Figure, figure),
    new(Bitmap, bitmap(resource(Image),@on)),
    send(Bitmap, name, 1),
    send(Figure, display, Bitmap),
    send(Figure, status, 1),
    send(Window, display, Figure, Position).


draw_image_hexagon(Window,Image,[Point_x, Point_y]):-
    X_center is Point_x - 23,
    Y_center is Point_y -23,
    new_image(Window,_, Image, point(X_center, Y_center)).


flat_hex_corner([X,Y], Size, I, [Corner_x, Corner_y]):-
    Angle_deg is 60 * I,
    Angle_rad is pi() / 180 * Angle_deg,

    Corner_x is X + Size * cos(Angle_rad),
    Corner_y is Y + Size * sin(Angle_rad).
    

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

draw_hexagon_axial(W, [R, Q], Size, Image, Color, Empty):-
    (
        hexagon:flat_hex_to_pixel(R,Q,Size,[Pixel_X, Pixel_Y]),

        Point_x is Pixel_X +400,
        Point_y is Pixel_Y +400,

        flat_hex_corner([Point_x, Point_y], Size, 1, [Corner_1_x, Corner_1_y]),
        flat_hex_corner([Point_x, Point_y], Size, 2, [Corner_2_x, Corner_2_y]),
        flat_hex_corner([Point_x, Point_y], Size, 3, [Corner_3_x, Corner_3_y]),
        flat_hex_corner([Point_x, Point_y], Size, 4, [Corner_4_x, Corner_4_y]),
        flat_hex_corner([Point_x, Point_y], Size, 5, [Corner_5_x, Corner_5_y]),
        flat_hex_corner([Point_x, Point_y], Size, 6, [Corner_6_x, Corner_6_y]),

        Empty,

        draw_background_hex(W, [Corner_1_x, Corner_1_y],[Corner_2_x, Corner_2_y],[Corner_3_x, Corner_3_y],
        [Corner_4_x, Corner_4_y],[Corner_5_x, Corner_5_y],[Corner_6_x, Corner_6_y], Color),

        draw_image_hexagon(W, Image, [Point_x, Point_y]),!
    );
    (
        hexagon:flat_hex_to_pixel(R,Q,Size,[Pixel_X, Pixel_Y]),

        Point_x is Pixel_X +400,
        Point_y is Pixel_Y +400,

        flat_hex_corner([Point_x, Point_y], Size, 1, [Corner_1_x, Corner_1_y]),
        flat_hex_corner([Point_x, Point_y], Size, 2, [Corner_2_x, Corner_2_y]),
        flat_hex_corner([Point_x, Point_y], Size, 3, [Corner_3_x, Corner_3_y]),
        flat_hex_corner([Point_x, Point_y], Size, 4, [Corner_4_x, Corner_4_y]),
        flat_hex_corner([Point_x, Point_y], Size, 5, [Corner_5_x, Corner_5_y]),
        flat_hex_corner([Point_x, Point_y], Size, 6, [Corner_6_x, Corner_6_y]),

        not(Empty),
        draw_foreground_hex(W, [Corner_1_x, Corner_1_y],[Corner_2_x, Corner_2_y],[Corner_3_x, Corner_3_y],
        [Corner_4_x, Corner_4_y],[Corner_5_x, Corner_5_y],[Corner_6_x, Corner_6_y], Color)
    ).


draw_hexagon_pixel(W, [Point_x, Point_y], Size, Image, Color):-
    
    
    flat_hex_corner([Point_x, Point_y], Size, 1, [Corner_1_x, Corner_1_y]),
    flat_hex_corner([Point_x, Point_y], Size, 2, [Corner_2_x, Corner_2_y]),
    flat_hex_corner([Point_x, Point_y], Size, 3, [Corner_3_x, Corner_3_y]),
    flat_hex_corner([Point_x, Point_y], Size, 4, [Corner_4_x, Corner_4_y]),
    flat_hex_corner([Point_x, Point_y], Size, 5, [Corner_5_x, Corner_5_y]),
    flat_hex_corner([Point_x, Point_y], Size, 6, [Corner_6_x, Corner_6_y]),

    draw_background_hex(W, [Corner_1_x, Corner_1_y],[Corner_2_x, Corner_2_y],[Corner_3_x, Corner_3_y],
    [Corner_4_x, Corner_4_y],[Corner_5_x, Corner_5_y],[Corner_6_x, Corner_6_y], Color),

    draw_image_hexagon(W, Image, [Point_x, Point_y]).
    


draw_possible_movements(W,L_hive,Size, Color):-
    findall(_, 
        (
            member([R,Q], L_hive),
            draw_hexagon_axial(W, [R,Q], Size, Image,Color, false)
        )
        ,_).


draw_initials_pieces(W, Size, Color, [X1, X2,X3,X4,X5,X6,X7,X8], [Y1,Y2,Y3]):-
    draw_hexagon_pixel(W, [X1, Y1], Size, hormiga, Color),
    draw_hexagon_pixel(W, [X1, Y2], Size, hormiga, Color),
    draw_hexagon_pixel(W, [X1, Y3], Size, hormiga, Color),

    draw_hexagon_pixel(W, [X2, Y1], Size, escarabajo, Color),
    draw_hexagon_pixel(W, [X2, Y2], Size, escarabajo, Color),

    draw_hexagon_pixel(W, [X3, Y1], Size, saltamonte, Color),
    draw_hexagon_pixel(W, [X3, Y2], Size, saltamonte, Color),
    draw_hexagon_pixel(W, [X3, Y3], Size, saltamonte, Color),

    draw_hexagon_pixel(W, [X4, Y1], Size, abejaReina, Color),

    draw_hexagon_pixel(W, [X5, Y1], Size, aranha, Color),
    draw_hexagon_pixel(W, [X5, Y2], Size, aranha, Color),

    draw_hexagon_pixel(W, [X6, Y1], Size, mariquita, Color),

    draw_hexagon_pixel(W, [X7, Y1], Size, mosquito, Color),

    draw_hexagon_pixel(W, [X8, Y1], Size, bichoBola, Color).

