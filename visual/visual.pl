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
    new(W, window("window", size(1000, 800))),
    send(W, background, colour(@default, 52000, 40000, 8000)),
    event_click(W),

    insects:start_insects(p1, [40,120,200,280,360,440,520,600], [40,70,100]),
    insects:start_insects(p2, [40,120,200,280,360,440,520,600], [700,730,760]),
         
    
    draw_initials_pieces(W, 35, colour(white), [40,120,200,280,360,440,520,600], [40,70,100]),
    draw_initials_pieces(W, 35, colour(black), [40,120,200,280,360,440,520,600], [700,730,760]),
    
    draw_possible_movements(W, [[0,0],[1,0],[2,0]], 35, colour(blue)),
    send(W, open).



event_click(W):-
    send(W, recogniser, click_gesture(
        left, '', single, 
        message(@prolog, select_piece, W, @event?position))).

select_piece(W, Position):-

    get(Position, x, Click_X),
    get(Position, y, Click_Y),
    draw_hexagon_pixel_axial(W, [Click_X, Click_Y], 35, aranha, colour(white)).









