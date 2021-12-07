:-use_module(library(pce)).
:- pce_image_directory('./images').

module(utils_visual, [
    pixel_to_axial/3, axial_to_pixel/3, get_X_Y_to_Pixel_to_flat_hex_and_Flat_hex_to_pixel/5,
    flat_hex_corner/4, new_image/4
]).

:-consult('../hexagon'), import('../hexagon').
:-consult('../insects'), import('../insects').


% este predicado es para tener la colmena centrada con el (0,0) en el centro de la pantalla
pixel_to_axial([X_pixel,Y_pixel], [X_axial,Y_axial], Size_hex):- 
    hexagon:pixel_to_flat_hex(X_pixel,Y_pixel,Size_hex,[H_x, H_y]),

    get_X_Y_to_Pixel_to_flat_hex_and_Flat_hex_to_pixel(1000, 800,Size_hex, [PF_x,PF_y], [FP_x,FP_y]),
    
    X_axial is H_x- PF_x,
    Y_axial is H_y - PF_y.


axial_to_pixel([X_axial,Y_axial], [X_pixel,Y_pixel], Size_hex):-
    hexagon:flat_hex_to_pixel(X_axial,Y_axial, Size_hex, [P_x, P_y]) ,
    
    get_X_Y_to_Pixel_to_flat_hex_and_Flat_hex_to_pixel(1000, 800,Size_hex, [PF_x,PF_y], [FP_x,FP_y]),
    
    X_pixel is P_x + FP_x ,
    Y_pixel is P_y + FP_y .




get_X_Y_to_Pixel_to_flat_hex_and_Flat_hex_to_pixel(Size_x, Size_y,Size_hex, [PF_x,PF_y], [FP_x,FP_y]):-

    FP_x is Size_x/2,
    FP_y is Size_y/2,
    hexagon:pixel_to_flat_hex(FP_x,FP_y,Size_hex,[PF_x, PF_y]).


flat_hex_corner([X,Y], Size, I, [Corner_x, Corner_y]):-
    Angle_deg is 60 * I,
    Angle_rad is pi() / 180 * Angle_deg,

    Corner_x is X + Size * cos(Angle_rad),
    Corner_y is Y + Size * sin(Angle_rad).


new_image(Window, Figure, Image, Position) :-
    new(Figure, figure),
    new(Bitmap, bitmap(resource(Image),@on)),
    send(Bitmap, name, 1),
    send(Figure, display, Bitmap),
    send(Figure, status, 1),
    send(Window, display, Figure, Position).