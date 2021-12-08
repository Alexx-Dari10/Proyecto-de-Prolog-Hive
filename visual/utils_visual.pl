:-use_module(library(pce)).
:- pce_image_directory('./images').

module(utils_visual, [
    pixel_to_axial/3, axial_to_pixel/5, get_X_Y_to_Pixel_to_flat_hex_and_Flat_hex_to_pixel/5,
    flat_hex_corner/4, new_image/4, check_positions_in_hand/5, check_position_in_hive/4
]).

:-consult('../hexagon'), import('../hexagon').
:-consult('../insects'), import('../insects').


% restamos o sumamos la mitad del tamanho del tablero para tener coordenadas en el centro 
pixel_to_axial([X_pixel,Y_pixel], [X_axial,Y_axial], Size_hex,Size_x, Size_y):-

    X is X_pixel - Size_x/2,
    Y is Y_pixel - Size_y/2,

    hexagon:pixel_to_flat_hex(X,Y,Size_hex,[X_axial, Y_axial]).


axial_to_pixel([X_axial,Y_axial], [X_pixel,Y_pixel], Size_hex, Size_x, Size_y):-
    hexagon:flat_hex_to_pixel(X_axial,Y_axial, Size_hex, [P_x, P_y]),
    
    X_pixel is P_x + Size_x/2 ,
    Y_pixel is P_y + Size_y/2 .



flat_hex_corner([X,Y], Size, I, [Corner_x,Corner_y]):-
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


check_positions_in_hand([Pixel_x, Pixel_y],Size_hex,[X1,X2,X3,X4,X5,X6,X7,X8],[Y1,Y3],Type):-
    (   
        check_limits([Pixel_x,Pixel_y],[X1,Y1,Y3], Size_hex),
        Type = hormiga
    ),!;
    (
        check_limits([Pixel_x,Pixel_y],[X2,Y1,Y3], Size_hex),
        Type = escarabajo
    ),!;
    (
        check_limits([Pixel_x,Pixel_y],[X3,Y1,Y3], Size_hex),
        Type = saltamonte
    ),!;
    (
        check_limits([Pixel_x,Pixel_y],[X4,Y1,Y3], Size_hex),
        Type = abejaReina
    ),!;
    (
        check_limits([Pixel_x,Pixel_y],[X5,Y1,Y3], Size_hex),
        Type = aranha
    ),!;
    (
        check_limits([Pixel_x,Pixel_y],[X6,Y1,Y3], Size_hex),
        Type = mariquita
    ),!;
    (
        check_limits([Pixel_x,Pixel_y],[X7,Y1,Y3], Size_hex),
        Type = mosquito
    ),!;
    (
        check_limits([Pixel_x,Pixel_y],[X8,Y1,Y3], Size_hex),
        Type = bichoBola
    ).
    

check_limits([Pixel_x, Pixel_y], [Xi, Y1, Y3], Size_hex):-
    Pixel_y < Y3 + Size_hex,
    Pixel_y > Y1 - Size_hex,
    Pixel_x < Xi + Size_hex,
    Pixel_x > Xi - Size_hex.

check_position_in_hive([Click_X, Click_Y],Size_hex, [X_ini, X_end], [Y_ini,Y_end]):-
    Click_X > X_ini - Size_hex, 
    Click_X < X_end + Size_hex,
    Click_Y > Y_ini + Size_hex,
    Click_Y < Y_end - Size_hex.



set_hex_to_type(Type, Id, Hex_select,[X1,X2,X3,X4,X5,X6,X7,X8], [Y1,Y2,Y3]):-
    (
        Id == 3,
        (
            ( Type == hormiga, Hex_select = [X1,Y3] );
            ( Type == escarabajo, Hex_select = [X2,Y3] );
            ( Type == saltamonte, Hex_select = [X3,Y3] );
            ( Type == abejaReina, Hex_select = [X4,Y3] );
            ( Type == aranha, Hex_select = [X5,Y3] );
            ( Type == mariquita, Hex_select = [X6,Y3] );
            ( Type == mosquito, Hex_select = [X7,Y3] );
            ( Type == bichoBola, Hex_select = [X8,Y3] )
        )
    );
    (
        Id == 2,
        (
            (Type == hormiga,Hex_select = [X1,Y2]);
            (Type == escarabajo,Hex_select = [X2,Y2]);
            (Type == saltamonte,Hex_select = [X3,Y2]);
            (Type == abejaReina,Hex_select = [X4,Y2]);
            (Type == aranha,Hex_select = [X5,Y2]);
            (Type == mariquita,Hex_select = [X6,Y2]);
            (Type == mosquito,Hex_select = [X7,Y2]);
            (Type == bichoBola,Hex_select = [X8,Y2])
        )
    );
    (
        Id == 1,
        (
            (Type == hormiga,Hex_select = [X1,Y1]);
            (Type == escarabajo,Hex_select = [X2,Y1]);
            (Type == saltamonte,Hex_select = [X3,Y1]);
            (Type == abejaReina,Hex_select = [X4,Y1]);
            (Type == aranha,Hex_select = [X5,Y1]);
            (Type == mariquita,Hex_select = [X6,Y1]);
            (Type == mosquito,Hex_select = [X7,Y1]);
            (Type == bichoBola,Hex_select = [X8,Y1] )   
        ) 
    ).



select_in_hand(Type, Player_id , Hex_select, Id,[X1,X2,X3,X4,X5,X6,X7,X8], [Y1,Y2,Y3]):-
    (
        findall(Type, insect(Type,_,Player_id, _, -1), L_insects),
        length(L_insects, Length),
        Length == 3,
        Id = 3,
        writeln("3"),
        
        set_hex_to_type(Type, 3, Hex_select,[X1,X2,X3,X4,X5,X6,X7,X8], [Y1,Y2,Y3])
    ),!;
    (
        
        findall(Type, insect(Type,_,Player_id, _, -1), L_insects),
        length(L_insects, Length),
        Length == 2,
        Id = 2,
        writeln("2"),
        set_hex_to_type(Type, 2, Hex_select,[X1,X2,X3,X4,X5,X6,X7,X8], [Y1,Y2,Y3])
    ),!;
    (
        findall(Type, insect(Type,_,Player_id, _, -1), L_insects),
        length(L_insects, Length),
        Length == 1,
        Id = 1,
        writeln("1"),
        set_hex_to_type(Type, 1, Hex_select,[X1,X2,X3,X4,X5,X6,X7,X8], [Y1,Y2,Y3])
    ).
    
    
