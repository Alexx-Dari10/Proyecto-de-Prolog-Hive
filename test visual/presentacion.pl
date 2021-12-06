:-use_module(library(pce)).
 
t1:-new(D,dialog('Demo Fenster')),send(D,open).
 
t2:-new(D,dialog('Demo Window')),
    send(D,append,button(hallo)),
    send(D,open).
 
t3:-new(D,dialog('Demo Window')),
    send(D,append,button(hallo,
                         message(@prolog,wenn_gedrueckt,D))),
    send(D,open).
wenn_gedrueckt(D):-send(D,destroy).
 
t4:-new(D,dialog('Demo Window')),
    send(D,append,text_item(hallo,'dummy')),
    send(D,open).
 
t5:-new(D,dialog('Demo Window')),
    new(T,text_item(hallo)),
    send(D,append,T),
    send(T,value,'muss nicht sein'),
    send(D,open).
 
t6:-new(D,dialog('Demo Window')),
    new(T,text_item(hallo)),
    send(D,append,T),
    send(T,value,'dummy'),
    send(D,open),
    get(T,value,Text),
    writeln(Text).
 
leihengxin:-new(D,dialog('Demo Window')),
    new(T,text_item(eingabe)),
    send(D,append,T),
    send(D,append,button(ok,
                         message(@prolog,lei,T))),
    send(D,append,button(haha,
                         message(@prolog,ende,D))),
    send(D,open).
lei(T):-get(T,value,Text),writeln(Text).
ende(D):-send(D,destroy).


ejemplo :-
    /*
    * Crea el objeto dialogo en la variable D
    */
    new(D, dialog("‘Nombre del Dialogo’")),
    /*
    * Crea el objeto boton almacenandolo en la variable @boton de tal forma
    * que al pulsar sobre el boton libere la memoria y cierre la ventana)
    */
    new(@boton, button("‘Cerrar Dialogo’",
    and(
    message(D, destroy),
    message(D, free),
    message(@boton, free)))),
    /*
    * Inserta el botón en el diálogo
    */
    send(D, append(@boton)),
    /*
    * Le envia el mensaje open al dialogo para que cree y muestre la ventana.
    */
    send(D, open).

ejemplo_mensajes :-
    % Crea el objeto dialogo en la variable D
    new(D, dialog("Nombre del Dialogo")),
    % Crea un boton que llama al predicado mostrar_mensaje
    new(B, button("Mostrar en Consola",
    message(@prolog, mostrar_mensaje, "Este es el valor que tendra la variable P"))),
    % Crea un boton para cerrar el dialogo
    new(@boton, button("Cerrar Dialogo",
    and(
    message(D, destroy),
    message(D, free),
    message(D, free),
    message(@boton, free)))),
    % Inserta los botones en el diálogo
    send(D, append(@boton)),
    send(D, append(B)),
    % Muestre la ventana.
    send(D, open).
    % Muestra un mensaje en la consola.
    mostrar_mensaje(P) :-
    write("La variable P vale "), write(P), nl.




show:-
    new(W, window("window", size(1000, 1000))),
    new(BM, bitmap('./fotosXPM/hormiga.xpm')),
    new(BM1, bitmap('./fotosXPM/abejaReina.xpm')),
    new(BM2, bitmap('./fotosXPM/aranha.xpm')),
    new(BM3, bitmap('./fotosXPM/escarabajo.xpm')),
    new(BM4, bitmap('./fotosXPM/mariquita.xpm')),
    new(BM5, bitmap('./fotosXPM/mosquito.xpm')),
    new(BM6, bitmap('./fotosXPM/bichoBola.xpm')),
    new(BM7, bitmap('./fotosXPM/saltamonte.xpm')),
    
    send(W, display, BM, point(500, 500)),
    send(W, display, BM1, point(250, 30)),
    send(W, display, BM2, point(15, 700)),
    send(W, display, BM3, point(40, 50)),
    send(W, display, BM4, point(400, 400)),
    send(W, display, BM5, point(600, 600)),
    send(W, display, BM6, point(67, 98)),
    send(W, display, BM7, point(400, 800)),
    send(W, open).