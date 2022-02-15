;Tipos de preguntas

;Pregunta si o no
(deffunction pregunta-si-no (?pregunta)
    (printout t crlf)
    (format t "%s (si/no): " ?pregunta)
    (bind ?respuesta (read))
    (if (or (eq ?respuesta si) (eq ?respuesta s))
        then TRUE
    else FALSE)
)

;Pregunta numerica
(deffunction pregunta-numerica (?pregunta ?min ?max)
    (printout t crlf)
    (format t "%s (numero): " ?pregunta)
    (bind ?respuesta (read))
    (while (not(and(>= ?respuesta ?min)(<= ?respuesta ?max))) do
        (format t "%s (numero): " ?pregunta)
        (bind ?respuesta (read))
    )
    ?respuesta
)

;Calcula la distancia euclidea entre dos puntos
(deffunction calcula-distancia (?x1 ?y1 ?x2 ?y2)
    (bind ?distX (- ?x1 ?x2))
    (bind ?distX (* ?distX ?distX))

    (bind ?distY (- ?y1 ?y2))
    (bind ?distY (* ?distY ?distY))

    (bind ?distance (sqrt (+ ?distX ?distY))) ;esta bien¿?

    ?distance
)

;En las funciones relacionadas con las distancias, devolveremos 3 tipos de valores:
;0 -> no hay transporte cerca
;1 -> hay un transporte a media distancia
;2 -> hay un transporte cerca

(deffunction dist-transporte (?vivienda)
    (bind ?x (send ?vivienda get-coordenadaX))
    (bind ?y (send ?vivienda get-coordenadaY))

    (bind ?i 1)
    (bind ?n 0)
    (bind ?transportes (find-all-instances ((?instancia Transporte)) TRUE))

    (while (and (<= ?i (length$ ?transportes)) (< ?n 2)) do
        ;t = transporte actual
        (bind ?t (nth$ ?i ?transportes))
        (bind ?tx (send ?t get-coordenadaX))
        (bind ?ty (send ?t get-coordenadaY))

        (bind ?dist (calcula-distancia ?x ?y ?tx ?ty))
        (if (<= ?dist 10)
            then (bind ?n 1)
        )
        (if (<= ?dist 5)
            then (bind ?n 2)
        )
        (bind ?i (+ ?i 1))
    )
    ?n
)

;^ punto extra, v resto del trabajo

;mismo sistema con devolucion de valores 0/1/2
(deffunction dist-parque (?vivienda)
    (bind ?x (send ?vivienda get-coordenadaX))
    (bind ?y (send ?vivienda get-coordenadaY))

    (bind ?i 1)
    (bind ?n 0)
    (bind ?parques (find-all-instances ((?instancia Parque)) TRUE))

    (while (and (<= ?i (length$ ?parques)) (< ?n 2)) do
        ;parque actual
        (bind ?p (nth$ ?i ?parques))
        (bind ?px (send ?p get-coordenadaX))
        (bind ?py (send ?p get-coordenadaY))
        
        (bind ?dist (calcula-distancia ?x ?y ?px ?py))

        (if (<= ?dist 100)
            then (bind ?n 1)
        )
        (if (<= ?dist 50)
            then (bind ?n 2)
        )
        (bind ?i (+ ?i 1))
    )
    ?n
)

;mismo sistema con devolucion de valores 0/1/2
(deffunction dist-salud (?vivienda)
    (bind ?x (send ?vivienda get-coordenadaX))
    (bind ?y (send ?vivienda get-coordenadaY))

    (bind ?i 1)
    (bind ?n 0)
    (bind ?centrossalud (find-all-instances ((?instancia Salud)) TRUE))

    (while (and (<= ?i (length$ ?centrossalud)) (< ?n 2)) do
        ;cs = centro de salud actual
        (bind ?cs (nth$ ?i ?centrossalud))
        (bind ?csx (send ?cs get-coordenadaX))
        (bind ?csy (send ?cs get-coordenadaY))
        
        (bind ?dist (calcula-distancia ?x ?y ?csx ?csy))

        (if (<= ?dist 100)
            then (bind ?n 1)
        )
        (if (<= ?dist 50)
            then (bind ?n 2)
        )
        (bind ?i (+ ?i 1))
    )
    ?n
)

;mismo sistema con devolucion de valores 0/1/2
(deffunction dist-ocio (?vivienda)
    (bind ?x (send ?vivienda get-coordenadaX))
    (bind ?y (send ?vivienda get-coordenadaY))

    (bind ?i 1)
    (bind ?n 0)
    (bind ?ocios (find-all-instances ((?instancia Ocio)) TRUE))

    (while (and (<= ?i (length$ ?ocios)) (< ?n 2)) do
        ;o = ocio actual
        (bind ?o (nth$ ?i ?ocios))
        (bind ?ox (send ?o get-coordenadaX))
        (bind ?oy (send ?o get-coordenadaY))
        
        (bind ?dist (calcula-distancia ?x ?y ?ox ?oy))

        (if (<= ?dist 100)
            then (bind ?n 1)
        )
        (if (<= ?dist 50)
            then (bind ?n 2)
        )
        (bind ?i (+ ?i 1))
    )
    ?n
)

;mismo sistema con devolucion de valores 0/1/2
(deffunction dist-discoteca (?vivienda)
    (bind ?x (send ?vivienda get-coordenadaX))
    (bind ?y (send ?vivienda get-coordenadaY))

    (bind ?i 1)
    (bind ?n 0)
    (bind ?ocios (find-all-instances ((?instancia Ocio)) TRUE))

    (while (and (<= ?i (length$ ?ocios)) (< ?n 2)) do
        ;o = ocio actual
        (bind ?o (nth$ ?i ?ocios))
        (bind ?tipo (send ?o get-tipoOcio))
        (if(eq ?tipo Discoteca)     ;;;solo calculamos si es de tipo discoteca
            then
                (bind ?ox (send ?o get-coordenadaX))
                (bind ?oy (send ?o get-coordenadaY))
                
                (bind ?dist (calcula-distancia ?x ?y ?ox ?oy))

            (if (<= ?dist 1000)
                then (bind ?n 1)
            )
            (if (<= ?dist 500)
                then (bind ?n 2)
            )
        )
        (bind ?i (+ ?i 1))
    )
    ?n
)

;mismo sistema con devolucion de valores 0/1/2
(deffunction dist-primaria (?vivienda)
    (bind ?x (send ?vivienda get-coordenadaX))
    (bind ?y (send ?vivienda get-coordenadaY))

    (bind ?i 1)
    (bind ?n 0)
    (bind ?centroseducacion (find-all-instances ((?instancia Educacion)) TRUE))

    (while (and (<= ?i (length$ ?centroseducacion)) (< ?n 2)) do
        ;ce = centro educacion actual
        (bind ?ce (nth$ ?i ?centroseducacion))
        (bind ?tipo (send ?ce get-tipoEducacion))
        (if(eq ?tipo Primaria)
            then
                (bind ?cex (send ?ce get-coordenadaX))
                (bind ?cey (send ?ce get-coordenadaY))
                
                (bind ?dist (calcula-distancia ?x ?y ?cex ?cey))

            (if (<= ?dist 100)
                then (bind ?n 1)
            )
            (if (<= ?dist 50)
                then (bind ?n 2)
            )
        )
        (bind ?i (+ ?i 1))
    )
    ?n
)

;mismo sistema con devolucion de valores 0/1/2
(deffunction dist-secundaria (?vivienda)
    (bind ?x (send ?vivienda get-coordenadaX))
    (bind ?y (send ?vivienda get-coordenadaY))

    (bind ?i 1)
    (bind ?n 0)
    (bind ?centroseducacion (find-all-instances ((?instancia Educacion)) TRUE))

    (while (and (<= ?i (length$ ?centroseducacion)) (< ?n 2)) do
        ;ce = centro educacion actual
        (bind ?ce (nth$ ?i ?centroseducacion))
        (bind ?tipo (send ?ce get-tipoEducacion))
        (if(eq ?tipo Secundaria)     ;;;solo calculamos si es de tipo secundaria
            then
                (bind ?cex (send ?ce get-coordenadaX))
                (bind ?cey (send ?ce get-coordenadaY))
                
                (bind ?dist (calcula-distancia ?x ?y ?cex ?cey))

            (if (<= ?dist 100)
                then (bind ?n 1)
            )
            (if (<= ?dist 50)
                then (bind ?n 2)
            )
        )
        (bind ?i (+ ?i 1))
    )
    ?n
)

;mismo sistema con devolucion de valores 0/1/2
(deffunction dist-universitaria (?vivienda)
    (bind ?x (send ?vivienda get-coordenadaX))
    (bind ?y (send ?vivienda get-coordenadaY))

    (bind ?i 1)
    (bind ?n 0)
    (bind ?centroseducacion (find-all-instances ((?instancia Educacion)) TRUE))

    (while (and (<= ?i (length$ ?centroseducacion)) (< ?n 2)) do
        ;ce = centro educacion actual
        (bind ?ce (nth$ ?i ?centroseducacion))
        (bind ?tipo (send ?ce get-tipoEducacion))
        (if(eq ?tipo Universitaria)     ;;;solo calculamos si es de tipo universitaria
            then
                (bind ?cex (send ?ce get-coordenadaX))    ;;;no se si esto automaticamente busca en la superclase servicio(?)
                (bind ?cey (send ?ce get-coordenadaY))
                
                (bind ?dist (calcula-distancia ?x ?y ?cex ?cey))

            (if (<= ?dist 100)
                then (bind ?n 1)
            )
            (if (<= ?dist 50)
                then (bind ?n 2)
            )
        )
        (bind ?i (+ ?i 1))
    )
    ?n
)

;mismo sistema con devolucion de valores 0/1/2
(deffunction dist-supermercado (?vivienda)
    (bind ?x (send ?vivienda get-coordenadaX))
    (bind ?y (send ?vivienda get-coordenadaY))

    (bind ?i 1)
    (bind ?n 0)
    (bind ?comercios (find-all-instances ((?instancia Comercio)) TRUE))

    (while (and (<= ?i (length$ ?comercios)) (< ?n 2)) do
        ;c = comercio actual
        (bind ?c (nth$ ?i ?comercios))
        (bind ?tipo (send ?c get-tipoComercio))
        (if(eq ?tipo Supermercado)     ;;;solo calculamos si es de tipo supermercado
            then
                (bind ?cx (send ?c get-coordenadaX))
                (bind ?cy (send ?c get-coordenadaY))
                
                (bind ?dist (calcula-distancia ?x ?y ?cx ?cy))

            (if (<= ?dist 100)
                then (bind ?n 1)
            )
            (if (<= ?dist 50)
                then (bind ?n 2)
            )
        )
        (bind ?i (+ ?i 1))
    )
    ?n
)

;Definición de Clases y Templates
(defclass Candidata
	(is-a USER)
	(role concrete)
	(slot vivienda
		(type INSTANCE)
		(allowed-classes Vivienda)
		(create-accessor read-write))
	(slot puntuacion
		(type INTEGER)
		(create-accessor read-write))
)

(defclass Adecuada
	(is-a Candidata)
	(role concrete)
)

(defclass NoAdecuada
	(is-a Candidata)
	(role concrete)
)

; **********************************************************************
; ****** obtener los datos de de las preferencias de los clientes ******
; **********************************************************************

;guardar las preferencias de los clientes
(deftemplate preferencias-clientes
    (slot precio-max (type INTEGER))
    (slot precio-min (type INTEGER))
	(slot metros-quadrados-max (type INTEGER))
	(slot metros-quadrados-min (type INTEGER))
	(slot num-personas (type INTEGER))
	(slot tiene-mascota (type SYMBOL) (allowed-values FALSE TRUE) (default FALSE))
	(slot tiene-coche (type SYMBOL) (allowed-values FALSE TRUE) (default FALSE))
)

(defrule saludo-initial "saludo"
	(initial-fact)
	=>
	(printout t crlf)
	(printout t "¡Bienvenido a nuestro sistema para buscar viviendas!" crlf)
	(printout t "Vamos a hacerle unas preguntas:" crlf)
	(assert (preguntar-preferencias))
)

(defrule preferencias "Preguntas para cliente"
	?puntero <- (preguntar-preferencias)

	=>
	;Precio
 	(bind ?precio-min (pregunta-numerica "Precio mínimo" 0 99999))
	(bind ?precio-max (pregunta-numerica "Precio máximo" 0 99999))

	;Metros cuadrados
	(bind ?sq-min (pregunta-numerica "Metros cuadrados mínimos" 0 99999))
	(bind ?sq-max (pregunta-numerica "Metros cuadrados máximos" 0 99999))

	;Numero personas
	(bind ?num-personas (pregunta-numerica "Número de personas" 0 20))

	;Mascotas
	(bind ?mascota (pregunta-si-no "¿Tiene mascotas?"))
	
	;Coche
	(bind ?coche (pregunta-si-no "¿Tiene coche?"))

	;Guardar datos del cliente
	(assert (preferencias-clientes
			(precio-max ?precio-max)
			(precio-min ?precio-min)
			(metros-quadrados-min ?sq-min)
			(metros-quadrados-max ?sq-max)
			(num-personas ?num-personas)
			(tiene-mascota ?mascota)
			(tiene-coche ?coche)
			)
	)
	
	;Notificar crear cliente
	(retract ?puntero)
	(assert (nuevo-cliente))
)

;Buscamos viviendas adecuadas a partir de los datos del cliente
(defrule buscar-vivienda "Busqueda de vivienda"
    ?preferencia <- (preferencias-clientes
        (precio-max ?pmax)
        (precio-min ?pmin)
        (metros-quadrados-min ?sq-min)
	    (metros-quadrados-max ?sq-max)
        (num-personas ?num-personas)
        (tiene-mascota ?mascota)
        (tiene-coche ?coche))

    ?puntero <- (nuevo-cliente)
	=>
    
    ;Iteramos sobre todas las instancias de viviendas
    (bind $?viviendas (find-all-instances ((?instancia Vivienda)) TRUE))
    (loop-for-count (?i 1 (length$ $?viviendas)) do                 
        ;Vivienda actual
        (bind ?actual (nth$ ?i ?viviendas))

        ;valor que tendra una vivienda para un usuario
        (bind ?valor 0)

        ;Presupuesto
        (bind ?precio-actual (send ?actual get-precio))
        (if (or (> ?precio-actual ?pmax) (< ?precio-actual ?pmin))
            then (bind ?valor (- ?valor 10))
        )
        (if (and (<= ?precio-actual ?pmax) (>= ?precio-actual ?pmin))
            then (bind ?valor (+ ?valor 20))
        )

        ;Metros cuadrados
        (bind ?medida-actual (send ?actual get-superficie))
        (if (or (> ?medida-actual ?sq-max) (< ?medida-actual ?sq-min))
            then (bind ?valor (- ?valor 10))
        )
        (if (and (<= ?medida-actual ?sq-max) (>= ?medida-actual ?sq-min))
            then (bind ?valor (+ ?valor 20))
        )

        ;Numero de personas
        (bind ?habitaciones-actual (send ?actual get-numeroHabitaciones))
        (if (>= ?habitaciones-actual ?num-personas)
            then (bind ?valor (+ ?valor 20))
        else 
            (bind ?valor (- ?valor 10))
        )
        ;Mascotas
        (bind ?mascotas-actual (send ?actual get-mascotas))
        (if (eq ?mascota TRUE)
            then
                (if (eq ?mascotas-actual "true")
                    then (bind ?valor (+ ?valor 10))
                else
                    then
                        (bind ?valor (- ?valor 9999))
                )
        )

        ;Coche
        (bind ?garaje-actual (send ?actual get-garaje))
        (if (eq ?coche TRUE)
            then
                (if (eq ?garaje-actual "true")
                    then (bind ?valor (+ ?valor 20))
                )
        else 
            then
                (bind ?x (dist-transporte ?actual))
                (if (eq ?x 1)
                    then (bind ?valor (+ ?valor 10))
                )

                (if (eq ?x 2)
                    then (bind ?valor (+ ?valor 20))
                )
        )
        (printout t ?actual crlf)
        (printout t ?valor crlf)
        (if (>= ?valor 0) then (make-instance (gensym) of Adecuada (vivienda ?actual) (puntuacion ?valor)))
        (if (< ?valor 0) then (make-instance (gensym) of NoAdecuada (vivienda ?actual) (puntuacion ?valor)))
    )

    (assert (mostrar-resultado))
    (retract ?puntero)
)


;Impresion de los resultados
(deffunction print-vivienda (?vivienda)
				(printout t " "(instance-name ?vivienda) " " crlf)
				(printout t crlf)
)

(defrule mostrar-resultado "Muestra los resultados"
	?p <- (mostrar-resultado)
	=>
	(bind ?limit (length$ (find-all-instances ((?inst Candidata)) TRUE)))
	(bind ?n 0)
	(bind ?total 0)

	(printout t crlf)	
	(bind ?soluciones (find-all-instances ((?inst Adecuada)) TRUE))
	(printout t crlf)
	(if (and (not (eq (length$ ?soluciones) 0)) (< ?n ?limit)) then (printout t "-------------Viviendas adecuadas-------------" crlf))
	(loop-for-count (?i 1 (length$ ?soluciones)) do
		(if (< ?n ?limit) then
			(bind ?curr (nth$ ?i ?soluciones))
			(printout t crlf)
			(printout t "-> Vivienda " (+ ?total ?i) ":")
			(print-vivienda (send ?curr get-vivienda))
			(printout t "______________________________" crlf)
			(bind ?n (+ ?n 1))
		)
	)
	(bind ?total (+ ?total (length$ ?soluciones)))

	(bind ?soluciones (find-all-instances ((?inst NoAdecuada)) TRUE))
	(printout t crlf)
	(if (and (not (eq (length$ ?soluciones) 0)) (< ?n ?limit)) then (printout t "NoAdecuada:" crlf))
	(loop-for-count (?i 1 (length$ ?soluciones)) do
		(if (< ?n ?limit) then
			(printout t crlf)
			(bind ?curr (nth$ ?i ?soluciones))
			(printout t "-> Vivienda " (+ ?total ?i) ":")
			(print-vivienda (send ?curr get-vivienda))
			(printout t "______________________________" crlf)
			(bind ?n (+ ?n 1))
		)
	)
	
	(printout t crlf)
	(printout t "Adios!" crlf)
	(printout t crlf)
	(retract ?p)
)