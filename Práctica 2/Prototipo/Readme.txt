INSTRUCCIONES PARA EJECUTAR EN CLIPS:
(reset)
(clear)
(load MiOntologia.pont)
(load-instances MiOntologia.pins)
(load reglas.clp)
(run)

En caso de que no funcione esa ejecución usa esta otra:
(reset)
(clear)
(load MiOntologia.pont)
(load-instances MiOntologia.pins)
(load reglas.clp)
(assert(initial-fact))
(run)

De momento nuestro prototipo inicial tiene 10 instancias,
4 pisos, 3 chalets y 3 duplexs. De momento tenemos todo lo
que define una vivienda, si tiene ascensor, si permite mascotas
su precio, su superfície, etc etc.

Tenemos también un servicio como instancia de momento

A partir de 5 preguntas es capaz de clasificarte las viviendass
en Adecuadas y NoAdecuadas gracias a la puntuación subjetiva
que le damos a la comparación entre lo que quieres y cómo es cada vivienda
generándote una solución.
