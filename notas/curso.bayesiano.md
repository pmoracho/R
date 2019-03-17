# Bayesian Statistics: From Concept to Data Analysis

Notas del curso


## Estadísticas Bayesianas: Del concepto al análisis de datos

### Lección 1.1 Probabilidad clásica y frecuentista

Estadísticas es el estudio de la incertidumbre. ¿Cómo la medimos? ¿Y cómo tomamos
las decisiones en presencia de ella? Una de las formas de tratar la
incertidumbre, de una manera más cuantificada, es pensar en las probabilidades.
Pensemos en algunos ejemplos de probabilidades. Supongamos que estamos lanzando
un dado de seis caras y queremos preguntar cuál es la probabilidad de que el
dado muestre un cuatro.

Podríamos hacer otras preguntas, por ejemplo, ¿es una decisión justa? ¿Tiene
sentido preguntar, cuál es la probabilidad de que la decisión sea justa?
Podríamos hacer preguntas como, ¿cuál es la probabilidad de que llueva mañana?
Si estamos tratando con Internet, es posible que tengamos un enrutador que esté
pasando información. Podemos preguntar, ¿cuál es la probabilidad de que se le
caiga un paquete?  Es posible que estemos comparando enrutadores de dos
compañías diferentes. Y podemos preguntarnos, ¿cuál es la probabilidad de que
un enrutador de una empresa sea más fiable que un enrutador de otra empresa?
Podemos hacernos más preguntas existenciales como, ¿cuál es la probabilidad de
que el universo siga expandiéndose para siempre?  Hay tres marcos diferentes
bajo los cuales podemos definir las probabilidades. Hablaremos de todos ellos
brevemente aquí. El primero es el marco clásico.  El segundo, es un marco de
trabajo de Frecuentista, y el tercero es un marco de trabajo Bayesiano.  Bajo
el marco clásico, los resultados que son igualmente probables tienen las mismas
probabilidades. Así que en el caso de tirar un dado imparcial, hay seis
posibles resultados, todos son igualmente probables. Así que la probabilidad de
tirar un cuatro, en un dado de seis caras, es sólo una en seis.  Podríamos
hacer una pregunta relacionada, que es cuál es la probabilidad de obtener una
suma de cuatro en un par de tiros. Si tiramos dos dados, dos imparciales dados
de seis caras. Entonces, ¿cuál es la probabilidad de que su suma muestre un
cuatro? Bueno, si pensamos en esto, ¿cuántos resultados igualmente probables
son posibles en un par de dados? Hay seis resultados igual de probables en la
primera serie. Hay seis resultados igual de probables en el segundo dado. Así
que hay un total de 6 por 6, o 36 posibles resultados igualmente probables en
el par. De esos resultados, ¿cuántos tendrán una suma de cuatro?  Podríamos
tirar un uno, en el primer dado y un tres en el segundo, un dos en el primero y
dos en el segundo, o un tres en el primero y un uno en el segundo. Así que hay
un total de 3 posibles resultados de 36 igualmente probables, y esa es una
probabilidad de 1 en 12. Este enfoque clásico funciona muy bien y tenemos
resultados igualmente probables o resultados igualmente probables y bien
definidos. Eso es muy difícil de aplicar en cualquiera de estos otros casos.

Luego podemos pasar a una definición frecuentista.  La definición Frecuentista,
requiere que tengamos una hipotética secuencia infinita de eventos, y luego
miremos la frecuencia relevante, en esa hipotética secuencia infinita.  En el
caso de tirar un dado, un dado de seis caras, podemos pensar en tirar el dado
un número infinito de veces, esto es bastante sencillo. Si es un dado justo, si
tiras un número infinito de veces, entonces una sexta parte del tiempo,
obtendremos un cuatro. Y así podemos continuar definiendo la probabilidad de
lanzar cuatro en un dado de seis caras como uno en seis.  Esto también se
aplica a situaciones como el tráfico de Internet que pasa a través de un
enrutador.  Si perdemos 1 de cada 10.000 paquetes, podemos definir la
probabilidad como 1 de cada 10.000.  Este enfoque funciona muy bien cuando
podemos definir una secuencia infinita hipotética. Pero entonces podemos hacer
otras preguntas, y se vuelven más complicadas bajo este enfoque.  Por ejemplo,
¿cuál es la probabilidad de que llueva mañana? En este caso, tenemos que pensar
en la hipotética secuencia infinita del mañana, y ver qué fracción de estos
infinitos posibles mañanas tiene lluvia, lo cual es un poco extraño de pensar.
O, en el caso de la pregunta, ¿es este un dado justo?  Bueno, si tenemos un
dado físico en particular, y estamos preguntando si es un dado justo, entonces
podemos tirarlo muchas veces, pero eso no va a cambiar si es o no un dado
justo.  Así que o es justo, o no es justo. Todos estos tiros no van a cambiar
eso.  Y así, bajo el paradigma del frecuentista, esta probabilidad es 0 o 1. Es
cero si no es una apuesta justa y lo es si es una apuesta justa. Esta no es
exactamente una respuesta intuitiva. En el caso del universo expandiéndose para
siempre, podemos preguntarnos, si esto es un universo determinista y sucede lo
mismo, entonces de nuevo, la respuesta va a ser cero o uno porque cada vez que
jugamos a la expansión del universo, o se expandirá para siempre o no. Por otro
lado, podríamos suscribirnos a algo así como una teoría del multiverso, donde
hay un número infinito de universos paralelos que pueden existir. En ese caso,
podemos considerar esta colección infinita y preguntarnos ¿qué fracción de esta
colección infinita tiene universos que se expanden para siempre?  Dependiendo
de lo que sepamos sobre el universo, podríamos obtener respuestas diferentes.
El enfoque frecuentista trata de ser objetivo en la definición de
probabilidades. Pero como puede ver, puede encontrarse con algunas cuestiones
filosóficas profundas.  A veces la objetividad es simplemente ilusoria.  A
veces también recibimos interpretaciones que no son particularmente intuitivas.


### Lección 1.2 Probabilidad y coherencia bayesiana

Otra forma de definir probabilidades es bajo la perspectiva bayesiana. La perspectiva bayesiana es una perspectiva personal.
Su probabilidad representa su propia perspectiva, es su medida de incertidumbre, y toma en cuenta lo que usted sabe sobre un problema en particular. Pero en este caso, es el personal y, por lo tanto, lo que ves puede ser diferente de lo que otra persona cree. Por ejemplo, quieres preguntar si es una apuesta justa. Si usted tiene información diferente a la de otra persona, entonces su probabilidad de un dado es justa, puede ser diferente a la probabilidad de esa persona. Así que es inherentemente un enfoque subjetivo de la probabilidad, pero puede funcionar bien en una base matemáticamente rigurosa, y conduce a resultados mucho más intuitivos en muchos casos que el enfoque de Frecuentista.
Podemos cuantificar las probabilidades pensando en lo que es una apuesta justa.
Así que, por ejemplo, queremos preguntar: ¿cuál es la probabilidad de que llueva mañana?
Entonces podemos preguntarle acerca de una apuesta que usted podría estar dispuesto a aceptar si cree que es justa. Suponga que está dispuesto a aceptar la apuesta de que si llueve mañana, usted gana $4. Si no llueve mañana, pierde $1, o lo que sea su moneda local.
Usted puede pensar en esto como probabilidades de 4 a 1. Si usted piensa que la apuesta, la apuesta justa que usted debe estar dispuesto a tomar en cualquier dirección que también significaría
Si llueve, pierdes $4. Y si no llueve, ganas $1. Si crees que ambos son justos, entonces estás definiendo la probabilidad de lluvia como 1 sobre 1 más 4 que es igual a 1 y 5. Podemos ver que esto es justo si observamos su rentabilidad esperada.
Bajo la primera apuesta, su retorno esperado es que usted gana 4 con probabilidad 1 y 5 y pierde 1 con probabilidad 4 y 5. Y puedes ver que es una apuesta justa de 0. En el segundo caso, ganas 1 con probabilidad de 4 y 5, y pierdes 4 con probabilidad de 1 y 5, y eso también es 0. Así que se equilibra.
Sólo se equilibra si sus probabilidades coinciden.
Por lo tanto, puede utilizar este marco de apuestas para pensar cuál es su probabilidad personal en función de las apuestas que estaría dispuesto a realizar. En la mayoría de los casos, es bastante fácil ponerlo entre paréntesis. Aceptas una apuesta que era de mil y una a favor, pero no aceptas una que era de mil y una en contra. Así que tu probabilidad personal está en algún punto intermedio.
Por último, quiero mencionar el concepto de coherencia. Las probabilidades deben seguir todas las reglas estándar de probabilidad, las que fueron definidas en el material suplementario de esta conferencia. Si no sigues todas las reglas de la probabilidad, entonces puedes ser incoherente, lo que lleva a que alguien construya una serie de apuestas en las que se garantiza que perderás dinero. A esto se le llama libro holandés. Si se siguen todas las reglas y se sigue el marco de las estadísticas bayesianas, se puede garantizar la coherencia.
