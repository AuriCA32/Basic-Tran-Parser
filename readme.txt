=========README=========


    En el presente archivo se relatan todos los detalles pertinentes relacionados
con la implementacion del parser presente en el archivo main.py de este directorio.


*****Como correr el programa*****
    
    Para correr el programa, basta con usar el script presente de la siguiente
manera:

    ./BasicTran <RutaDelArchivo>

    Tambien esta disponible la opcion de imprimir como arbol parentizado. Esta
opcion fue incluida a manera de garantizar que el arbol esta siendo bien construido.
Para ello puede ejecutar:

    ../BasicTran <RutaDelArchivo> -b

    Es necesario mantener ese estricto orden para su correcto funcionamiento.


*****Casos de prueba*****

    Tambien fueron incluidos casos de prueba. Estos se encuentran en el archivo
CdePrueba. Para correrlos, basta con hacer:
    
    ./probarCasos CdePrueba

    Se generara un archivo outputcasos en el directorio con la respuesta a cada caso.
    
    De igual forma, para imprimir el arbol parentizado:

    ./probarCasos CdePrueba -b


*****Acerca de la gramatica*****

    Si bien en los requerimientos se exigia una gramatica a izquierda y ambigua, el
equipo decidio hacerla a derecha y no ambigua, primero porque toda la documentacion
esta a derecha y segundo porque por sugerencia del profesor de teoria, no podía
haber ambiguedad. Asimismo, el profesor recomendo colocar una ruta de inicio que
fuera por un solo camino (por lo cual hay una regla que solo redirecciona al start).

    En la definicion de p_start, se colocaron las reglas para indicar los posibles
patrones a seguir por un programa del lenguaje, por ejemplo, que tenga definiciones
o no, que tenga un bloque begin vacio, etc.

    Las declaraciones se colocaron como redirecciones desde declaracionVar.
declaracionVar indica que se puede derivar en TkVar con algo mas. Se puede declarar
un int, char o bool (por declaracionId) o un arreglo (por declaracionArray), ambos
metodos con sus respectivas reglas. declaracionId se subdivide en para int, bool, y
char, pues los tokens terminales que determinan el tipo son diferentes.
declaracionArray tiene define el tipo por medio de la derivacion a la regla tipo,
que lleva a los diferentes terminales.

    Las instrucciones del bloque begin se encuentran agrupadas en p_cond. Para
instrucciones como if[otherwise],for,while,read y print, redirecciones a la
definicion particular para esa instruccion. Se incluyen otras reglas para que
considere bloques with-begin internos (aun cuando no haya declaraciones y/o), y vea
asignaciones, ya sea a una variable o a un elemento de un arreglo.

    Es importante destacar que dicho elemento de arreglo puede estar dentro de otro
arreglo, es decir, instrucciones como a[0][1] <- 1; son reconocidas con esta gramatica.
A su vez, se permite referenciar un rango del arreglo, como a[1,2].

    Las operaciones sobre cualquier tipo de dato estan agrupadas en una sola definicion
llamada p_operacion. Con esto se redujeron los conflictos (ademas de con ayuda de las
precedencias). Aqui se incluyen los terminales Id, num, true, false o Caracter.


*****De la construccion del arbol*****

    En la implementacion, fue decidido utilizar la clase nodo para modelar los nodos
del arbol. La misma guarda un string que indica el tipo de lo que está en el nodo, la hoja
o lo que identifica la expresion, y por ultimo un arreglo con los nodos u hojas hijas.

    Debajo de cada funcion, se definio como se tomarian los nodos y se retornarian para
ser unidos a su padre. De esta forma, el yacc construye el arbol de abajo hacia arriba.