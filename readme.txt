=========README=========


    En el presente archivo se relatan todos los detalles pertinentes relacionados
con la implementacion del parser presente en el archivo main.py de este directorio.


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

    Las instrucciones del bloque begin se encuentran agrudas en p_cond. Para
instrucciones como if[otherwise],for,while,read y print, redirecciones a la
definicion particular para esa instruccion. Se incluyen otras reglas para que
considere bloques with-begin internos (aun cuando no haya declaraciones y/o)