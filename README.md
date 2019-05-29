# Proyecto: Traductores e Interpretadores
## Basic Tran Parser

[Basic Tran](https://github.com/AuriCA32/Basic-Tran-Parser/blob/master/Enunciados/Proyecto.pdf) es un lenguaje parecido a Fortran pero un poco más sencillo, diseñado para programar tareas simples. El presente proyecto tuvo como objetivo desarrollar un interpretador (parser) que reconociera el lenguaje y ejecutara el programa escrito en dicho lenguaje, por medio de uso de `lex` y `yacc` de la librería `ply` y la creación de estructuras para el manejo de datos.

Este parser de Basic Tran es capaz de:

* Reconocer el tokens válidos del lenguaje Basic Tran y abortar con mensaje de error de introducir algún caracter inválido por medio del uso del lexer ply.lex.

* Reconocer frases del lenguaje de acuerdo a un ordenamiento específico de tokens, cuyas reglas fueron diseñadas por el equipo de trabajo, y el contexto en el cual ocurre la frase en particular.

* Crear el árbol sintáctico (AST).

* Resolver las operaciones descritas en el AST (decorar).

En el presente archivo se relatan todos los detalles pertinentes relacionados con la siguiente fase de implementacion del lexer y parser presente en el archivo `main.py` de directorio root.

El proyecto fue realizado en 4 fases, cada una desarrollando una funcionalidad del parser. A continucación se presentan cada una de ellas:

1. [Analizador Lexicográfico](https://github.com/AuriCA32/Basic-Tran-Parser/blob/master/Enunciados/Fase%201.pdf).

2. Analizador sintáxtico.

3. [Análisis de contexto de Basic Tran](https://github.com/AuriCA32/Basic-Tran-Parser/blob/master/Enunciados/Fase%203.pdf).

4. [Intérprete y verificaciones dinámicas](https://github.com/AuriCA32/Basic-Tran-Parser/blob/master/Enunciados/Fase%203.pdf)

### Autores:

* Aurivan Castro (14-10205@usb.ve)
* Sandra Vera (14-11130@usb.ve)

## Ejecución del programa
    
Para ejecutar el programa, basta con usar el script presente de la siguiente manera:

```Bash
./BasicTran RutaDelArchivo
```

Tambien está disponible la opcion de imprimir "verboso". Para ello puede ejecutar:

```Bash
../BasicTran RutaDelArchivo -v
```

Es necesario mantener ese estricto orden para su correcto funcionamiento.


## De la implementación y otros detalles

### Acerca del Analizador lexicográfico (lexer)

Los tokens fueron definidos con expresiones regulares, uno por uno, o dentro de un diccionario en el caso de las palabras reservadas, dependiendo de su uso en el lenguaje especificado en el enunciado. Las palabras reservadas se colocaron aparte con la finalidad de que se reconocieran por sí solas y no repetidas (Ex. `while` debe ser reconocida como palabra reservada, pero `whilewhile` no).

Aunado a lo anterior, algunos tokens requirieron funciones para su definición, puesto que las expresiones regulares por sí solas no bastaban para definirlos. Entre ellos se encuentran `t_TkNum` para lo digitos y `t_TkId` para los ID. Aparte de esto, fue necesario desarrollar funciones auxiliares para cumplir los requerimientos del enunciado, por ejemplo, una para listar los tokens según su tipo, a manera de imprimirlos una vez procesado todo el input.

A medida que se van reconociendo los tokens, programa los va tomando a una lista llamada `tokensList`. En caso de existir errores, tales como caracteres especiales no permitidos, se agrega un string con el error correspondiente en la lista tokError. En caso de que la lista de errores tenga al menos un error, se imprime ésta; y de lo contrario, se imprime la lista de tokens.

#### Obstáculos y otras circunstancias

Puesto que la librería PLY hace la mayor parte del trabajo, no se presentaron sino un par de problemas al momento de implementar. En primer lugar, dado que Python usar el backslash (“\”) para reconocer caracteres especiales en expresiones regulares, al principio se encontraron muchos inconvenientes para reconocer el `TkConjuncion` (“/\”) y el `TkDisyuncion` (“\/”). Luego de arregladas las expresiones regulares que definen estos dos tokens, a pesar de reconocer de manera correcta estos tokens por sí solos, los encontraba a su vez en input como “\/\” en vez de lanzar un error. Una vez discutido con el preparador de la materia, se concluye que es una falla del lenguaje.

En segunda instancia, se presentó la disyuntiva del reconocimiento de input tales como “45hola” como dos tokens, `TkNum(45)` y `TkId(“hola”)`, o como un error. Después de discusiones con otros equipos de trabajo y el preparador, se ha llegado al acuerdo de que la primera opción es la correcta, pues se trata de un análisis sólo al nivel lexicográfico.

### Acerca del árbol sintáctico y su decoración.

Luego de una inspección sobre los requerimientos del proyecto, fue decidido utilizar una función recursiva para recorrer el árbol ya construido por la librería ply.yacc. Dicha función, al encontrar un nodo con `comienzo` o `beginInterno` de tipo, chequea si tiene declaraciones disponibles, en cuyo caso recorre la rama izquierda con las declaraciones, las agrega a la lista del diccionario, y luego baja por la rama derecha con las instrucciones.

Baja hasta las hojas del arbol, y va decorando y consiguiendo y adjuntando errores a medida que sube a los padres. Para ello se usan funciones y atributos auxiliares definidas en la clase `Nodo`, especialmente la función calc_tipo, que ve el tipo y el valor de los hijos del nodo para calcular su respectivo tipo y valor.

Al finalizar, el programa imprime los diccionarios y el árbol, de no haber errores.
