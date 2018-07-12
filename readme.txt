#README


    En el presente archivo se relatan todos los detalles pertinentes relacionados
con la siguiente fase de implementacion del parser presente en el archivo main.py
de este directorio.


*****Como correr el programa*****
    
    Para correr el programa, basta con usar el script presente de la siguiente
manera:

    ./BasicTran <RutaDelArchivo>

    Tambien esta disponible la opcion de imprimir "verboso". Para ello puede ejecutar:

    ../BasicTran <RutaDelArchivo> -v

    Es necesario mantener ese estricto orden para su correcto funcionamiento.


*****De la implementación y otros detalles*****

    Luego de una inspección sobre los requerimientos del proyecto, fue decidido utilizar
una función recursiva para recorrer el árbol ya construido por la librería ply.yacc. Dicha
función, al encontrar un nodo con "comienzo" o "beginInterno" de tipo, chequea si tiene
declaraciones disponibles, en cuyo caso recorre la rama izquierda con las declaraciones,
las agrega a la lista del diccionario, y luego baja por la rama derecha con las
instrucciones.

    Baja hasta las hojas del arbol, y va decorando y consiguiendo y adjuntando errores
a medida que sube a los padres. Para ello se usan funciones y atributos auxiliares
definidas en la clase Nodo, especialmente la función calc_tipo, que ve el tipo y el valor
de los hijos del nodo para calcular su respectivo tipo y valor.

    Al finalizar, el programa imprime los diccionarios y el árbol, de no haber errores.