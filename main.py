import ply.lex as lex
import ply.yacc as yacc
import sys
import re
from collections import OrderedDict
from collections import deque

#Listas de tokens y errores
tokError=[] #Global, si esta vacio no hay errores
errores_sintacticos=[]
errores_contexto=[]
tokensList=[]

# Lista de nombres de palabras reservadas
reserved={
	'with' : 'TkWith',
	'begin' : 'TkBegin',
	'end' : 'TkEnd',
	'int' : 'TkInt',
	'bool' : 'TkBool',
	'char' : 'TkChar',
	'array' : 'TkArray',
	'of' : 'TkOf',
	'if' : 'TkIf',
	'otherwise' : 'TkOtherwise',
	'while' : 'TkWhile',
	'read' : 'TkRead',
	'var' : 'TkVar',
	'for' : 'TkFor',
	'from' : 'TkFrom',
	'to' : 'TkTo',
	'step' : 'TkStep',
	'print' : 'TkPrint',
	'true' : 'TkTrue',
	'false' : 'TkFalse',
	'not' : 'TkNegacion'
}

#Lista de nombres de tokens
tokens = [
	'TkComa',
	'TkPunto',
	'TkPuntoComa',
	'TkDosPuntos',
	'TkParAbre',
	'TkParCierra',
	'TkCorcheteAbre',
	'TkCorcheteCierra',
#	'TkLlaveAbre',
#	'TkLlaveCierra',
	'TkHacer',
	'TkAsignacion',
	'TkSuma',
	'TkResta',
	'TkMult',
	'TkDiv',
	'TkMod',
	'TkConjuncion',
	'TkDisyuncion',
	'TkMenor',
	'TkMenorIgual',
	'TkMayor',
	'TkMayorIgual',
	'TkIgual',
	'TkDiferente',
	'TkSiguienteCar',
	'TkAnteriorCar',
	'TkValorAscii',
	'TkConcatenacion',
	'TkShift',
	'TkNum',
	'TkId',
	#'TKerror',
	'TkCaracter',
	'TkMenos'
] + list(reserved.values())


# Definiciones de los tokens
t_TkComa  = r','
t_TkPunto = r'\.'
t_TkPuntoComa = r';'
t_TkDosPuntos = r':'
t_TkParAbre = r'\('
t_TkParCierra = r'\)'
t_TkCorcheteAbre  = r'\['
t_TkCorcheteCierra  = r'\]'
#t_TkLlaveAbre = r'\{'
#t_TkLlaveCierra = r'\}'
t_TkHacer = r'->'
t_TkAsignacion = r'<-'
t_TkSuma  = r'\+'
t_TkResta = r'-'
t_TkMenos = r'-'
t_TkMult  = r'\*'
t_TkDiv = r'/'
t_TkMod = r'%'
t_TkConjuncion = r'/\\'
t_TkDisyuncion  = r'\\/'
t_TkMenor = r'<'
t_TkMenorIgual  = r'<='
t_TkMayor = r'>'
t_TkMayorIgual  = r'>='
t_TkIgual = r'='
t_TkDiferente = r'/='
t_TkSiguienteCar  = r'[+][+]'
t_TkAnteriorCar = r'--'
t_TkValorAscii  = r'\#'
t_TkConcatenacion = r'::'
t_TkShift = r'\$'
t_TkCaracter = r'[\'\"][a-zA-Z0-9][\'\"]|[\'\"][\t][\'\"]|[\'\"][\n][\'\"]|[\'\"][\'][\'\"]|[\'\"][\\][\'\"]' #0-9 ni espacio

# String con caracteres ignorados (espacios y tab)
t_ignore  = ' \t'


###############################################
########### Funciones para el lexer ###########
###############################################

# Regla para identificar numeros
def t_TkNum(t):
	r'\d+'
	t.value = int(t.value)    
	return t

# Identifica una nueva linea
def t_newline(t):
	r'\n+'
	t.lexer.lineno += len(t.value)

# Handler para errores
def t_error(t):
	tokError.append("Error: Caracter inesperado \"%s\"" % t.value[0] +" en la fila "+str(lexer.lineno)+", columna "+encontrar_col(data,t))
	t.lexer.skip(1)

# Regla para identificadores
def t_TkId(t):
	r'[a-zA-Z][_a-zA-Z_0-9]*'
	t.type = reserved.get(t.value,'TkId')    # Check for reserved words
	return t

#Funcion para encontrar columna en el input del lexer
def encontrar_col(linea, token):
	inicio_linea=linea.rfind('\n',0,token.lexpos) + 1
	return str((token.lexpos - inicio_linea) + 1)

#Funcion auxiliar para imprimir caracteres '\n' y '\t'
def get_especial(tok):
	if (tok.value=='\'\n\''):
		return '\''+'\\'+'n'+'\''
	elif (tok.value=='\'\t\''):
		return '\''+'\\'+'t'+'\''
	else:
		return tok.value

#Funcion para listar el string del token dependiendo de su valor
def listar_token(tok):
	if(reserved.get(tok.value)!=None): #Reserved Word
		tokensList.append(tok.type+" "+str(lexer.lineno)+" "+encontrar_col(data,tok))
	elif(tok.type=='TkId'): #Variables
		tokensList.append(tok.type+"(\""+tok.value+"\") "+str(lexer.lineno)+" "+encontrar_col(data,tok))
	elif(tok.type=='TkNum'): #Numeros
		tokensList.append(tok.type+"("+str(tok.value)+") "+str(lexer.lineno)+" "+encontrar_col(data,tok))
	elif(tok.type=='TkCaracter'): #Caracteres
		tokensList.append(tok.type+"("+get_especial(tok)+") "+str(lexer.lineno)+" "+encontrar_col(data,tok))
	else:
		tokensList.append(tok.type+" "+str(lexer.lineno)+" "+encontrar_col(data,tok))

#Funcion para imprimir la lista de tokens o errores dependiendo del caso
def print_tokens_or_errors():
	if(len(tokError)>0):
		for i in range(len(tokError)):
			print(tokError[i])
		return -1
	if(len(errores_sintacticos)>0):
		print(errores_sintacticos[0])
		return -1
	return 0
	# else:
	# 	for i in range(len(tokensList)):
	# 		print(tokensList[i])

#Funcion que lee un archivo y lo retorna como string
def read_given_file(gfile):
	try:
		file = open(gfile,"r")
		return file.read()
	except IOError as e:
		print("I/O error({0}): {1}".format(e.errno, e.strerror))
		exit()
	except:
		print("Unexpected error:", sys.exc_info()[0])
		exit()

################################################
########### Funciones para el parser ###########
################################################

#Precedencia de variables, menor precendencia arriba
precedence=(
	('left', 'TkMayor', 'TkMenor', 'TkMayorIgual', 'TkMenorIgual', 'TkIgual','TkDiferente'),
	('left', 'TkSuma', 'TkResta'),
	('left', 'TkMult', 'TkDiv', 'TkMod'),
	('left', 'TkConjuncion', 'TkDisyuncion'),
	('right', 'TkMenos', 'TkNegacion'),
	('left', 'TkSiguienteCar','TkAnteriorCar'),
	('right', 'TkValorAscii'),
	('left', 'TkConcatenacion'),
	('right', 'TkShift'),
	('left', 'TkCorcheteCierra'),
	('right', 'TkCorcheteAbre'),
	)

declaracion=["secuencia_declaraciones",
"secuencia_declaracionId",
"secuencia_declaracionIdNum",
"declaracionIdNum",
"secuencia_declaracionIdChar",
"declaracionIdChar",
"secuencia_declaracionIdBool",
"declaracionIdBool",
"declaracionArray",
"secuencia_declaracionArray"]

operacion=["operacion-suma",
"operacion-resta",
"operacion-multiplicacion",
"operacion-division",
"operacion-modulo",
"operacion-punto",
"operacion-conjuncion",
"operacion-disyuncion",
"operacion-igual",
"operacion-diferente",
"operacion-menor",
"operacion-menorIgual",
"operacion-mayor",
"operacion-mayorIgual",
"operacion-menosUnario",
"operacion-negacion"]

arreglo=["concatenacion",
"accederEnArreglo",
"shift",
"arrayInterno",
"rangoArreglo",
"secuencia-accederEnArreglo"]

caracter=["siguienteChar",
"anteriorChar",
"valorAscii"]

lista_repetidas=deque([])
lista_diccionarios=deque([])
diccionario=OrderedDict()
repetidas=deque([])
lista_values=deque([])
values=OrderedDict()
lista_repetidas_aux=deque([])
lista_diccionarios_aux=deque([])
lista_values_aux=deque([])
altura = 0
errorFor = False

def __getVarType__(node):
	if isinstance(node,Node) and "declaracion" in node.type:
		return __getVarType__(node.children[len(node.children)-1])
	else:
		return node

# Retorna la dimensión y tipo del array en una lista con el formato:
# 	[i,j,,k...,type] con i,j,k... enteros y type = ("bool"|"int"|"char")
def __getListArrayType__(node):
	types = []
	if isinstance(node,Node):
		y = buildtree(node)
		types.append(node.children[len(node.children)-2])
		temp = __getListArrayType__(node.children[len(node.children)-1])
		for i in temp:
			types.append(i)
	else:
		types.append(node)
	return types

# Retorna la dimensión y tipo del array en un string con el formato:
# 	[i][j][k]...(type) con i,j,k... enteros y type = ("bool"|"int"|"char")
def __getArrayTypeString__(node):
	types = __getListArrayType__(node)
	string=""
	for i in types:
		if isinstance(i,int):
			string+="["+str(i)+"]"
		else:
			string+="("+i+")"
	return string

# Dada la declaración de un arreglo dado en formato string como en la
# función anterior, retorna un arreglo ya creado con las dimensiones
# especificadas.
def __getArrayFromDeclaration__(node):
	types = __getListArrayType__(node)
	finalArray = []
	tempArray = None
	for p in range(len(types)-1,-1,-1):
		i = types[p]
		tempArray = []
		if isinstance(i,int):
			for m in range(i):
				tempArray.append(None)
			if finalArray==[]:
				finalArray = tempArray.copy()
			else:
				for num in range(len(tempArray)):
					tempArray[num] = finalArray.copy()
				finalArray = tempArray.copy()
	return finalArray

def __searchElementinDictReturnDict__(nodeStr):
	diccionario = None
	valores = None
	diccionario_aux = deque([])
	valores_aux = deque([])
	found = False
	while lista_diccionarios_aux and lista_values_aux:
		diccionario=lista_diccionarios_aux.popleft()
		valores=lista_values_aux.popleft()
		diccionario_aux.append(diccionario)
		valores_aux.append(valores)
		if str(nodeStr) in diccionario.keys(): #Si esta en algun diccionario
			found=True
			break
	#Restaurar listas
	while diccionario_aux and valores_aux:
		diccionario = diccionario_aux.popleft()
		valores=valores_aux.popleft()
		lista_diccionarios_aux.append(diccionario)
		lista_values_aux.append(valores)
	if not found:
		diccionario = None
		valores = None
	return [diccionario,valores]

def __getDetailsFromDeclaration__(strDeclaracion):
	strDeclaracion = strDeclaracion.strip("array")
	temp = strDeclaracion.split("]")
	for p in range(len(temp)):
		i = temp[p]
		if "[" in i:
			i = i.strip("[")
			temp[p] = int(i)
		elif "(" in i:
			i = i.strip(")")
			temp[p] = i.strip("(")
	return temp

def __checkReturnArrayElement__(node):
	detailsArrayElement = __getListArrayType__(node)
	numerrores = len(errores_contexto)
	print("la lista obtenida es "+str(detailsArrayElement))
	for p in range(len(detailsArrayElement)):
		i = detailsArrayElement[p]
		print("trabajando con " + str(i))
		if isinstance(i,str) and i!="true" and i!="false" and "\"" not in i and "\'" not in i:
			print("es un id")
			[diccionario,valores] = __searchElementinDictReturnDict__(i)
			if diccionario==None:
				print("no está en el diccionario")
				errores_contexto.append("Error: variable "+i+" no declarada, línea "+str(node.linea)+".")
				if p==0:
					break
			elif diccionario!=None and p!=0 and diccionario[str(i)]!="int":
				print("Está en el diccionario pero no es int")
				errores_contexto.append("Error: El acceso a una posición del arreglo "+detailsArrayElement[0]+" sólo es posible con variables de tipo int, pero se encontró "+str(i)+" de tipo "+diccionario[str(i)]+". Linea No. "+str(node.linea)+".")
			elif diccionario!=None and p!=0 and diccionario[str(i)]=="int":
				print("Está en el dict y es int")
				detailsArrayElement[p] = valores[str(i)]
		elif isinstance(i,str) and (i=="true" or i=="false"):
			print("es un bool")
			errores_contexto.append("Error: El acceso a una posición del arreglo "+detailsArrayElement[0]+" sólo es posible con variables de tipo int, pero se encontró "+str(i)+" de tipo bool. Linea No. "+str(node.linea)+".")
		elif isinstance(i,str) and ("\"" in i or "\'" in i):
			print("es un char")
			errores_contexto.append("Error: El acceso a una posición del arreglo "+detailsArrayElement[0]+" sólo es posible con variables de tipo int, pero se encontró "+str(i)+" de tipo char. Linea No. "+str(node.linea)+".")
	print("la lista actualizada es "+str(detailsArrayElement))
	if len(errores_contexto)>numerrores:
		print("Se generaron errores al acceder en el arreglo. Return")
		return None
	detailsArrayElement.reverse()
	arrayname = detailsArrayElement.pop()
	detailsArrayElement.reverse()
	print("El nombre del arreglo es " + str(arrayname)+" y sus especificaciones aqui son "+str(detailsArrayElement))
	[diccionario,valores] = __searchElementinDictReturnDict__(arrayname)
	detailsfromDeclaration = __getDetailsFromDeclaration__(diccionario[str(arrayname)])
	tipeofarray = detailsfromDeclaration.pop()
	print("Los detalles obtenidos de la declaracion son "+str(detailsfromDeclaration))
	for i in range(len(detailsArrayElement)):
		if detailsArrayElement[i] >= detailsfromDeclaration[i]:
			print("Index del array out of range.")
			errores_contexto.append("Error: Index fuera de rango. Linea No. "+str(node.linea)+".")
	if len(detailsArrayElement)>len(detailsfromDeclaration):
		print("se está intentando acceder en arreglos internos que no existen")
		errores_contexto.append("Error: Se intenta acceder a arreglos internos que no existen. Linea No. "+str(node.linea)+".")
	if len(errores_contexto)>numerrores:
		print("Se generaron errores al acceder en el arreglo. Return")
		return None
	return [valores,arrayname,detailsArrayElement,detailsfromDeclaration,tipeofarray]

def __modifyArray__(listofparameters,node,linea):
	print("se va modificar el arreglo")
	[valoresArray,arrayname,detailsArrayElement,detailsfromDeclaration,tipeofarray] = listofparameters
	numerrores = len(errores_contexto)
	if isinstance(node,str):
		print("el hijo izquierdo es un string")
		if node=="true" or node=="false":
			print("es un bool")
			nodetype = "bool"
			nodevalue = node
		elif "\"" in node or "\'" in node:
			print("es un char")
			nodetype = "char"
			nodevalue = node
		else:
			print("es un id")
			[dictnode,valnode]=__searchElementinDictReturnDict__(node)
			if dictnode==None:
				errores_contexto.append("Error: variable "+node+" no declarada, línea "+str(linea)+".")
			nodetype = dictnode[str(node)]
			nodevalue = valnode[str(node)]
	elif isinstance(node,int):
		print("el hijo izquierdo es un int")
		nodetype = "int"
		nodevalue = node
	else: #es una instancia de la clase nodo
		print("el hijo izquierdo es un nodo")
		nodetype = node.tipo_var
		nodevalue = node.value
		linea = node.linea
	print("el hijo izq es de tipo "+str(nodetype)+" y tiene valor "+str(nodevalue))
	print("el hijo derecho tiene tipo "+str(tipeofarray))
	if len(detailsArrayElement)==len(detailsfromDeclaration):
		print("Coincide el acceso con lo que dice la declaración")
		if nodetype!=tipeofarray:
			print("ERROR-array")
			errores_contexto.append("Error: operación asignación sobre tipos incompatibles en la línea "+str(linea)+".")
	else:
		print("El hijo izquierdo tiene que ser arreglo porque el derecho no llega al fondo")
		#el otro lado tiene que ser un arreglo y del mismo tipo
		nodetypelist = __getDetailsFromDeclaration__(nodetype)
		print("los detalles del hijo izq son "+str(nodetypelist))
		nodetype = nodetypelist.pop()
		temp = len(detailsfromDeclaration) - (len(detailsfromDeclaration) - len(detailsArrayElement))
		detailsfromDeclaration = detailsfromDeclaration[temp:len(detailsfromDeclaration)]
		print(detailsfromDeclaration)
		if nodetype!=tipeofarray or len(detailsfromDeclaration)!=len(nodetypelist):
			print("ERROR-array2")
			errores_contexto.append("Error: operación asignación sobre tipos incompatibles en la línea "+str(linea)+".")
		else:
			for i in range(len(nodetypelist)):
				if detailsfromDeclaration[i]!=nodetypelist[i]:
					errores_contexto.append("Error: operación asignación sobre tipos incompatibles en la línea "+str(linea)+".")
					break
	if len(errores_contexto)>numerrores:
		print("Se generaron errores al modificar el arreglo. Return")
		return
	temp = valoresArray[str(arrayname)]
	detailsArrayElement.reverse()
	# Se accede en el arreglo
	while detailsArrayElement:
		if len(detailsArrayElement)==1:
			break
		i = detailsArrayElement.pop()
		temp = temp[i]
	# Se modifica el valor
	temp[detailsArrayElement[0]] = nodevalue
	return
def __fixBoolValuesInDict__(diccionario,valores):
	for i in diccionario.keys():
		if diccionario[i]=="bool":
			if valores[i]=="true":
				valores[i]=True
			elif valores[i]=="false":
				valores[i]=False

#Crea el arbol
class Node:
	def __init__(self,type,children=None,leaf=None,linea=None):
		self.type = type
		if children:
			self.children = children
		else:
			self.children = [ ]
		self.leaf = leaf
		self.value=None
		self.tipo_var=None
		self.linea=linea
	
	def adjuntarTablaSimbolos(self):
		diccionario=OrderedDict()
		repetidas=deque([])
		values=OrderedDict()
		diccionario,repetidas,values = self.__recorrerDeclaraciones__(diccionario,repetidas,values)
		lista_repetidas_aux.append(repetidas)
		lista_diccionarios_aux.append(diccionario)
		lista_values_aux.append(values)

	def __recorrerDeclaraciones__(self,diccionario,repetidas,values):	
		print("entro")
		print("diccionario")
		print(diccionario)
		print("valores")
		print(values)
		if self==None:
			return diccionario,repetidas,values
		if isinstance(self,Node):
			print(self.type)
			print(self.children)
			if "declaracion" in self.type:
				print("Declaracion en tipo")
				for child in self.children:
					if isinstance(child,Node) and "declaracion" in child.type:
						diccionario,repetidas,values = child.__recorrerDeclaraciones__(diccionario,repetidas,values)
					if isinstance(child,Node) and "asignacion" in self.type:
						print("asignacion en algun tipo de algun hijo")
						values[str(child.children[0])]=str(child.children[1])
						if str(child.children[0]) in diccionario.keys():
							repetidas.append([str(child.children[0]),child.linea])
						diccionario[str(child.children[0])]=__getVarType__(self)
				if len(self.children)==3:
					print("longitud de arreglo de hijos igual a 3")
					if isinstance(self.children[1],Node):
						print("el segundo hijo es un nodo, es una expresion larga")
						decorateTreeDeclaracion(self.children[1])
						y = buildtree(self.children[1])
						print(y)
						values[str(self.children[0])]=str(self.children[1].value)
					else:
						print("el segundo nodo es un terminal")
						if "declaracionArray" in self.type:
							values[str(self.children[0])]=__getArrayFromDeclaration__(self)
						else:
							values[str(self.children[0])]=str(self.children[1])
					if str(self.children[0]) in diccionario.keys():
						repetidas.append([str(self.children[0]),self.linea])
					if "declaracionArray" in self.type:
						diccionario[str(self.children[0])]="array"+__getArrayTypeString__(self)
					else:
						diccionario[str(self.children[0])]=__getVarType__(self.children[2])
				else:
					print("longitud de arreglo de hijos diferente a 3")
					if not isinstance(self.children[0],Node):
						print("el hijo izq no es un nodo")
						values[str(self.children[0])]=None
						if str(self.children[0]) in diccionario.keys():
							repetidas.append([str(self.children[0]),self.linea])
						diccionario[str(self.children[0])]=__getVarType__(self.children[1])
					else:
						print("el hijo izq es un nodo")
						if "asignacion" in self.children[0].type:
							print("el hijo izquierdo es una asignacion")
							node = self.children[0]
							print("se decora el mini arbol de asignacion")
							decorateTreeDeclaracion(node)
							p=buildtree(node)
							print(p)
							if str(node.children[0]) in diccionario.keys():
								repetidas.append([str(node.children[0]),self.linea])
							if isinstance(node.children[1],Node):
								print("el hijo derecho de la asignacion es un arbol")
								values[str(node.children[0])]=str(node.children[1].value)
								diccionario[str(node.children[0])]=node.children[1].tipo_var
							else:
								print("el hijo derecho de la asignacion es un terminal")
								values[str(node.children[0])]=str(node.children[1])
								if isinstance(node.children[1],int):
									diccionario[str(node.children[0])]="int"
								elif node.children[1] in ['true','false']:
									diccionario[str(node.children[0])]="bool"
								else:
									diccionario[str(node.children[0])]="char"
		print("salio")
		__fixBoolValuesInDict__(diccionario, values)
		print("diccionario")
		print(diccionario)
		print("values")
		print(values)
		return diccionario,repetidas,values

	def calc_tipo(self,isDeclaracion):
		global errorFor
		#Vemos los hijos y sus tipos 
		print("calc type del nodo "+self.type)
		print("isDeclaracion "+str(isDeclaracion) )
		diccionario_aux=deque([])
		valores_aux=deque([])
		value_hijo=[]
		type_hijo=[]
		diccionario = None
		valores = None
		diccionario1 = None
		valores1 = None
		diccionario2 = None
		valores2 = None
		i=0
		for i in range(len(self.children)): #Es 2 o 1
			if isinstance(self.children[i],Node):
				value_hijo.append(self.children[i].value)
				type_hijo.append(self.children[i].tipo_var)
			elif isinstance(self.children[i],str): 
				#Si es true o false
				if self.children[i]=="true":
					value_hijo.append(True)
					type_hijo.append("bool")
				elif self.children[i]=="false":
					value_hijo.append(False)
					type_hijo.append("bool")
				#Si tiene ' o "
				elif ("\"" in self.children[i]) or ("\'" in self.children[i]):
					value_hijo.append(self.children[i][1:-1])
					type_hijo.append("char")
				else: 
					if not isDeclaracion:
						print("Buscando "+self.children[i] + "en el diccionario")
						print(lista_diccionarios_aux)
						#Buscamos en la tabla de simbolos la variable 
						declarada=False
						while lista_diccionarios_aux and lista_values_aux:
							diccionario=lista_diccionarios_aux.popleft()
							valores=lista_values_aux.popleft()
							diccionario_aux.append(diccionario)
							valores_aux.append(valores)
							if str(self.children[i]) in diccionario.keys(): #Si esta en algun diccionario
								type_hijo.append(diccionario[self.children[i]])
								if diccionario[self.children[i]] == "int":
									if not isDeclaracion:
										try:
											value_hijo.append(int(valores[self.children[i]]))
										except:
											value_hijo.append(str(valores[self.children[i]]))
									else:
										value_hijo.append("None")
								else:
									value_hijo.append(valores[self.children[i]])
								declarada=True
								if diccionario1!=None:
									diccionario2=diccionario
									valores2=valores
								else:
									diccionario1=diccionario
									valores1=valores
								break
						print("var declarada " +str(declarada))
						while diccionario_aux and valores_aux:
							diccionario = diccionario_aux.popleft()
							valores=valores_aux.popleft()
							lista_diccionarios_aux.append(diccionario)
							lista_values_aux.append(valores)
						
						diccionario=diccionario1
						valores=valores1
						#Si se vacia la cola y no fue declarada
						if not declarada:#Error
							while diccionario_aux and valores_aux:
								diccionario = diccionario_aux.popleft()
								valores=valores_aux.popleft()
								lista_diccionarios_aux.append(diccionario)
								lista_values_aux.append(valores)
							errores_contexto.append("Error: variable "+self.children[i]+" no declarada.")
							return
			elif isinstance(self.children[i],int):
				type_hijo.append("int")
				value_hijo.append(self.children[i])
		
		if "declaracionArray" in self.type:
			while diccionario_aux and valores_aux:
				diccionario = diccionario_aux.popleft()
				valores=valores_aux.popleft()
				lista_diccionarios_aux.append(diccionario)
				lista_values_aux.append(valores)
			return
		print(self.type)
		print("lista de values de los hijos")
		print(value_hijo)
		print("lista de tipos de los hijos")
		print(type_hijo)
		#Si el value de algun hijo es None, no se puede realizar la operacion
		for hijo in value_hijo:
			if hijo=="None" and "asignacion" not in self.type:
				errores_contexto.append("Error: No se puede realizar la operación \""+self.type+"\" sobre variable con valor None en la línea "+str(self.linea)+".")
				return
		
		if len(type_hijo)==2: #operaciones y arreglo sin shift

			if self.type in operacion:
				print("aqui con " + self.type)
				for hijo in type_hijo:
					hijo.strip("array-")
				
				if type_hijo[0]==type_hijo[1]:
					self.tipo_var=type_hijo[0]
				else:
					errores_contexto.append("Error: operación sobre tipos de variables incompatibles en la línea "+str(self.linea)+".")
					while diccionario_aux and valores_aux:
						diccionario = diccionario_aux.popleft()
						valores=valores_aux.popleft()
						lista_diccionarios_aux.append(diccionario)
						lista_values_aux.append(valores)
					return
				if self.tipo_var=="int":
					if "suma" in self.type:
						self.value=value_hijo[0]+value_hijo[1]

					elif "resta" in self.type:
						self.value=value_hijo[0]-value_hijo[1]

					elif "multiplicacion" in self.type:
						self.value=value_hijo[0]*value_hijo[1]

					elif "division" in self.type:
						self.value=value_hijo[0]//value_hijo[1]

					elif "modulo" in self.type:
						self.value=value_hijo[0]%value_hijo[1]

					elif "punto" in self.type:
						self.value=value_hijo[0]-value_hijo[1]

					elif "igual" in self.type:
						self.value = value_hijo[0]==value_hijo[1]
						self.tipo_var = "bool"

					elif "diferente" in self.type:
						self.value = value_hijo[0]!=value_hijo[1]
						self.tipo_var = "bool"

					elif self.type=="operacion-menor":
						self.value = value_hijo[0]<value_hijo[1]
						self.tipo_var = "bool"
						
					elif "menorIgual" in self.type: 
						self.value = value_hijo[0] <= value_hijo[1]
						self.tipo_var = "bool"

					elif self.type=="operacion-mayor":
						self.value = value_hijo[0]>value_hijo[1]
						self.tipo_var = "bool"
						
					elif "mayorIgual" in self.type: 
						self.value = value_hijo[0]>=value_hijo[1]
						self.tipo_var = "bool"
					else:
						print("ERROR1")
						errores_contexto.append("Error: operación sobre tipos de variables incompatibles en la línea "+str(self.linea)+".")
						while diccionario_aux and valores_aux:
							diccionario = diccionario_aux.popleft()
							valores=valores_aux.popleft()
							lista_diccionarios_aux.append(diccionario)
							lista_values_aux.append(valores)
						return
				elif self.tipo_var=="bool":
					if "conjuncion" in self.type:
						self.value = value_hijo[0] and value_hijo[1]

					elif "disyuncion" in self.type:
						self.value = value_hijo[0] or value_hijo[1]

					elif "igual" in self.type:
						self.value = value_hijo[0]==value_hijo[1]

					elif "diferente" in self.type:
						self.value = value_hijo[0]!=value_hijo[1]

					else:
						print("ERROR2")
						errores_contexto.append("Error: operación sobre tipos de variables incompatibles en la línea "+str(self.linea)+".")
						while diccionario_aux and valores_aux:
							diccionario = diccionario_aux.popleft()
							valores=valores_aux.popleft()
							lista_diccionarios_aux.append(diccionario)
							lista_values_aux.append(valores)
						return

				else:
					print("ERROR3")
					errores_contexto.append("Error: operación sobre tipos de variables incompatibles en la línea "+str(self.linea)+".")
					while diccionario_aux and valores_aux:
						diccionario = diccionario_aux.popleft()
						valores=valores_aux.popleft()
						lista_diccionarios_aux.append(diccionario)
						lista_values_aux.append(valores)
					return

			elif self.type in arreglo and self.type!="shift":
				if ("array" in type_hijo[0]) and ("array" in type_hijo[1]):
					if type_hijo[0]==type_hijo[1]:
						self.tipo_var=type_hijo[0]
					else:
						errores_contexto.append("Error: operación sobre tipos de arreglos incompatibles en la línea "+str(self.linea)+".")
						while diccionario_aux and valores_aux:
							diccionario = diccionario_aux.popleft()
							valores=valores_aux.popleft()
							lista_diccionarios_aux.append(diccionario)
							lista_values_aux.append(valores)
						return
				else:
					print("ERROR4")
					errores_contexto.append("Error: operación sobre tipos de variables incompatibles en la línea "+str(self.linea)+".")
					while diccionario_aux and valores_aux:
						diccionario = diccionario_aux.popleft()
						valores=valores_aux.popleft()
						lista_diccionarios_aux.append(diccionario)
						lista_values_aux.append(valores)
					return
				
				if "concatenacion" in self.type:
					pass
				#NO ESTOY CLARA COMO MANEJAR ESTO, hay que poner value
				elif "arrayInterno" in self.type:
					pass
				elif "rango" in self.type:
					pass
				elif "secuencia" in self.type:
					pass
				else:
					print("ERROR5")
					errores_contexto.append("Error: operación sobre tipos de variables incompatibles en la línea "+str(self.linea)+".")
					while diccionario_aux and valores_aux:
						diccionario = diccionario_aux.popleft()
						valores=valores_aux.popleft()
						lista_diccionarios_aux.append(diccionario)
						lista_values_aux.append(valores)
					return

			elif "asignacion" in self.type:
				print("type_hijo[0]==type_hijo[1]: "+str(type_hijo[0]==type_hijo[1]))
				if diccionario!=None and "IsAForCicle" in diccionario.keys() and str(self.children[0])==valores["IsAForCicle"]:
					errores_contexto.append("Error: no se puede modificar la variable de control "+str(self.children[0])+" de este ciclo; linea No. "+str(self.linea)+".")
					errorFor=True
					return
				if isinstance(self.children[0],Node) and "accederEnArreglo" in self.children[0].type:
					temp = __checkReturnArrayElement__(self.children[0])
					if temp==None:
						return
					# Si continua es porque el array existe, no existen index fuera de rango
					#	y no se está excediendo el numero de arreglos dentro de arreglos
					__modifyArray__(temp,self.children[1],self.linea)
					return
				if not isDeclaracion and type_hijo[0]==type_hijo[1]:
					self.tipo_var=type_hijo[1]
					if isinstance(self.children[1],Node):
						valores[str(self.children[0])]=str(self.children[1].value)
					else:
						if self.children[0]!=None and self.children[1]!=None:
							if isinstance(self.children[1],str):
								if self.children[1]=="true" or self.children[1]=="false" or "\"" in self.children[1] or "\'" in self.children[1]:
									valores[str(self.children[0])]=str(self.children[1])
									__fixBoolValuesInDict__(diccionario,valores)
								else:
									valores[str(self.children[0])]=valores2[str(self.children[1])]
							else:
								valores[str(self.children[0])]=int(self.children[1])
				elif isDeclaracion:
					while diccionario_aux and valores_aux:
						diccionario = diccionario_aux.popleft()
						valores=valores_aux.popleft()
						lista_diccionarios_aux.append(diccionario)
						lista_values_aux.append(valores)
					return
				
				else:
					print("ERRORCITO")
					errores_contexto.append("Error: operación asignación sobre tipos incompatibles en la línea "+str(self.linea)+".")
					while diccionario_aux and valores_aux:
						diccionario = diccionario_aux.popleft()
						valores=valores_aux.popleft()
						lista_diccionarios_aux.append(diccionario)
						lista_values_aux.append(valores)
					return
				
				#EN EL ARRAY DE VALORES GLOBAL value_hijo[0] se reemplaza con el value_hijo[1]
			
			elif self.type=="comienzo" or self.type=="secuencia" or self.type=="beginInterno":
				while diccionario_aux and valores_aux:
					diccionario = diccionario_aux.popleft()
					valores=valores_aux.popleft()
					lista_diccionarios_aux.append(diccionario)
					lista_values_aux.append(valores)
				return
			else:
				print("ERROR")
				errores_contexto.append("Error: operación asignación sobre tipos de variables incompatibles en la línea "+str(self.linea)+".")
				while diccionario_aux and valores_aux:
					diccionario = diccionario_aux.popleft()
					valores=valores_aux.popleft()
					lista_diccionarios_aux.append(diccionario)
					lista_values_aux.append(valores)
				return
			
		elif len(type_hijo)==1: #shift, caracter, menosUnario, negacion
			
			if self.type in operacion:
				type_hijo[0].strip("array-")
				self.tipo_var=type_hijo[0]
				#AQUI SE REALIZAN LAS OPERACIONES
				if self.tipo_var=="int":
					if "menosUnario" in self.type:
						self.value=(-1)*value_hijo[0]
					else:
						errores_contexto.append("Error: operación sobre tipo de variable incompatible en la línea "+str(self.linea)+".")
						while diccionario_aux and valores_aux:
							diccionario = diccionario_aux.popleft()
							valores=valores_aux.popleft()
							lista_diccionarios_aux.append(diccionario)
							lista_values_aux.append(valores)
						return

				elif self.tipo_var=="bool":
					if "negacion" in self.type:
						self.value = not value_hijo[0]
					else:
						errores_contexto.append("Error: operación sobre tipo de variable incompatible en la línea "+str(self.linea)+".")
						while diccionario_aux and valores_aux:
							diccionario = diccionario_aux.popleft()
							valores=valores_aux.popleft()
							lista_diccionarios_aux.append(diccionario)
							lista_values_aux.append(valores)
						return

				else:
					errores_contexto.append("Error: operación sobre tipo de variable incompatible en la línea "+str(self.linea)+".")
					while diccionario_aux and valores_aux:
						diccionario = diccionario_aux.popleft()
						valores=valores_aux.popleft()
						lista_diccionarios_aux.append(diccionario)
						lista_values_aux.append(valores)
					return

			elif self.type=="shift":
				if "array" in type_hijo[0]:
					self.type=type_hijo[0]
				else:
					errores_contexto.append("Error: operación sobre tipo de variable incompatible en la línea "+str(self.linea)+".")
					while diccionario_aux and valores_aux:
						diccionario = diccionario_aux.popleft()
						valores=valores_aux.popleft()
						lista_diccionarios_aux.append(diccionario)
						lista_values_aux.append(valores)
					return

				##HACERLE SHIFT AL ARREGLO EN value_hijos[0]
				
			elif self.type in caracter:
				if "array" in type_hijo[0]:
					type_hijo[0].strip("array-")
				elif type_hijo[0]=="char":
					self.tipo_var=type_hijo[0]
				else:
					errores_contexto.append("Error: operación sobre tipo de variable incompatible en la línea "+str(self.linea)+".")
					while diccionario_aux and valores_aux:
						diccionario = diccionario_aux.popleft()
						valores=valores_aux.popleft()
						lista_diccionarios_aux.append(diccionario)
						lista_values_aux.append(valores)
					return

				if self.tipo_var=="char":
					if "anterior" in self.type:
						print("Hace operacion anterior de char")
						value_hijo[0]=value_hijo[0].strip("\'")
						self.value="\'"+chr((ord(value_hijo[0])-1)%128)+"\'"
					elif "siguiente" in self.type:
						print("Hace operacion siguiente de char")
						value_hijo[0]=value_hijo[0].strip("\'")
						self.value="\'"+chr((ord(value_hijo[0])+1)%128)+"\'"
					elif "Ascii" in self.type:
						print("Hace operacion ascii de char")
						value_hijo[0]=value_hijo[0].strip("\'")
						self.value=ord(value_hijo[0])
						self.tipo_var="int"
				else:
					errores_contexto.append("Error: operación sobre tipo de variable incompatible en la línea "+str(self.linea)+".")
					while diccionario_aux and valores_aux:
						diccionario = diccionario_aux.popleft()
						valores=valores_aux.popleft()
						lista_diccionarios_aux.append(diccionario)
						lista_values_aux.append(valores)
					return

			else:
				if isDeclaracion:
					return
				print("ERROR6")
				errores_contexto.append("Error: operación sobre tipos de variables incompatibles en la línea "+str(self.linea)+".")
				while diccionario_aux and valores_aux:
					diccionario = diccionario_aux.popleft()
					valores=valores_aux.popleft()
					lista_diccionarios_aux.append(diccionario)
					lista_values_aux.append(valores)
				return
		print("el nodo de tipo "+self.type+" tiene value "+str(self.value)+" y tipo "+self.tipo_var)
		#m = buildtree(self)
		#print(m)
		while diccionario_aux and valores_aux:
			diccionario = diccionario_aux.popleft()
			valores=valores_aux.popleft()
			lista_diccionarios_aux.append(diccionario)
			lista_values_aux.append(valores)
		print(lista_diccionarios_aux)
		print(lista_values_aux)
		return

def p_program(p):
	'''program : start'''
	p[0] = p[1]

def p_start(p):
	'''start : TkWith declaracionVar TkBegin cond TkEnd
			 | TkBegin cond TkEnd
			 | TkWith declaracionVar TkBegin TkEnd
			 | TkWith TkBegin TkEnd
			 | TkWith TkBegin cond TkEnd
			 | TkBegin TkEnd'''
	if len(p)>4:
		if p[2] == "begin":
			p[0] = p[3]
		elif p[3] == "begin" and p[4]=="end":
			p[0] = Node('comienzo',[p[2]],None,p.lineno(3))
		else:
			p[0] = Node('comienzo',[p[2],p[4]],None,p.lineno(3))
	elif len(p)==4 and p[2] != "begin":
		p[0] = p[2]
	else:
		p[0] = None

#Var o begin
def p_declaracion_var(p):
	'''declaracionVar : TkVar declaracionId
					  | TkVar declaracionArray
					  | TkVar declaracionId declaracionVar
					  | TkVar declaracionArray declaracionVar'''
	if len(p)>3:
		p[0] = Node('secuencia_declaraciones',[p[2],p[3]],p[1],p.lineno(1))
	else:
		p[0] = p[2]

#declaracion de id variables
def p_declaracion_id(p):
	'''declaracionId : TkId TkComa declaracionId
					 | TkId TkAsignacion exp TkComa declaracionId
					 | declaracionIdNum
					 | declaracionIdBool
					 | declaracionIdChar'''
	if len(p)>2:
		if len(p)>4:
			p[0]=Node('secuencia_declaracionId',[p[1],p[3],p[5]],None,p.lineno(1))
		else:
			p[0]=Node('secuencia_declaracionId',[p[1],p[3]],None,p.lineno(1))
	else:
		p[0] = p[1]

#declaracion de id tipo int
def p_declaracion_idNum(p):
	'''declaracionIdNum : TkId TkAsignacion exp TkDosPuntos TkInt
						| TkId TkDosPuntos TkInt'''
	if len(p)>4:
		Nodo = Node('asignacion',[p[1],p[3]],p[2],p.lineno(2))
		if p[4]==",":
			p[0]=Node('secuencia_declaracionIdNum',[Nodo,p[5]],None,p.lineno(1))   ######REVISAR
		else:
			p[0]=Node('declaracionIdNum',[Nodo,p[5]],p[4],p.lineno(5))
	else:
		p[0]=Node('declaracionIdNum',[p[1],p[3]],p[2],p.lineno(3))
	

#declaracion de id tipo char
def p_declaracion_idChar(p):
	'''declaracionIdChar : TkId TkAsignacion exp TkDosPuntos TkChar
						 | TkId TkDosPuntos TkChar'''
	if len(p)>4:
		Nodo = Node('asignacion',[p[1],p[3]],p[2],p.lineno(2))
		if p[4]==",":
			p[0]=Node('secuencia_declaracionIdChar',[Nodo,p[5]],None,p.lineno(1))   ######REVISAR
		else:
			p[0]=Node('declaracionIdChar',[Nodo,p[5]],p[4],p.lineno(5))
	
	else:
		p[0]=Node('declaracionIdChar',[p[1],p[3]],p[2],p.lineno(3))
	

#declaracion de id tipo bool
def p_declaracion_idBool(p):
	'''declaracionIdBool : TkId TkAsignacion exp TkDosPuntos TkBool
						 | TkId TkDosPuntos TkBool'''
	if len(p)>4:
		Nodo = Node('asignacion',[p[1],p[3]],p[2],p.lineno(2))
		if p[4]==",":
			p[0]=Node('secuencia_declaracionIdBool',[Nodo,p[5]],None,p.lineno(1))   ######REVISAR
		else:
			p[0]=Node('declaracionIdBool',[Nodo,p[5]],p[4],p.lineno(5))
	else:
		p[0]=Node('declaracionIdBool',[p[1],p[3]],p[2],p.lineno(3))
	

#declaracion de arreglos
def p_declaracion_array(p):
	'''declaracionArray : TkId TkComa declaracionArray
						| TkId TkDosPuntos TkArray TkCorcheteAbre exp TkCorcheteCierra type2'''
	if len(p)>4:
		p[0]=Node('declaracionArray',[p[1],p[5],p[7]],p[3],p.lineno(1))
	else:
		p[0]=Node('secuencia_declaracionArray',[p[1],p[3]],None,p.lineno(2))
	

def p_type2(p):
	'''type2 : TkOf TkArray TkCorcheteAbre exp TkCorcheteCierra type2
			 | TkOf type'''
	if len(p)>3:
		p[0] = Node('arrayInterno',[p[4],p[6]],p[1],p.lineno(1))
	else:
		p[0] = p[2]

def p_type(p):
	'''type : TkInt
			| TkChar
			| TkBool'''
	p[0] = p[1]

def p_arrayaux(p):
	'''arrayaux : TkNum
				| TkId
				| TkNum TkComa TkNum'''
	if len(p)>2:
		p[0] = Node('rangoArreglo',[p[1],p[3]],p[2],p.lineno(1))
	else:
		p[0] = p[1]

def p_ingresarEnArreglo(p):
	'''ingresarEnArreglo : TkCorcheteAbre arrayaux TkCorcheteCierra ingresarEnArreglo
						 | TkCorcheteAbre arrayaux TkCorcheteCierra'''
	if len(p)>4:
		print(p[3])
		p[0] = Node('secuencia-accederEnArreglo',[p[2],p[4]],None,p.lineno(1))
	else:
		print(p[2])
		p[0] = p[2]
	
def p_cond(p):
	'''cond : if
			| if cond
			| while
			| while cond
			| for cond
			| for
			| read
			| read cond
			| print
			| print cond
			| TkWith declaracionVar TkBegin cond TkEnd 
			| TkWith declaracionVar TkBegin TkEnd 
			| TkWith declaracionVar TkBegin cond TkEnd cond
			| TkWith TkBegin cond TkEnd 
			| TkWith TkBegin TkEnd 
			| TkBegin cond TkEnd 
			| TkBegin cond TkEnd cond
			| TkId TkAsignacion exp TkPuntoComa
			| TkId TkAsignacion exp TkPuntoComa cond
			| TkId ingresarEnArreglo TkAsignacion exp TkPuntoComa
			| TkId ingresarEnArreglo TkAsignacion exp TkPuntoComa cond'''
	if p[1]=="with":
		if p[2] == "begin":
			if p[3] == "end":
				p[0] = None
			else:
				p[0] = p[3]
		else:
			if p[4]=="end":
				Nodo = Node('beginInterno',[p[2]],p[3],p.lineno(1))
			else:
				Nodo = Node('beginInterno',[p[2],p[4]],p[3],p.lineno(1))
			if len(p)>6:
				p[0] = Node('secuencia',[Nodo,p[6]],None,p.lineno(1))
			else:
				p[0] = Nodo
	elif p[1]=="begin":
		if len(p)==4:
			p[0] = p[2]
		else:
			Nodo = Node('beginInterno',[p[2]],p[3],p.lineno(1))
			p[0] = Node('secuencia',[Nodo,p[4]],None,p.lineno(1))
	elif len(p)>4 and p[1]!="with" and p[1]!="begin":
		if p[4]==";":
			Nodo = Node('asignacion',[p[1],p[3]],p[2],p.lineno(2))
			if len(p)>5:
				p[0] = Node('secuencia',[Nodo,p[5]],None,p.lineno(1))
			else:
				p[0] = Nodo
		elif p[5]==";":
			NodoInterno = Node('accederEnArreglo',[p[1],p[2]],"[",p.lineno(3))
			Nodo = Node('asignacion',[NodoInterno,p[4]],p[3],p.lineno(2))
			if len(p)>8:
				p[0] = Node('secuencia',[Nodo,p[6]],None,p.lineno(1))
			else:
				p[0] = Nodo
	else:
		if len(p)>2:
			p[0] = Node('secuencia',[p[1],p[2]],None,p.lineno(1))
		else:
			p[0] = p[1]

def p_if(p):
	'''if : TkIf operacion TkHacer cond TkOtherwise TkHacer cond TkEnd
		  | TkIf operacion TkHacer cond TkEnd'''
	if len(p)>6:
		p[0] = Node('condicional_otherwise',[p[2],p[4],p[7]],p[1],p.lineno(1))
	else:
		p[0] = Node('condicional',[p[2],p[4]],p[1],p.lineno(1))

def p_while(p):
	'''while : TkWhile operacion TkHacer cond TkEnd'''
	p[0] = Node('while',[p[2],p[4]],p[1],p.lineno(1))

def p_for(p):
	'''for : TkFor TkId TkFrom exp TkTo exp TkStep exp TkHacer cond TkEnd
		   | TkFor TkId TkFrom exp TkTo exp TkHacer cond TkEnd'''
	if len(p)>10:
		p[0] = Node('for_step',[p[2],p[4],p[6],p[8],p[10]],p[1],p.lineno(1))
	else:
		p[0] = Node('for',[p[2],p[4],p[6],p[8]],p[1],p.lineno(1))

def p_read(p):
	'''read : TkRead TkId TkPuntoComa'''
	p[0] = Node('read',[p[2]],p[1],p.lineno(1))

def p_print(p):
	'''print : TkPrint exp TkPuntoComa'''
	p[0] = Node('print',[p[2]],p[1],p.lineno(1))

def p_exp(p):
	'''exp : operacion'''
	p[0] = p[1]

def p_operacion(p):
	'''operacion : TkParAbre operacion TkParCierra
				  | operacion TkSuma operacion
				  | operacion TkResta operacion
				  | operacion TkMult operacion
				  | operacion TkDiv operacion
				  | operacion TkMod operacion
				  | TkId TkPunto TkNum
				  | TkMenos operacion
				  | TkResta operacion
				  | operacion TkMenos operacion
				  | operacion TkConjuncion operacion
				  | operacion TkDisyuncion operacion
				  | operacion TkIgual operacion
				  | operacion TkDiferente operacion
				  | TkNegacion operacion
				  | operacion TkMenor operacion
				  | operacion TkMenorIgual operacion
				  | operacion TkMayor operacion
				  | operacion TkMayorIgual operacion
				  | operacion TkConcatenacion operacion
				  | TkShift operacion
				  | operacion ingresarEnArreglo
				  | operacion TkSiguienteCar
				  | operacion TkAnteriorCar
				  | TkValorAscii operacion
				  | TkId
				  | TkNum
				  | TkTrue
				  | TkFalse
				  | TkCaracter'''
	if len(p)==4:
		if p[1]=="(" and p[3]==")":
			p[0] = p[2]
		elif p[2]=="+":
			p[0] = Node('operacion-suma',[p[1],p[3]],p[2],p.lineno(2))
		elif p[2]=="-":
			p[0] = Node('operacion-resta',[p[1],p[3]],p[2],p.lineno(2))
		elif p[2]=="*":
			p[0] = Node('operacion-multiplicacion',[p[1],p[3]],p[2],p.lineno(2))
		elif p[2]=="/":
			p[0] = Node('operacion-division',[p[1],p[3]],p[2],p.lineno(2))
		elif p[2]=="%":
			p[0] = Node('operacion-modulo',[p[1],p[3]],p[2],p.lineno(2))
		elif p[2]==".":
			p[0] = Node('operacion-punto',[p[1],p[3]],p[2],p.lineno(2))
		elif p[2]=="/\\":
			p[0] = Node('operacion-conjuncion',[p[1],p[3]],p[2],p.lineno(2))
		elif p[2]=="\\/":
			p[0] = Node('operacion-disyuncion',[p[1],p[3]],p[2],p.lineno(2))
		elif p[2]=="=":
			p[0] = Node('operacion-igual',[p[1],p[3]],p[2],p.lineno(2))
		elif p[2]=="/=":
			p[0] = Node('operacion-diferente',[p[1],p[3]],p[2],p.lineno(2))
		elif p[2]=="<":
			p[0] = Node('operacion-menor',[p[1],p[3]],p[2],p.lineno(2))
		elif p[2]=="<=":
			p[0] = Node('operacion-menorIgual',[p[1],p[3]],p[2],p.lineno(2))
		elif p[2]==">":
			p[0] = Node('operacion-mayor',[p[1],p[3]],p[2],p.lineno(2))
		elif p[2]==">=":
			p[0] = Node('operacion-mayorIgual',[p[1],p[3]],p[2],p.lineno(2))
		elif p[2]=="::":
			p[0] = Node('concatenacion',[p[1],p[3]],p[2],p.lineno(2))
	elif len(p)==3:
		if p[1]=="-":
			p[0] = Node('operacion-menosUnario',[p[2]],p[1],p.lineno(1))
		if p[1]=="not":
			p[0] = Node('operacion-negacion',[p[2]],p[1],p.lineno(1))
		elif p[2]=="++":
			p[0] = Node('siguienteChar',[p[1]],p[2],p.lineno(1))
		elif p[2]=="--":
			p[0] = Node('anteriorChar',[p[1]],p[2],p.lineno(1))
		elif p[1]=="#":
			p[0] = Node('valorAscii',[p[2]],p[1],p.lineno(1))
		elif p[1]=="$":
			p[0] = Node('shift',[p[2]],p[1],p.lineno(1))
		else:
			p[0] = Node('accederEnArreglo',[p[1],p[2]],"[",p.lineno(1))
	else:
		p[0] = p[1]

# Error rule for syntax errors
def p_error(p):
	if (not p):
		print("Fin de entrada inesperado.")
	else:
		errores_sintacticos.append("Error de sintaxis en la entrada.\nError: '" + str(p.value) +"' ubicado en la fila "+str(p.lineno)+", columna "+str(encontrar_col(data, p))+".")
	#sys.exit()

#Inicializacion del lexer
data=read_given_file(sys.argv[1])
lexer = lex.lex()
lexer.input(data)
while True:
	tok = lexer.token()
	if not tok: 
		break      
	listar_token(tok)

def buildtree(node):
	sting=""
	if node==None:
		return sting
	if isinstance(node,Node):
		sting+="( tipo: "+node.type
		if node.leaf!=None:
			sting+=", hoja: "+node.leaf
		i=0
		for child in node.children:
			sting+=", hijo"+str(i)+"="+buildtree(child)+""
			i=i+1
		if node.value:	
			sting+=", value="+str(node.value)
		if node.tipo_var:
			sting+=", tipo_resultante="+node.tipo_var
		sting+=") "
		
	else:
		if isinstance(node,int):
			sting+=str(node)
		else:
			sting+=node
	return sting

def buildtree2(node):
	sting=""
	if node==None:
		return sting
	if isinstance(node,Node):
		sting+="( tipo: "+node.type
		if node.leaf!=None:
			sting+=", hoja: "+node.leaf
		if node.value:	
			sting+=", value="+str(node.value)
		if node.tipo_var:
			sting+=", tipo_resultante="+node.tipo_var
		sting+=", linea="+str(node.linea)+")"
		i=0
		for child in node.children:
			sting+="\n|_ hijo "+str(i)+": "
			temp = buildtree2(child).split("\n")
			sting+=temp[0]+"\n"
			if len(temp)>1:
				for m in range(1,len(temp)):
					if m==len(temp)-1:
						sting+="   "+temp[m]
					else:
						sting+="   "+temp[m]+"\n"
			i=i+1	
	else:
		if isinstance(node,int):
			sting+=str(node)
		else:
			sting+=node
	return sting

anterior=""
cond=""
estado=""
def print_tree(node,n):
	sting=""
	global anterior,cond,estado
	if node==None:
		return sting
	if isinstance(node,Node):
		s=""

		if node.type in operacion:
			s=node.type[10:]
			if cond in ["if","while","if_otherwise"]:
				sting+="guardia: "+s
				if cond=="if":
					estado="exito1"
				elif cond=="if_otherwise":
					estado="exito"
				cond=""
			else:
				sting+="operacion: "+s
			anterior="izq"
		elif node.type in declaracion:
			if "secuencia" in node.type:
				s=node.type[10:]
			if "Char" in s:
				sting+="DECLARACION DE CARACTER"
				anterior="var"
			elif "Bool" in s:
				sting+="DECLARACION DE BOOLEANO"
				anterior="var"
			elif "Num" in s:
				sting+="DECLARACION DE ENTERO"
				anterior="var"
			elif "Array" in s:
				sting+="DECLARACION DE ARREGLO"
				anterior="array"
			elif "Id" in s:
				sting+="DECLARACION DE ID"
				anterior="var"
			else:
				sting+="DECLARACIONES"
				anterior="var"
				n-=1
		elif node.type=="asignacion":
			sting+="ASIGNACION"
			anterior="asig"
		elif node.type=="comienzo" or node.type=="beginInterno":
			sting+="SECUENCIACION\n"
		elif node.type=="condicional":
			sting+="CONDICIONAL"
			cond="if"
		elif node.type=="condicional_otherwise":
			sting+="CONDICIONAL"
			cond="if_otherwise"
		elif node.type=="while":
			sting+="ITERACION INDETERMINADA"
			cond="while"
		elif node.type=="for":
			sting+="ITERACION DETERMINADA"
			cond="for"
			anterior="inf"
		elif node.type=="for_step":
			sting+="ITERACION DETERMINADA"
			cond="for_step"
			anterior="inf"
		elif node.type=="read" or node.type=="print":
			sting+="ENTRADA/SALIDA"
		elif node.type in arreglo:
			if node.type=="concatenacion":
				sting+="operacion: concatenacion"
				anterior="izq"
				cond="concatenacion"
			elif node.type=="accederEnArreglo":
				sting+="operacion: acceder a elemento del arreglo"
				cond="acceder"######FALTA
			elif node.type=="shift":
				sting+="operacion: shift"
				cond="shift"
		elif node.type in caracter:
			if node.type=="siguienteChar":
				sting+="operacion: siguiente caracter"
			elif node.type=="anteriorChar":
				sting+="operacion: caracter anterior"
			elif node.type=="valorAscii":
				sting+="operacion: valor ASCII del caracter"
		elif node.type=="secuencia":
			sting+="SECUENCIA"
			n-=1
		else:
			sting+=node.type
			n-=1

		if anterior!="izq" and anterior!="der" and estado=="exito":
			sting+="\n"+("\t"*n)+"EXITO"
			estado="fracaso"
		elif anterior!="izq" and anterior!="der" and estado=="exito1":
			sting+="\n"+("\t"*n)+"EXITO"
			estado=""
		elif anterior!="izq" and anterior!="der" and estado=="fracaso":
			sting+="\n"+("\t"*n)+"FRACASO"
			estado=""

		n+=1

		for child in node.children:
			sting+="\n"+("\t"*n)+print_tree(child,n)
	else: 
		if isinstance(node,int):
			if anterior=="izq":
				sting+="operador izquierdo: LITERAL ENTERO\n"
				sting+=("\t"*n)+"valor: "+str(node)
				anterior="der"
			elif anterior=="der":
				sting+="operador derecho: LITERAL ENTERO\n"
				sting+=("\t"*n)+"valor: "+str(node)
				anterior=""
			elif anterior=="asig":
				sting+="expresion: LITERAL ENTERO\n"
				sting+=("\t"*n)+"valor: "+str(node)
				anterior=""
			elif anterior=="var":
				sting+="ARREGLO de tamaño: "+str(node)
				anterior=""
			elif anterior=="array":
				anterior=""
			elif cond=="for":
				if anterior=="inf":
					sting+="valor inferior: "+str(node)
					anterior="sup"
				elif anterior=="sup":
					sting+="valor superior: "+str(node)
					anterior=""
					cond=""
			elif cond=="for_step":
				if anterior=="inf":
					sting+="valor inferior: "+str(node)
					anterior="sup"
				elif anterior=="sup":
					sting+="valor superior: "+str(node)
					anterior="paso"
				elif anterior=="paso":
					sting+="paso: "+str(node)
					anterior=""
					cond=""
			else:
				sting+="valor: "+str(node)
		elif node=="true" or node=="false":
			if anterior=="izq":
				sting+="operador izquierdo: BOOLEANO\n"
				sting+=("\t"*n)+"valor: "+str(node)
				anterior="der"
			elif anterior=="der":
				sting+="operador derecho: BOOLEANO\n"
				sting+=("\t"*n)+"valor: "+str(node)
				anterior=""
			elif anterior=="asig":
				sting+="expresion: BOOLEANO\n"
				sting+=("\t"*n)+"valor: "+str(node)
				anterior=""
			elif anterior=="var":
				sting+="contenedor: VARIABLE\n"
				sting+=("\t"*n)+"valor: "+str(node)
				anterior=""
			elif anterior=="array":
				anterior=""
			else:
				sting+="valor: "+str(node)
		elif node in ["int","bool","char","array"]:
			if anterior=="array":
				sting+="expresion: ARREGLO\n"
			else:
				if node=="int":
					sting+="expresion: LITERAL ENTERO"
				elif node=="bool":
					sting+="expresion: BOOLEANO"
				elif node=="char":
					sting+="\texpresion: CARACTER"
				elif node=="array":
					sting+="expresion: ARREGLO"
				anterior=""
		elif ("\'" in node) or ("\"" in node):
			if anterior=="izq":
				sting+="operador izquierdo: CARACTER\n"
				sting+=("\t"*n)+"valor: "+str(node)
				anterior="der"
			elif anterior=="der":
				sting+="operador derecho: CARACTER\n"
				sting+=("\t"*n)+"valor: "+str(node)
				anterior=""
			elif anterior=="asig":
				sting+="expresion: CARACTER\n"
				sting+=("\t"*n)+"valor: "+str(node)
				anterior=""
			elif anterior=="var":
				sting+="caracter: "+node
				anterior=""
			else:
				sting+="valor: "+str(node)
		else:
			if anterior=="var":
				sting+="contenedor: VARIABLE\n"
				sting+=("\t"*n)+"identificador: "+node
				#anterior=""
			elif anterior=="izq":
				if cond=="concatenacion" or cond=="shift":
					sting+="operador izquierdo: VARIABLE\n"
					sting+=("\t"*n)+"arreglo: "+str(node)
					anterior="der"
				else:
					sting+="operador izquierdo: VARIABLE\n"
					sting+=("\t"*n)+"valor: "+str(node)
					anterior="der"
					cond=""
			elif anterior=="der":
				if cond=="concatenacion" or cond=="shift":
					sting+="operador derecho: VARIABLE\n"
					sting+=("\t"*n)+"arreglo: "+str(node)
					anterior=""
					cond=""
				else:
					sting+="operador derecho: VARIABLE\n"
					sting+=("\t"*n)+"valor: "+str(node)
					anterior=""
					cond=""
			elif anterior=="asig":
				sting+="identificador: "+node
				anterior="valor"
			elif anterior=="valor":
				if cond=="shift":
					sting+="arreglo: "+node
					anterior=""
					cond=""
				else:
					sting+="valor: "+node
					anterior=""
	return sting

def redeclaracion():
	#Busca en la cola por diccionario
	for lista in lista_repetidas_aux:
		while lista:
			igual=lista.popleft()
			errores_contexto.append("Error: redeclaración de variable "+str(igual[0])+" en la linea No. "+str(igual[1])+".")

def __getWordFromForCicle__(num):
	if num==1:
		return "\"From\""
	elif num==2:
		return "\"To\""
	else:
		return "\"Step\""

def decorateTree(node):
	global errorFor
	if node==None:
		return
	if isinstance(node,Node):
		print("\n**decorando nodo de tipo " + node.type)
		if len(node.children)!=0:
			if "accederEnArreglo" in node.type:
				return
			if "condicional" in node.type:
				print("el nodo es un condicional")
				decorateTree(node.children[0])
				tiporesult = None
				valresultante = None
				if isinstance(node.children[0],str):
					if "true" in node.children[0]:
						tiporesult = "bool"
						valresultante = True
					elif "false" in node.children[0]:
						tiporesult = "bool"
						valresultante = False
					elif "\"" in node.children[0] or "\'" in node.children[0]:
						tiporesult = "char"
					else:
						[diccionario,valores]=__searchElementinDictReturnDict__(node.children[0])
						if diccionario==None:
							errores_contexto.append("Error: Variable "+str(node.children[0])+" no declarada, linea No. "+ str(node.linea) + ".")
						else:
							tiporesult = diccionario[str(node.children[0])]
							valresultante = valores[str(node.children[0])]
				elif isinstance(node.children[0],int):
					tiporesult = "int"
				else: #es un nodo
					tiporesult = node.children[0].tipo_var
					valresultante = node.children[0].value
				print("el valor de la variable es "+ str(valresultante))
				if tiporesult!="bool":
					errores_contexto.append("Error: La expresión en el \"while\" tiene que ser de tipo bool, pero se encontró "+str(tiporesult)+". Linea No. "+str(node.linea)+".")
					return
				if tiporesult=="bool" and (valresultante ==None or valresultante=="None"):
					errores_contexto.append("Error: variable no inicializada. Linea No. "+str(node.linea)+".")
					return
				else:
					if valresultante==True:
						print("entra por el condicional")
						decorateTree(node.children[1])
					else:
						if "otherwise" in node.type:
							print("el nodo es un condicional")
							decorateTree(node.children[2])
					return
			if "while" in node.type:
				print("el nodo es un while")
				decorateTree(node.children[0])
				tiporesult = None
				valresultante = None
				if isinstance(node.children[0],str):
					if "true" in node.children[0]:
						errores_contexto.append("Error: Un while con condición true que no puede ser cambiada conlleva a un ciclo infinito. Linea No. "+str(node.linea)+".")
						return
					elif "false" in node.children[0]:
						tiporesult = "bool"
						valresultante = False
					elif "\"" in node.children[0] or "\'" in node.children[0]:
						tiporesult = "char"
					else:
						[diccionario,valores]=__searchElementinDictReturnDict__(node.children[0])
						if diccionario==None:
							errores_contexto.append("Error: Variable "+str(node.children[0])+" no declarada, linea No. "+ str(node.linea) + ".")
						else:
							tiporesult = diccionario[str(node.children[0])]
							valresultante = valores[str(node.children[0])]
				elif isinstance(node.children[0],int):
					tiporesult = "int"
				else: #es un nodo
					tiporesult = node.children[0].tipo_var
					valresultante = node.children[0].value
				print("el valor de la variable es "+ str(valresultante))
				if tiporesult!="bool":
					errores_contexto.append("Error: La expresión en el \"while\" tiene que ser de tipo bool, pero se encontró "+str(tiporesult)+". Linea No. "+str(node.linea)+".")
					return
				if tiporesult=="bool" and (valresultante ==None or valresultante=="None"):
					errores_contexto.append("Error: variable no inicializada. Linea No. "+str(node.linea)+".")
					return
				else:
					numrecursions = 0
					while valresultante==True:
						print("está en el ciclo while")
						decorateTree(node.children[1])
						if isinstance(node.children[0],Node):
							decorateTree(node.children[0])
							valresultante=node.children[0].value
						elif node.children[0] in diccionario.keys():
							valresultante = valores[str(node.children[0])]
						if numrecursions > 1000:
							errores_contexto.append("Error: Stack overflow en ciclo while. Linea No. "+str(node.linea)+".")
							break
						numrecursions+=1
					print("salio del while")
					return
			if "for" in node.type:
				print("ciclo for")
				y=buildtree(node)
				print(y)
				limit = len(node.children)-1
				print("el numero de hijos es "+str(limit)+" + 1")
				valuesresult=[]
				errortype = []
				for i in range(1,limit):
					print ("hijo "+str(i))
					decorateTree(node.children[i])
					if isinstance(node.children[i],int):
						print("la rama es un entero")
						valuesresult.append(node.children[i])
					if isinstance(node.children[i],str):
						print("la rama es un string")
						print("se va a buscar en la lista de diccionarios")
						diccionario_aux = deque([])
						valores_aux = deque([])
						diccionario = None
						valores = None
						Revised = False
						# Se busca en la lista de diccionarios
						while lista_diccionarios_aux and lista_values_aux:
							diccionario=lista_diccionarios_aux.popleft()
							valores=lista_values_aux.popleft()
							diccionario_aux.append(diccionario)
							valores_aux.append(valores)
							if str(node.children[i]) in diccionario.keys(): #Si esta en algun diccionario
								print("está en un diccionario")
								Revised = True
								if diccionario[str(node.children[i])]!="int":
									print("ERROR: no es entero")
									errortype.append(diccionario[str(node.children[i])])
									errores_contexto.append("Error: La expresión luego de "+__getWordFromForCicle__(i)+" no es de tipo entero, es de tipo "+diccionario[str(node.children[i])]+"; linea No. "+ str(node.linea) + ".")
								else:
									print("puede ser entero")
									if valores[str(node.children[i])]=="None" or valores[str(node.children[i])]==None or "None" in valores[str(node.children[i])]:
										print("ERROR: es None")
										errortype.append(diccionario[str(node.children[i])])
										errores_contexto.append("Error: La expresión luego de "+__getWordFromForCicle__(i)+" es de tipo entero, pero no fue inicializada; linea No. "+ str(node.linea) + ".")
									else:
										valuesresult.append(int(valores[str(node.children[i])]))
								break
						# Se restaura la cola de diccionarios y valores
						while diccionario_aux and valores_aux:
							diccionario = diccionario_aux.popleft()
							valores=valores_aux.popleft()
							lista_diccionarios_aux.append(diccionario)
							lista_values_aux.append(valores)
						if not Revised: # No se encuentra en el diccionario
							if node.children[i]=="true" or node.children[i]=="false":
								errortype.append("bool")
								errores_contexto.append("Error: La expresión luego de "+__getWordFromForCicle__(i)+" no es de tipo entero, es de tipo bool; linea No. "+ str(node.linea) + ".")
							elif "\"" in node.children[i] or "\'" in node.children[i]:
								errortype.append("char")
								errores_contexto.append("Error: La expresión luego de "+__getWordFromForCicle__(i)+" no es de tipo entero, es de tipo char; linea No. "+ str(node.linea) + ".")
							else:
								errortype.append("None")
								errores_contexto.append("Error: Variable "+str(node.children[i])+" no declarada, linea No. "+ str(node.linea) + ".")
					elif isinstance(node.children[i],Node):
						if node.children[i].tipo_var != "int":
							errortype.append(node.children[i].tipo_var)
							errores_contexto.append("Error: La expresión luego de "+__getWordFromForCicle__(i)+" no es de tipo entero, es de tipo "+node.children[i].tipo_var+"; linea No. "+ node.children[i].linea + ".")
						else:
							valuesresult.append(node.children[i]).value
				if len(errortype)>0:
					return
				diccionario = OrderedDict()
				valores = OrderedDict()
				diccionario[str(node.children[0])]="int"
				diccionario["IsAForCicle"]="cicle"
				if len(valuesresult)>2:
					[start,stop,step] = valuesresult
				else:
					[start,stop] = valuesresult
					step = 1
				valores[str(node.children[0])]=start
				valores["IsAForCicle"]=str(node.children[0])
				backup_lista_diccionarios_aux=lista_diccionarios_aux.copy()
				backup_lista_values_aux=lista_values_aux.copy()
				lista_diccionarios_aux.append(diccionario)
				lista_values_aux.append(valores)
				print("comienza el ciclo con la var de contro igual a "+str(start))
				i = start
				while i <= stop:
					print("la var de control tiene valor "+str(i))
					valores[str(node.children[0])] = i
					decorateTree(node.children[len(node.children)-1])
					print(lista_diccionarios_aux)
					print(lista_values_aux)
					if errorFor:
						print("hay un error de for")
						break
					i+=step
				if not errorFor:
					diccionario=lista_diccionarios_aux.popleft()
					lista_diccionarios.append(diccionario)
					valores = lista_values_aux.popleft()
					lista_values.append(valores)
				else:
					print("recuperando la lista, se substituye con ")
					print(backup_lista_diccionarios_aux)
					print(backup_lista_values_aux)
					while lista_diccionarios_aux:
						diccionario = lista_diccionarios_aux.popleft()
						valores = lista_values_aux.popleft()
					while backup_lista_diccionarios_aux:
						diccionario = backup_lista_diccionarios_aux.popleft()
						valores = backup_lista_values_aux.popleft()
						lista_diccionarios_aux.append(diccionario)
						lista_values_aux.append(valores)
					errorFor=False
				print("termina el ciclo for")
				print(lista_diccionarios_aux)
				print(lista_values_aux)
				return
				
			if len(node.children)>0 and isinstance(node.children[0],Node) and "declaracion" in node.children[0].type:
				print("creando tabla de simbolos")
				node.children[0].adjuntarTablaSimbolos()
				if len(node.children)>1 and isinstance(node.children[1],Node):
					print("decorando el arbol")
					decorateTree(node.children[1])
				print("sacando el diccionario de este bloque with")
				diccionario=lista_diccionarios_aux.popleft()
				lista_diccionarios.append(diccionario)
				valores = lista_values_aux.popleft()
				lista_values.append(valores)
				return
			else:
				for child in node.children:
					decorateTree(child)		
		node.calc_tipo(False)

def decorateTreeDeclaracion(node):
	if node==None:
		return
	if isinstance(node,Node):
		print(node.type)
		if len(node.children)!=0:
			for child in node.children:
				decorateTreeDeclaracion(child)		
		node.calc_tipo(True)
	else:
		print(node)

def fixLineaNodeAsignacion(node):
	if node==None:
		return
	if isinstance(node,Node):
		if "asignacion" in node.type:
			if node.linea == 0:
				for child in node.children:
					if isinstance(child,Node):
						node.linea = child.linea
						return
		for child in node.children:
			if isinstance(child,Node) and "asignacion" in child.type and child.linea==0:
				if "comienzo" in node.type:
					temp = node.children[0]
					if isinstance(temp,Node) and "declaracion" in temp.type:
						temp2 = temp
						while isinstance(temp2,Node):
							temp = temp2
							temp2 = temp.children[len(temp.children)-1]
					child.linea = temp.linea + 2
				else:
					child.linea = node.linea + 1
			if isinstance(child,Node) and "declaracion" in child.type:
				pass
			else:
				fixLineaNodeAsignacion(child)

lexer.lineno=1

yacc.yacc()
y = yacc.parse(data)
fixLineaNodeAsignacion(y)
if print_tokens_or_errors()==0: ####Falta formato de errores
	p = buildtree(y)
	if p=="":
		print("No se ha generado el árbol.")
	else:
		if len(sys.argv)>2 and sys.argv[2]=="-b":
			print(p)
		else:
			print(p)
			x = buildtree2(y)
			print(x)
			decorateTree(y)
			print(lista_diccionarios)
			print(lista_values)
			print(lista_repetidas_aux)
			redeclaracion()
			p = buildtree(y)
			print(p)
			x = buildtree2(y)
			print(x)
			if len(errores_contexto)!=0:
				#errores_contexto.reverse()
				for error in errores_contexto:
					print(error)
