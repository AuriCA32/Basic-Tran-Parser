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
	('right', 'TkCorcheteAbre'),
	('left', 'TkCorcheteCierra')
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

def __getVarType__(node):
	if isinstance(node,Node) and "declaracion" in node.type:
		return __getVarType__(node.children(len(node.children)-1))
	else:
		return node

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
		if self==None:
			return diccionario,repetidas,values
		if isinstance(self,Node):
			if "declaracion" in self.type:
				for child in self.children:
					if isinstance(child,Node) and "declaracion" in child.type:
						diccionario,repetidas,values = child.__recorrerDeclaraciones__(diccionario,repetidas,values)
					if "asignacion" in self.type:
						values[str(child.children[0])]=str(child.children[1])
						if diccionario[str(child.children[0])]:
							repetidas.append(str(child.children[0]))
						diccionario[str(child.children[0])]=__getVarType__(self)
				if len(self.children)==3:
					values[str(self.children[0])]=str(self.children[1])
				else:
					values[str(self.children[0])]=None
				if diccionario[str(self.children[0])]:
					repetidas.append(str(self.children[0]))
				diccionario[str(self.children[0])]=__getVarType__(self)
		return diccionario,repetidas,values

	def calc_tipo(self, cola_dicc, cola_values):
		#Vemos los hijos y sus tipos 
		value_hijo=[]
		type_hijo=[]
		cola_aux=deque([])
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
				else: #Buscamos en la tabla de simbolos la variable 
					declarada=False
					diccionario_aux=[]
					valores_aux=[]
					while cola_dicc and cola_values:
						diccionario=cola_dicc.popleft()
						valores=cola_values.popleft()
						diccionario_aux.append(diccionario)
						valores_aux.append(valores)
						if self.children[i] in diccionario: #Si esta en algun diccionario
							type_hijo.append(diccionario[self.children[i]])
							value_hijo.append(valores[self.children[i]])
							declarada=True
							break
					#Si se vacia la cola y no fue declarada
					if not cola_dicc and not declarada:#Error
						errores_contexto.append("Error: variable "+self.children[i]+" no declarada.")
						return
					#Acomodo las colas si no se terminaron de vaciar
					else:
						while cola_dicc:
							diccionario=cola_dicc.popleft()
							diccionario_aux.append(diccionario)
						while cola_values:
							valores=cola_values.popleft()
							valores_aux.append(valores)
						cola_dicc=diccionario_aux
						cola_values=valores_aux
			elif isinstance(self.children[i],int):
				type_hijo.append("int")
				value_hijo.append(self.children[i])
		
		#Si el value de algun hijo es None, no se puede realizar la operacion
		for hijo in value_hijo:
			if hijo=="None":
				errores_contexto.append("Error: No se puede realizar operación sobre variable con \
										valor None en la línea "+str(self.linea)+".")
				return
		
		if len(type_hijo)==2: #operaciones y arreglo sin shift
			
			if self.type in operacion:
				for hijo in type_hijo:
					hijo.strip("array-")
				
				if type_hijo[0]==type_hijo[1]:
					self.tipo_var=type_hijo[0]
				else:
					errores_contexto.append("Error: operación sobre tipos de variables incompatibles\
											en la línea "+str(self.linea)+".")
					return
				######FALTA MODIFICAR EL DICCIONARIO GLOBAL DE VALORES
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
						errores_contexto.append("Error: operación sobre tipos de variables incompatibles\
												en la línea "+str(self.linea)+".")
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
						errores_contexto.append("Error: operación sobre tipos de variables incompatibles\
												en la línea "+str(self.linea)+".")
						return

				else:
					errores_contexto.append("Error: operación sobre tipos de variables incompatibles\
											en la línea "+str(self.linea)+".")
					return

			elif self.type in arreglo and self.type!="shift":
				if ("array" in type_hijo[0]) and ("array" in type_hijo[1]):
					if type_hijo[0]==type_hijo[1]:
						self.tipo_var=type_hijo[0]
					else:
						errores_contexto.append("Error: operación sobre tipos de arreglos incompatibles\
									en la línea "+str(self.linea)+".")
						return
				else:
					errores_contexto.append("Error: operación sobre tipos de variables incompatibles\
									en la línea "+str(self.linea)+".")
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
				elif self.type=="accederEnArreglo":
					pass
				else:
					errores_contexto.append("Error: operación sobre tipos de variables incompatibles\
									en la línea "+str(self.linea)+".")
					return

			elif self.type=="asignacion":
				if type_hijo[0]==type_hijo[1]:
					self.tipo_var=type_hijo[0]
				else:
					errores_contexto.append("Error: operación asignación sobre tipos incompatibles\
								en la línea "+str(self.linea)+".")
					return
				
				#EN EL ARRAY DE VALORES GLOBAL value_hijo[0] se reemplaza con el value_hijo[1]
			
			else:
				errores_contexto.append("Error: operación asignación sobre tipos de variables incompatibles\
										en la línea "+str(self.linea)+".")
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
						errores_contexto.append("Error: operación sobre tipo de variable incompatible\
												en la línea "+str(self.linea)+".")
						return

				elif self.tipo_var=="bool":
					if "negacion" in self.type:
						self.value = not value_hijo[0]
					else:
						errores_contexto.append("Error: operación sobre tipo de variable incompatible\
												en la línea "+str(self.linea)+".")
						return

				else:
					errores_contexto.append("Error: operación sobre tipo de variable incompatible\
											en la línea "+str(self.linea)+".")
					return

			elif self.type=="shift":
				if "array" in type_hijo[0]:
					self.type=type_hijo[0]
				else:
					errores_contexto.append("Error: operación sobre tipo de variable incompatible\
									en la línea "+str(self.linea)+".")
					return

				##HACERLE SHIFT AL ARREGLO EN value_hijos[0]
				
			elif self.type in caracter:
				if "array" in type_hijo[0]:
					type_hijo[0].strip("array-")
				elif type_hijo[0]=="char":
					self.tipo_var=type_hijo[0]
				else:
					errores_contexto.append("Error: operación sobre tipo de variable incompatible\
										en la línea "+str(self.linea)+".")
					return

				if self.tipo_var=="char":
					if "anterior" in self.type:
						self.value=chr((ord(value_hijo[0])-1)%128)
					elif "siguiente" in self.type:
						self.value=chr((ord(value_hijo[0])+1)%128)
					elif "Ascii" in self.type:
						self.value=ord(value_hijo[0])
				else:
					errores_contexto.append("Error: operación sobre tipo de variable incompatible\
										en la línea "+str(self.linea)+".")
					return

			else:
				errores_contexto.append("Error: operación sobre tipos de variables incompatibles\
										en la línea "+str(self.linea)+".")
				return
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
	global diccionario,repetidas,values
	if len(p)>4:
		diccionario=OrderedDict()
		repetidas=deque([])
		values=OrderedDict()
		if p[2] == "begin":
			p[0] = p[3]
		elif p[3] == "begin" and p[4]=="end":
			p[0] = Node('comienzo',[p[2]],None,p.lineno)
		else:
			p[0] = Node('comienzo',[p[2],p[4]],None,p.lineno)
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
	global diccionario,repetidas,values
	if len(p)>3:
		p[0] = Node('secuencia_declaraciones',[p[2],p[3]],p[1],p.lineno)
	else:
		p[0] = p[2]

#declaracion de id variables
def p_declaracion_id(p):
	'''declaracionId : TkId TkComa declaracionId
					 | TkId TkAsignacion exp TkComa declaracionId
					 | declaracionIdNum
					 | declaracionIdBool
					 | declaracionIdChar'''
	global diccionario,repetidas,values
	if len(p)>2:
		if str(p[1]) in diccionario.keys():
			repetidas.append(str(p[1]))
		diccionario[str(p[1])]="id"
		if len(p)>4:
			print(str(p[1])+str(p[3]))
			values[str(p[1])]=p[3]
			p[0]=Node('secuencia_declaracionId',[p[1],p[3],p[5]],None,p.lineno)
		else:
			values[str(p[1])]=None
			p[0]=Node('secuencia_declaracionId',[p[1],p[3]],None,p.lineno)
	else:
		p[0] = p[1]

#declaracion de id tipo int
def p_declaracion_idNum(p):
	'''declaracionIdNum : TkId TkAsignacion exp TkDosPuntos TkInt
						| TkId TkDosPuntos TkInt'''
	global diccionario,repetidas,values
	if str(p[1]) in diccionario.keys():
		repetidas.append(str(p[1]))
	diccionario[str(p[1])]="int"
	
	if len(p)>4:
		print(str(p[1])+str(p[3])+str(p[5]))
		values[str(p[1])]=p[3]
		Nodo = Node('asignacion',[p[1],p[3]],p[2],p.lineno)
		if p[4]==",":
			p[0]=Node('secuencia_declaracionIdNum',[Nodo,p[5]],None,p.lineno)   ######REVISAR
		else:
			p[0]=Node('declaracionIdNum',[Nodo,p[5]],p[4],p.lineno)
	else:
		print(str(p[1])+str(p[3]))
		values[str(p[1])]=None
		p[0]=Node('declaracionIdNum',[p[1],p[3]],p[2],p.lineno)
	

#declaracion de id tipo char
def p_declaracion_idChar(p):
	'''declaracionIdChar : TkId TkAsignacion exp TkDosPuntos TkChar
						 | TkId TkDosPuntos TkChar'''
	global diccionario,repetidas,values
	if str(p[1]) in diccionario.keys():
		repetidas.append(str(p[1]))
	diccionario[str(p[1])]="char"

	if len(p)>4:
		values[str(p[1])]=p[3]
		Nodo = Node('asignacion',[p[1],p[3]],p[2],p.lineno)
		if p[4]==",":
			p[0]=Node('secuencia_declaracionIdChar',[Nodo,p[5]],None,p.lineno)   ######REVISAR
		else:
			p[0]=Node('declaracionIdChar',[Nodo,p[5]],p[4],p.lineno)
	
	else:
		values[str(p[1])]=None
		p[0]=Node('declaracionIdChar',[p[1],p[3]],p[2],p.lineno)
	

#declaracion de id tipo bool
def p_declaracion_idBool(p):
	'''declaracionIdBool : TkId TkAsignacion exp TkDosPuntos TkBool
						 | TkId TkDosPuntos TkBool'''
	global diccionario,repetidas,values
	if str(p[1]) in diccionario.keys():
		repetidas.append(str(p[1]))
	diccionario[str(p[1])]="bool"

	if len(p)>4:
		values[str(p[1])]=p[3]
		Nodo = Node('asignacion',[p[1],p[3]],p[2],p.lineno)
		if p[4]==",":
			p[0]=Node('secuencia_declaracionIdBool',[Nodo,p[5]],None,p.lineno)   ######REVISAR
		else:
			p[0]=Node('declaracionIdBool',[Nodo,p[5]],p[4],p.lineno)
	else:
		values[str(p[1])]=None
		p[0]=Node('declaracionIdBool',[p[1],p[3]],p[2],p.lineno)
	

#declaracion de arreglos
def p_declaracion_array(p):
	'''declaracionArray : TkId TkComa declaracionArray
						| TkId TkDosPuntos TkArray TkCorcheteAbre TkNum TkCorcheteCierra type2'''
	global diccionario,repetidas,values
	if str(p[1]) in diccionario.keys():
		repetidas.append(str(p[1]))
	
	if len(p)>4:
		diccionario[str(p[1])]="array-"+str(p[8])
		string="["		
		for i in range(p[5]):
			string=string+"None,"
		string=string[:-1]
		string=string+"]"
		values[str(p[1])]=string
		p[0]=Node('declaracionArray',[p[1],p[5],p[8]],p[3],p.lineno)
	else:
		diccionario[str(p[1])]="array-" + p[1]
		values[str(p[1])]="array"
		p[0]=Node('secuencia_declaracionArray',[p[1],p[3]],None,p.lineno)
	

def p_type2(p):
	'''type2 : TkOf TkArray TkCorcheteAbre exp TkCorcheteCierra  type2
			 | TkOf type'''
	if len(p)>3:
		p[0] = Node('arrayInterno',[p[4],p[6]],p[1])
	else:
		p[0] = p[2]

def p_type(p):
	'''type : TkInt
			| TkChar
			| TkBool'''
	p[0] = p[1]

def p_arrayaux(p):
	'''arrayaux : TkNum
				| TkNum TkComa TkNum'''
	if len(p)>2:
		p[0] = Node('rangoArreglo',[p[1],p[3]],p[2],p.lineno)
	else:
		p[0] = p[1]

def p_ingresarEnArreglo(p):
	'''ingresarEnArreglo : TkCorcheteAbre arrayaux TkCorcheteCierra ingresarEnArreglo
						 | TkCorcheteAbre arrayaux TkCorcheteCierra'''
	if len(p)>4:
		p[0] = Node('secuencia-accederEnArreglo',[p[2],p[4]],None,p.lineno)
	else:
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
	global diccionario,repetidas,values
	if p[1]=="with":
		#Se crea un nuevo diccionario
		lista_diccionarios.append(diccionario)
		lista_repetidas.append(repetidas)
		lista_values.append(values)
		diccionario=OrderedDict()
		repetidas=deque([])
		values=OrderedDict()
		if p[2] == "begin":
			if p[3] == "end":
				p[0] = None
			else:
				p[0] = p[3]
		else:
			if p[4]=="end":
				Nodo = Node('beginInterno',[p[2]],p[3],p.lineno)
			else:
				Nodo = Node('beginInterno',[p[2],p[4]],p[3],p.lineno)
			if len(p)>6:
				p[0] = Node('secuencia',[Nodo,p[6]],None,p.lineno)
			else:
				p[0] = Nodo
	elif p[1]=="begin":
		if len(p)==4:
			p[0] = p[2]
		else:
			Nodo = Node('beginInterno',[p[2]],p[3],p.lineno)
			p[0] = Node('secuencia',[Nodo,p[4]],None,p.lineno)
	elif len(p)>4 and p[1]!="with" and p[1]!="begin":
		if p[4]==";":
			Nodo = Node('asignacion',[p[1],p[3]],p[2],p.lineno)
			if len(p)>5:
				p[0] = Node('secuencia',[Nodo,p[5]],None,p.lineno)
			else:
				p[0] = Nodo
		elif p[5]==";":
			NodoInterno = Node('accederEnArreglo',[p[1],p[2]],"[",p.lineno)
			Nodo = Node('asignacion',[NodoInterno,p[4]],p[3],p.lineno)
			if len(p)>8:
				p[0] = Node('secuencia',[Nodo,p[6]],None,p.lineno)
			else:
				p[0] = Nodo
	else:
		if len(p)>2:
			p[0] = Node('secuencia',[p[1],p[2]],None,p.lineno)
		else:
			p[0] = p[1]

def p_if(p):
	'''if : TkIf operacion TkHacer cond TkOtherwise TkHacer cond TkEnd
		  | TkIf operacion TkHacer cond TkEnd'''
	if len(p)>6:
		p[0] = Node('condicional_otherwise',[p[2],p[4],p[7]],p[1],p.lineno)
	else:
		p[0] = Node('condicional',[p[2],p[4]],p[1],p.lineno)

def p_while(p):
	'''while : TkWhile operacion TkHacer cond TkEnd'''
	p[0] = Node('while',[p[2],p[4]],p[1],p.lineno)

def p_for(p):
	'''for : TkFor TkId TkFrom exp TkTo exp TkStep TkNum TkHacer cond TkEnd
		   | TkFor TkId TkFrom exp TkTo exp TkHacer cond TkEnd'''
	if len(p)>10:
		p[0] = Node('for_step',[p[2],p[4],p[6],p[8],p[10]],p[1],p.lineno)
	else:
		p[0] = Node('for',[p[2],p[4],p[6],p[8]],p[1],p.lineno)

def p_read(p):
	'''read : TkRead TkId TkPuntoComa'''
	p[0] = Node('read',[p[2]],p[1],p.lineno)

def p_print(p):
	'''print : TkPrint exp TkPuntoComa'''
	p[0] = Node('print',[p[2]],p[1],p.lineno)

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
			p[0] = Node('operacion-suma',[p[1],p[3]],p[2],p.lineno)
		elif p[2]=="-":
			p[0] = Node('operacion-resta',[p[1],p[3]],p[2],p.lineno)
		elif p[2]=="*":
			p[0] = Node('operacion-multiplicacion',[p[1],p[3]],p[2],p.lineno)
		elif p[2]=="/":
			p[0] = Node('operacion-division',[p[1],p[3]],p[2],p.lineno)
		elif p[2]=="%":
			p[0] = Node('operacion-modulo',[p[1],p[3]],p[2],p.lineno)
		elif p[2]==".":
			p[0] = Node('operacion-punto',[p[1],p[3]],p[2],p.lineno)
		elif p[2]=="/\\":
			p[0] = Node('operacion-conjuncion',[p[1],p[3]],p[2],p.lineno)
		elif p[2]=="\\/":
			p[0] = Node('operacion-disyuncion',[p[1],p[3]],p[2],p.lineno)
		elif p[2]=="=":
			p[0] = Node('operacion-igual',[p[1],p[3]],p[2],p.lineno)
		elif p[2]=="/=":
			p[0] = Node('operacion-diferente',[p[1],p[3]],p[2],p.lineno)
		elif p[2]=="<":
			p[0] = Node('operacion-menor',[p[1],p[3]],p[2],p.lineno)
		elif p[2]=="<=":
			p[0] = Node('operacion-menorIgual',[p[1],p[3]],p[2],p.lineno)
		elif p[2]==">":
			p[0] = Node('operacion-mayor',[p[1],p[3]],p[2],p.lineno)
		elif p[2]==">=":
			p[0] = Node('operacion-mayorIgual',[p[1],p[3]],p[2],p.lineno)
		elif p[2]=="::":
			p[0] = Node('concatenacion',[p[1],p[3]],p[2],p.lineno)
	elif len(p)==3:
		if p[1]=="-":
			p[0] = Node('operacion-menosUnario',[p[2]],p[1],p.lineno)
		if p[1]=="not":
			p[0] = Node('operacion-negacion',[p[2]],p[1],p.lineno)
		elif p[2]=="++":
			p[0] = Node('siguienteChar',[p[1]],p[2],p.lineno)
		elif p[2]=="--":
			p[0] = Node('anteriorChar',[p[1]],p[2],p.lineno)
		elif p[1]=="\#":
			p[0] = Node('valorAscii',[p[2]],p[1],p.lineno)
		elif p[1]=="$":
			p[0] = Node('shift',[p[2]],p[1],p.lineno)
		else:
			p[0] = Node('accederEnArreglo',[p[1],p[2]],"[",p.lineno)
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
		sting+=")\n"
		
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
	global lista_repetidas
	i=0
	for lista in lista_repetidas:
		while lista:
			igual=str(lista.popleft())
			errores_contexto.append("Error: redeclaración de variable "+igual+" en el bloque interno de declaraciones No. "+str(i)+".")
		i=i+1
		return
	return

def decorateTree(node):
	if node==None:
		return
	if isinstance(node,Node):
		if len(node.children)!=0:
			for child in node.children:
				if isinstance(node,Node):
					decorateTree(child)		
		node.calc_tipo(lista_diccionarios,lista_values)
lexer.lineno=1

yacc.yacc()
y = yacc.parse(data)
if print_tokens_or_errors()==0: ####Falta formato de errores
	p = buildtree(y)
	if p=="":
		print("No se ha generado el árbol.")
	else:
		if len(sys.argv)>2 and sys.argv[2]=="-b":
			print(p)
		else:
			print(p)
			print(lista_diccionarios)
			print(lista_values)
			for diccionario in lista_diccionarios:
				lista=list(diccionario.keys())
				for i in range(len(lista)):
					key=lista[i]
					if diccionario[key]=="id":
						prev=lista[i-1]
						diccionario[key]=diccionario[prev]
				print(diccionario)

			for valor in lista_values:
				lista=list(valor.keys())
				for i in range(len(lista)):
					key=lista[i]
					if valor[key]=="array":
						prev=lista[i-1]
						valor[key]=valor[prev]
				print(valor)
			#redeclaracion()
			if "declaracion" in y.children[0].type:
				y.children[0].adjuntarTablaSimbolos()
				# if len(y.children)>1:
				# 	decorateTree(y.children[1])
			# else:
			# 	decorateTree(y)
			# p = buildtree(y)
			# print(p)
			if len(errores_contexto)!=0:
				for error in errores_contexto:
					print(error)
			
			#Prueba de funcion de tipos y valores
			nodo=Node("valorAscii",["\'b\'"],None,1)
			nodo.calc_tipo(lista_diccionarios,lista_values)
			print(str(nodo.value))
			print(str(nodo.tipo_var))
