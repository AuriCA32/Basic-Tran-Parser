import ply.lex as lex
import ply.yacc as yacc
import sys
import re

#Listas de tokens y errores
tokError=[] #Global, si esta vacio no hay errores
errores_sintacticos=[]
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
	('right', 'TkMenos', 'TkNegacion')
	)

#Crea el arbol
class Node:
	def __init__(self,type,children=None,leaf=None):
		self.type = type
		if children:
			self.children = children
		else:
			self.children = [ ]
		self.leaf = leaf

def p_program(p):
	'''program : start'''
	p[0] = p[1]

def p_start(p):
	'''start : TkWith declaracionVar TkBegin cond TkEnd
			 | TkBegin cond TkEnd
			 | TkWith declaracionVar TkBegin TkEnd
			 | TkWith TkBegin TkEnd
			 | TkBegin TkEnd'''
	if len(p)>4:
		p[0] = Node('comienzo',[p[2],p[4]],None)
	elif len(p)>3:
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
		p[0] = Node('secuencia_declaraciones',[p[2],p[3]],p[1]) 
	else:
		p[0] = p[2]

#declaracion de id variables
def p_declaracion_id(p):
	'''declaracionId : TkId TkComa declaracionId
					 | declaracionIdNum
					 | declaracionIdBool
					 | declaracionIdChar'''
	if len(p)>2:
		p[0]=Node('secuencia_declaracionId',[p[1],p[3]],None)
	else:
		p[0] = p[1]

#declaracion de id tipo int
def p_declaracion_idNum(p):
	'''declaracionIdNum : TkId TkAsignacion TkNum TkComa declaracionIdNum
						| TkId TkAsignacion TkNum TkDosPuntos TkInt
						| TkId TkDosPuntos TkInt'''
	if len(p)>4:
		Nodo = Node('asignacion',[p[1],p[3]],p[2])
		if p[4]==",":
			p[0]=Node('secuencia_declaracionIdNum',[Nodo,p[5]],None)   ######REVISAR
		else:
			p[0]=Node('declaracionIdNum',[Nodo,p[5]],p[4])
	else:
		p[0]=Node('declaracionIdNum',[p[1],p[3]],p[2])

#declaracion de id tipo char
def p_declaracion_idChar(p):
	'''declaracionIdChar : TkId TkAsignacion TkCaracter TkComa declaracionIdChar
						 | TkId TkAsignacion TkCaracter TkDosPuntos TkChar
						 | TkId TkDosPuntos TkChar'''
	if len(p)>4:
		Nodo = Node('asignacion',[p[1],p[3]],p[2])
		if p[4]==",":
			p[0]=Node('secuencia_declaracionIdChar',[Nodo,p[5]],None)   ######REVISAR
		else:
			p[0]=Node('declaracionIdChar',[Nodo,p[5]],p[4])
	else:
		p[0]=Node('declaracionIdChar',[p[1],p[3]],p[2])

#declaracion de id tipo bool
def p_declaracion_idBool(p):
	'''declaracionIdBool : TkId TkAsignacion TkTrue TkComa declaracionIdBool
						 | TkId TkAsignacion TkTrue TkDosPuntos TkBool
						 | TkId TkAsignacion TkFalse TkComa declaracionIdBool
						 | TkId TkAsignacion TkFalse TkDosPuntos TkBool
						 | TkId TkDosPuntos TkBool'''
	if len(p)>4:
		Nodo = Node('asignacion',[p[1],p[3]],p[2])
		if p[4]==",":
			p[0]=Node('secuencia_declaracionIdBool',[Nodo,p[5]],None)   ######REVISAR
		else:
			p[0]=Node('declaracionIdBool',[Nodo,p[5]],p[4])
	else:
		p[0]=Node('declaracionIdBool',[p[1],p[3]],p[2])

#declaracion de arreglos
def p_declaracion_array(p):
	'''declaracionArray : TkId TkComa declaracionArray
						| TkId TkDosPuntos TkArray TkCorcheteAbre TkNum TkCorcheteCierra TkOf type'''
	if len(p)>4:
		p[0]=Node('declaracionArray',[p[1],p[5],p[8]],p[3])
	else:
		p[0]=Node('secuencia_declaracionArray',[p[1],p[3]],None)

def p_type(p):
	'''type : TkInt
			| TkChar
			| TkBool'''
	p[0] = p[1]

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
			| TkWith declaracionVar TkBegin cond TkEnd cond
			| TkId TkAsignacion exp TkPuntoComa
			| TkId TkAsignacion exp TkPuntoComa cond'''	
	if p[1]=="with":
		Nodo = Node('beginInterno',[p[2],p[4]],p[3])
		if len(p)>6:
			p[0] = Node('secuencia',[Nodo,p[6]],None)
		else:
			p[0] = Nodo
	elif len(p)>4 and p[4]==";":
		Nodo = Node('asignacion',[p[1],p[3]],p[2])
		if len(p)>5:
			p[0] = Node('secuencia',[Nodo,p[5]],None)
		else:
			p[0] = Nodo
	else:
		if len(p)>2:
			p[0] = Node('secuencia',[p[1],p[2]],None)
		else:
			p[0] = p[1]

def p_if(p):
	'''if : TkIf operacion TkHacer cond TkOtherwise TkHacer cond TkEnd
		  | TkIf operacion TkHacer cond TkEnd'''
	if len(p)>6:
		p[0] = Node('condicional_otherwise',[p[2],p[4],p[7]],p[1])
	else:
		p[0] = Node('condicional',[p[2],p[4]],p[1])

def p_while(p):
	'''while : TkWhile operacion TkHacer cond TkEnd'''
	p[0] = Node('while',[p[2],p[4]],p[1])

def p_for(p):
	'''for : TkFor TkId TkFrom TkNum TkTo TkNum TkStep TkNum TkHacer cond TkEnd
		   | TkFor TkId TkFrom TkNum TkTo TkNum TkHacer cond TkEnd'''
	if len(p)>10:
		p[0] = Node('for_step',[p[2],p[4],p[6],p[8],p[10]],p[1])
	else:
		p[0] = Node('for',[p[2],p[4],p[6],p[8]],p[1])

def p_read(p):
	'''read : TkRead TkId TkPuntoComa'''
	p[0] = Node('read',[p[2]],p[1])

def p_print(p):
	'''print : TkPrint exp TkPuntoComa'''
	p[0] = Node('print',[p[2]],p[1])

def p_exp(p):
	'''exp : char
		   | array
		   | operacion'''
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
				  | operacion TkConjuncion operacion
				  | operacion TkDisyuncion operacion
				  | operacion TkIgual operacion
				  | operacion TkDiferente operacion
				  | TkNegacion operacion
				  | operacion TkMenor operacion
				  | operacion TkMenorIgual operacion
				  | operacion TkMayor operacion
				  | operacion TkMayorIgual operacion
				  | TkId
				  | TkNum
				  | TkTrue
				  | TkFalse'''
	if len(p)==4:
		if p[1]=="(" and p[3]==")":
			p[0] = p[2]
		elif p[2]=="+":
			p[0] = Node('operacion-suma',[p[1],p[3]],p[2])
		elif p[2]=="-":
			p[0] = Node('operacion-resta',[p[1],p[3]],p[2])
		elif p[2]=="*":
			p[0] = Node('operacion-multiplicacion',[p[1],p[3]],p[2])
		elif p[2]=="/":
			p[0] = Node('operacion-division',[p[1],p[3]],p[2])
		elif p[2]=="%":
			p[0] = Node('operacion-modulo',[p[1],p[3]],p[2])
		elif p[2]==".":
			p[0] = Node('operacion-punto',[p[1],p[3]],p[2])
		elif p[2]=="/\\":
			p[0] = Node('operacion-conjuncion',[p[1],p[3]],p[2])
		elif p[2]=="\\/":
			p[0] = Node('operacion-disyuncion',[p[1],p[3]],p[2])
		elif p[2]=="=":
			p[0] = Node('operacion-igual',[p[1],p[3]],p[2])
		elif p[2]=="/=":
			p[0] = Node('operacion-diferente',[p[1],p[3]],p[2])
		elif p[2]=="<":
			p[0] = Node('operacion-menor',[p[1],p[3]],p[2])
		elif p[2]=="<=":
			p[0] = Node('operacion-menorIgual',[p[1],p[3]],p[2])
		elif p[2]==">":
			p[0] = Node('operacion-mayor',[p[1],p[3]],p[2])
		elif p[2]==">=":
			p[0] = Node('operacion-mayorIgual',[p[1],p[3]],p[2])
	elif len(p)==3:
		if p[1]=="-":
			p[0] = Node('operacion-menosUnario',[p[2]],p[1])
		if p[1]=="not":
			p[0] = Node('operacion-negacion',[p[2]],p[1])
	else:
		p[0] = p[1]

# def p_aritmetica(p):
# 	#OJO revisar lo del punto porque no entiendo########
# 	'''aritmetica : TkId TkPunto TkNum
# 				  | TkNum
# 				  | TkParAbre aritmetica TkParCierra
# 				  | aritmetica TkSuma aritmetica
# 				  | TkId TkSuma TkId
# 				  | aritmetica TkSuma TkId
# 				  | TkId TkSuma aritmetica
# 				  | aritmetica TkResta aritmetica
# 				  | TkId TkResta TkId
# 				  | aritmetica TkResta TkId
# 				  | TkId TkResta aritmetica
# 				  | aritmetica TkMult aritmetica
# 				  | TkId TkMult TkId
# 				  | aritmetica TkMult TkId
# 				  | TkId TkMult aritmetica
# 				  | aritmetica TkDiv aritmetica
# 				  | TkId TkDiv TkId
# 				  | aritmetica TkDiv TkId
# 				  | TkId TkDiv aritmetica
# 				  | aritmetica TkMod aritmetica
# 				  | TkId TkMod TkId
# 				  | aritmetica TkMod TkId
# 				  | TkId TkMod aritmetica
# 				  | TkMenos aritmetica
# 				  | TkMenos TkId''' #menos unario
# 	if len(p)==4:
# 		if p[1]=="(" and p[3]==")":
# 			p[0] = p[2]
# 		elif p[2]=="+":
# 			p[0] = Node('suma',[p[1],p[3]],p[2])
# 		elif p[2]=="-":
# 			p[0] = Node('resta',[p[1],p[3]],p[2])
# 		elif p[2]=="*":
# 			p[0] = Node('multiplicacion',[p[1],p[3]],p[2])
# 		elif p[2]=="/":
# 			p[0] = Node('division',[p[1],p[3]],p[2])
# 		elif p[2]=="%":
# 			p[0] = Node('modulo',[p[1],p[3]],p[2])
# 		else:
# 			p[0] = Node('punto',[p[1],p[3]],p[2])
# 	elif len(p)==3:
# 		p[0] = Node('menosUnario',[p[2]],p[1])
# 	else:
# 		p[0] = p[1]

# def p_booleana(p):
# 	'''booleana : TkTrue
# 				| TkFalse
# 				| TkParAbre booleana TkParCierra
# 				| booleana TkConjuncion booleana
# 				| TkId TkConjuncion booleana
# 				| booleana TkConjuncion TkId
# 				| TkId TkConjuncion TkId
# 				| booleana TkDisyuncion booleana
# 				| TkId TkDisyuncion booleana
# 				| booleana TkDisyuncion TkId
# 				| TkId TkDisyuncion TkId
# 				| booleana TkIgual booleana
# 				| TkId TkIgual booleana
# 				| booleana TkIgual TkId
# 				| TkId TkIgual TkId
# 				| booleana TkDiferente booleana
# 				| TkId TkDiferente booleana
# 				| booleana TkDiferente TkId
# 				| TkId TkDiferente TkId
# 				| TkNegacion booleana
# 				| TkNegacion TkId'''
# 	if len(p)==4:
# 		if p[1]=="(" and p[3]==")":
# 			p[0] = p[2]
# 		elif p[2]=="/\\":
# 			p[0] = Node('conjuncion',[p[1],p[3]],p[2])
# 		elif p[2]=="\\/":
# 			p[0] = Node('disyuncion',[p[1],p[3]],p[2])
# 		elif p[2]=="=":
# 			p[0] = Node('igual',[p[1],p[3]],p[2])
# 		elif p[2]=="/=":
# 			p[0] = Node('diferente',[p[1],p[3]],p[2])
# 	elif len(p)==3:
# 		p[0] = Node('negacion',[p[2]],p[1])
# 	else:
# 		p[0] = p[1]

def p_array(p):
	'''array : array TkConcatenacion array
			 | TkId TkConcatenacion array
			 | array TkConcatenacion TkId
			 | TkId TkConcatenacion TkId
			 | TkShift array
			 | TkShift TkId
			 | array TkCorcheteAbre TkNum TkCorcheteCierra
			 | TkParAbre array TkParCierra'''
	if len(p)>3:
		if p[1]=="(" and p[3]==")":
			p[0] = p[2]
		elif p[2]=="::":
			p[0] = Node('concatenacion',[p[1],p[3]],p[2])
		else:
			p[0] = Node('accederEnArreglo',[p[1],p[3]],p[2])
	elif len(p)==3:
		p[0] = Node('shift',[p[2]],p[1])

def p_char(p):
	'''char : TkCaracter TkSiguienteCar
			| TkCaracter TkAnteriorCar
			| TkValorAscii TkCaracter'''
	if p[2]=="++":
		p[0] = Node('siguienteChar',[p[1]],p[2])
	if p[2]=="--":
		p[0] = Node('anteriorChar',[p[1]],p[2])
	else:
		p[0] = Node('valorAscii',[p[2]],p[1])

# def p_relacionales(p):
# 	'''relacionales : booleana
# 					| aritmetica TkMenor aritmetica
# 					| aritmetica TkMenorIgual aritmetica
# 					| aritmetica TkMayor aritmetica
# 					| aritmetica TkMayorIgual aritmetica
# 					| aritmetica TkIgual aritmetica
# 					| aritmetica TkDiferente aritmetica'''
# 	if len(p)==2:
# 		p[0] = p[1]
# 	else:
# 		if p[2]=="<":
# 			p[0] = Node('menor',[p[1],p[3]],p[2])
# 		elif p[2]=="<=":
# 			p[0] = Node('menorIgual',[p[1],p[3]],p[2])
# 		elif p[2]==">":
# 			p[0] = Node('mayor',[p[1],p[3]],p[2])
# 		elif p[2]==">=":
# 			p[0] = Node('mayorIgual',[p[1],p[3]],p[2])
# 		elif p[2]=="=":
# 			p[0] = Node('igual',[p[1],p[3]],p[2])
# 		elif p[2]=="/=":
# 			p[0] = Node('diferente',[p[1],p[3]],p[2])

# Error rule for syntax errors
def p_error(p):
	errores_sintacticos.append(p)
	print("Syntax error in input")

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
		sting+="("+node.type
		if node.leaf!=None:
			sting+=","+node.leaf
		for child in node.children:
			sting+=","+buildtree(child)
		sting+=")"
	else:
		if isinstance(node,int):
			sting+=str(node)
		else:
			sting+=node
	return sting

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
"shift"]

caracter=["siguienteChar",
"anteriorChar",
"valorAscii"]

anterior=""
cond=""
def print_tree(node,n):
	sting=""
	global anterior,cond
	if node==None:
		return sting
	if isinstance(node,Node):
		s=""
		if node.type in operacion:
			s=node.type[10:]
			if cond in ["if","while","if_otherwise"]:
				sting+="guardia: "+s
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
			else:
				sting+="DECLARACION"
				anterior="var"
				n-=1
		elif node.type=="asignacion":
			sting+="ASIGNACION"
			anterior="asig"
		elif node.type=="comienzo" or node.type=="beginInterno":
			sting+="SECUENCIACION"
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
			sting+="OPERACION SOBRE ARREGLO"
		elif node.type in caracter:
			sting+="OPERACION SOBRE CARACTER"
		elif cond=="if":
			sting+="exito: "
			cond=""
		elif cond=="if_otherwise":
			sting+="exito: "
			cond="otherwise"
			n-=1
		elif cond=="otherwise":
			n-=1
			sting+="fallo: "
			cond=""
		else:
			sting+=node.type
			n-=1
		#if node.leaf!=None:
		#	sting+=", HOJA: "+node.leaf
		n+=1
		for child in node.children:
			sting+="\n"+("\t"*n)+print_tree(child,n)
	else: #Terminales
		##FALTA CHAR Y ARRAY
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
				sting+="contenedor: VARIABLE\n"
				sting+=("\t"*n)+"valor: "+str(node)
				anterior=""
			elif anterior=="array":
				sting+="contenedor: ARREGLO\n"
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
				sting+="contenedor: ARREGLO\n"
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
					sting+="expresion: CARACTER"
				elif node=="array":
					sting+="expresion: ARREGLO"
				anterior=""
		else:
			if anterior=="var":
				sting+="contenedor: VARIABLE\n"
				sting+=("\t"*n)+"identificador: "+node
				anterior=""
			elif anterior=="izq":
				sting+="operador izquierdo: VARIABLE\n"
				sting+=("\t"*n)+"valor: "+str(node)
				anterior="der"
			elif anterior=="der":
				sting+="operador derecho: VARIABLE\n"
				sting+=("\t"*n)+"valor: "+str(node)
				anterior=""
			elif anterior=="asig":
				sting+="identificador: "+node
				anterior="valor"
			elif anterior=="valor":
				sting+="valor: "+node
				anterior=""
		# if isinstance(node,int):
		# 	sting+="valor: "+str(node)
		# elif node=="true" or node=="false":
		# 	sting+="valor: "+node
		# elif node=="int" or node=="bool" or node=="char" or node=="array":
		# 	sting+="expresion: "+node
		# else:
		# 	sting+="identificador: "+node
	return sting

yacc.yacc()
y = yacc.parse(data)
if print_tokens_or_errors()==0: ####Falta formato de errores
	p = buildtree(y)
	if p=="":
		print("()")
	else:
		print(p)
		print()
		print(print_tree(y,0))
	