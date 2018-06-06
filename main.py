import ply.lex as lex
import ply.yacc as yacc
import sys
import re

#Listas de tokens y errores
tokError=[] #Global, si esta vacio no hay errores
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
	'TkLlaveAbre',
	'TkLlaveCierra',
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
t_TkLlaveAbre = r'\{'
t_TkLlaveCierra = r'\}'
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
	else:
		for i in range(len(tokensList)):
			print(tokensList[i])

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
			 | TkBegin cond TkEnd'''
	if len(p)>4:
#		p[0] = Node('comienzo',[p[2],p[4]],p[3])
		p[0] = p[4]
	else:
		p[0] = p[2]

#Var o begin
def p_declaracion_var(p):
	'''declaracionVar : TkVar declaracionId
					  | TkVar declaracionArray
					  | TkVar declaracionId declaracionVar
					  | TkVar declaracionArray declaracionVar'''
	if len(p)>3:
		p[0]=Node('declaraciones',[p[2],p[3]],p[1]) ####acomodar
	else:
		p[0]=Node('declaraciones',[p[2]],p[1])

#declaracion de id variables
def p_declaracion_id(p):
	'''declaracionId : TkId TkComa declaracionId
					 | TkId TkDosPuntos type
					 | declaracionIdNum
					 | declaracionIdBool
					 | declaracionIdChar'''
	if len(p)>2:
		p[0]=Node('declaracion_id',[p[1],p[3]],None)
	else:
		p[0]=p[1]

#declaracion de id tipo int
def p_declaracion_idNum(p):
	'''declaracionIdNum : TkId TkAsignacion TkNum TkComa declaracionIdNum
						| TkId TkAsignacion TkNum TkDosPuntos typeInt'''
	if p[4]==",": ####Hay que acomodar esto
		p[0]=Node('declaraciones_id_int',[p[1],p[3],p[5]],p[2])
	else:
		p[0]=Node('declaraciones_id_int',[p[1],p[3],p[5]],p[2])

#declaracion de id tipo char
def p_declaracion_idChar(p):
	'''declaracionIdChar : TkId TkAsignacion TkCaracter TkComa declaracionIdChar
						 | TkId TkAsignacion TkCaracter TkDosPuntos typeChar'''
	p[0]=Node('declaraciones_id_int',[p[1],p[3],p[5]],p[2])

#declaracion de id tipo bool
def p_declaracion_idBool(p):
	'''declaracionIdBool : TkId TkAsignacion TkTrue TkComa declaracionIdBool
						 | TkId TkAsignacion TkTrue TkDosPuntos typeBool
						 | TkId TkAsignacion TkFalse TkComa declaracionIdBool
						 | TkId TkAsignacion TkFalse TkDosPuntos typeBool'''
	if p[3]=="true":
		p[0]=Node('declaraciones_id_true',[p[1],p[3],p[5]],p[2])
	elif p[3]=="false":
		p[0]=Node('declaraciones_id_true',[p[1],p[3],p[5]],p[2])


#declaracion de arreglos
def p_declaracion_array(p):
	'''declaracionArray : TkId TkComa declaracionArray
						| TkId TkDosPuntos TkArray TkCorcheteAbre TkNum TkCorcheteCierra TkOf type'''
	if len(p)>4:
		p[0]=Node('declaracion_array',[p[1],p[5],p[8]],p[3])
	else:
		p[0]=Node('declaracion_array',[p[1],p[3]],None)

def p_type(p):
	'''type : typeInt
			| typeBool
			| typeChar'''
	p[0]=p[1]

def p_type_int(p):
	'''typeInt : TkInt declaracionVar
			   | TkInt'''
	if len(p)>2:
		p[0]=Node('tipo_entero',[p[1],p[2]],None)
	else:
		p[0]=p[1]


def p_type_bool(p):
	'''typeBool : TkBool declaracionVar
				| TkBool'''
	if len(p)>2:
		p[0]=Node('tipo_bool',[p[1],p[2]],None)
	else:
		p[0]=p[1]

def p_type_char(p):	
	'''typeChar : TkChar declaracionVar
				| TkChar'''
	if len(p)>2:
		p[0]=Node('tipo_char',[p[1],p[2]],None)
	else:
		p[0]=p[1]

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
	if p[1]=="if" or p[1]=="for" or p[1]=="read" or p[1]=="print" or p[1]=="while":
		if len(p)>2:
			p[0] = Node('sequencia',[p[1],p[2]],None)
		else:
			p[0] = p[1]
	elif p[1]=="with":
		Nodo = Node('beginInterno',[p[2],p[4]],p[3])
		if len(p)>6:
			p[0] = Node('sequencia',[Nodo,p[6]],None)
		else:
			p[0] = Nodo
	else:
		Nodo = Node('asignacion',[p[1],p[3]],p[2])
		if len(p)>5:
			p[0] = Node('sequencia',[Nodo,p[5]],None)
		else:
			p[0] = Nodo

def p_if(p):
	'''if : TkIf relacionales TkHacer cond TkOtherwise TkHacer cond TkEnd
		  | TkIf relacionales TkHacer cond TkEnd'''
	if len(p)>6:
		p[0] = Node('if',[p[2],p[4],p[7]],p[1])
	else:
		p[0] = Node('if',[p[2],p[4]],p[1])

def p_while(p):
	'''while : TkWhile relacionales TkHacer cond TkEnd'''
	p[0] = Node('while',[p[2],p[4]],p[1])

def p_for(p):
	'''for : TkFor TkId TkFrom TkNum TkTo TkNum TkStep TkNum TkHacer cond TkEnd
		   | TkFor TkId TkFrom TkNum TkTo TkNum TkHacer cond TkEnd'''
	if len(p)>10:
		p[0] = Node('for',[p[2],p[4],p[6],p[8],p[10]],p[1])
	else:
		p[0] = Node('for',[p[2],p[4],p[6],p[8]],p[1])

def p_read(p):
	'''read : TkRead TkId TkPuntoComa'''
	p[0] = Node('read',[None,p[2]],p[1])

def p_print(p):
	'''print : TkPrint exp TkPuntoComa'''
	p[0] = Node('print',[None,p[2]],p[1])

def p_exp(p):
	'''exp : char
		   | aritmetica
		   | array
		   | booleana
		   | TkId
		   | TkParAbre TkId TkParCierra'''
	if len(p)>2:
		p[0] = p[2]
	else:
		p[0] = p[1]

def p_aritmetica(p):
	#OJO revisar lo del punto porque no entiendo########
	'''aritmetica : TkId TkPunto TkNum
				  | TkNum
				  | TkParAbre aritmetica TkParCierra
				  | aritmetica TkSuma aritmetica
				  | TkId TkSuma TkId
				  | aritmetica TkSuma TkId
				  | TkId TkSuma aritmetica
				  | aritmetica TkResta aritmetica
				  | TkId TkResta TkId
				  | aritmetica TkResta TkId
				  | TkId TkResta aritmetica
				  | aritmetica TkMult aritmetica
				  | TkId TkMult TkId
				  | aritmetica TkMult TkId
				  | TkId TkMult aritmetica
				  | aritmetica TkDiv aritmetica
				  | TkId TkDiv TkId
				  | aritmetica TkDiv TkId
				  | TkId TkDiv aritmetica
				  | aritmetica TkMod aritmetica
				  | TkId TkMod TkId
				  | aritmetica TkMod TkId
				  | TkId TkMod aritmetica
				  | TkMenos aritmetica
				  | TkMenos TkId''' #menos unario
	if len(p)==4:
		if p[1]=="(" and p[3]==")":
			p[0] = p[2]
		elif p[2]=="+":
			p[0] = Node('suma',[p[1],p[3]],p[2])
		elif p[2]=="-":
			p[0] = Node('resta',[p[1],p[3]],p[2])
		elif p[2]=="*":
			p[0] = Node('multiplicacion',[p[1],p[3]],p[2])
		elif p[2]=="/":
			p[0] = Node('division',[p[1],p[3]],p[2])
		elif p[2]=="%":
			p[0] = Node('modulo',[p[1],p[3]],p[2])
		else:
			p[0] = Node('punto',[p[1],p[3]],p[2])
	elif len(p)==3:
		p[0] = Node('menosUnario',[None,p[2]],p[1])
	else:
		p[0] = p[1]

def p_booleana(p):
	'''booleana : TkTrue
				| TkFalse
				| TkParAbre booleana TkParCierra
				| booleana TkConjuncion booleana
				| TkId TkConjuncion booleana
				| booleana TkConjuncion TkId
				| TkId TkConjuncion TkId
				| booleana TkDisyuncion booleana
				| TkId TkDisyuncion booleana
				| booleana TkDisyuncion TkId
				| TkId TkDisyuncion TkId
				| booleana TkIgual booleana
				| TkId TkIgual booleana
				| booleana TkIgual TkId
				| TkId TkIgual TkId
				| booleana TkDiferente booleana
				| TkId TkDiferente booleana
				| booleana TkDiferente TkId
				| TkId TkDiferente TkId
				| TkNegacion booleana
				| TkNegacion TkId'''
	if len(p)==4:
		if p[1]=="(" and p[3]==")":
			p[0] = p[2]
		elif p[2]=="/\\":
			p[0] = Node('conjuncion',[p[1],p[3]],p[2])
		elif p[2]=="\\/":
			p[0] = Node('disyuncion',[p[1],p[3]],p[2])
		elif p[2]=="=":
			p[0] = Node('igual',[p[1],p[3]],p[2])
		elif p[2]=="/=":
			p[0] = Node('diferente',[p[1],p[3]],p[2])
	elif len(p)==3:
		p[0] = Node('negacion',[None,p[2]],p[1])
	else:
		p[0] = p[1]

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
		p[0] = Node('shift',[None,p[2]],p[1])

def p_char(p):
	'''char : TkCaracter TkSiguienteCar
			| TkCaracter TkAnteriorCar
			| TkValorAscii TkCaracter'''
	if p[2]=="++":
		p[0] = Node('siguienteChar',[None,p[1]],p[2])
	if p[2]=="--":
		p[0] = Node('anteriorChar',[None,p[1]],p[2])
	else:
		p[0] = Node('valorAscii',[None,p[2]],p[1])

def p_relacionales(p):
	'''relacionales : booleana
					| aritmetica TkMenor aritmetica
					| aritmetica TkMenorIgual aritmetica
					| aritmetica TkMayor aritmetica
					| aritmetica TkMayorIgual aritmetica
					| aritmetica TkIgual aritmetica
					| aritmetica TkDiferente aritmetica'''
	if len(p)==2:
		p[0] = p[1]
	else:
		if p[2]=="<":
			p[0] = Node('menor',[p[1],p[3]],p[2])
		elif p[2]=="<=":
			p[0] = Node('menorIgual',[p[1],p[3]],p[2])
		elif p[2]==">":
			p[0] = Node('mayor',[p[1],p[3]],p[2])
		elif p[2]==">=":
			p[0] = Node('mayorIgual',[p[1],p[3]],p[2])
		elif p[2]=="=":
			p[0] = Node('igual',[p[1],p[3]],p[2])
		elif p[2]=="/=":
			p[0] = Node('diferente',[p[1],p[3]],p[2])

# Error rule for syntax errors
def p_error(p):
	print("Syntax error in input")

#Inicializacion del lexer
lexer = lex.lex()
data=read_given_file(sys.argv[1])
lexer.input(data)
while True:
	tok = lexer.token()
	if not tok: 
		break      
	listar_token(tok)
print_tokens_or_errors()

def buildtree(node):
	sting=""
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

#Inicializacion del parser
yacc.yacc()
y = yacc.parse(data)
print(buildtree(y))
	
