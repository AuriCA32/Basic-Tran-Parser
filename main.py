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
	'error',
	'TkCaracter'
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

#Crea el arbol
class Node:
	def __init__(self,type,children=None,leaf=None):
		self.type = type
		if children:
              self.children = children
         else:
			self.children = [ ]
		self.leaf = leaf

#Inicio declaraciones de variables
def p_with(p):
	'''with : TkWith declaracionVar'''

#Var o begin
def p_declaracion_var(p):
	'''declaracionVar : TkVar declaracionId
					  | TkVar declaracionArray
					  | begin'''

#declaracion de id variables
def p_declaracion_id(p):
	'''declaracionId : TkId TkComa declaracionId
					 | TkId TkDosPuntos type
					 | TkId TkAsignacion TkNum TkComa declaracionIdNum
					 | TkId TkAsignacion TkTrue TkComa declaracionIdBool
					 | TkId TkAsignacion TkTrue TkComa declaracionIdChar'''

#declaracion de id tipo int
def p_declaracion_idNum(p):
	'''declaracionIdNum : TkId TkComa declaracionIdNum
						| TkId TkAsignacion TkNum TkComa declaracionIdNum
						| TkId TkAsignacion TkNum TkDosPuntos typeInt'''

#declaracion de id tipo char
def p_declaracion_idChar(p):
	'''declaracionIdChar : TkId TkComa declaracionIdChar
						 | TkId TkAsignacion TkCaracter TkComa declaracionIdChar
						 | TkId TkAsignacion TkCaracter TkDosPuntos typeChar'''

#declaracion de id tipo bool
def p_declaracion_idBool(p):
	'''declaracionIdBool : TkId TkComa declaracionIdBool
						 | TkId TkAsignacion TkTrue TkComa declaracionIdBool
						 | TkId TkAsignacion TkTrue TkDosPuntos typeBool
						 | TkId TkAsignacion TkFalse TkComa declaracionIdBool
						 | TkId TkAsignacion TkFalse TkDosPuntos typeBool'''

#declaracion de arreglos
def p_declaracion_array(p):
	'''declaracionArray : TkId TkComa declaracionArray
						| TkId TkDosPuntos TkArray TkCorcheteAbre TkNum TkCorcheteCierra TkOf typeInt
						| TkId TkDosPuntos TkArray TkCorcheteAbre TkNum TkCorcheteCierra TkOf typeBool
						| TkId TkDosPuntos TkArray TkCorcheteAbre TkNum TkCorcheteCierra TkOf typeChar'''

def p_type_int(p):
	'''typeInt : TkInt declaracionVar'''

def p_type_bool(p):
	'''typeBool : TkBool declaracionVar'''

def p_type_char(p):	
	'''typeChar : TkChar declaracionVar'''

def p_begin(p):
	'''begin: TkBegin cond'''

def p_cond(p):
	'''cond : if
			| while
			| for
			| read
			| print
			| TkParAbre exp TkParCierra
			| exp
			| with
			| TkLlaveAbre cond TkLlaveCierra
			| TkId TkAsignacion exp
			| TkEnd'''

def p_exp(p):
	'''exp : char
		   | aritmetica
		   | array
		   | booleana'''

def p_if(p):
	'''if : TkIf relacionales TkHacer cond TkOtherwise TkHacer cond TkEnd
		  | TkIf relacionales TkHacer cond TkEnd'''

def p_while(p):
	'''while : TkWhile relacionales TkHacer cond TkEnd'''

def p_for(p):
	'''for : TkFor TkId TkFrom TkNum TkTo TkNum TkStep TkNum TkHacer cond TkEnd
		   | TkFor TkId TkFrom TkNum TkTo TkNum TkHacer cond TkEnd'''

def p_read(p):
	'''read : TkRead TkId'''

def p_print(p):
	'''print : TkPrint exp'''

def p_aritmetica(p):
	'''aritmetica : TkId TkPunto TkNum
				  | TkId
				  | TkNum
				  | TkId TkCorcheteAbre TkNum TkCorcheteCierra
				  | aritmetica TkSuma aritmetica
				  | aritmetica TkResta aritmetica
				  | aritmetica TkMult aritmetica
				  | aritmetica TkDiv aritmetica
				  | aritmetica TkMod aritmetica
				  | TkResta TkId''' #menos unario

def p_booleana(p):
	'''booleana : TkTrue
				| TkFalse
				| TkId TkCorcheteAbre TkNum TkCorcheteCierra
				| TkId
				| booleana TkConjuncion booleana
				| booleana TkDisyuncion booleana
				| booleana TkIgual booleana
				| booleana TkDiferente booleana
				| TkNegacion booleana'''

def p_array(p):
	'''array : TkId TkConcatenacion TkId
			| TkShift TkId'''

def p_char(p):
	'''char : TkCaracter TkSiguienteCar
			| TkCaracter TkAnteriorCar
			| TkValorAscii TkCaracter'''

def p_relacionales(p):
	'''relacionales : booleana
					| aritmetica TkMenor aritmetica
					| aritmetica TkMenorIgual aritmetica
					| aritmetica TkMayor aritmetica
					| aritmetica TkMayorIgual aritmetica
					| aritmetica TkIgual aritmetica
					| aritmetica TkDiferente aritmetica'''

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

#Inicializacion del parser