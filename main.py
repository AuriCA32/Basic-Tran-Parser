import ply.lex as lex
import sys
import re


tokError=[] #Array para los tokens error, si esta vacio no hay errores
tokensList=[] #Array para la lista de tokens


""" Lista de nombres de Tokens para palabras reservadas """
reserved={
	'with' : 'TkWith',
	'begin' : 'TkBegin',
	'end' : 'TkEnd',
	'int' : 'TkInt',
	'bool' : 'TkBool',
	'char' : 'TkChar',
	'array' : 'TkArray',
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

""" Lista de nombres de Tokens """
tokens = [
	'TkComa',
	'TkPunto',
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


""" Definiciones de los tokens """
t_TkComa  = r','
t_TkPunto = r'\.'
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
t_TkSiguienteCar  = r'[+][+]'
t_TkAnteriorCar = r'--'
t_TkValorAscii  = r'\#'
t_TkConcatenacion = r'::'
t_TkShift = r'\$'
t_TkCaracter = r'[\'\"][a-zA-Z0-9][\'\"]|[\'\"][\t][\'\"]|[\'\"][\n][\'\"]|[\'\"][\'][\'\"]|[\'\"][\\][\'\"]' #0-9 ni espacio

""" Caracteres ignorados """
t_ignore  = ' \t'

def t_TkNum(t):
	""" Regla para identificar numeros """
    r'\d+'
    t.value = int(t.value)    
    return t

def t_newline(t):
	""" Identifica una nueva linea. """
    r'\n+'
    t.lexer.lineno += len(t.value)

def t_error(t):
	""" Handler para errores. """
    tokError.append("Error: Caracter inesperado \"%s\"" % t.value[0] +" en la fila "+str(lexer.lineno)+", columna "+encontrar_col(data,t))
    t.lexer.skip(1)

def t_TkId(t):
	""" Regla para identificar numeros. """
    r'[a-zA-Z][_a-zA-Z_0-9]*'
    t.type = reserved.get(t.value,'TkId')    #Chequea palabras reservadas
    return t

def encontrar_col(linea, token):
	""" Encuentra la columna para el input dado. """
    inicio_linea=linea.rfind('\n',0,token.lexpos) + 1
    return str((token.lexpos - inicio_linea) + 1)

def get_especial(tok):
	""" Identifica los caracteres especiales """
	if (tok.value=='\'\n\''):
		return '\''+'\\'+'n'+'\''
	elif (tok.value=='\'\t\''):
		return '\''+'\\'+'t'+'\''
	else:
		return tok.value

def listar_token(tok):
	""" Lista el string del token dependiendo de su valor """
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

def print_tokens_or_errors():
	""" Imprime la lista de tokens o errores dependiendo del caso """
	if(len(tokError)>0):
	    for i in range(len(tokError)):
	        print(tokError[i])
	else:
	    for i in range(len(tokensList)):
	        print(tokensList[i])

def read_given_file(gfile):
	""" Lee un archivo y retorna su contenido como string """
    try:
        file = open(gfile,"r")
        return file.read()
    except IOError as e:
        print("I/O error({0}): {1}".format(e.errno, e.strerror))
        exit()
    except:
        print("Unexpected error:", sys.exc_info()[0])
        exit()


""" Implementacion del Lexer """
lexer = lex.lex()
data=read_given_file(sys.argv[1])
lexer.input(data)
while True:
    tok = lexer.token()
    if not tok: 
        break      # No more input
    listar_token(tok)
print_tokens_or_errors()