import ply.lex as lex
import sys
import re


tokError=[] #Global, si esta vacio no hay errores
tokensList=[]


# Lista de nombres de Tokens

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


# Definiciones de los tokens
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

# A string containing ignored characters (spaces and tabs)
t_ignore  = ' \t'

# Regla para identificar numeros
def t_TkNum(t):
    r'\d+'
    t.value = int(t.value)    
    return t

# New line
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

# Error handling rule
def t_error(t):
    #Error: Caracter inesperado "!" en la fila 2, columna 20
    tokError.append("Error: Caracter inesperado \"%s\"" % t.value[0] +" en la fila "+str(lexer.lineno)+", columna "+encontrar_col(data,t))
    t.lexer.skip(1)

def t_TkId(t):
    r'[a-zA-Z_][a-zA-Z_0-9]+'
    t.type = reserved.get(t.value,'TkId')    # Check for reserved words
    return t

#Para encontrar columna
def encontrar_col(linea, token):
    inicio_linea=linea.rfind('\n',0,token.lexpos) + 1
    return str((token.lexpos - inicio_linea) + 1)

def get_especial(tok):
	if (tok.value=='\'\n\''):
		return '\''+'\\'+'n'+'\''
	elif (tok.value=='\'\t\''):
		return '\''+'\\'+'t'+'\''
	else:
		return tok.value

#Para listar el string del token dependiendo de su valor
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

#Para imprimir la lista de tokens o errores dependiendo del caso
def print_tokens_or_errors():
	if(len(tokError)>0):
	    for i in range(len(tokError)):
	        print(tokError[i])
	else:
	    for i in range(len(tokensList)):
	        print(tokensList[i])

#Lee un archivo y lo retorna como string
def read_given_file():
	pass								##############CAMBIAR
	string=""+sys.argv[1]
	return string

#main del programa
def main():
	lexer=lex.lex()  #build lexer
	data=read_given_file()   #feed the lexer the input given
	while True:
		tok = lexer.token()
		if not tok: 
			break      # No more input
		listar_token(tok)


main()