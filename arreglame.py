
def inicio(stringlargo):
	#file = open(“arregladito.txt”,”w+”) 
	largo = stringlargo.split("\n")
	for i in largo:
		#arreglame(i)
		print("t_"+i)
	#file.close()

def arreglame (all):
	arreglodesarreglado = all.split(" ")
	if (len(arreglodesarreglado) == 4):
		string = "t_"+arreglodesarreglado[3]
		elemento = "'"+arreglodesarreglado[1].split('"')[1]+"'"
		string += "	= r" + elemento
		print(string)


stringlargo='''
TkComa  = r','
TkPunto = r'.'
TkDosPuntos = r':'
TkParAbre = r'\('
TkParCierra = r')'
TkCorcheteAbre  = r'\['
TkCorcheteCierra  = r'\]'
TkLlaveAbre = r'\{'
TkLlaveCierra = r'\}'
TkHacer = r'->'
TkAsignacion = r'<-'
TkSuma  = r'\+'
TkResta = r'-'
TkMult  = r'\*'
TkDiv = r'/'
TkMod = r'%'
TkConjuncion  = r'[/][\]'
TkDisyuncion  = r'[\][/]'
TkNegacion  = r'not'
TkMenor = r'<'
TkMenorIgual  = r'<='
TkMayor = r'>'
TkMayorIgual  = r'>='
TkIgual = r'='
TkSiguienteCar  = r'[\+][\+]'
TkAnteriorCar = r'[-][-]'
TkValorAscii  = r'#'
TkConcatenacion = r'::'
TkShift = r'$'
'''

inicio(stringlargo)