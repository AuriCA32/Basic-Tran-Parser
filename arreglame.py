
def inicio(stringlargo):
	#file = open(“arregladito.txt”,”w+”) 
	#largo = stringlargo.split("\n")

	"""
	for i in largo:
		#arreglame(i)
		print("t_"+i)
	#file.close()"""
	crearTokenPalabraR(stringlargo)

def arreglame (all):
	arreglodesarreglado = all.split(" ")
	if (len(arreglodesarreglado) == 4):
		string = "t_"+arreglodesarreglado[3]
		elemento = "'"+arreglodesarreglado[1].split('"')[1]+"'"
		string += "	= r" + elemento
		print(string)

def crearTokenPalabraR (reservadas):
	for i in reservadas:
		print("'"+i+"' : 'Tk"+i.title()+"',")


stringlargo='''
with
begin
end
int
bool
char
array
if
otherwise
while
read
var
for
from
to
step
print
true
false
not
'''

inicio(stringlargo.split("\n"))