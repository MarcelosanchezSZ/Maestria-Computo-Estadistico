# -*- coding: utf-8 -*-
"""
Created on Tue May 18 13:19:23 2021

@author: Marcelo Sanchez
"""

""" ALUMNO: MARCELO ALBERTO SANCHEZ ZARAGOZA
"""

import numpy as np
import pandas as pd
from matplotlib import pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
from mpl_toolkits import mplot3d
from sklearn import linear_model
from sklearn import datasets
import seaborn as sns
import random

""" Definimos una función que nos va a ayudar a encontrar una ruta que cumple 
con nuestras condiciones y es generada de forma aleatoria.

Nuestra función se llama Kobe y recibe como parametros: N(conjunto de nodos), 
c(costo de la arista que va del nodo i al nodo j), 
nodo_inicial(nodo inicial) y nodo_final(nodo_final). 
En este caso la variable N no es de mucha ayuda ya que todos nuestros valores 
que vamos a ocupar estan en la variable c, ya que contiene los pesos de cada paso."""

def Kobe(N, c, nodo_inicial, nodo_final):
    # definimos las dos variables que nos van a ayudar a generar los valores aleatorios
    n_i = nodo_inicial + 1 # El inicio lo localizamos una unidad arriba ya que no tendria sentido que salga el inicio
    n_f = nodo_final + 1 # El final lo localizamos una unidad arriba ya que el comando que ocupamos omite el numero de limite
    # superior por lo que es necesario hacer esto.
    m = [] # lista vacia que nos ayuda a guardar nuestra ruta. 
    costo = 0 # inicializamos el costo
    m.append(nodo_inicial) # guardamos el nodo inicial, desde donde comenzamos
    x = nodo_inicial-1 # inicializamos el nodo inicial para trabajar con la matriz o data frame que nos proporcionaron ya que
    # en python empezamos desde cero.
    
    #### observar si hay error en algún renglón, es decir, que un renglón antes de llegar a la ultima iteración
    ##  tenga puros cero, esto genera un ciclo infinito ya que no puede avanzar, se toma en cuenta por si ocurriera un error
    # de dedo.
    renglon_malo = "r"
    for i in range(nodo_final):
        suma = 0
        for j in range(nodo_final):
            suma = suma + c.iloc[i,j]
        if(suma == 0 and i != (len(c)-1) ):
            renglon_malo = i
    ##########################################
    
    while(m[-1] != nodo_final): # mientras nuestra lista no tenga guardado el nodo inicial el while se va a seguir ejecutando
        no_n = random.randrange(n_i,n_f, 1)# generamos un numero aleatorio.
        indice = no_n-1 # inidice que nos ayuda a buscar el valor, este inidice le restamos una unidad por lo antes mencionado
        # python comienzo en cero los indices.
        
        if(c.iloc[x,indice] != 0 and renglon_malo != x): # realizamos la preguta, si el numero generado aleatorio tiene 
            # un valor que permita al numero anterior a llegar a el se avanza, como extra se agrega la restricción de numero malo
            #print("costo",c.iloc[x,indice])
            costo = costo + c.iloc[x,indice] # vamos guardando los pesos de los arcos
            m.append(no_n) # guardamos el nodo nuevo
            x = no_n-1 # actualizamos nuestra variable que nos ayuda a ir avanzando 
            #print("Se puede pasar", no_n, costo)
        elif(renglon_malo == x): ## se imprime un mensaje en caso de algun error en la matriz o data frame
            m.append(nodo_final)
            #print("No se puede pasar", no_n)
            print("Hay un problema, un valor no deja avanzar a la ruta :( ",renglon_malo)
            return -100
        lista = [m,costo] # al final regresamos la ruta que es una lista y el total de pesos que acumulamos
        #m.append(costo)
    return lista # regresamos la lista con lo antes mencionado

""" Mostramos los 3 conjuntos con los que se practico la función para 
encontrar la mejor ruta.
Para trabajar con cada uno de ellos hay que prestar atención con la dimensión 
y que los tres conjuntos tienen el mismo nombre."""

#### Conjunto 1 de datos de practica
datos = [[0,6,4,0,0,0],[0,0,2,2,0,0],[0,0,0,1,2,0],[0,0,0,0,0,7],[0,0,0,1,0,3]
         ,[0,0,0,0,0,0]]
data1 = pd.DataFrame(datos)
data1

#### Conjunto 2 de datos de practica
datos = [[0,3,6,1,0,0,0,0,0,0],[0,0,0,0,3,7,0,0,0,0],[0,0,0,0,8,0,2,0,0,0],
        [0,0,0,0,0,0,5,0,0,0],[0,0,0,0,0,0,0,9,3,0],[0,0,0,0,6,0,0,0,0,20],
        [0,0,0,0,0,0,0,0,4,0],[0,0,0,0,0,0,0,0,0,11],[0,0,0,0,0,0,0,7,0,13],
        [0,0,0,0,0,0,0,0,0,0]]
data2 = pd.DataFrame(datos)
data2

### Conjunto 3 de datos de practica
datos = [[0,100,30,0,0],[0,0,20,0,0],[0,0,0,10,60],[0,15,0,0,50],
        [0,0,0,0,0]]
data3 = pd.DataFrame(datos)
data3

"""Hacemos el ejemplo para mostrar como regresa los valores. En este caso 
debemos ingresar los nodos de inicio y final, debemos prestar 
atención en el tamaño de nuestro dataframe que ingresamos."""


## En estos dos valores colocamos el principio y fin de la ruta
nodo_inicio = 1
nodo_final = 6
#### ponemos el numeros de variables
N = 5
### LLamamos la funcion que nos devuelve la ruta, solo la ruta
m = Kobe(N, data1, nodo_inicio, nodo_final)
"""Observemos que nos regresa la ruta y a un lado nos regresa el costo"""
print("Ejemplo 1, datos ingresados a mano: ",m)


"""En la siguiente linea se muestra como los datos se pueden cargar si se 
cuenta con un documento de excel .xlsx, los datos que ingresamos a dicho 
documento vienen de la misma forma que los tenemos anteriormente."""
info = pd.read_excel('Datos_MarceloSanchez.xlsx',  header=0)
info.columns = ['0','1', '2', '3', '4', '5']

"""Realizamos el mismo proceso para observar el resultado."""
## Realizamos el mismo ejemplo pero con datos que viene de un archivo .xlsx
nodo_inicio = 1
nodo_final = 6
#### ponemos el numeros de variables
N = 6
### LLamamos la funcion que nos devuelve la ruta, solo la ruta
"""Observemos que nos regresa la ruta y a un lado nos regresa el costo"""
print("Ejemplo 2, datos de un archivo: ",Kobe(N, info, nodo_inicio, nodo_final))

#############################################################################
#############################################################################
## SOLO A MANERA DE EJEMPLO 

"""Hacemos una lista de tamaño 100 para buscar una ruta con minimo pesos de 
los arcos, ya que las estamos generando de manera aleatoria no hay forma de 
cortar el proceso antes. """
lista2 = []
for i in range(100):
    s = Kobe(N, data1, nodo_inicio, nodo_final)
    if(s == -100):# Se detiene el proceso si existe un error 
        break
    else:
        lista2.append(s)

# simplemente se realiza un data frame con los 100 valores regresados porla funcion Kobe
pp = pd.DataFrame(lista2)
#tmp = pp[1].min()
pp.columns = ['Ruta', 'Costo']
#pp

"""Al final solo mostramos la ruta más corta de toda la lista que generamos. 
Para el caso donde se observan una o dos rutas con el mismo valor se toma la 
ruta que se encuentre primero y la otra se omite."""
# se muestra la ruta y el peso total o costo total 
"""Observemos que nos regresa la ruta y a un lado nos regresa el costo"""
print("Resultado de buscar la ruta más corta: ")
print(pp.loc[pp['Costo'].idxmin()])


























