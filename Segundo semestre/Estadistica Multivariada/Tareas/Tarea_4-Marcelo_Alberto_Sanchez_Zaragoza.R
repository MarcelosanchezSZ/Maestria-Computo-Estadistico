############################### TAREA 4 #######################################
#***************** Alumno: Marcelo Alberto Sanchez Zaragoza*******************#



###############################################################################
################### Ejercicio 1
##### Iniciso a) 
library("MASS")
library("stats")
library("smacof")

m1 <- c(0.21, 0.25, 0.22,0.19, 0.18, 0.23, 0.30, 0.10, 0.27, 0.21)
m2 <- c(0.06, 0.04, 0.06, 0.04, 0.00, 0.00, 0.00, 0.06, 0.04, 0.05)
m3 <- c(0.06, 0.14, 0.08, 0.02, 0.15, 0.28, 0.06, 0.13, 0.06, 0.20)
m4 <- c(0.67, 0.57, 0.64, 0.75, 0.67, 0.49, 0.64, 0.71, 0.63, 0.54)


D <- cbind(m1, m2, m3, m4)
D
################## Encomtramos la distancia de Bhattacharyya ###################

nrow <- nrow(D)
ncol <- ncol(D)

#### definimos la funcion para los calculos #######
Kobe <- function(arg_1, arg_2)
{
  sum <- 0
  p <- 0
  for(i in 1:ncol)
  {
    p <- sqrt(D[arg_1,i]*D[arg_2,i])
    sum <- p + sum
  }
  p1 = acos(sum)
  p1
  return(p1)
}

M <- matrix(ncol=10, nrow=10)
for(i in 1:nrow)
{
  for(j in 1:nrow)
  {
    v1 = Kobe(i,j)
    M[i,j] = v1
  }
}

###### Hemos encontrado nuestra matriz de distancias de Bhattacharyya entre
##    las 10 poblaciones
M
###############################################################################

##### Inciso b)

resul_mds_clas <- cmdscale(sqrt(M), k = 2, eig = TRUE, add = FALSE, x.ret = FALSE)
#resul_mds_clas$eig
nombres_1 <- c("Francesa", "Checa", "Germanica", "Vasca", "China", "Ainu",
               "Esquimal", "Afromericaba USA", "Española", "Egipcia")
#nombres_1

config_nations <- resul_mds_clas$points

#se añaden los nombres de las naciones a la configuracion solucion X
dimnames(config_nations)[[1]] <- nombres_1

plot(config_nations[,1],config_nations[,2],
     main="Configuracion Solucion Mediante MDS-Clasico",
     ylim=range(config_nations[,1]),
     xlab="dim 1",ylab="dim 2",type="n",lwd=2)
text(config_nations[,1],config_nations[,2],
     labels=abbreviate(row.names(config_nations),minlength=8),cex=0.6,lwd=2)
#### varianza explicada 
resul_mds_clas$GOF
###############################################################################

#### Inciso c)

### necesito la matriz con distancias euclidianas
Kobe2 <- function(arg_1, arg_2)
{
  sum <- 0
  p <- 0
  for(i in 1:ncol)
  {
    p <- (D[arg_1,i]-D[arg_2,i])^2
    sum <- p + sum
  }
  p1 = sqrt(sum)
  return(p1)
}

#m <- Kobe2(1,2)
#m

M_eu <- matrix(ncol=10, nrow=10)
for(i in 1:nrow)
{
  for(j in 1:nrow)
  {
    v1 = Kobe2(i,j)
    M_eu[i,j] = v1
  }
}
### Mostramos la matriz con distancia Euclidiana
M_eu

### Buscamos la representación

resul_mds_clas2 <- cmdscale(M_eu, k = 2, eig = TRUE, add = FALSE, x.ret = FALSE)
nombres_1 <- c("Francesa", "Checa", "Germanica", "Vasca", "China", "Ainu",
               "Esquimal", "Afromericaba USA", "Española", "Egipcia")
#nombres_1

config_nations2 <- resul_mds_clas2$points

#se añaden los nombres de las naciones a la configuracion solucion X
dimnames(config_nations2)[[1]] <- nombres_1

plot(config_nations2[,1],config_nations2[,2],
     main="Configuracion Mediante MDS-clasico(Euclidiana)",
     ylim=range(config_nations2[,1]),
     xlab="dim 1",ylab="dim 2",type="n",lwd=2)
text(config_nations2[,1],config_nations2[,2],
     labels=abbreviate(row.names(config_nations2),minlength=8),cex=0.6,lwd=2)

#### Varianza explicada por las dos dimensiones
resul_mds_clas2$GOF
#### 
resul_mds_clas2$GOF



###############################################################################

#### Inciso d)
### Por minimos cuadrados con la matriz de distancias Bhattacharyy
# type = "ratio", "interval", "ordinal"
mds_mc_ratio <- smacofSym(sqrt(M), ndim=2,type = "ratio",verbose = FALSE, 
                          init = "torgerson", itmax = 1000, eps = 1e-06)

mds_mc_interval <- smacofSym(sqrt(M), ndim=2,type = "interval", verbose = FALSE,
                          init = "torgerson", itmax = 1000, eps = 1e-06) 


mds_mc_ordinal <- smacofSym(sqrt(M), ndim=2,type = "ordinal", verbose = FALSE,
                          init = "torgerson", itmax = 1000, eps = 1e-06) 


#primero obtenemos el diagrama de Shepard para evaluar la calidad de la solución graficando
#las disparidades vs las distancias ajustadas
plot(mds_mc_ratio,
     main = "Grafica de las disparidades vs las distancias ajustadas(Razón)",
     plot.type = "resplot")

plot(mds_mc_interval,
     main = "Grafica de las disparidades vs las distancias ajustadas(Intervalo)",
     plot.type = "resplot")

plot(mds_mc_ordinal,
     main = "Grafica de las disparidades vs las distancias ajustadas(Ordinal)",
     plot.type = "resplot")


### Buscamos el valor del Stress
mds_mc_ratio$stress
mds_mc_interval$stress
mds_mc_ordinal$stress


###### Realizamos las gráficas respectivamente 
config_nations_mc <- mds_mc_ratio$conf
plot(config_nations_mc[,1],config_nations_mc[,2],
     main="configuracion MDS-MC con transformación Razón ",
     ylim=range(config_nations_mc[,1]),
     xlab="Dim 1",ylab="Dim 2",type="n",lwd=2)
text(config_nations_mc[,1],config_nations_mc[,2],
     labels=abbreviate(nombres_1,minlength=8),cex=0.6,lwd=2)


config_nations_mc <- mds_mc_interval$conf
plot(config_nations_mc[,1],config_nations_mc[,2],
     main="configuracion MDS-MC con transformación Intervalo ",
     ylim=range(config_nations_mc[,1]),
     xlab="Dim 1",ylab="Dim 2",type="n",lwd=2)
text(config_nations_mc[,1],config_nations_mc[,2],
     labels=abbreviate(nombres_1,minlength=8),cex=0.6,lwd=2)


config_nations_mc <- mds_mc_ordinal$conf
plot(config_nations_mc[,1],config_nations_mc[,2],
     main="configuracion MDS-MC con transformación Ordinal ",
     ylim=range(config_nations_mc[,1]),
     xlab="Dim 1",ylab="Dim 2",type="n",lwd=2)
text(config_nations_mc[,1],config_nations_mc[,2],
     labels=abbreviate(nombres_1,minlength=8),cex=0.6,lwd=2)

###############################################################################
###############################################################################

################### Ejercicio 2
#### Inciso a)
x1 <- c(1,1,1,1,1,0)
x2 <- c(1,1,0,0,0,0)
x3 <- c(0,1,0,0,0,0)
x4 <- c(0,0,1,1,0,0)
x5 <- c(1,0,0,0,1,1)
x6 <- c(1,1,1,1,1,0)
#c_1 <- c(4,4,4,2,3)
#c_0 <- c(2,2,2,4,3)

nombre_anim <- c("Leon", "Jirafa", "Vaca", "Oveja", "Gato-domes","Humano")

X_datos <- cbind(x1, x2, x3, x4, x5, x6)#, c_1, c_0)
X_datos

nrow1 <- nrow(X_datos)
ncol1 <- ncol(X_datos)

Lebron <- function(ind1, ind2)
{
  a <- 0
  b <- 0
  c <- 0
  d <- 0
  for(i in 1:ncol1)
  {
    if((X_datos[ind1,i] == X_datos[ind2,i]) & X_datos[ind1,i] == 1 & X_datos[ind2,i] == 1)
    {
      #print(i)
      a <- a+1
    }
    else if( X_datos[ind1,i] == 0 & X_datos[ind2,i] == 1)
    {
      b <- b+1
    }
    else if( X_datos[ind1,i] == 1 & X_datos[ind2,i] == 0)
    {
      c <- c+1
    }
    else if((X_datos[ind1,i] == X_datos[ind2,i]) & X_datos[ind1,i] == 0 & X_datos[ind2,i] == 0)
    {
      d <- d+1
    }
  }
  m <- c(a,b,c,d)
  return(m)
}

m <- Lebron(1,5)
m

#p <- 6
#s <- (m[1] + m[4])/p
#s
###############################################################################

### Inciso b)

p <- 6
Sol_Mic <- matrix(ncol=6, nrow=6)
Jacard <- matrix(ncol=6, nrow=6)
for(i in 1:nrow1)
{
  #print(i)
  for(j in 1:nrow1)
  {
    #print(j)
    m <- Lebron(i,j)
    s <- (m[1]+m[4])/p
    Sol_Mic[i,j] <- s
    
    s1 <- (m[1])/(m[1]+m[2]+m[3])
    Jacard[i,j] <- s1
  }
}

##### Encontramos los coeficientes de similaridad de Sokal-Michener y Jacard
Sol_Mic
Jacard

##### Obtenemos las matrices de distancias asociadas 
l_1 <- c(1, 1, 1, 1, 1, 1)
D_1 <- 2*(l_1%*%(t(l_1))) - 2*Sol_Mic
D_2 <- 2*(l_1%*%(t(l_1))) - 2*Jacard

D_1
D_2
###############################################################################
###############################################################################

######### Ejercicio 3
#### Inciso a)

#### Obtenemos una representación utilizando la matriz de distancias calculads
### a partit del coeficiente de similaridad de Sokal-Michener

mds_clas_SM <- cmdscale(sqrt(D_1), k = 4, eig = TRUE, add = FALSE, x.ret = FALSE)

nombre_anim <- c("Leon", "Jirafa", "Vaca", "Oveja", "Gato-domes","Humano")
SK_resul <- mds_clas_SM$points

#se añaden los nombres de las naciones a la configuracion solucion X
dimnames(SK_resul)[[1]] <- nombre_anim

plot(SK_resul[,1], SK_resul[,2],
     main="Configuracion Mediante MDS clasico-Animales",
     ylim=range(SK_resul[,2]),xlim=range(SK_resul[,1]),
     xlab="dim 1",ylab="dim 2",type="n",lwd=2)
text(SK_resul[,1], SK_resul[,2],
     labels=abbreviate(row.names(SK_resul),minlength=8),cex=0.6,lwd=2)

#### Varianza explicada por las dos dimensiones
mds_clas_SM$GOF

#### Inciso b)

x1 <- c(1,1,1,1,1,0,1)
x2 <- c(1,1,0,0,0,0,1)
x3 <- c(0,1,0,0,0,0,0)
x4 <- c(0,0,1,1,0,0,0)
x5 <- c(1,0,0,0,1,1,0)
x6 <- c(1,1,1,1,1,0,1)

X_datos <- cbind(x1, x2, x3, x4, x5, x6)#, c_1, c_0)
X_datos

nrow1 <- nrow(X_datos)
ncol1 <- ncol(X_datos)

Sol_Mic_b <- matrix(ncol=7, nrow=7)
Jacard_b <- matrix(ncol=7, nrow=7)
for(i in 1:nrow1)
{
  #print(i)
  for(j in 1:nrow1)
  {
    #print(j)
    m <- Lebron(i,j)
    s <- (m[1]+m[4])/p
    Sol_Mic_b[i,j] <- s
    
    s1 <- (m[1])/(m[1]+m[2]+m[3])
    Jacard_b[i,j] <- s1
  }
}

l_1 <- c(1, 1, 1, 1, 1, 1, 1)
D_1_b <- 2*(l_1%*%(t(l_1))) - 2*Sol_Mic_b
D_2_b <- 2*(l_1%*%(t(l_1))) - 2*Jacard_b

D_1_b
D_2_b
####################
D_1_b[,6]^2



x_ele <- c(1, 1, 0, 0, 0, 1)
d <- t(t(D_1_b[,7]))
d[1:6,1]
d <- t(t(d[1:6,1]))
d

X_33 <- mds_clas_SM$points

#S_11 <- (t(X_33))%*%(X_33)
#ss <- eigen(S_11)

B <- (X_33)%*%(t(X_33))

p1 <- diag(B)
p1s <- eigen(B)
delta <- diag(1/p1s$values,4,4)
b <- t(t(p1))
x_ele_coord <- (1/2)*(delta)%*%(t(X_33))%*%(b-d)
x_ele_coord[1:2,1]


#### realizamos la grafica
vvvv <- c(SK_resul[,1], x_ele_coord[1,1])
vvvv

uuuu <- c(SK_resul[,2], x_ele_coord[2,1])
uuuu

nombre_anim <- c("Leon", "Jirafa", "Vaca", "Oveja", "Gato-domes", 
                 "Hombre", "Elefante")
mi_df <- data.frame(vvvv, uuuu)
dimnames(mi_df)[[1]] <- nombre_anim

plot(mi_df[,1], mi_df[,2],
     main="Configuracion MDS con Elefante",
     ylim=range(mi_df[,2]),xlim=range(mi_df[,1]),
     xlab="dim 1",ylab="dim 2",type="n",lwd=2)
text(mi_df[,1], mi_df[,2],
     labels=abbreviate(row.names(mi_df),minlength=8),cex=0.6,lwd=2)






###### Lo resolvemos tomando en cuenta los valores del elefante ###############
##### Solo para comprobar.

nombre_anim <- c("Leon", "Jirafa", "Vaca", "Oveja", "Gato-domes", 
                 "Hombre", "Elefante")
mds_clas_SM <- cmdscale(sqrt(D_1_b), k = 4, eig = TRUE, add = FALSE, x.ret = FALSE)

SK_resul <- mds_clas_SM$points

#se añaden los nombres de las naciones a la configuracion solucion X
dimnames(SK_resul)[[1]] <- nombre_anim

plot(SK_resul[,1], SK_resul[,2],
     main="configuracion mediante mds clasico",
     ylim=range(SK_resul[,1]),xlim=range(SK_resul[,1]),
     xlab="dim 1",ylab="dim 2",type="n",lwd=2)
text(SK_resul[,1], SK_resul[,2],
     labels=abbreviate(row.names(SK_resul),minlength=8),cex=0.6,lwd=2)




