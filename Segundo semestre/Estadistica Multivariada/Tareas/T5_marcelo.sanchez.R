####################################### TAREA 5 ################################
######## ALUMNO: MARCELO ALBERTO SANCHEZ ZARAGOZA

library("MASS")
library("alr4")
library("stats")
library("scatterplot3d")
library("smacof")
library("ca")

############ EJERCICIO 2
contigencia_f <- matrix(c(10,0,0,0,7,12,0,0,0,5,15,0,0,0,23,19),
                        ncol=4,byrow=TRUE) 
contigencia_f

##### buscamos el valor de n
n <- sum(contigencia_f)
n

#### Realizmos la prueba de la chi-cuadrada
chiR <- chisq.test(contigencia_f)
inetot <- chiR$statistic/n

### buscamos nuestra matriz F
nrow <- nrow(contigencia_f)
ncol <- ncol(contigencia_f)
F_fisher <- (contigencia_f)/n
F_fisher

### realizamos la sumas respectivas de columnas y renglones
rtot <- apply(F_fisher,1,sum)
ctot <- apply(F_fisher,2,sum)
rtot
ctot

### encontramos las matrices Dr y Dc
Dr <- diag(rtot)
Dc <- diag(ctot)

########
Y <- solve(Dr)%*%F_fisher%*%sqrt(solve(Dc))
Y

# se calcula la matriz de perfiles por fila (R)
table.pro <- F_fisher/rtot  #matriz R de renglones

# se calcula la matriz de perfiles por columna (Rc)
table.pcol <- t(t(F_fisher)/ctot)  #matriz R de columnas

#se calcula la matriz Z
Z <- (sqrt(solve(Dr)))%*%as.matrix(F_fisher)%*%(sqrt(solve(Dc)))
Z

### encontramos su descomposición SVD
dvalsing <- svd(Z)

### Tomamos apartir del segundo vector, construismos Cr y Cc
ind<-c(2,3)
Cr <- (sqrt(solve(Dr)))%*%Z%*%dvalsing$v[,ind]
Cr

Cc <- (sqrt(solve(Dc)))%*%t(Z)%*%dvalsing$u[,ind]
Cc

#se calcula la proporcion de la inercia explicada por las dos  dimensiones (asociadas a valores propios mas grandes distintos de uno)
vp<-(dvalsing$d)^2
vp_dist1<-vp[-1]
vp_dist1
var_expl <- sum(vp_dist1[1:2])/sum(vp_dist1)


######## gráfica de mujeres
par(pty="s")
plot(Cr,xlab="Coordenada 1",ylab="Coordenada 2",lwd=2)
text(Cr,labels=c("M1","M2","M3","M4"),col=1,lwd=2)

######## gráfica de hombres
plot(Cc, xlab="Coordenada 1",ylab="Coordenada 2",lwd=2,col=2)
text(Cc,labels=c("H1","H2","H3","H4"),col=2,lwd=2)


######## gráfica de ambos generos
plot(Cr,xlim=c(-0.90,1.70),ylim=c(-1.5,1.08),
     xlab="Coordenada 1",ylab="Coordenada 2",lwd=2)
points(Cc,col=2)
text(Cr,labels=c("M1","M2","M3","M4"),col=1,lwd=2)
text(Cc,labels=c("H1","H2","H3","H4"),col=2,lwd=2)
abline(h=0,lty=2)
abline(v=0,lty=2) 


