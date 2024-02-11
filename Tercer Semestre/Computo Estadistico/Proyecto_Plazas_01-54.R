###############################################################
###############################################################
#### En el siguiente código se trabaja con cada unas de las estaciones
####  de la 01 a la 54


library(readr)
library(tidyverse)
library(fitdistrplus)
library(ciTools)
library(MASS)
library(arm)


################################################################################
########################## PLAZA 01: Boca del Rio  #############################

data_plaza_ <- read_csv("C:/Users/KM Reyes Maya/Downloads/Tercer semestre/Computo Estadistico/Proyecto/Datos_by_Plaza/PLAZA_01.csv")

plaza_ <- data_plaza_[,4:18]
plaza_$Max_Precipitacion_MM = data_plaza_$Max_Precipitacion_MM+0.001
plaza_$Media_Precipitacion = data_plaza_$Media_Precipitacion+0.001

##### Exploración de la variable de respuest
##### ¿La variable sigue una distribución gamma?
fit.gamma <- fitdist(plaza_$Max_Precipitacion_MM,
                     distr = "gamma", method = "mme")

plot.ts(plaza_$Max_Precipitacion_MM)
plot(fit.gamma)


plaza_model<-subset(plaza_,select=-c(mes_12))
regresion_todos = glm(Max_Precipitacion_MM ~.,
                      family = Gamma(link='log'),
                      data = plaza_model)
summary(regresion_todos)


regresion_sim = glm(Max_Precipitacion_MM ~ Media_Precipitacion+
                      mes_1+mes_2+ mes_3+mes_4+mes_5+mes_6+mes_7+ mes_8+mes_9+mes_10+mes_11,
                    family = Gamma(link='log'),
                    data = plaza_model)
summary(regresion_sim)

####### Agregamos la media #####

predi_reg<- data_plaza_[,5:18] ### contemplamos los meses
predi_reg

#se construye el modelo de regresion con los casos completos
lm_media <- lm (Media_Precipitacion ~ mes_1+mes_2+mes_3+mes_4+
                  mes_5+mes_7+mes_8+mes_9+mes_10+mes_11+mes_12, predi_reg)
summary(lm_media)
#nota: lm construye el modelo de regresion implicitamente con los casos completos
#se eligen los valores predichos para los datos faltantes

new_mes_12 <- data.frame(predi_reg$mes_1,predi_reg$mes_2, predi_reg$mes_3, predi_reg$mes_4,
                         predi_reg$mes_5, predi_reg$mes_6, predi_reg$mes_7,
                         predi_reg$mes_8, predi_reg$mes_9, predi_reg$mes_10,
                         predi_reg$mes_11,predi_reg$mes_12)

new_mes_12 <- new_mes_12[12:25,]
names(new_mes_12) <- c('mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                       'mes_9',"mes_10",'mes_11',"mes_12")
pred.1 <- predict (lm_media, new_mes_12)
pred.1

#### Lo unimos
tend<-c(72:85)
imputa_dat <- data.frame(Media_Precipitacion =pred.1, Tendencia= tend, new_mes_12)
imputa_dat
#reg_data <- rbind(predi_reg,imputa_dat)
predi_reg_t <- subset(imputa_dat,select=-c(Tendencia))

### Retiramos las columnas(meses) que no son significativos
# el mes que vaya 

prediccion <- predict(regresion_sim,predi_reg_t,type="response")
predict_mes<-cbind(prediccion,predi_reg_t)
predict_mes$prediccion<-predict_mes$prediccion-0.001
predict_mes$Media_Precipitacion<-predict_mes$Media_Precipitacion-0.001
Plaza_01_pred <- predict_mes

Plaza_01_pred
data_plaza_[60:71,4:18]


maximos <- Plaza_01_pred[3:14,]
mmaximos <-maximos$prediccion
maximo_01 <- max(mmaximos)
indice = match(maximo_01,mmaximos)

Data_P <- data.frame(maximos)
names(Data_P) = c('Max_Precipitacion_MM','Media_Precipitacion',
                  'mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                  'mes_9',"mes_10",'mes_11',"mes_12")
pred <- add_pi(Data_P,regresion_sim, alpha = 0.1, names = c("lwr", "upr"))
conf <- add_ci(Data_P, regresion_sim, alpha = 0.1, names = c("lwr", "upr"))
intervalo_01 <- c(conf$lwr[indice],conf$upr[indice])

###############################################################################
###############################################################################
### CONTRASTES
annos <- as.numeric(unique(data_plaza_$Ano))

lista_mx_01 <-c(rep(0,length(annos)))

for ( i in 1:length(annos)){
  lista_mx_01[i] <- max(data_plaza_[data_plaza_$Ano==annos[i],4]) 
}
lista_mx_01


################################################################################
########################## PLAZA 02: COATZACOALCOS #############################
data_plaza_ <- read_csv("C:/Users/KM Reyes Maya/Downloads/Tercer semestre/Computo Estadistico/Proyecto/Datos_by_Plaza/PLAZA_02.csv")

plaza_ <- data_plaza_[,4:18]
plaza_$Max_Precipitacion_MM = data_plaza_$Max_Precipitacion_MM+0.001
plaza_$Media_Precipitacion = data_plaza_$Media_Precipitacion+0.001

##### Exploración de la variable de respuest
##### ¿La variable sigue una distribución gamma?
fit.gamma <- fitdist(plaza_$Max_Precipitacion_MM,
                     distr = "gamma", method = "mme")

plot.ts(plaza_$Max_Precipitacion_MM)
plot(fit.gamma)


plaza_model<-subset(plaza_,select=-c(mes_12))
regresion_todos = glm(Max_Precipitacion_MM ~.,
                      family = Gamma(link='log'),
                      data = plaza_model)
summary(regresion_todos)


regresion_sim = glm(Max_Precipitacion_MM ~ Media_Precipitacion+
                      mes_1+mes_2+ mes_3+mes_4+mes_5+mes_6+mes_7+ mes_8+mes_9+mes_10+mes_11,
                    family = Gamma(link='log'),
                    data = plaza_model)
summary(regresion_sim)

####### Agregamos la media #####

predi_reg<- data_plaza_[,5:18] ### contemplamos los meses
predi_reg

#se construye el modelo de regresion con los casos completos
lm_media <- lm (Media_Precipitacion ~ mes_1+mes_2+mes_3+mes_4+
                  mes_5+mes_7+mes_8+mes_9+mes_10+mes_11+mes_12, predi_reg)
summary(lm_media)
#nota: lm construye el modelo de regresion implicitamente con los casos completos
#se eligen los valores predichos para los datos faltantes

new_mes_12 <- data.frame(predi_reg$mes_1,predi_reg$mes_2, predi_reg$mes_3, predi_reg$mes_4,
                         predi_reg$mes_5, predi_reg$mes_6, predi_reg$mes_7,
                         predi_reg$mes_8, predi_reg$mes_9, predi_reg$mes_10,
                         predi_reg$mes_11,predi_reg$mes_12)

new_mes_12 <- new_mes_12[12:25,]
names(new_mes_12) <- c('mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                       'mes_9',"mes_10",'mes_11',"mes_12")
pred.1 <- predict (lm_media, new_mes_12)
pred.1

#### Lo unimos
tend<-c(72:85)
imputa_dat <- data.frame(Media_Precipitacion =pred.1, Tendencia= tend, new_mes_12)
imputa_dat
#reg_data <- rbind(predi_reg,imputa_dat)
predi_reg_t <- subset(imputa_dat,select=-c(Tendencia))

### Retiramos las columnas(meses) que no son significativos
# el mes que vaya 

prediccion <- predict(regresion_sim,predi_reg_t,type="response")
predict_mes<-cbind(prediccion,predi_reg_t)
predict_mes$prediccion<-predict_mes$prediccion-0.001
predict_mes$Media_Precipitacion<-predict_mes$Media_Precipitacion-0.001
Plaza_02_pred <- predict_mes

data_plaza_[60:71,4:18]

maximos <- Plaza_02_pred[3:14,]
mmaximos <-maximos$prediccion
maximo_02 <- max(mmaximos)
indice = match(maximo_02,mmaximos)

Data_P <- data.frame(maximos)
names(Data_P) = c('Max_Precipitacion_MM','Media_Precipitacion',
                  'mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                  'mes_9',"mes_10",'mes_11',"mes_12")
pred <- add_pi(Data_P,regresion_sim, alpha = 0.1, names = c("lwr", "upr"))
conf <- add_ci(Data_P, regresion_sim, alpha = 0.1, names = c("lwr", "upr"))
intervalo_02 <- c(conf$lwr[indice],conf$upr[indice])

###############################################################################
###############################################################################
### CONTRASTES
annos <- as.numeric(unique(data_plaza_$Ano))

lista_mx_02 <-c(rep(0,length(annos)))

for ( i in 1:length(annos)){
  lista_mx_02[i] <- max(data_plaza_[data_plaza_$Ano==annos[i],4]) 
}
lista_mx_02



################################################################################
############################## PLAZA 03: CORDOBA  ##############################
data_plaza_ <- read_csv("C:/Users/KM Reyes Maya/Downloads/Tercer semestre/Computo Estadistico/Proyecto/Datos_by_Plaza/PLAZA_03.csv")

plaza_ <- data_plaza_[,4:18]
plaza_$Max_Precipitacion_MM = data_plaza_$Max_Precipitacion_MM+0.001
plaza_$Media_Precipitacion = data_plaza_$Media_Precipitacion+0.001

##### Exploración de la variable de respuest
##### ¿La variable sigue una distribución gamma?
fit.gamma <- fitdist(plaza_$Max_Precipitacion_MM,
                     distr = "gamma", method = "mme")

plot.ts(plaza_$Max_Precipitacion_MM)
plot(fit.gamma)


plaza_model<-subset(plaza_,select=-c(mes_12))
regresion_todos = glm(Max_Precipitacion_MM ~.,
                      family = Gamma(link='log'),
                      data = plaza_model)
summary(regresion_todos)


regresion_sim = glm(Max_Precipitacion_MM ~ Media_Precipitacion+
                      mes_1+mes_2+ mes_3+mes_4+mes_5+mes_6+mes_7+ mes_8+mes_9+mes_10+mes_11,
                    family = Gamma(link='log'),
                    data = plaza_model)
summary(regresion_sim)

####### Agregamos la media #####

predi_reg<- data_plaza_[,5:18] ### contemplamos los meses
predi_reg

#se construye el modelo de regresion con los casos completos
lm_media <- lm (Media_Precipitacion ~ mes_1+mes_2+mes_3+mes_4+
                  mes_5+mes_7+mes_8+mes_9+mes_10+mes_11+mes_12, predi_reg)
summary(lm_media)
#nota: lm construye el modelo de regresion implicitamente con los casos completos
#se eligen los valores predichos para los datos faltantes

new_mes_12 <- data.frame(predi_reg$mes_1,predi_reg$mes_2, predi_reg$mes_3, predi_reg$mes_4,
                         predi_reg$mes_5, predi_reg$mes_6, predi_reg$mes_7,
                         predi_reg$mes_8, predi_reg$mes_9, predi_reg$mes_10,
                         predi_reg$mes_11,predi_reg$mes_12)

new_mes_12 <- new_mes_12[12:25,]
names(new_mes_12) <- c('mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                       'mes_9',"mes_10",'mes_11',"mes_12")
pred.1 <- predict (lm_media, new_mes_12)
pred.1

#### Lo unimos
tend<-c(72:85)
imputa_dat <- data.frame(Media_Precipitacion =pred.1, Tendencia= tend, new_mes_12)
imputa_dat
#reg_data <- rbind(predi_reg,imputa_dat)
predi_reg_t <- subset(imputa_dat,select=-c(Tendencia))

### Retiramos las columnas(meses) que no son significativos
# el mes que vaya 


prediccion <- predict(regresion_sim,predi_reg_t,type="response")
predict_mes<-cbind(prediccion,predi_reg_t)
predict_mes$prediccion<-predict_mes$prediccion-0.001
predict_mes$Media_Precipitacion<-predict_mes$Media_Precipitacion-0.001
Plaza_03_pred <- predict_mes

data_plaza_[60:71,4:18]

maximos <- Plaza_03_pred[3:14,]
mmaximos <-maximos$prediccion
maximo_03 <- max(mmaximos)
indice = match(maximo_03,mmaximos)

Data_P <- data.frame(maximos)
names(Data_P) = c('Max_Precipitacion_MM','Media_Precipitacion',
                  'mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                  'mes_9',"mes_10",'mes_11',"mes_12")
pred <- add_pi(Data_P,regresion_sim, alpha = 0.1, names = c("lwr", "upr"))
conf <- add_ci(Data_P, regresion_sim, alpha = 0.1, names = c("lwr", "upr"))
intervalo_03 <- c(conf$lwr[indice],conf$upr[indice])



###############################################################################
###############################################################################
### CONTRASTES
annos <- as.numeric(unique(data_plaza_$Ano))

lista_mx_03 <-c(rep(0,length(annos)))

for ( i in 1:length(annos)){
  lista_mx_03[i] <- max(data_plaza_[data_plaza_$Ano==annos[i],4]) 
}
lista_mx_03



################################################################################
########################## PLAZA 04: POZA RICA #############################
data_plaza_ <- read_csv("C:/Users/KM Reyes Maya/Downloads/Tercer semestre/Computo Estadistico/Proyecto/Datos_by_Plaza/PLAZA_04.csv")

plaza_ <- data_plaza_[,4:18]
plaza_$Max_Precipitacion_MM = data_plaza_$Max_Precipitacion_MM+0.001
plaza_$Media_Precipitacion = data_plaza_$Media_Precipitacion+0.001

##### Exploración de la variable de respuest
##### ¿La variable sigue una distribución gamma?
fit.gamma <- fitdist(plaza_$Max_Precipitacion_MM,
                     distr = "gamma", method = "mme")

plot.ts(plaza_$Max_Precipitacion_MM)
plot(fit.gamma)


plaza_model<-subset(plaza_,select=-c(mes_12))
regresion_todos = glm(Max_Precipitacion_MM ~.,
                      family = Gamma(link='log'),
                      data = plaza_model)
summary(regresion_todos)


regresion_sim = glm(Max_Precipitacion_MM ~  Media_Precipitacion+
                      mes_1+mes_2+ mes_3+mes_4+mes_5+mes_6+mes_7+ mes_8+mes_9+mes_10+mes_11,
                    family = Gamma(link='log'),
                    data = plaza_model)
summary(regresion_sim)

####### Agregamos la media #####

predi_reg<- data_plaza_[,5:18] ### contemplamos los meses
predi_reg

#se construye el modelo de regresion con los casos completos
lm_media <- lm (Media_Precipitacion ~ mes_1+mes_2+mes_3+
                  mes_4+mes_5+mes_7+mes_8+mes_9+mes_10+mes_11+mes_12, predi_reg)
summary(lm_media)
#nota: lm construye el modelo de regresion implicitamente con los casos completos
#se eligen los valores predichos para los datos faltantes

new_mes_12 <- data.frame(predi_reg$mes_1,predi_reg$mes_2, predi_reg$mes_3, predi_reg$mes_4,
                         predi_reg$mes_5, predi_reg$mes_6, predi_reg$mes_7,
                         predi_reg$mes_8, predi_reg$mes_9, predi_reg$mes_10,
                         predi_reg$mes_11,predi_reg$mes_12)

new_mes_12 <- new_mes_12[12:25,]
names(new_mes_12) <- c('mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                       'mes_9',"mes_10",'mes_11',"mes_12")
pred.1 <- predict (lm_media, new_mes_12)
pred.1

#### Lo unimos
tend<-c(72:85)
imputa_dat <- data.frame(Media_Precipitacion =pred.1, Tendencia= tend, new_mes_12)
imputa_dat
#reg_data <- rbind(predi_reg,imputa_dat)
predi_reg_t <- subset(imputa_dat,select=-c(Tendencia))

### Retiramos las columnas(meses) que no son significativos
# el mes que vaya 

prediccion <- predict(regresion_sim,predi_reg_t,type="response")
predict_mes<-cbind(prediccion,predi_reg_t)
predict_mes$prediccion<-predict_mes$prediccion-0.001
predict_mes$Media_Precipitacion<-predict_mes$Media_Precipitacion-0.001
Plaza_04_pred <- predict_mes

data_plaza_[60:71,]

maximos <- Plaza_04_pred[3:14,]
mmaximos <-maximos$prediccion
maximo_04 <- max(mmaximos)
indice = match(maximo_04,mmaximos)

Data_P <- data.frame(maximos)
names(Data_P) = c('Max_Precipitacion_MM','Media_Precipitacion',
                  'mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                  'mes_9',"mes_10",'mes_11',"mes_12")
pred <- add_pi(Data_P,regresion_sim, alpha = 0.1, names = c("lwr", "upr"))
conf <- add_ci(Data_P, regresion_sim, alpha = 0.1, names = c("lwr", "upr"))
intervalo_04 <- c(conf$lwr[indice],conf$upr[indice])


###############################################################################
###############################################################################
### CONTRASTES
annos <- as.numeric(unique(data_plaza_$Ano))

lista_mx_04 <-c(rep(0,length(annos)))

for ( i in 1:length(annos)){
  lista_mx_04[i] <- max(data_plaza_[data_plaza_$Ano==annos[i],4]) 
}
lista_mx_04



################################################################################
########################## PLAZA 05: XALAPA #############################
data_plaza_ <- read_csv("C:/Users/KM Reyes Maya/Downloads/Tercer semestre/Computo Estadistico/Proyecto/Datos_by_Plaza/PLAZA_05.csv")

plaza_ <- data_plaza_[,4:18]
plaza_$Max_Precipitacion_MM = data_plaza_$Max_Precipitacion_MM+0.001
plaza_$Media_Precipitacion = data_plaza_$Media_Precipitacion+0.001

##### Exploración de la variable de respuest
##### ¿La variable sigue una distribución gamma?
fit.gamma <- fitdist(plaza_$Max_Precipitacion_MM,
                     distr = "gamma", method = "mme")

plot.ts(plaza_$Max_Precipitacion_MM)
plot(fit.gamma)


plaza_model<-subset(plaza_,select=-c(mes_12))
regresion_todos = glm(Max_Precipitacion_MM ~.,
                      family = Gamma(link='log'),
                      data = plaza_model)
summary(regresion_todos)


regresion_sim = glm(Max_Precipitacion_MM ~ Media_Precipitacion+
                      mes_1+mes_2+ mes_3+mes_4+mes_5+mes_6+mes_7+ mes_8+mes_9+mes_10+mes_11,
                    family = Gamma(link='log'),
                    data = plaza_model)
summary(regresion_sim)

####### Agregamos la media #####

predi_reg<- data_plaza_[,5:18] ### contemplamos los meses
predi_reg

#se construye el modelo de regresion con los casos completos
lm_media <- lm (Media_Precipitacion ~ mes_1+mes_2+mes_3+
                  mes_4+mes_5+mes_7+mes_8+mes_9+mes_10+mes_11+mes_12, predi_reg)
summary(lm_media)
#nota: lm construye el modelo de regresion implicitamente con los casos completos
#se eligen los valores predichos para los datos faltantes

new_mes_12 <- data.frame(predi_reg$mes_1,predi_reg$mes_2, predi_reg$mes_3, predi_reg$mes_4,
                         predi_reg$mes_5, predi_reg$mes_6, predi_reg$mes_7,
                         predi_reg$mes_8, predi_reg$mes_9, predi_reg$mes_10,
                         predi_reg$mes_11,predi_reg$mes_12)

new_mes_12 <- new_mes_12[12:25,]
names(new_mes_12) <- c('mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                       'mes_9',"mes_10",'mes_11',"mes_12")
pred.1 <- predict (lm_media, new_mes_12)
pred.1

#### Lo unimos
tend<-c(72:85)
imputa_dat <- data.frame(Media_Precipitacion =pred.1, Tendencia= tend, new_mes_12)
imputa_dat
#reg_data <- rbind(predi_reg,imputa_dat)
predi_reg_t <- subset(imputa_dat,select=-c(Tendencia))

### Retiramos las columnas(meses) que no son significativos
# el mes que vaya 

prediccion <- predict(regresion_sim,predi_reg_t,type="response")
predict_mes<-cbind(prediccion,predi_reg_t)
predict_mes$prediccion<-predict_mes$prediccion-0.001
predict_mes$Media_Precipitacion<-predict_mes$Media_Precipitacion-0.001
Plaza_05_pred <- predict_mes
Plaza_05_pred

data_plaza_[60:71,]


maximos <- Plaza_05_pred[3:14,]
mmaximos <-maximos$prediccion
maximo_05 <- max(mmaximos)
indice = match(maximo_05,mmaximos)

Data_P <- data.frame(maximos)
names(Data_P) = c('Max_Precipitacion_MM','Media_Precipitacion',
                  'mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                  'mes_9',"mes_10",'mes_11',"mes_12")
pred <- add_pi(Data_P,regresion_sim, alpha = 0.1, names = c("lwr", "upr"))
conf <- add_ci(Data_P, regresion_sim, alpha = 0.1, names = c("lwr", "upr"))
intervalo_05 <- c(conf$lwr[indice],conf$upr[indice])

###############################################################################
###############################################################################
### CONTRASTES
annos <- as.numeric(unique(data_plaza_$Ano))

lista_mx_05 <-c(rep(0,length(annos)))

for ( i in 1:length(annos)){
  lista_mx_05[i] <- max(data_plaza_[data_plaza_$Ano==annos[i],4]) 
}
lista_mx_05


################################################################################
########################## PLAZA 06: ACAPULCO #############################
data_plaza_ <- read_csv("C:/Users/KM Reyes Maya/Downloads/Tercer semestre/Computo Estadistico/Proyecto/Datos_by_Plaza/PLAZA_06.csv")

plaza_ <- data_plaza_[,4:18]
plaza_$Max_Precipitacion_MM = data_plaza_$Max_Precipitacion_MM+0.001
plaza_$Media_Precipitacion = data_plaza_$Media_Precipitacion+0.001

##### Exploración de la variable de respuest
##### ¿La variable sigue una distribución gamma?
fit.gamma <- fitdist(plaza_$Max_Precipitacion_MM,
                     distr = "gamma", method = "mme")

plot.ts(plaza_$Max_Precipitacion_MM)
plot(fit.gamma)


plaza_model<-subset(plaza_,select=-c(mes_12))
regresion_todos = glm(Max_Precipitacion_MM ~.,
                      family = Gamma(link='log'),
                      data = plaza_model)
summary(regresion_todos)


regresion_sim = glm(Max_Precipitacion_MM ~ Media_Precipitacion+
                      mes_1+mes_2+ mes_3+mes_4+mes_5+mes_6+mes_7+ mes_8+mes_9+mes_10+mes_11,
                    family = Gamma(link='log'),
                    data = plaza_model)
summary(regresion_sim)
plaza_model




####### Agregamos la media #####

predi_reg<- data_plaza_[,5:18] ### contemplamos los meses
predi_reg

#se construye el modelo de regresion con los casos completos
lm_media <- lm (Media_Precipitacion ~ mes_1+mes_2+mes_3+
                  mes_4+mes_5+mes_6+mes_7+mes_8+mes_9+mes_10+mes_11, predi_reg)
summary(lm_media)
#nota: lm construye el modelo de regresion implicitamente con los casos completos
#se eligen los valores predichos para los datos faltantes

new_mes_12 <- data.frame(predi_reg$mes_1,predi_reg$mes_2, predi_reg$mes_3, predi_reg$mes_4,
                         predi_reg$mes_5, predi_reg$mes_6, predi_reg$mes_7,
                         predi_reg$mes_8, predi_reg$mes_9, predi_reg$mes_10,
                         predi_reg$mes_11,predi_reg$mes_12)

new_mes_12 <- new_mes_12[12:25,]
names(new_mes_12) <- c('mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                       'mes_9',"mes_10",'mes_11',"mes_12")
pred.1 <- predict (lm_media, new_mes_12)
pred.1

#### Lo unimos
tend<-c(72:85)
imputa_dat <- data.frame(Media_Precipitacion =pred.1, Tendencia= tend, new_mes_12)
imputa_dat
#reg_data <- rbind(predi_reg,imputa_dat)
predi_reg_t <- subset(imputa_dat,select=-c(Tendencia))
predi_reg_t
new_mes_12
### Retiramos las columnas(meses) que no son significativos
# el mes que vaya 
Tendencia<-tend
prueba<-data.frame(predi_reg_t,Tendencia)
prueba
lm_mmmm <- lm (Max_Precipitacion_MM ~ Media_Precipitacion+ Tendencia+
                 mes_1+mes_2+ mes_3+mes_4+mes_5+mes_6+mes_7+ mes_8+mes_9+mes_10+mes_11, plaza_model)
summary(lm_mmmm)
predict(lm_mmmm,prueba)



prediccion <- predict(regresion_sim,predi_reg_t,type="response")
predict_mes<-cbind(prediccion,predi_reg_t)
predict_mes$prediccion<-predict_mes$prediccion-0.001
predict_mes$Media_Precipitacion<-predict_mes$Media_Precipitacion-0.001
Plaza_06_pred <- predict_mes
Plaza_06_pred

data_plaza_[60:71,]


maximos <- Plaza_06_pred[3:14,]
mmaximos <-maximos$prediccion
maximo_06 <- max(mmaximos)
indice = match(maximo_06,mmaximos)

Data_P <- data.frame(maximos)
names(Data_P) = c('Max_Precipitacion_MM','Media_Precipitacion',
                  'mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                  'mes_9',"mes_10",'mes_11',"mes_12")
pred <- add_pi(Data_P,regresion_sim, alpha = 0.1, names = c("lwr", "upr"))
conf <- add_ci(Data_P, regresion_sim, alpha = 0.1, names = c("lwr", "upr"))
intervalo_06 <- c(conf$lwr[indice],conf$upr[indice])
intervalo_06
###############################################################################
###############################################################################
### CONTRASTES
annos <- as.numeric(unique(data_plaza_$Ano))

lista_mx_06 <-c(rep(0,length(annos)))

for ( i in 1:length(annos)){
  lista_mx_06[i] <- max(data_plaza_[data_plaza_$Ano==annos[i],4]) 
}
lista_mx_06




################################################################################
########################## PLAZA 07: Aguascalientes  #############################
data_plaza_ <- read_csv("C:/Users/KM Reyes Maya/Downloads/Tercer semestre/Computo Estadistico/Proyecto/Datos_by_Plaza/PLAZA_07.csv")

plaza_ <- data_plaza_[,4:18]
plaza_$Max_Precipitacion_MM = data_plaza_$Max_Precipitacion_MM+0.001
plaza_$Media_Precipitacion = data_plaza_$Media_Precipitacion+0.001

##### Exploración de la variable de respuest
##### ¿La variable sigue una distribución gamma?
fit.gamma <- fitdist(plaza_$Max_Precipitacion_MM,
                     distr = "gamma", method = "mme")

plot.ts(plaza_$Max_Precipitacion_MM)
plot(fit.gamma)


plaza_model<-subset(plaza_,select=-c(mes_1))
regresion_todos = glm(Max_Precipitacion_MM ~.,
                      family = Gamma(link='log'),
                      data = plaza_model)
summary(regresion_todos)


regresion_sim = glm(Max_Precipitacion_MM ~ Media_Precipitacion+
                      mes_1+mes_2+ mes_3+mes_4+mes_5+mes_6+mes_7+ mes_8+mes_9+mes_10+mes_11,
                    family = Gamma(link='log'),
                    data = plaza_model)
summary(regresion_sim)

####### Agregamos la media #####

predi_reg<- data_plaza_[,5:18] ### contemplamos los meses
predi_reg

#se construye el modelo de regresion con los casos completos
lm_media <- lm (Media_Precipitacion ~ mes_1+mes_2+mes_3+
                  mes_4+mes_5+mes_7+mes_8+mes_9+mes_10+mes_11+mes_12, predi_reg)
summary(lm_media)
#nota: lm construye el modelo de regresion implicitamente con los casos completos
#se eligen los valores predichos para los datos faltantes

new_mes_12 <- data.frame(predi_reg$mes_1,predi_reg$mes_2, predi_reg$mes_3, predi_reg$mes_4,
                         predi_reg$mes_5, predi_reg$mes_6, predi_reg$mes_7,
                         predi_reg$mes_8, predi_reg$mes_9, predi_reg$mes_10,
                         predi_reg$mes_11,predi_reg$mes_12)

new_mes_12 <- new_mes_12[12:25,]
names(new_mes_12) <- c('mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                       'mes_9',"mes_10",'mes_11',"mes_12")
pred.1 <- predict (lm_media, new_mes_12)
pred.1

#### Lo unimos
tend<-c(72:85)
imputa_dat <- data.frame(Media_Precipitacion =pred.1, Tendencia= tend, new_mes_12)
imputa_dat
#reg_data <- rbind(predi_reg,imputa_dat)
predi_reg_t <- subset(imputa_dat,select=-c(Tendencia))

### Retiramos las columnas(meses) que no son significativos
# el mes que vaya 

prediccion <- predict(regresion_sim,predi_reg_t,type="response")
predict_mes<-cbind(prediccion,predi_reg_t)
predict_mes$prediccion<-predict_mes$prediccion-0.001
predict_mes$Media_Precipitacion<-predict_mes$Media_Precipitacion-0.001
Plaza_07_pred <- predict_mes
Plaza_07_pred

data_plaza_[60:71,]

maximos <- Plaza_07_pred[3:14,]
mmaximos <-maximos$prediccion
maximo_07 <- max(mmaximos)
indice = match(maximo_07,mmaximos)

Data_P <- data.frame(maximos)
names(Data_P) = c('Max_Precipitacion_MM','Media_Precipitacion',
                  'mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                  'mes_9',"mes_10",'mes_11',"mes_12")
pred <- add_pi(Data_P,regresion_sim, alpha = 0.1, names = c("lwr", "upr"))
conf <- add_ci(Data_P, regresion_sim, alpha = 0.1, names = c("lwr", "upr"))
intervalo_07 <- c(conf$lwr[indice],conf$upr[indice])

###############################################################################
###############################################################################
### CONTRASTES
annos <- as.numeric(unique(data_plaza_$Ano))

lista_mx_07 <-c(rep(0,length(annos)))

for ( i in 1:length(annos)){
  lista_mx_07[i] <- max(data_plaza_[data_plaza_$Ano==annos[i],4]) 
}
lista_mx_07



################################################################################
########################## PLAZA 08:  Cancun#############################
data_plaza_ <- read_csv("C:/Users/KM Reyes Maya/Downloads/Tercer semestre/Computo Estadistico/Proyecto/Datos_by_Plaza/PLAZA_08.csv")

plaza_ <- data_plaza_[,4:18]
plaza_$Max_Precipitacion_MM = data_plaza_$Max_Precipitacion_MM+0.001
plaza_$Media_Precipitacion = data_plaza_$Media_Precipitacion+0.001

##### Exploración de la variable de respuest
##### ¿La variable sigue una distribución gamma?
fit.gamma <- fitdist(plaza_$Max_Precipitacion_MM,
                     distr = "gamma", method = "mme")

plot.ts(plaza_$Max_Precipitacion_MM)
plot(fit.gamma)


plaza_model<-subset(plaza_,select=-c(mes_12))
plaza_model
regresion_todos = glm(Max_Precipitacion_MM ~.,
                      family = Gamma(link='log'),
                      data = plaza_model)
summary(regresion_todos)


regresion_sim = glm(Max_Precipitacion_MM ~ 1/Media_Precipitacion+
                      mes_2+ mes_3+mes_4+mes_5+mes_6+mes_7+ mes_8+mes_9+mes_10+mes_11,
                    family = Gamma(link="identity"),
                    data = plaza_model)
summary(regresion_sim)

####### Agregamos la media #####

predi_reg<- data_plaza_[,5:18] ### contemplamos los meses
predi_reg

#se construye el modelo de regresion con los casos completos
lm_media <- lm (Media_Precipitacion ~ mes_1+mes_2+mes_3+
                  mes_4+mes_5+mes_7+mes_8+mes_9+mes_10+mes_11+mes_12, predi_reg)
summary(lm_media)
#nota: lm construye el modelo de regresion implicitamente con los casos completos
#se eligen los valores predichos para los datos faltantes

new_mes_12 <- data.frame(predi_reg$mes_1,predi_reg$mes_2, predi_reg$mes_3, predi_reg$mes_4,
                         predi_reg$mes_5, predi_reg$mes_6, predi_reg$mes_7,
                         predi_reg$mes_8, predi_reg$mes_9, predi_reg$mes_10,
                         predi_reg$mes_11,predi_reg$mes_12)

new_mes_12 <- new_mes_12[12:25,]
names(new_mes_12) <- c('mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                       'mes_9',"mes_10",'mes_11',"mes_12")
pred.1 <- predict (lm_media, new_mes_12)
pred.1

#### Lo unimos
tend<-c(72:85)
imputa_dat <- data.frame(Media_Precipitacion =pred.1, Tendencia= tend, new_mes_12)
imputa_dat
#reg_data <- rbind(predi_reg,imputa_dat)
predi_reg_t <- subset(imputa_dat,select=-c(Tendencia))

### Retiramos las columnas(meses) que no son significativos
# el mes que vaya 

prediccion <- predict(regresion_sim,predi_reg_t,type="response")
predict_mes<-cbind(prediccion,predi_reg_t)
predict_mes$prediccion<-predict_mes$prediccion-0.001
predict_mes$Media_Precipitacion<-predict_mes$Media_Precipitacion-0.001
Plaza_08_pred <- predict_mes
Plaza_08_pred

data_plaza_[60:71,]

maximos <- Plaza_08_pred[3:14,]
mmaximos <-maximos$prediccion
maximo_08 <- max(mmaximos)
indice = match(maximo_08,mmaximos)

Data_P <- data.frame(maximos)
names(Data_P) = c('Max_Precipitacion_MM','Media_Precipitacion',
                  'mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                  'mes_9',"mes_10",'mes_11',"mes_12")
pred <- add_pi(Data_P,regresion_sim, alpha = 0.1, names = c("lwr", "upr"))
conf <- add_ci(Data_P, regresion_sim, alpha = 0.1, names = c("lwr", "upr"))
intervalo_08 <- c(conf$lwr[indice],conf$upr[indice])


###############################################################################
###############################################################################
### CONTRASTES
annos <- as.numeric(unique(data_plaza_$Ano))

lista_mx_08 <-c(rep(0,length(annos)))

for ( i in 1:length(annos)){
  lista_mx_08[i] <- max(data_plaza_[data_plaza_$Ano==annos[i],4]) 
}
lista_mx_08




################################################################################
########################## PLAZA 09: Chihuahua #############################
data_plaza_ <- read_csv("C:/Users/KM Reyes Maya/Downloads/Tercer semestre/Computo Estadistico/Proyecto/Datos_by_Plaza/PLAZA_09.csv")

plaza_ <- data_plaza_[,4:18]
plaza_$Max_Precipitacion_MM = data_plaza_$Max_Precipitacion_MM+0.001
plaza_$Media_Precipitacion = data_plaza_$Media_Precipitacion+0.001

##### Exploración de la variable de respuest
##### ¿La variable sigue una distribución gamma?
fit.gamma <- fitdist(plaza_$Max_Precipitacion_MM,
                     distr = "gamma", method = "mme")

plot.ts(plaza_$Max_Precipitacion_MM)
plot(fit.gamma)


plaza_model<-subset(plaza_,select=-c(mes_12))
regresion_todos = glm(Max_Precipitacion_MM ~.,
                      family = Gamma(link='log'),
                      data = plaza_model)
summary(regresion_todos)


regresion_sim = glm(Max_Precipitacion_MM ~ Media_Precipitacion+
                      mes_1+mes_2+ mes_3+mes_4+mes_5+mes_6+mes_7+ mes_8+mes_9+mes_10+mes_11,
                    family = Gamma(link='log'),
                    data = plaza_model)
summary(regresion_sim)

####### Agregamos la media #####

predi_reg<- data_plaza_[,5:18] ### contemplamos los meses
predi_reg

#se construye el modelo de regresion con los casos completos
lm_media <- lm (Media_Precipitacion ~ mes_1+mes_2+mes_3+
                  mes_4+mes_5+mes_7+mes_8+mes_9+mes_10+mes_11+mes_12, predi_reg)
summary(lm_media)
#nota: lm construye el modelo de regresion implicitamente con los casos completos
#se eligen los valores predichos para los datos faltantes

new_mes_12 <- data.frame(predi_reg$mes_1,predi_reg$mes_2, predi_reg$mes_3, predi_reg$mes_4,
                         predi_reg$mes_5, predi_reg$mes_6, predi_reg$mes_7,
                         predi_reg$mes_8, predi_reg$mes_9, predi_reg$mes_10,
                         predi_reg$mes_11,predi_reg$mes_12)

new_mes_12 <- new_mes_12[12:25,]
names(new_mes_12) <- c('mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                       'mes_9',"mes_10",'mes_11',"mes_12")
pred.1 <- predict (lm_media, new_mes_12)
pred.1

#### Lo unimos
tend<-c(72:85)
imputa_dat <- data.frame(Media_Precipitacion =pred.1, Tendencia= tend, new_mes_12)
imputa_dat
#reg_data <- rbind(predi_reg,imputa_dat)
predi_reg_t <- subset(imputa_dat,select=-c(Tendencia))

### Retiramos las columnas(meses) que no son significativos
# el mes que vaya 

prediccion <- predict(regresion_sim,predi_reg_t,type="response")
predict_mes<-cbind(prediccion,predi_reg_t)
predict_mes$prediccion<-predict_mes$prediccion-0.001
predict_mes$Media_Precipitacion<-predict_mes$Media_Precipitacion-0.001
Plaza_09_pred <- predict_mes
Plaza_09_pred

data_plaza_[60:71,]


maximos <- Plaza_09_pred[3:14,]
mmaximos <-maximos$prediccion
maximo_09 <- max(mmaximos)
indice = match(maximo_09,mmaximos)

Data_P <- data.frame(maximos)
names(Data_P) = c('Max_Precipitacion_MM','Media_Precipitacion',
                  'mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                  'mes_9',"mes_10",'mes_11',"mes_12")
pred <- add_pi(Data_P,regresion_sim, alpha = 0.1, names = c("lwr", "upr"))
conf <- add_ci(Data_P, regresion_sim, alpha = 0.1, names = c("lwr", "upr"))
intervalo_09 <- c(conf$lwr[indice],conf$upr[indice])

###############################################################################
###############################################################################
### CONTRASTES
annos <- as.numeric(unique(data_plaza_$Ano))

lista_mx_09 <-c(rep(0,length(annos)))

for ( i in 1:length(annos)){
  lista_mx_09[i] <- max(data_plaza_[data_plaza_$Ano==annos[i],4]) 
}
lista_mx_09


################################################################################
########################## PLAZA 10:  Ciudad Juarez #############################
data_plaza_ <- read_csv("C:/Users/KM Reyes Maya/Downloads/Tercer semestre/Computo Estadistico/Proyecto/Datos_by_Plaza/PLAZA_10.csv")

plaza_ <- data_plaza_[,4:18]
plaza_$Max_Precipitacion_MM = data_plaza_$Max_Precipitacion_MM+0.001
plaza_$Media_Precipitacion = data_plaza_$Media_Precipitacion+0.001

##### Exploración de la variable de respuest
##### ¿La variable sigue una distribución gamma?
fit.gamma <- fitdist(plaza_$Max_Precipitacion_MM,
                     distr = "gamma", method = "mme")

plot.ts(plaza_$Max_Precipitacion_MM)
plot(fit.gamma)


plaza_model<-subset(plaza_,select=-c(mes_12))
regresion_todos = glm(Max_Precipitacion_MM ~.,
                      family = Gamma(link='log'),
                      data = plaza_model)
summary(regresion_todos)


regresion_sim = glm(Max_Precipitacion_MM ~ Media_Precipitacion+
                      mes_1+mes_2+ mes_3+mes_4+mes_5+mes_6+mes_7+ mes_8+mes_9+mes_10+mes_11,
                    family = Gamma(link='log'),
                    data = plaza_model)
summary(regresion_sim)


####### Agregamos la media #####

predi_reg<- data_plaza_[,5:18] ### contemplamos los meses
predi_reg

#se construye el modelo de regresion con los casos completos
lm_media <- lm (Media_Precipitacion ~ mes_1+mes_2+mes_3+
                  mes_4+mes_5+mes_7+mes_8+mes_9+mes_10+mes_11+mes_12, predi_reg)
summary(lm_media)
#nota: lm construye el modelo de regresion implicitamente con los casos completos
#se eligen los valores predichos para los datos faltantes

new_mes_12 <- data.frame(predi_reg$mes_1,predi_reg$mes_2, predi_reg$mes_3, predi_reg$mes_4,
                         predi_reg$mes_5, predi_reg$mes_6, predi_reg$mes_7,
                         predi_reg$mes_8, predi_reg$mes_9, predi_reg$mes_10,
                         predi_reg$mes_11,predi_reg$mes_12)

new_mes_12 <- new_mes_12[12:25,]
names(new_mes_12) <- c('mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                       'mes_9',"mes_10",'mes_11',"mes_12")
pred.1 <- predict (lm_media, new_mes_12)
pred.1

#### Lo unimos
tend<-c(72:85)
imputa_dat <- data.frame(Media_Precipitacion =pred.1, Tendencia= tend, new_mes_12)
imputa_dat
#reg_data <- rbind(predi_reg,imputa_dat)
predi_reg_t <- subset(imputa_dat,select=-c(Tendencia))

### Retiramos las columnas(meses) que no son significativos
# el mes que vaya 

prediccion <- predict(regresion_sim,predi_reg_t,type="response")
predict_mes<-cbind(prediccion,predi_reg_t)
predict_mes$prediccion<-predict_mes$prediccion-0.001
predict_mes$Media_Precipitacion<-predict_mes$Media_Precipitacion-0.001
Plaza_10_pred <- predict_mes
Plaza_10_pred

data_plaza_[60:71,4:18]


maximos <- Plaza_10_pred[3:14,]
mmaximos <-maximos$prediccion
maximo_10 <- max(mmaximos)
indice = match(maximo_10,mmaximos)

Data_P <- data.frame(maximos)
names(Data_P) = c('Max_Precipitacion_MM','Media_Precipitacion',
                  'mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                  'mes_9',"mes_10",'mes_11',"mes_12")
pred <- add_pi(Data_P,regresion_sim, alpha = 0.1, names = c("lwr", "upr"))
conf <- add_ci(Data_P, regresion_sim, alpha = 0.1, names = c("lwr", "upr"))
add_ci(Data_P, regresion_sim, alpha = 0.1, names = c("lwr", "upr"))
intervalo_10 <- c(conf$lwr[indice],conf$upr[indice])
intervalo_10
###############################################################################
###############################################################################
### CONTRASTES
annos <- as.numeric(unique(data_plaza_$Ano))

lista_mx_10 <-c(rep(0,length(annos)))

for ( i in 1:length(annos)){
  lista_mx_10[i] <- max(data_plaza_[data_plaza_$Ano==annos[i],4]) 
}
lista_mx_10


################################################################################
########################## PLAZA 11: Colima #############################
data_plaza_ <- read_csv("C:/Users/KM Reyes Maya/Downloads/Tercer semestre/Computo Estadistico/Proyecto/Datos_by_Plaza/PLAZA_11.csv")

plaza_ <- data_plaza_[,4:18]
plaza_$Max_Precipitacion_MM = data_plaza_$Max_Precipitacion_MM+0.001
plaza_$Media_Precipitacion = data_plaza_$Media_Precipitacion+0.001

##### Exploración de la variable de respuest
##### ¿La variable sigue una distribución gamma?
fit.gamma <- fitdist(plaza_$Max_Precipitacion_MM,
                     distr = "gamma", method = "mme")


plot.ts(plaza_$Max_Precipitacion_MM)
plot(fit.gamma)


plaza_model<-subset(plaza_,select=-c(mes_12))
regresion_todos = glm(Max_Precipitacion_MM ~.,
                      family = Gamma(link='log'),
                      data = plaza_model)
summary(regresion_todos)


regresion_sim = glm(Max_Precipitacion_MM ~ Media_Precipitacion+
                      mes_1+mes_2+ mes_3+mes_4+mes_5+mes_6+mes_7+ mes_8+mes_9+mes_10+mes_11,
                    family = Gamma(link='log'),
                    data = plaza_model)
summary(regresion_sim)

confint(regresion_sim)

par(mfrow=c(1,2))
residuales = resid(regresion_sim,type='pearson')
plot(fitted(regresion_sim),residuales);abline(h=0,col='red')
qqnorm(resid(regresion_sim));qqline(resid(regresion_sim),col='red')
####### Agregamos la media #####

predi_reg<- data_plaza_[,5:18] ### contemplamos los meses
predi_reg

par(mfrow=c(1,1))
#se construye el modelo de regresion con los casos completos
lm_media <- lm (Media_Precipitacion ~ mes_1+mes_2+mes_3+
                  mes_4+mes_5+mes_6+mes_7+mes_8+mes_9+mes_10+mes_11, predi_reg)
summary(lm_media)
#nota: lm construye el modelo de regresion implicitamente con los casos completos
#se eligen los valores predichos para los datos faltantes

new_mes_12 <- data.frame(predi_reg$mes_1,predi_reg$mes_2, predi_reg$mes_3, predi_reg$mes_4,
                         predi_reg$mes_5, predi_reg$mes_6, predi_reg$mes_7,
                         predi_reg$mes_8, predi_reg$mes_9, predi_reg$mes_10,
                         predi_reg$mes_11,predi_reg$mes_12)

new_mes_12 <- new_mes_12[12:25,]
names(new_mes_12) <- c('mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                       'mes_9',"mes_10",'mes_11',"mes_12")
pred.1 <- predict (lm_media, new_mes_12)
pred.1

#### Lo unimos
tend<-c(72:85)
imputa_dat <- data.frame(Media_Precipitacion =pred.1, Tendencia= tend, new_mes_12)
imputa_dat
#reg_data <- rbind(predi_reg,imputa_dat)
predi_reg_t <- subset(imputa_dat,select=-c(Tendencia))

### Retiramos las columnas(meses) que no son significativos
# el mes que vaya 

prediccion <- predict(regresion_sim,predi_reg_t,type="response")
predict_mes<-cbind(prediccion,predi_reg_t)
predict_mes$prediccion<-predict_mes$prediccion-0.001
predict_mes$Media_Precipitacion<-predict_mes$Media_Precipitacion-0.001
Plaza_11_pred <- predict_mes
Plaza_11_pred

data_plaza_[60:71,4:18]

maximos <- Plaza_11_pred[3:14,]
mmaximos <-maximos$prediccion
maximo_11 <- max(mmaximos)
indice = match(maximo_11,mmaximos)

Data_P <- data.frame(maximos)
names(Data_P) = c('Max_Precipitacion_MM','Media_Precipitacion',
                  'mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                  'mes_9',"mes_10",'mes_11',"mes_12")
pred <- add_pi(Data_P,regresion_sim, alpha = 0.1, names = c("lwr", "upr"))
conf <- add_ci(Data_P, regresion_sim, alpha = 0.1, names = c("lwr", "upr"))
intervalo_11 <- c(conf$lwr[indice],conf$upr[indice])

###############################################################################
###############################################################################
### CONTRASTES
annos <- as.numeric(unique(data_plaza_$Ano))

lista_mx_11 <-c(rep(0,length(annos)))

for ( i in 1:length(annos)){
  lista_mx_11[i] <- max(data_plaza_[data_plaza_$Ano==annos[i],4]) 
}
lista_mx_11



################################################################################
########################## PLAZA 12: Cuernavaca #############################
data_plaza_ <- read_csv("C:/Users/KM Reyes Maya/Downloads/Tercer semestre/Computo Estadistico/Proyecto/Datos_by_Plaza/PLAZA_12.csv")

plaza_ <- data_plaza_[,4:18]
plaza_$Max_Precipitacion_MM = data_plaza_$Max_Precipitacion_MM+0.001
plaza_$Media_Precipitacion = data_plaza_$Media_Precipitacion+0.001

##### Exploración de la variable de respuest
##### ¿La variable sigue una distribución gamma?
fit.gamma <- fitdist(plaza_$Max_Precipitacion_MM,
                     distr = "gamma", method = "mme")

plot.ts(plaza_$Max_Precipitacion_MM)
plot(fit.gamma)


plaza_model<-subset(plaza_,select=-c(mes_12))
regresion_todos = glm(Max_Precipitacion_MM ~.,
                      family = Gamma(link='log'),
                      data = plaza_model)
summary(regresion_todos)


regresion_sim = glm(Max_Precipitacion_MM ~ Media_Precipitacion+
                      mes_1+mes_2+ mes_3+mes_4+mes_5+mes_6+mes_7+ mes_8+mes_9+mes_10+mes_11,
                    family = Gamma(link='log'),
                    data = plaza_model)
summary(regresion_sim)

var = resid(regresion_sim,type='pearson')
plot(fitted(regresion_sim),var);abline(h=0)
qqnorm(resid(regresion_sim));qqline(resid(regresion_sim))

####### Agregamos la media #####

predi_reg<- data_plaza_[,5:18] ### contemplamos los meses
predi_reg

#se construye el modelo de regresion con los casos completos
lm_media <- lm (Media_Precipitacion ~ mes_1+mes_2+mes_3+
                  mes_4+mes_5+mes_7+mes_8+mes_9+mes_10+mes_11+mes_12, predi_reg)
summary(lm_media)
#nota: lm construye el modelo de regresion implicitamente con los casos completos
#se eligen los valores predichos para los datos faltantes

new_mes_12 <- data.frame(predi_reg$mes_1,predi_reg$mes_2, predi_reg$mes_3, predi_reg$mes_4,
                         predi_reg$mes_5, predi_reg$mes_6, predi_reg$mes_7,
                         predi_reg$mes_8, predi_reg$mes_9, predi_reg$mes_10,
                         predi_reg$mes_11,predi_reg$mes_12)

new_mes_12 <- new_mes_12[12:25,]
names(new_mes_12) <- c('mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                       'mes_9',"mes_10",'mes_11',"mes_12")
pred.1 <- predict (lm_media, new_mes_12)
pred.1

#### Lo unimos
tend<-c(72:85)
imputa_dat <- data.frame(Media_Precipitacion =pred.1, Tendencia= tend, new_mes_12)
imputa_dat
#reg_data <- rbind(predi_reg,imputa_dat)
predi_reg_t <- subset(imputa_dat,select=-c(Tendencia))

### Retiramos las columnas(meses) que no son significativos
# el mes que vaya 

prediccion <- predict(regresion_sim,predi_reg_t,type="response")
predict_mes<-cbind(prediccion,predi_reg_t)
predict_mes$prediccion<-predict_mes$prediccion-0.001
predict_mes$Media_Precipitacion<-predict_mes$Media_Precipitacion-0.001
Plaza_12_pred <- predict_mes
Plaza_12_pred

data_plaza_[60:71,4:18]


maximos <- Plaza_12_pred[3:14,]
mmaximos <-maximos$prediccion
maximo_12 <- max(mmaximos)
indice = match(maximo_12,mmaximos)

Data_P <- data.frame(maximos)
names(Data_P) = c('Max_Precipitacion_MM','Media_Precipitacion',
                  'mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                  'mes_9',"mes_10",'mes_11',"mes_12")
pred <- add_pi(Data_P,regresion_sim, alpha = 0.1, names = c("lwr", "upr"))
conf <- add_ci(Data_P, regresion_sim, alpha = 0.1, names = c("lwr", "upr"))
intervalo_12 <- c(conf$lwr[indice],conf$upr[indice])

###############################################################################
###############################################################################
### CONTRASTES
annos <- as.numeric(unique(data_plaza_$Ano))

lista_mx_12 <-c(rep(0,length(annos)))

for ( i in 1:length(annos)){
  lista_mx_12[i] <- max(data_plaza_[data_plaza_$Ano==annos[i],4]) 
}
lista_mx_12




################################################################################
########################## PLAZA 13:  Culiacan #############################
data_plaza_ <- read_csv("C:/Users/KM Reyes Maya/Downloads/Tercer semestre/Computo Estadistico/Proyecto/Datos_by_Plaza/PLAZA_13.csv")

plaza_ <- data_plaza_[,4:18]
plaza_$Max_Precipitacion_MM = data_plaza_$Max_Precipitacion_MM+0.001
plaza_$Media_Precipitacion = data_plaza_$Media_Precipitacion+0.001

##### Exploración de la variable de respuest
##### ¿La variable sigue una distribución gamma?
fit.gamma <- fitdist(plaza_$Max_Precipitacion_MM,
                     distr = "gamma", method = "mme")

plot.ts(plaza_$Max_Precipitacion_MM)
plot(fit.gamma)


plaza_model<-subset(plaza_,select=-c(mes_12))
regresion_todos = glm(Max_Precipitacion_MM ~.,
                      family = Gamma(link='log'),
                      data = plaza_model)
summary(regresion_todos)


regresion_sim = glm(Max_Precipitacion_MM ~ Media_Precipitacion+
                      mes_1+mes_2+ mes_3+mes_4+mes_5+mes_6+mes_7+ mes_8+mes_9+mes_10+mes_11,
                    family = Gamma(link='log'),
                    data = plaza_model)
summary(regresion_sim)


####### Agregamos la media #####

predi_reg<- data_plaza_[,5:18] ### contemplamos los meses
predi_reg

#se construye el modelo de regresion con los casos completos
lm_media <- lm (Media_Precipitacion ~ mes_1+mes_2+mes_3+
                  mes_4+mes_5+mes_7+mes_8+mes_9+mes_10+mes_11+mes_12, predi_reg)
summary(lm_media)
#nota: lm construye el modelo de regresion implicitamente con los casos completos
#se eligen los valores predichos para los datos faltantes

new_mes_12 <- data.frame(predi_reg$mes_1,predi_reg$mes_2, predi_reg$mes_3, predi_reg$mes_4,
                         predi_reg$mes_5, predi_reg$mes_6, predi_reg$mes_7,
                         predi_reg$mes_8, predi_reg$mes_9, predi_reg$mes_10,
                         predi_reg$mes_11,predi_reg$mes_12)

new_mes_12 <- new_mes_12[12:25,]
names(new_mes_12) <- c('mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                       'mes_9',"mes_10",'mes_11',"mes_12")
pred.1 <- predict (lm_media, new_mes_12)
pred.1

#### Lo unimos
tend<-c(72:85)
imputa_dat <- data.frame(Media_Precipitacion =pred.1, Tendencia= tend, new_mes_12)
imputa_dat
#reg_data <- rbind(predi_reg,imputa_dat)
predi_reg_t <- subset(imputa_dat,select=-c(Tendencia))

### Retiramos las columnas(meses) que no son significativos
# el mes que vaya 

prediccion <- predict(regresion_sim,predi_reg_t,type="response")
predict_mes<-cbind(prediccion,predi_reg_t)
predict_mes$prediccion<-predict_mes$prediccion-0.001
predict_mes$Media_Precipitacion<-predict_mes$Media_Precipitacion-0.001
Plaza_13_pred <- predict_mes
Plaza_13_pred

data_plaza_[60:71,4:18]

maximos <- Plaza_13_pred[3:14,]
mmaximos <-maximos$prediccion
maximo_13 <- max(mmaximos)
indice = match(maximo_13,mmaximos)

Data_P <- data.frame(maximos)
names(Data_P) = c('Max_Precipitacion_MM','Media_Precipitacion',
                  'mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                  'mes_9',"mes_10",'mes_11',"mes_12")
pred <- add_pi(Data_P,regresion_sim, alpha = 0.1, names = c("lwr", "upr"))
conf <- add_ci(Data_P, regresion_sim, alpha = 0.1, names = c("lwr", "upr"))
intervalo_13 <- c(conf$lwr[indice],conf$upr[indice])

###############################################################################
###############################################################################
### CONTRASTES
annos <- as.numeric(unique(data_plaza_$Ano))

lista_mx_13 <-c(rep(0,length(annos)))

for ( i in 1:length(annos)){
  lista_mx_13[i] <- max(data_plaza_[data_plaza_$Ano==annos[i],4]) 
}
lista_mx_13


################################################################################
########################## PLAZA 14: Durango #############################
data_plaza_ <- read_csv("C:/Users/KM Reyes Maya/Downloads/Tercer semestre/Computo Estadistico/Proyecto/Datos_by_Plaza/PLAZA_14.csv")

plaza_ <- data_plaza_[,4:18]
plaza_$Max_Precipitacion_MM = data_plaza_$Max_Precipitacion_MM+0.001
plaza_$Media_Precipitacion = data_plaza_$Media_Precipitacion+0.001

##### Exploración de la variable de respuest
##### ¿La variable sigue una distribución gamma?
fit.gamma <- fitdist(plaza_$Max_Precipitacion_MM,
                     distr = "gamma", method = "mme")

plot.ts(plaza_$Max_Precipitacion_MM)
plot(fit.gamma)


plaza_model<-subset(plaza_,select=-c(mes_12))
regresion_todos = glm(Max_Precipitacion_MM ~.,
                      family = Gamma(link='log'),
                      data = plaza_model)
summary(regresion_todos)


regresion_sim = glm(Max_Precipitacion_MM ~ Media_Precipitacion+
                      mes_1+mes_2+ mes_3+mes_4+mes_5+mes_6+mes_7+ mes_8+mes_9+mes_10+mes_11,
                    family = Gamma(link='log'),
                    data = plaza_model)
summary(regresion_sim)


####### Agregamos la media #####

predi_reg<- data_plaza_[,5:18] ### contemplamos los meses
predi_reg

#se construye el modelo de regresion con los casos completos
lm_media <- lm (Media_Precipitacion ~ mes_1+mes_2+mes_3+
                  mes_4+mes_5+mes_7+mes_8+mes_9+mes_10+mes_11+mes_12, predi_reg)
summary(lm_media)
#nota: lm construye el modelo de regresion implicitamente con los casos completos
#se eligen los valores predichos para los datos faltantes

new_mes_12 <- data.frame(predi_reg$mes_1,predi_reg$mes_2, predi_reg$mes_3, predi_reg$mes_4,
                         predi_reg$mes_5, predi_reg$mes_6, predi_reg$mes_7,
                         predi_reg$mes_8, predi_reg$mes_9, predi_reg$mes_10,
                         predi_reg$mes_11,predi_reg$mes_12)

new_mes_12 <- new_mes_12[12:25,]
names(new_mes_12) <- c('mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                       'mes_9',"mes_10",'mes_11',"mes_12")
pred.1 <- predict (lm_media, new_mes_12)
pred.1

#### Lo unimos
tend<-c(72:85)

imputa_dat <- data.frame(Media_Precipitacion =pred.1, Tendencia= tend, new_mes_12)
imputa_dat
#reg_data <- rbind(predi_reg,imputa_dat)
predi_reg_t <- subset(imputa_dat,select=-c(Tendencia))

### Retiramos las columnas(meses) que no son significativos
# el mes que vaya 

prediccion <- predict(regresion_sim,predi_reg_t,type="response")
predict_mes<-cbind(prediccion,predi_reg_t)
predict_mes$prediccion<-predict_mes$prediccion-0.001
predict_mes$Media_Precipitacion<-predict_mes$Media_Precipitacion-0.001
Plaza_14_pred <- predict_mes
Plaza_14_pred

data_plaza_[60:71,4:18]

maximos <- Plaza_14_pred[3:14,]
mmaximos <-maximos$prediccion
maximo_14 <- max(mmaximos)
indice = match(maximo_14,mmaximos)

Data_P <- data.frame(maximos)
names(Data_P) = c('Max_Precipitacion_MM','Media_Precipitacion',
                  'mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                  'mes_9',"mes_10",'mes_11',"mes_12")
pred <- add_pi(Data_P,regresion_sim, alpha = 0.1, names = c("lwr", "upr"))
conf <- add_ci(Data_P, regresion_sim, alpha = 0.1, names = c("lwr", "upr"))
intervalo_14 <- c(conf$lwr[indice],conf$upr[indice])

###############################################################################
###############################################################################
### CONTRASTES
annos <- as.numeric(unique(data_plaza_$Ano))

lista_mx_14 <-c(rep(0,length(annos)))

for ( i in 1:length(annos)){
  lista_mx_14[i] <- max(data_plaza_[data_plaza_$Ano==annos[i],4]) 
}
lista_mx_14

################################################################################
########################## PLAZA 15: Guadalajara Altos #############################
data_plaza_ <- read_csv("C:/Users/KM Reyes Maya/Downloads/Tercer semestre/Computo Estadistico/Proyecto/Datos_by_Plaza/PLAZA_15.csv")

plaza_ <- data_plaza_[,4:18]
plaza_$Max_Precipitacion_MM = data_plaza_$Max_Precipitacion_MM+0.001
plaza_$Media_Precipitacion = data_plaza_$Media_Precipitacion+0.001

##### Exploración de la variable de respuest
##### ¿La variable sigue una distribución gamma?
fit.gamma <- fitdist(plaza_$Max_Precipitacion_MM,
                     distr = "gamma", method = "mme")

plot.ts(plaza_$Max_Precipitacion_MM)
plot(fit.gamma)


plaza_model<-subset(plaza_,select=-c(mes_12))
regresion_todos = glm(Max_Precipitacion_MM ~.,
                      family = Gamma(link='log'),
                      data = plaza_model)
summary(regresion_todos)


regresion_sim = glm(Max_Precipitacion_MM ~ Media_Precipitacion+mes_1+mes_2+ mes_3+mes_4+mes_5+mes_6+mes_7+ mes_8+mes_9+mes_10+mes_11,
                    data = plaza_model)
summary(regresion_sim)
###### Agregamos la media #####

predi_reg<- data_plaza_[,5:18] ### contemplamos los meses
predi_reg

#se construye el modelo de regresion con los casos completos
lm_media <- lm (Media_Precipitacion ~ mes_1+mes_2+mes_3+
                  mes_4+mes_5+mes_7+mes_8+mes_9+mes_10+mes_11+mes_12, predi_reg)
summary(lm_media)
#nota: lm construye el modelo de regresion implicitamente con los casos completos
#se eligen los valores predichos para los datos faltantes

new_mes_12 <- data.frame(predi_reg$mes_1,predi_reg$mes_2, predi_reg$mes_3, predi_reg$mes_4,
                         predi_reg$mes_5, predi_reg$mes_6, predi_reg$mes_7,
                         predi_reg$mes_8, predi_reg$mes_9, predi_reg$mes_10,
                         predi_reg$mes_11,predi_reg$mes_12)

new_mes_12 <- new_mes_12[12:25,]
names(new_mes_12) <- c('mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                       'mes_9',"mes_10",'mes_11',"mes_12")
pred.1 <- predict (lm_media, new_mes_12)
pred.1

#### Lo unimos
tend<-c(72:85)
imputa_dat <- data.frame(Media_Precipitacion =pred.1, Tendencia= tend, new_mes_12)
imputa_dat
#reg_data <- rbind(predi_reg,imputa_dat)
predi_reg_t <- subset(imputa_dat,select=-c(Tendencia))

### Retiramos las columnas(meses) que no son significativos
# el mes que vaya 

prediccion <- predict(regresion_sim,predi_reg_t,type="response")
predict_mes<-cbind(prediccion,predi_reg_t)
predict_mes$prediccion<-predict_mes$prediccion-0.001
predict_mes$Media_Precipitacion<-predict_mes$Media_Precipitacion-0.001
Plaza_15_pred <- predict_mes
Plaza_15_pred

data_plaza_[60:71,4:18]


maximos <- Plaza_15_pred[3:14,]
mmaximos <-maximos$prediccion
maximo_15 <- max(mmaximos)
indice = match(maximo_15,mmaximos)

Data_P <- data.frame(maximos)
names(Data_P) = c('Max_Precipitacion_MM','Media_Precipitacion',
                  'mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                  'mes_9',"mes_10",'mes_11',"mes_12")
pred <- add_pi(Data_P,regresion_sim, alpha = 0.1, names = c("lwr", "upr"))
conf <- add_ci(Data_P, regresion_sim, alpha = 0.1, names = c("lwr", "upr"))
intervalo_15 <- c(conf$lwr[indice],conf$upr[indice])

###############################################################################
###############################################################################
### CONTRASTES
annos <- as.numeric(unique(data_plaza_$Ano))

lista_mx_15 <-c(rep(0,length(annos)))

for ( i in 1:length(annos)){
  lista_mx_15[i] <- max(data_plaza_[data_plaza_$Ano==annos[i],4]) 
}
lista_mx_15


################################################################################
########################## PLAZA 16:  Chapala Guadalajara #############################
data_plaza_ <- read_csv("C:/Users/KM Reyes Maya/Downloads/Tercer semestre/Computo Estadistico/Proyecto/Datos_by_Plaza/PLAZA_16.csv")

plaza_ <- data_plaza_[,4:18]
plaza_$Max_Precipitacion_MM = data_plaza_$Max_Precipitacion_MM+0.001
plaza_$Media_Precipitacion = data_plaza_$Media_Precipitacion+0.001

##### Exploración de la variable de respuest
##### ¿La variable sigue una distribución gamma?
fit.gamma <- fitdist(plaza_$Max_Precipitacion_MM,
                     distr = "gamma", method = "mme")

plot.ts(plaza_$Max_Precipitacion_MM)
plot(fit.gamma)


plaza_model<-subset(plaza_,select=-c(mes_12))
regresion_todos = glm(Max_Precipitacion_MM ~.,
                      family = Gamma(link='log'),
                      data = plaza_model)
summary(regresion_todos)


regresion_sim = glm(Max_Precipitacion_MM ~ Media_Precipitacion+mes_1+mes_2+ mes_3+mes_4+mes_5+mes_6+mes_7+ mes_8+mes_9+mes_10+mes_11,
                    family = Gamma(link='log'),
                    data = plaza_model)
summary(regresion_sim)

###### Agregamos la media #####

predi_reg<- data_plaza_[,5:18] ### contemplamos los meses
predi_reg

#se construye el modelo de regresion con los casos completos
lm_media <- lm (Media_Precipitacion ~ mes_1+mes_2+mes_3+
                  mes_4+mes_5+mes_7+mes_8+mes_9+mes_10+mes_11+mes_12, predi_reg)
summary(lm_media)
#nota: lm construye el modelo de regresion implicitamente con los casos completos
#se eligen los valores predichos para los datos faltantes

new_mes_12 <- data.frame(predi_reg$mes_1,predi_reg$mes_2, predi_reg$mes_3, predi_reg$mes_4,
                         predi_reg$mes_5, predi_reg$mes_6, predi_reg$mes_7,
                         predi_reg$mes_8, predi_reg$mes_9, predi_reg$mes_10,
                         predi_reg$mes_11,predi_reg$mes_12)

new_mes_12 <- new_mes_12[12:25,]
names(new_mes_12) <- c('mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                       'mes_9',"mes_10",'mes_11',"mes_12")
pred.1 <- predict (lm_media, new_mes_12)
pred.1

#### Lo unimos
tend<-c(72:85)
imputa_dat <- data.frame(Media_Precipitacion =pred.1, Tendencia= tend, new_mes_12)
imputa_dat
#reg_data <- rbind(predi_reg,imputa_dat)
predi_reg_t <- subset(imputa_dat,select=-c(Tendencia))

### Retiramos las columnas(meses) que no son significativos
# el mes que vaya 

prediccion <- predict(regresion_sim,predi_reg_t,type="response")
predict_mes<-cbind(prediccion,predi_reg_t)
predict_mes$prediccion<-predict_mes$prediccion-0.001
predict_mes$Media_Precipitacion<-predict_mes$Media_Precipitacion-0.001
Plaza_16_pred <- predict_mes
Plaza_16_pred

data_plaza_[60:71,4:18]


maximos <- Plaza_16_pred[3:14,]
mmaximos <-maximos$prediccion
maximo_16 <- max(mmaximos)
indice = match(maximo_16,mmaximos)

Data_P <- data.frame(maximos)
names(Data_P) = c('Max_Precipitacion_MM','Media_Precipitacion',
                  'mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                  'mes_9',"mes_10",'mes_11',"mes_12")
pred <- add_pi(Data_P,regresion_sim, alpha = 0.1, names = c("lwr", "upr"))
conf <- add_ci(Data_P, regresion_sim, alpha = 0.1, names = c("lwr", "upr"))
intervalo_16 <- c(conf$lwr[indice],conf$upr[indice])

###############################################################################
###############################################################################
### CONTRASTES
annos <- as.numeric(unique(data_plaza_$Ano))

lista_mx_16<-c(rep(0,length(annos)))

for ( i in 1:length(annos)){
  lista_mx_16[i] <- max(data_plaza_[data_plaza_$Ano==annos[i],4]) 
}
lista_mx_16

################################################################################
########################## PLAZA 17:  Tequila Guadalajara #############################
data_plaza_ <- read_csv("C:/Users/KM Reyes Maya/Downloads/Tercer semestre/Computo Estadistico/Proyecto/Datos_by_Plaza/PLAZA_17.csv")

plaza_ <- data_plaza_[,4:18]
plaza_$Max_Precipitacion_MM = data_plaza_$Max_Precipitacion_MM+0.001
plaza_$Media_Precipitacion = data_plaza_$Media_Precipitacion+0.001

##### Exploración de la variable de respuest
##### ¿La variable sigue una distribución gamma?
fit.gamma <- fitdist(plaza_$Max_Precipitacion_MM,
                     distr = "gamma", method = "mme")

plot.ts(plaza_$Max_Precipitacion_MM)
plot(fit.gamma)


plaza_model<-subset(plaza_,select=-c(mes_12))
regresion_todos = glm(Max_Precipitacion_MM ~.,
                      family = Gamma(link='log'),
                      data = plaza_model)
summary(regresion_todos)


regresion_sim = glm(Max_Precipitacion_MM ~ Media_Precipitacion+mes_1+mes_2+mes_3+mes_4+mes_5+mes_6+mes_7+mes_8+mes_9+mes_10+mes_11,
                    family = Gamma(link='log'),
                    data = plaza_model)
summary(regresion_sim)

###### Agregamos la media #####

predi_reg<- data_plaza_[,5:18] ### contemplamos los meses
predi_reg

#se construye el modelo de regresion con los casos completos
lm_media <- lm (Media_Precipitacion ~ mes_1+mes_2+mes_3+
                  mes_4+mes_5+mes_7+mes_8+mes_9+mes_10+mes_11+mes_12, predi_reg)
summary(lm_media)
#nota: lm construye el modelo de regresion implicitamente con los casos completos
#se eligen los valores predichos para los datos faltantes

new_mes_12 <- data.frame(predi_reg$mes_1,predi_reg$mes_2, predi_reg$mes_3, predi_reg$mes_4,
                         predi_reg$mes_5, predi_reg$mes_6, predi_reg$mes_7,
                         predi_reg$mes_8, predi_reg$mes_9, predi_reg$mes_10,
                         predi_reg$mes_11,predi_reg$mes_12)

new_mes_12 <- new_mes_12[12:25,]
names(new_mes_12) <- c('mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                       'mes_9',"mes_10",'mes_11',"mes_12")
pred.1 <- predict (lm_media, new_mes_12)
pred.1

#### Lo unimos
tend<-c(72:85)
imputa_dat <- data.frame(Media_Precipitacion =pred.1, Tendencia= tend, new_mes_12)
imputa_dat
#reg_data <- rbind(predi_reg,imputa_dat)
predi_reg_t <- subset(imputa_dat,select=-c(Tendencia))

### Retiramos las columnas(meses) que no son significativos
# el mes que vaya 

prediccion <- predict(regresion_sim,predi_reg_t,type="response")
predict_mes<-cbind(prediccion,predi_reg_t)
predict_mes$prediccion<-predict_mes$prediccion-0.001
predict_mes$Media_Precipitacion<-predict_mes$Media_Precipitacion-0.001
Plaza_17_pred <- predict_mes
Plaza_17_pred

data_plaza_[60:71,4:18]


maximos <- Plaza_17_pred[3:14,]
mmaximos <-maximos$prediccion
maximo_17 <- max(mmaximos)
indice = match(maximo_17,mmaximos)

Data_P <- data.frame(maximos)
names(Data_P) = c('Max_Precipitacion_MM','Media_Precipitacion',
                  'mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                  'mes_9',"mes_10",'mes_11',"mes_12")
pred <- add_pi(Data_P,regresion_sim, alpha = 0.1, names = c("lwr", "upr"))
conf <- add_ci(Data_P, regresion_sim, alpha = 0.1, names = c("lwr", "upr"))
intervalo_17 <- c(conf$lwr[indice],conf$upr[indice])
intervalo_17
maximo_17
add_ci(Data_P, regresion_sim, alpha = 0.1, names = c("lwr", "upr"))
###############################################################################
###############################################################################
### CONTRASTES
annos <- as.numeric(unique(data_plaza_$Ano))

lista_mx_17 <-c(rep(0,length(annos)))

for ( i in 1:length(annos)){
  lista_mx_17[i] <- max(data_plaza_[data_plaza_$Ano==annos[i],4]) 
}
lista_mx_17

################################################################################
########################## PLAZA 18: Hermosillo #############################
data_plaza_ <- read_csv("C:/Users/KM Reyes Maya/Downloads/Tercer semestre/Computo Estadistico/Proyecto/Datos_by_Plaza/PLAZA_18.csv")

plaza_ <- data_plaza_[,4:18]
plaza_$Max_Precipitacion_MM = data_plaza_$Max_Precipitacion_MM+0.001
plaza_$Media_Precipitacion = data_plaza_$Media_Precipitacion+0.001

##### Exploración de la variable de respuest
##### ¿La variable sigue una distribución gamma?
fit.gamma <- fitdist(plaza_$Max_Precipitacion_MM,
                     distr = "gamma", method = "mme")

plot.ts(plaza_$Max_Precipitacion_MM)
plot(fit.gamma)


plaza_model<-subset(plaza_,select=-c(mes_12))
regresion_todos = glm(Max_Precipitacion_MM ~.,
                      family = Gamma(link='log'),
                      data = plaza_model)
summary(regresion_todos)


regresion_sim = glm(Max_Precipitacion_MM ~ Media_Precipitacion+
                      mes_1+mes_2+mes_3+mes_4+mes_5+mes_6+mes_7+mes_8+mes_9+mes_10+mes_11,
                    family = Gamma(link='log'),
                    data = plaza_model)
summary(regresion_sim)

predi_reg<- data_plaza_[12:25,7:18] ### contemplamos los meses
predi_reg
###### Agregamos la media #####

predi_reg<- data_plaza_[,5:18] ### contemplamos los meses
predi_reg

#se construye el modelo de regresion con los casos completos
lm_media <- lm (Media_Precipitacion ~ mes_1+mes_2+mes_3+
                  mes_4+mes_5+mes_7+mes_8+mes_9+mes_10+mes_11+mes_12, predi_reg)
summary(lm_media)
#nota: lm construye el modelo de regresion implicitamente con los casos completos
#se eligen los valores predichos para los datos faltantes

new_mes_12 <- data.frame(predi_reg$mes_1,predi_reg$mes_2, predi_reg$mes_3, predi_reg$mes_4,
                         predi_reg$mes_5, predi_reg$mes_6, predi_reg$mes_7,
                         predi_reg$mes_8, predi_reg$mes_9, predi_reg$mes_10,
                         predi_reg$mes_11,predi_reg$mes_12)

new_mes_12 <- new_mes_12[12:25,]
names(new_mes_12) <- c('mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                       'mes_9',"mes_10",'mes_11',"mes_12")
pred.1 <- predict (lm_media, new_mes_12)
pred.1

#### Lo unimos
tend<-c(72:85)
imputa_dat <- data.frame(Media_Precipitacion =pred.1, Tendencia= tend, new_mes_12)
imputa_dat
#reg_data <- rbind(predi_reg,imputa_dat)
predi_reg_t <- subset(imputa_dat,select=-c(Tendencia))

### Retiramos las columnas(meses) que no son significativos
# el mes que vaya 

prediccion <- predict(regresion_sim,predi_reg_t,type="response")
predict_mes<-cbind(prediccion,predi_reg_t)
predict_mes$prediccion<-predict_mes$prediccion-0.001
predict_mes$Media_Precipitacion<-predict_mes$Media_Precipitacion-0.001
Plaza_18_pred <- predict_mes
Plaza_18_pred

data_plaza_[60:71,4:18]


maximos <- Plaza_18_pred[3:14,]
mmaximos <-maximos$prediccion
maximo_18 <- max(mmaximos)
indice = match(maximo_18,mmaximos)

Data_P <- data.frame(maximos)
names(Data_P) = c('Max_Precipitacion_MM','Media_Precipitacion',
                  'mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                  'mes_9',"mes_10",'mes_11',"mes_12")
pred <- add_pi(Data_P,regresion_sim, alpha = 0.1, names = c("lwr", "upr"))
conf <- add_ci(Data_P, regresion_sim, alpha = 0.1, names = c("lwr", "upr"))
intervalo_18 <- c(conf$lwr[indice],conf$upr[indice])

###############################################################################
###############################################################################
### CONTRASTES
annos <- as.numeric(unique(data_plaza_$Ano))

lista_mx_18 <-c(rep(0,length(annos)))

for ( i in 1:length(annos)){
  lista_mx_18[i] <- max(data_plaza_[data_plaza_$Ano==annos[i],4]) 
}
lista_mx_18


################################################################################
########################## PLAZA 19:  La paz  #############################

data_plaza_ <- read_csv("C:/Users/KM Reyes Maya/Downloads/Tercer semestre/Computo Estadistico/Proyecto/Datos_by_Plaza/PLAZA_19.csv")

plaza_ <- data_plaza_[,4:18]
plaza_$Max_Precipitacion_MM = data_plaza_$Max_Precipitacion_MM+0.001
plaza_$Media_Precipitacion = data_plaza_$Media_Precipitacion+0.001

##### Exploración de la variable de respuest
##### ¿La variable sigue una distribución gamma?
fit.gamma <- fitdist(plaza_$Max_Precipitacion_MM,
                     distr = "gamma", method = "mme")

plot.ts(plaza_$Max_Precipitacion_MM)
plot(fit.gamma)


plaza_model<-subset(plaza_,select=-c(mes_12))
regresion_todos = glm(Max_Precipitacion_MM ~.,
                      family = Gamma(link='identity'),
                      data = plaza_model)
summary(regresion_todos)


regresion_sim = glm(Max_Precipitacion_MM ~ Media_Precipitacion+
                      mes_1+mes_2+mes_3+mes_4+mes_5+mes_6+mes_7+mes_8+mes_9+mes_10+mes_11,
                    family = Gamma(link='identity'),
                    data = plaza_model)
summary(regresion_sim)

###### Agregamos la media #####

predi_reg<- data_plaza_[,5:18] ### contemplamos los meses
predi_reg

#se construye el modelo de regresion con los casos completos
lm_media <- lm (Media_Precipitacion ~ mes_1+mes_2+mes_3+
                  mes_4+mes_5+mes_7+mes_8+mes_9+mes_10+mes_11+mes_12, predi_reg)
summary(lm_media)
#nota: lm construye el modelo de regresion implicitamente con los casos completos
#se eligen los valores predichos para los datos faltantes

new_mes_12 <- data.frame(predi_reg$mes_1,predi_reg$mes_2, predi_reg$mes_3, predi_reg$mes_4,
                         predi_reg$mes_5, predi_reg$mes_6, predi_reg$mes_7,
                         predi_reg$mes_8, predi_reg$mes_9, predi_reg$mes_10,
                         predi_reg$mes_11,predi_reg$mes_12)

new_mes_12 <- new_mes_12[12:25,]
names(new_mes_12) <- c('mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                       'mes_9',"mes_10",'mes_11',"mes_12")

pred.1 <- predict (lm_media, new_mes_12)
pred.1

#### Lo unimos
tend<-c(72:85)
imputa_dat <- data.frame(Media_Precipitacion =pred.1, Tendencia= tend, new_mes_12)
imputa_dat
#reg_data <- rbind(predi_reg,imputa_dat)
predi_reg_t <- subset(imputa_dat,select=-c(Tendencia))

### Retiramos las columnas(meses) que no son significativos
# el mes que vaya 


prediccion <- predict(regresion_sim,predi_reg_t,type="response")
predict_mes<-cbind(prediccion,predi_reg_t)
predict_mes$prediccion<-predict_mes$prediccion-0.001
predict_mes$Media_Precipitacion<-predict_mes$Media_Precipitacion-0.001
Plaza_19_pred <- predict_mes
Plaza_19_pred
data_plaza_[60:71,4:18]


maximos <- Plaza_19_pred[3:14,]
mmaximos <-maximos$prediccion
maximo_19 <- max(mmaximos)
indice = match(maximo_19,mmaximos)

Data_P <- data.frame(maximos)
names(Data_P) = c('Max_Precipitacion_MM','Media_Precipitacion',
                  'mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                  'mes_9',"mes_10",'mes_11',"mes_12")
pred <- add_pi(Data_P,regresion_sim, alpha = 0.1, names = c("lwr", "upr"))
conf <- add_ci(Data_P, regresion_sim, alpha = 0.1, names = c("lwr", "upr"))
intervalo_19 <- c(conf$lwr[indice],conf$upr[indice])
intervalo_19

###############################################################################
###############################################################################
### CONTRASTES
annos <- as.numeric(unique(data_plaza_$Ano))

lista_mx_19 <-c(rep(0,length(annos)))

for ( i in 1:length(annos)){
  lista_mx_19[i] <- max(data_plaza_[data_plaza_$Ano==annos[i],4]) 
}
lista_mx_19

###############################################################################

################################################################################
########################## PLAZA 20:  Laguna #############################
data_plaza_ <- read_csv("C:/Users/KM Reyes Maya/Downloads/Tercer semestre/Computo Estadistico/Proyecto/Datos_by_Plaza/PLAZA_20.csv")

plaza_ <- data_plaza_[,4:18]
plaza_$Max_Precipitacion_MM = data_plaza_$Max_Precipitacion_MM+0.001
plaza_$Media_Precipitacion = data_plaza_$Media_Precipitacion+0.001

##### Exploración de la variable de respuest
##### ¿La variable sigue una distribución gamma?
fit.gamma <- fitdist(plaza_$Max_Precipitacion_MM,
                     distr = "gamma", method = "mme")

plot.ts(plaza_$Max_Precipitacion_MM)
plot(fit.gamma)


plaza_model<-subset(plaza_,select=-c(mes_12))
regresion_todos = glm(Max_Precipitacion_MM ~.,
                      family = Gamma(link='log'),
                      data = plaza_model)
summary(regresion_todos)
regresion_sim = glm(Max_Precipitacion_MM ~ Media_Precipitacion+
                      mes_1+mes_2+mes_3+mes_4+mes_5+mes_6+mes_7+mes_8+mes_9+mes_10+mes_11,
                    family = Gamma(link='log'),
                    data = plaza_model)
summary(regresion_sim)

###### Agregamos la media #####

predi_reg<- data_plaza_[,5:18] ### contemplamos los meses
predi_reg

#se construye el modelo de regresion con los casos completos
lm_media <- lm (Media_Precipitacion ~ mes_1+mes_2+mes_3+
                  mes_4+mes_5+mes_7+mes_8+mes_9+mes_10+mes_11+mes_12, predi_reg)
summary(lm_media)
#nota: lm construye el modelo de regresion implicitamente con los casos completos
#se eligen los valores predichos para los datos faltantes

new_mes_12 <- data.frame(predi_reg$mes_1,predi_reg$mes_2, predi_reg$mes_3, predi_reg$mes_4,
                         predi_reg$mes_5, predi_reg$mes_6, predi_reg$mes_7,
                         predi_reg$mes_8, predi_reg$mes_9, predi_reg$mes_10,
                         predi_reg$mes_11,predi_reg$mes_12)

new_mes_12 <- new_mes_12[12:25,]
names(new_mes_12) <- c('mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                       'mes_9',"mes_10",'mes_11',"mes_12")

pred.1 <- predict (lm_media, new_mes_12)
pred.1

#### Lo unimos
tend<-c(72:85)
imputa_dat <- data.frame(Media_Precipitacion =pred.1, Tendencia= tend, new_mes_12)
imputa_dat
#reg_data <- rbind(predi_reg,imputa_dat)
predi_reg_t <- subset(imputa_dat,select=-c(Tendencia))

### Retiramos las columnas(meses) que no son significativos
# el mes que vaya 

prediccion <- predict(regresion_sim,predi_reg_t,type="response")
predict_mes<-cbind(prediccion,predi_reg_t)
predict_mes$prediccion<-predict_mes$prediccion-0.001
predict_mes$Media_Precipitacion<-predict_mes$Media_Precipitacion-0.001
Plaza_20_pred <- predict_mes
Plaza_20_pred
data_plaza_[60:71,4:18]

maximos <- Plaza_20_pred[3:14,]
mmaximos <-maximos$prediccion
maximo_20 <- max(mmaximos)
indice = match(maximo_20,mmaximos)

Data_P <- data.frame(maximos)
names(Data_P) = c('Max_Precipitacion_MM','Media_Precipitacion',
                  'mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                  'mes_9',"mes_10",'mes_11',"mes_12")
pred <- add_pi(Data_P,regresion_sim, alpha = 0.1, names = c("lwr", "upr"))
conf <- add_ci(Data_P, regresion_sim, alpha = 0.1, names = c("lwr", "upr"))
intervalo_20 <- c(conf$lwr[indice],conf$upr[indice])

data_plaza_[60:71,4:18]


###############################################################################
###############################################################################
### CONTRASTES
annos <- as.numeric(unique(data_plaza_$Ano))

lista_mx_20 <-c(rep(0,length(annos)))

for ( i in 1:length(annos)){
  lista_mx_20[i] <- max(data_plaza_[data_plaza_$Ano==annos[i],4]) 
}
lista_mx_20

################################################################################
########################## PLAZA 21:  Laredo #############################
data_plaza_ <- read_csv("C:/Users/KM Reyes Maya/Downloads/Tercer semestre/Computo Estadistico/Proyecto/Datos_by_Plaza/PLAZA_21.csv")

plaza_ <- data_plaza_[,4:18]
plaza_$Max_Precipitacion_MM = data_plaza_$Max_Precipitacion_MM+0.001
plaza_$Media_Precipitacion = data_plaza_$Media_Precipitacion+0.001

##### Exploración de la variable de respuest
##### ¿La variable sigue una distribución gamma?
fit.gamma <- fitdist(plaza_$Max_Precipitacion_MM,
                     distr = "gamma", method = "mme")

plot.ts(plaza_$Max_Precipitacion_MM)
plot(fit.gamma)


plaza_model<-subset(plaza_,select=-c(mes_12))
regresion_todos = glm(Max_Precipitacion_MM ~.,
                      family = Gamma(link='log'),
                      data = plaza_model)
summary(regresion_todos)
regresion_sim = glm(Max_Precipitacion_MM ~ Media_Precipitacion+
                      mes_1+mes_2+mes_3+mes_4+mes_5+mes_6+mes_7+mes_8+mes_9+mes_10+mes_11,
                    family = Gamma(link='log'),
                    data = plaza_model)
summary(regresion_sim)

###### Agregamos la media #####

predi_reg<- data_plaza_[,5:18] ### contemplamos los meses
predi_reg

#se construye el modelo de regresion con los casos completos
lm_media <- lm (Media_Precipitacion ~ mes_1+mes_2+mes_3+
                  mes_4+mes_5+mes_7+mes_8+mes_9+mes_10+mes_11+mes_12, predi_reg)
summary(lm_media)
#nota: lm construye el modelo de regresion implicitamente con los casos completos
#se eligen los valores predichos para los datos faltantes

new_mes_12 <- data.frame(predi_reg$mes_1,predi_reg$mes_2, predi_reg$mes_3, predi_reg$mes_4,
                         predi_reg$mes_5, predi_reg$mes_6, predi_reg$mes_7,
                         predi_reg$mes_8, predi_reg$mes_9, predi_reg$mes_10,
                         predi_reg$mes_11,predi_reg$mes_12)

new_mes_12 <- new_mes_12[12:25,]
names(new_mes_12) <- c('mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                       'mes_9',"mes_10",'mes_11',"mes_12")

pred.1 <- predict (lm_media, new_mes_12)
pred.1

#### Lo unimos
tend<-c(72:85)
imputa_dat <- data.frame(Media_Precipitacion =pred.1, Tendencia= tend, new_mes_12)
imputa_dat
#reg_data <- rbind(predi_reg,imputa_dat)
predi_reg_t <- subset(imputa_dat,select=-c(Tendencia))

### Retiramos las columnas(meses) que no son significativos
# el mes que vaya 

prediccion <- predict(regresion_sim,predi_reg_t,type="response")
predict_mes<-cbind(prediccion,predi_reg_t)
predict_mes$prediccion<-predict_mes$prediccion-0.001
predict_mes$Media_Precipitacion<-predict_mes$Media_Precipitacion-0.001
Plaza_21_pred <- predict_mes
Plaza_21_pred

data_plaza_[60:71,4:18]

maximos <- Plaza_21_pred[3:14,]
mmaximos <-maximos$prediccion
maximo_21 <- max(mmaximos)
indice = match(maximo_21,mmaximos)

Data_P <- data.frame(maximos)
names(Data_P) = c('Max_Precipitacion_MM','Media_Precipitacion',
                  'mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                  'mes_9',"mes_10",'mes_11',"mes_12")
pred <- add_pi(Data_P,regresion_sim, alpha = 0.1, names = c("lwr", "upr"))
conf <- add_ci(Data_P, regresion_sim, alpha = 0.1, names = c("lwr", "upr"))
intervalo_21 <- c(conf$lwr[indice],conf$upr[indice])

data_plaza_[60:71,4:18]


###############################################################################
###############################################################################
### CONTRASTES
annos <- as.numeric(unique(data_plaza_$Ano))

lista_mx_21 <-c(rep(0,length(annos)))

for ( i in 1:length(annos)){
  lista_mx_21[i] <- max(data_plaza_[data_plaza_$Ano==annos[i],4]) 
}
lista_mx_21

################################################################################
########################## PLAZA 22:  León #############################
data_plaza_ <- read_csv("C:/Users/KM Reyes Maya/Downloads/Tercer semestre/Computo Estadistico/Proyecto/Datos_by_Plaza/PLAZA_22.csv")

plaza_ <- data_plaza_[,4:18]
plaza_$Max_Precipitacion_MM = data_plaza_$Max_Precipitacion_MM+0.001
plaza_$Media_Precipitacion = data_plaza_$Media_Precipitacion+0.001

##### Exploración de la variable de respuest
##### ¿La variable sigue una distribución gamma?
fit.gamma <- fitdist(plaza_$Max_Precipitacion_MM,
                     distr = "gamma", method = "mme")

plot.ts(plaza_$Max_Precipitacion_MM)
plot(fit.gamma)


plaza_model<-subset(plaza_,select=-c(mes_12))
regresion_todos = glm(Max_Precipitacion_MM ~.,
                      family = Gamma(link='log'),
                      data = plaza_model)
summary(regresion_todos)
regresion_sim = glm(Max_Precipitacion_MM ~ Media_Precipitacion+
                      mes_1+mes_2+mes_3+mes_4+mes_5+mes_6+mes_7+mes_8+mes_9+mes_10+mes_11,
                    family = Gamma(link='log'),
                    data = plaza_model)
summary(regresion_sim)

###### Agregamos la media #####

predi_reg<- data_plaza_[,5:18] ### contemplamos los meses
predi_reg

#se construye el modelo de regresion con los casos completos
lm_media <- lm (Media_Precipitacion ~ mes_1+mes_2+mes_3+
                  mes_4+mes_5+mes_7+mes_8+mes_9+mes_10+mes_11+mes_12, predi_reg)
summary(lm_media)
#nota: lm construye el modelo de regresion implicitamente con los casos completos
#se eligen los valores predichos para los datos faltantes

new_mes_12 <- data.frame(predi_reg$mes_1,predi_reg$mes_2, predi_reg$mes_3, predi_reg$mes_4,
                         predi_reg$mes_5, predi_reg$mes_6, predi_reg$mes_7,
                         predi_reg$mes_8, predi_reg$mes_9, predi_reg$mes_10,
                         predi_reg$mes_11,predi_reg$mes_12)

new_mes_12 <- new_mes_12[12:25,]
names(new_mes_12) <- c('mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                       'mes_9',"mes_10",'mes_11',"mes_12")

pred.1 <- predict (lm_media, new_mes_12)
pred.1

#### Lo unimos
tend<-c(72:85)
imputa_dat <- data.frame(Media_Precipitacion =pred.1, Tendencia= tend, new_mes_12)
imputa_dat
#reg_data <- rbind(predi_reg,imputa_dat)
predi_reg_t <- subset(imputa_dat,select=-c(Tendencia))

### Retiramos las columnas(meses) que no son significativos
# el mes que vaya 

prediccion <- predict(regresion_sim,predi_reg_t,type="response")
predict_mes<-cbind(prediccion,predi_reg_t)
predict_mes$prediccion<-predict_mes$prediccion-0.001
predict_mes$Media_Precipitacion<-predict_mes$Media_Precipitacion-0.001
Plaza_22_pred <- predict_mes
Plaza_22_pred

data_plaza_[60:71,4:18]

maximos <- Plaza_22_pred[3:14,]
mmaximos <-maximos$prediccion
maximo_22 <- max(mmaximos)
indice = match(maximo_22,mmaximos)

Data_P <- data.frame(maximos)
names(Data_P) = c('Max_Precipitacion_MM','Media_Precipitacion',
                  'mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                  'mes_9',"mes_10",'mes_11',"mes_12")
pred <- add_pi(Data_P,regresion_sim, alpha = 0.1, names = c("lwr", "upr"))
conf <- add_ci(Data_P, regresion_sim, alpha = 0.1, names = c("lwr", "upr"))
intervalo_22 <- c(conf$lwr[indice],conf$upr[indice])


###############################################################################
###############################################################################
### CONTRASTES
annos <- as.numeric(unique(data_plaza_$Ano))

lista_mx_22 <-c(rep(0,length(annos)))

for ( i in 1:length(annos)){
  lista_mx_22[i] <- max(data_plaza_[data_plaza_$Ano==annos[i],4]) 
}
lista_mx_22


################################################################################
########################## PLAZA 23:  Los Mochis #############################
data_plaza_ <- read_csv("C:/Users/KM Reyes Maya/Downloads/Tercer semestre/Computo Estadistico/Proyecto/Datos_by_Plaza/PLAZA_23.csv")

plaza_ <- data_plaza_[,4:18]
plaza_$Max_Precipitacion_MM = data_plaza_$Max_Precipitacion_MM+0.001
plaza_$Media_Precipitacion = data_plaza_$Media_Precipitacion+0.001

##### Exploración de la variable de respuest
##### ¿La variable sigue una distribución gamma?
fit.gamma <- fitdist(plaza_$Max_Precipitacion_MM,
                     distr = "gamma", method = "mme")

plot.ts(plaza_$Max_Precipitacion_MM)
plot(fit.gamma)


plaza_model<-subset(plaza_,select=-c(mes_12))
regresion_todos = glm(Max_Precipitacion_MM ~.,
                      family = Gamma(link='log'),
                      data = plaza_model)
summary(regresion_todos)
regresion_sim = glm(Max_Precipitacion_MM ~ Media_Precipitacion+
                      mes_1+mes_2+mes_3+mes_4+mes_5+mes_6+mes_7+mes_8+mes_9+mes_10+mes_11,
                    family = Gamma(link='log'),
                    data = plaza_model)
summary(regresion_sim)

###### Agregamos la media #####

predi_reg<- data_plaza_[,5:18] ### contemplamos los meses
predi_reg

#se construye el modelo de regresion con los casos completos
lm_media <- lm (Media_Precipitacion ~ mes_1+mes_2+mes_3+
                  mes_4+mes_5+mes_7+mes_8+mes_9+mes_10+mes_11+mes_12, predi_reg)
summary(lm_media)
#nota: lm construye el modelo de regresion implicitamente con los casos completos
#se eligen los valores predichos para los datos faltantes

new_mes_12 <- data.frame(predi_reg$mes_1,predi_reg$mes_2, predi_reg$mes_3, predi_reg$mes_4,
                         predi_reg$mes_5, predi_reg$mes_6, predi_reg$mes_7,
                         predi_reg$mes_8, predi_reg$mes_9, predi_reg$mes_10,
                         predi_reg$mes_11,predi_reg$mes_12)

new_mes_12 <- new_mes_12[12:25,]
names(new_mes_12) <- c('mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                       'mes_9',"mes_10",'mes_11',"mes_12")

pred.1 <- predict (lm_media, new_mes_12)
pred.1

#### Lo unimos
tend<-c(72:85)
imputa_dat <- data.frame(Media_Precipitacion =pred.1, Tendencia= tend, new_mes_12)
imputa_dat
#reg_data <- rbind(predi_reg,imputa_dat)
predi_reg_t <- subset(imputa_dat,select=-c(Tendencia))

### Retiramos las columnas(meses) que no son significativos
# el mes que vaya 

prediccion <- predict(regresion_sim,predi_reg_t,type="response")
predict_mes<-cbind(prediccion,predi_reg_t)
predict_mes$prediccion<-predict_mes$prediccion-0.001
predict_mes$Media_Precipitacion<-predict_mes$Media_Precipitacion-0.001
Plaza_23_pred <- predict_mes
Plaza_23_pred

data_plaza_[60:71,4:18]

maximos <- Plaza_23_pred[3:14,]
mmaximos <-maximos$prediccion
maximo_23 <- max(mmaximos)
indice = match(maximo_23,mmaximos)

Data_P <- data.frame(maximos)
names(Data_P) = c('Max_Precipitacion_MM','Media_Precipitacion',
                  'mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                  'mes_9',"mes_10",'mes_11',"mes_12")
pred <- add_pi(Data_P,regresion_sim, alpha = 0.1, names = c("lwr", "upr"))
conf <- add_ci(Data_P, regresion_sim, alpha = 0.1, names = c("lwr", "upr"))
intervalo_23 <- c(conf$lwr[indice],conf$upr[indice])

data_plaza_[60:71,4:18]


###############################################################################
###############################################################################
### CONTRASTES
annos <- as.numeric(unique(data_plaza_$Ano))

lista_mx_23 <-c(rep(0,length(annos)))

for ( i in 1:length(annos)){
  lista_mx_23[i] <- max(data_plaza_[data_plaza_$Ano==annos[i],4]) 
}
lista_mx_23



################################################################################
########################## PLAZA 24:  Matamoros #############################
data_plaza_ <- read_csv("C:/Users/KM Reyes Maya/Downloads/Tercer semestre/Computo Estadistico/Proyecto/Datos_by_Plaza/PLAZA_24.csv")

plaza_ <- data_plaza_[,4:18]
plaza_$Max_Precipitacion_MM = data_plaza_$Max_Precipitacion_MM+0.001
plaza_$Media_Precipitacion = data_plaza_$Media_Precipitacion+0.001

##### Exploración de la variable de respuest
##### ¿La variable sigue una distribución gamma?
fit.gamma <- fitdist(plaza_$Max_Precipitacion_MM,
                     distr = "gamma", method = "mme")

plot.ts(plaza_$Max_Precipitacion_MM)
plot(fit.gamma)


plaza_model<-subset(plaza_,select=-c(mes_12))
regresion_todos = glm(Max_Precipitacion_MM ~.,
                      family = Gamma(link='log'),
                      data = plaza_model)
summary(regresion_todos)
regresion_sim = glm(Max_Precipitacion_MM ~ Media_Precipitacion+
                      mes_1+mes_2+mes_3+mes_4+mes_5+mes_6+mes_7+mes_8+mes_9+mes_10+mes_11,
                    family = Gamma(link='log'),
                    data = plaza_model)
summary(regresion_sim)
###### Agregamos la media #####

predi_reg<- data_plaza_[,5:18] ### contemplamos los meses
predi_reg

#se construye el modelo de regresion con los casos completos
lm_media <- lm (Media_Precipitacion ~ mes_1+mes_2+mes_3+
                  mes_4+mes_5+mes_7+mes_8+mes_9+mes_10+mes_11+mes_12, predi_reg)
summary(lm_media)
#nota: lm construye el modelo de regresion implicitamente con los casos completos
#se eligen los valores predichos para los datos faltantes

new_mes_12 <- data.frame(predi_reg$mes_1,predi_reg$mes_2, predi_reg$mes_3, predi_reg$mes_4,
                         predi_reg$mes_5, predi_reg$mes_6, predi_reg$mes_7,
                         predi_reg$mes_8, predi_reg$mes_9, predi_reg$mes_10,
                         predi_reg$mes_11,predi_reg$mes_12)

new_mes_12 <- new_mes_12[12:25,]
names(new_mes_12) <- c('mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                       'mes_9',"mes_10",'mes_11',"mes_12")

pred.1 <- predict (lm_media, new_mes_12)
pred.1

#### Lo unimos
tend<-c(72:85)
imputa_dat <- data.frame(Media_Precipitacion =pred.1, Tendencia= tend, new_mes_12)
imputa_dat
#reg_data <- rbind(predi_reg,imputa_dat)
predi_reg_t <- subset(imputa_dat,select=-c(Tendencia))

### Retiramos las columnas(meses) que no son significativos
# el mes que vaya 

prediccion <- predict(regresion_sim,predi_reg_t,type="response")
predict_mes<-cbind(prediccion,predi_reg_t)
predict_mes$prediccion<-predict_mes$prediccion-0.001
predict_mes$Media_Precipitacion<-predict_mes$Media_Precipitacion-0.001
Plaza_24_pred <- predict_mes
Plaza_24_pred
data_plaza_[60:71,4:18]

maximos <- Plaza_24_pred[3:14,]
mmaximos <-maximos$prediccion
maximo_24 <- max(mmaximos)
indice = match(maximo_24,mmaximos)

Data_P <- data.frame(maximos)
names(Data_P) = c('Max_Precipitacion_MM','Media_Precipitacion',
                  'mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                  'mes_9',"mes_10",'mes_11',"mes_12")
pred <- add_pi(Data_P,regresion_sim, alpha = 0.1, names = c("lwr", "upr"))
conf <- add_ci(Data_P, regresion_sim, alpha = 0.1, names = c("lwr", "upr"))
intervalo_24 <- c(conf$lwr[indice],conf$upr[indice])

data_plaza_[60:71,4:18]


###############################################################################
###############################################################################
### CONTRASTES
annos <- as.numeric(unique(data_plaza_$Ano))

lista_mx_24 <-c(rep(0,length(annos)))

for ( i in 1:length(annos)){
  lista_mx_24[i] <- max(data_plaza_[data_plaza_$Ano==annos[i],4]) 
}
lista_mx_24

################################################################################
########################## PLAZA 25:  Merida #############################
data_plaza_ <- read_csv("C:/Users/KM Reyes Maya/Downloads/Tercer semestre/Computo Estadistico/Proyecto/Datos_by_Plaza/PLAZA_25.csv")

plaza_ <- data_plaza_[,4:18]
plaza_$Max_Precipitacion_MM = data_plaza_$Max_Precipitacion_MM+0.001
plaza_$Media_Precipitacion = data_plaza_$Media_Precipitacion+0.001

##### Exploración de la variable de respuest
##### ¿La variable sigue una distribución gamma?
fit.gamma <- fitdist(plaza_$Max_Precipitacion_MM,
                     distr = "gamma", method = "mme")

plot.ts(plaza_$Max_Precipitacion_MM)
plot(fit.gamma)


plaza_model<-subset(plaza_,select=-c(mes_12))
regresion_todos = glm(Max_Precipitacion_MM ~.,
                      family = Gamma(link='log'),
                      data = plaza_model)
summary(regresion_todos)

regresion_sim = glm(Max_Precipitacion_MM ~ Media_Precipitacion+ mes_1+mes_2+mes_3+mes_4+mes_5+
                      mes_6+mes_7+mes_8+mes_9+mes_10+mes_11,
                    family = Gamma(link='log'),
                    data = plaza_model)
summary(regresion_sim)


###### Agregamos la media #####

predi_reg<- data_plaza_[,5:18] ### contemplamos los meses
predi_reg

#se construye el modelo de regresion con los casos completos
lm_media <- lm (Media_Precipitacion ~ mes_1+mes_2+mes_3+
                  mes_4+mes_5+mes_7+mes_8+mes_9+mes_10+mes_11+mes_12, predi_reg)
summary(lm_media)
#nota: lm construye el modelo de regresion implicitamente con los casos completos
#se eligen los valores predichos para los datos faltantes

new_mes_12 <- data.frame(predi_reg$mes_1,predi_reg$mes_2, predi_reg$mes_3, predi_reg$mes_4,
                         predi_reg$mes_5, predi_reg$mes_6, predi_reg$mes_7,
                         predi_reg$mes_8, predi_reg$mes_9, predi_reg$mes_10,
                         predi_reg$mes_11,predi_reg$mes_12)

new_mes_12 <- new_mes_12[12:25,]
names(new_mes_12) <- c('mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                       'mes_9',"mes_10",'mes_11',"mes_12")

pred.1 <- predict (lm_media, new_mes_12)
pred.1

#### Lo unimos
tend<-c(72:85)
imputa_dat <- data.frame(Media_Precipitacion =pred.1, Tendencia= tend, new_mes_12)
imputa_dat
#reg_data <- rbind(predi_reg,imputa_dat)
predi_reg_t <- subset(imputa_dat,select=-c(Tendencia))

### Retiramos las columnas(meses) que no son significativos
# el mes que vaya 

prediccion <- predict(regresion_sim,predi_reg_t,type="response")
predict_mes<-cbind(prediccion,predi_reg_t)
predict_mes$prediccion<-predict_mes$prediccion-0.001
predict_mes$Media_Precipitacion<-predict_mes$Media_Precipitacion-0.001
Plaza_25_pred <- predict_mes
Plaza_25_pred
data_plaza_[60:71,4:18]

maximos <- Plaza_25_pred[3:14,]
mmaximos <-maximos$prediccion
maximo_25 <- max(mmaximos)
indice = match(maximo_25,mmaximos)

Data_P <- data.frame(maximos)
names(Data_P) = c('Max_Precipitacion_MM','Media_Precipitacion',
                  'mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                  'mes_9',"mes_10",'mes_11',"mes_12")
pred <- add_pi(Data_P,regresion_sim, alpha = 0.1, names = c("lwr", "upr"))
conf <- add_ci(Data_P, regresion_sim, alpha = 0.1, names = c("lwr", "upr"))
intervalo_25 <- c(conf$lwr[indice],conf$upr[indice])

data_plaza_[60:71,4:18]


###############################################################################
###############################################################################
### CONTRASTES
annos <- as.numeric(unique(data_plaza_$Ano))

lista_mx_25 <-c(rep(0,length(annos)))

for ( i in 1:length(annos)){
  lista_mx_25[i] <- max(data_plaza_[data_plaza_$Ano==annos[i],4]) 
}
lista_mx_25


################################################################################
########################## PLAZA 26:  Mexicali #############################
data_plaza_ <- read_csv("C:/Users/KM Reyes Maya/Downloads/Tercer semestre/Computo Estadistico/Proyecto/Datos_by_Plaza/PLAZA_26.csv")

plaza_ <- data_plaza_[,4:18]
plaza_$Max_Precipitacion_MM = data_plaza_$Max_Precipitacion_MM+0.001
plaza_$Media_Precipitacion = data_plaza_$Media_Precipitacion+0.001

##### Exploración de la variable de respuest
##### ¿La variable sigue una distribución gamma?
fit.gamma <- fitdist(plaza_$Max_Precipitacion_MM,
                     distr = "gamma", method = "mme")

plot.ts(plaza_$Max_Precipitacion_MM)
plot(fit.gamma)


plaza_model<-subset(plaza_,select=-c(mes_12))
regresion_todos = glm(Max_Precipitacion_MM ~.,
                      family = Gamma(link='log'),
                      data = plaza_model)
summary(regresion_todos)
regresion_sim = glm(Max_Precipitacion_MM ~ Media_Precipitacion+
                      mes_1+mes_2+mes_3+mes_4+mes_5+mes_6+mes_7+mes_8+mes_9+mes_10+mes_11,
                    family = Gamma(link='log'),
                    data = plaza_model)
summary(regresion_sim)

###### Agregamos la media #####

predi_reg<- data_plaza_[,5:18] ### contemplamos los meses
predi_reg

#se construye el modelo de regresion con los casos completos
lm_media <- lm (Media_Precipitacion ~ mes_1+mes_2+mes_3+
                  mes_4+mes_5+mes_7+mes_8+mes_9+mes_10+mes_11+mes_12, predi_reg)
summary(lm_media)
#nota: lm construye el modelo de regresion implicitamente con los casos completos
#se eligen los valores predichos para los datos faltantes

new_mes_12 <- data.frame(predi_reg$mes_1,predi_reg$mes_2, predi_reg$mes_3, predi_reg$mes_4,
                         predi_reg$mes_5, predi_reg$mes_6, predi_reg$mes_7,
                         predi_reg$mes_8, predi_reg$mes_9, predi_reg$mes_10,
                         predi_reg$mes_11,predi_reg$mes_12)

new_mes_12 <- new_mes_12[12:25,]
names(new_mes_12) <- c('mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                       'mes_9',"mes_10",'mes_11',"mes_12")

pred.1 <- predict (lm_media, new_mes_12)
pred.1

#### Lo unimos
tend<-c(72:85)
imputa_dat <- data.frame(Media_Precipitacion =pred.1, Tendencia= tend, new_mes_12)
imputa_dat
#reg_data <- rbind(predi_reg,imputa_dat)
predi_reg_t <- subset(imputa_dat,select=-c(Tendencia))

### Retiramos las columnas(meses) que no son significativos
# el mes que vaya 

prediccion <- predict(regresion_sim,predi_reg_t,type="response")
predict_mes<-cbind(prediccion,predi_reg_t)
predict_mes$prediccion<-predict_mes$prediccion-0.001
predict_mes$Media_Precipitacion<-predict_mes$Media_Precipitacion-0.001
Plaza_26_pred <- predict_mes
Plaza_26_pred
data_plaza_[60:71,4:18]

maximos <- Plaza_26_pred[3:14,]
mmaximos <-maximos$prediccion
maximo_26 <- max(mmaximos)
indice = match(maximo_26,mmaximos)

Data_P <- data.frame(maximos)
names(Data_P) = c('Max_Precipitacion_MM','Media_Precipitacion',
                  'mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                  'mes_9',"mes_10",'mes_11',"mes_12")
pred <- add_pi(Data_P,regresion_sim, alpha = 0.1, names = c("lwr", "upr"))
conf <- add_ci(Data_P, regresion_sim, alpha = 0.1, names = c("lwr", "upr"))
intervalo_26 <- c(conf$lwr[indice],conf$upr[indice])

data_plaza_[60:71,4:18]
###############################################################################
###############################################################################
### CONTRASTES
annos <- as.numeric(unique(data_plaza_$Ano))

lista_mx_26 <-c(rep(0,length(annos)))

for ( i in 1:length(annos)){
  lista_mx_26[i] <- max(data_plaza_[data_plaza_$Ano==annos[i],4]) 
}
lista_mx_26

################################################################################
########################## PLAZA 27:  Ajusco #############################
data_plaza_ <- read_csv("C:/Users/KM Reyes Maya/Downloads/Tercer semestre/Computo Estadistico/Proyecto/Datos_by_Plaza/PLAZA_27.csv")

plaza_ <- data_plaza_[,4:18]
plaza_$Max_Precipitacion_MM = data_plaza_$Max_Precipitacion_MM+0.001
plaza_$Media_Precipitacion = data_plaza_$Media_Precipitacion+0.001

##### Exploración de la variable de respuest
##### ¿La variable sigue una distribución gamma?
fit.gamma <- fitdist(plaza_$Max_Precipitacion_MM,
                     distr = "gamma", method = "mme")

plot.ts(plaza_$Max_Precipitacion_MM)
plot(fit.gamma)


plaza_model<-subset(plaza_,select=-c(mes_12))
regresion_todos = glm(Max_Precipitacion_MM ~.,
                      family = Gamma(link='log'),
                      data = plaza_model)
summary(regresion_todos)
regresion_sim = glm(Max_Precipitacion_MM ~ Media_Precipitacion+
                      mes_1+mes_2+mes_3+mes_4+mes_5+mes_6+mes_7+mes_8+mes_9+mes_10+mes_11,
                    family = Gamma(link='log'),
                    data = plaza_model)
summary(regresion_sim)

conf.int(regresion_sim, level=0.99)

###### Agregamos la media #####

predi_reg<- data_plaza_[,5:18] ### contemplamos los meses
predi_reg

#se construye el modelo de regresion con los casos completos
lm_media <- lm (Media_Precipitacion ~ mes_1+mes_2+mes_3+
                  mes_4+mes_5+mes_7+mes_8+mes_9+mes_10+mes_11+mes_12, predi_reg)
summary(lm_media)
#nota: lm construye el modelo de regresion implicitamente con los casos completos
#se eligen los valores predichos para los datos faltantes

new_mes_12 <- data.frame(predi_reg$mes_1,predi_reg$mes_2, predi_reg$mes_3, predi_reg$mes_4,
                         predi_reg$mes_5, predi_reg$mes_6, predi_reg$mes_7,
                         predi_reg$mes_8, predi_reg$mes_9, predi_reg$mes_10,
                         predi_reg$mes_11,predi_reg$mes_12)

new_mes_12 <- new_mes_12[12:25,]
names(new_mes_12) <- c('mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                       'mes_9',"mes_10",'mes_11',"mes_12")

pred.1 <- predict (lm_media, new_mes_12)
pred.1

#### Lo unimos
tend<-c(72:85)
imputa_dat <- data.frame(Media_Precipitacion =pred.1, Tendencia= tend, new_mes_12)
imputa_dat
#reg_data <- rbind(predi_reg,imputa_dat)
predi_reg_t <- subset(imputa_dat,select=-c(Tendencia))

### Retiramos las columnas(meses) que no son significativos
# el mes que vaya 
predi_reg_t

prediccion <- predict(regresion_sim,predi_reg_t,type="response")
prediccion

predict_mes<-cbind(prediccion,predi_reg_t)
predict_mes$prediccion<-predict_mes$prediccion-0.001
predict_mes$Media_Precipitacion<-predict_mes$Media_Precipitacion-0.001
Plaza_27_pred <- predict_mes
Plaza_27_pred


maximos <- Plaza_27_pred[3:14,]
mmaximos <-maximos$prediccion
maximo_27 <- max(mmaximos)
indice = match(maximo_27,mmaximos)

Data_P <- data.frame(maximos)
names(Data_P) = c('Max_Precipitacion_MM','Media_Precipitacion',
                  'mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                  'mes_9',"mes_10",'mes_11',"mes_12")
pred <- add_pi(Data_P,regresion_sim, alpha = 0.1, names = c("lwr", "upr"))
conf <- add_ci(Data_P, regresion_sim, alpha = 0.1, names = c("lwr", "upr"))
intervalo_27 <- c(conf$lwr[indice],conf$upr[indice])

data_plaza_[60:71,4:18]


###############################################################################
###############################################################################
### CONTRASTES
annos <- as.numeric(unique(data_plaza_$Ano))

lista_mx_27 <-c(rep(0,length(annos)))

for ( i in 1:length(annos)){
  lista_mx_27[i] <- max(data_plaza_[data_plaza_$Ano==annos[i],4]) 
}
lista_mx_27

################################################################################
################################################################################
########################### MAXIMO DE LAS PLAZAS ############################

Maximos <- c(maximo_01,maximo_02,
             maximo_03,maximo_04,
             maximo_05,maximo_06,
             maximo_07,maximo_08,
             maximo_09,maximo_10,
             maximo_11,maximo_12,
             maximo_13,maximo_14,
             maximo_15,maximo_16,
             maximo_17,maximo_18,
             maximo_19,maximo_20,
             maximo_21,maximo_22,
             maximo_23,maximo_24,
             maximo_25,maximo_26,
             maximo_27)
Maximos
Plazas <- c('10VCZ Boca del Rio', '10VCZ Coatzacoalcos',
            '10VCZ Cordoba', '10VCZ Poza Rica', '10VCZ Xalapa',
            'Acapulco', 'Aguascalientes', 'Cancun', 'Chihuahua',
            'Ciudad Juarez', 'Colima','Cuernavaca', 'Culiacan',
            'Durango', 'Guadalajara Altos', 'Guadalajara Chapala',
            'Guadalajara Tequila', 'Hermosillo', 'La Paz', 'Laguna',
            'Laredo', 'Leon', 'Los Mochis', 'Matamoros', 'Merida',
            'Mexicali', 'Mexico Ajusco')
Estimaciones <-cbind(Plazas,Maximos)
Estimaciones <-data.frame(Estimaciones)
Estimaciones
Histoicos <-rbind(
  lista_mx_01,lista_mx_02,
  lista_mx_03,lista_mx_04,
  lista_mx_05,lista_mx_06,
  lista_mx_07,lista_mx_08,
  lista_mx_09,lista_mx_10,
  lista_mx_11,lista_mx_12,
  lista_mx_13,lista_mx_14,
  lista_mx_15,lista_mx_16,
  lista_mx_17,lista_mx_18,
  lista_mx_19,lista_mx_20,
  lista_mx_21,lista_mx_22,
  lista_mx_23,lista_mx_24,
  lista_mx_25,lista_mx_26,
  lista_mx_27)
Histoicos

Historicos <- Histoicos
Historicos
annos

Intervalos <- rbind(intervalo_01,intervalo_02,
                    intervalo_03,intervalo_04,
                    intervalo_05,intervalo_06,
                    intervalo_07,intervalo_08,
                    intervalo_09,intervalo_10,
                    intervalo_11,intervalo_12,
                    intervalo_13,intervalo_14,
                    intervalo_15,intervalo_16,
                    intervalo_17,intervalo_18,
                    intervalo_19,intervalo_20,
                    intervalo_21,intervalo_22,
                    intervalo_23,intervalo_24,
                    intervalo_25,intervalo_26,
                    intervalo_27)

Contraste <-data.frame(Estimaciones$Plazas,
                       Historicos, Estimaciones$Maximos,Intervalos)
names(Contraste)=c('Plaza','2015','2016','2017','2018','2019','2020','2021','2022','Intervalo_INF','intervalos SUP')
Contraste
library(xlsx)
write.csv(Contraste, "C:/Users/KM Reyes Maya/Downloads/Tercer semestre/Computo Estadistico/Proyecto/Contraste_1.csv")



#########################################################################
############################## PLAZA_28 #################################
#########################################################################

#data_plaza_ <- read_csv("C:/Users/KM Reyes Maya/Downloads/Tercer semestre/Computo Estadistico/Proyecto/Datos_by_Plaza/PLAZA_01.csv")
data_plaza_ <- read.csv("C:/Users/Marcelo Sanchez/OneDrive/Escritorio/Tercer Semestre CIMAT/Computo Estadistico/Proyecto/PLAZA_28.csv")
View(data_plaza_)

plaza_ <- data_plaza_[,3:18]
plaza_$Max_Precipitacion_MM <- data_plaza_$Max_Precipitacion_MM+0.001
plaza_$Media_Precipitacion <- data_plaza_$Media_Precipitacion+0.001

##### Exploración de la variable de respuest
##### ¿La variable sigue una distribución gamma?
fit.gamma <- fitdist(plaza_$Max_Precipitacion_MM,
                     distr = "gamma", method = "mme")

plot.ts(plaza_$Max_Precipitacion_MM)
plot(fit.gamma)

regresion_todos <- glm(Max_Precipitacion_MM ~ 
                      mes_3+mes_4+mes_5+
                      mes_6+mes_7+mes_8+mes_9+mes_A+mes_B,
                      family = Gamma(link='log'),
                      data = plaza_)
summary(regresion_todos)


predi_reg <- data_plaza_[36:49,7:18] ### contemplamos los meses
predi_reg

### Retiramos las columnas(meses) que no son significativos
# el mes que vaya 

predi_reg_t <- subset(predi_reg,select=-c(Tendencia))
predi_reg_t

pr_28 <- predict(regresion_todos,predi_reg_t,type="response")
cbind(predi_reg_t,pr_28)

####################################
## intervalo que vamos a agregar a cada uno de los datos 

Data_P <- cbind(pr_28, predi_reg_t)
names(Data_P) = c('Max_Precipitacion_MM',# 'Media_Precipitacion'
                  'mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                  'mes_9',"mes_A",'mes_B')

pred <- add_pi(Data_P,regresion_todos, alpha = 0.1, 
               names = c("lwr", "upr"))
conf <- add_ci(Data_P, regresion_todos, alpha = 0.1, 
               names = c("lwr", "upr"))
indice <- which.max(Data_P$Max_Precipitacion_MM)
intervalo_28 <- c(conf$lwr[indice],conf$upr[indice])


#########################################################################
############################## PLAZA_29 #################################
#########################################################################

data_plaza_ <- read.csv("C:/Users/Marcelo Sanchez/OneDrive/Escritorio/Tercer Semestre CIMAT/Computo Estadistico/Proyecto/PLAZA_29.csv")
View(data_plaza_)

plaza_ <- data_plaza_[,3:18]
plaza_$Max_Precipitacion_MM <- data_plaza_$Max_Precipitacion_MM+0.001
plaza_$Media_Precipitacion <- data_plaza_$Media_Precipitacion+0.001

##### Exploración de la variable de respuest
##### ¿La variable sigue una distribución gamma?
fit.gamma <- fitdist(plaza_$Max_Precipitacion_MM,
                     distr = "gamma", method = "mme")

plot.ts(plaza_$Max_Precipitacion_MM)
plot(fit.gamma)

regresion_todos <- glm(Max_Precipitacion_MM ~
                        1/mes_3+1/mes_4+1/mes_5+
                        1/mes_6+1/mes_7+1/mes_8+1/mes_9+1/mes_A+1/mes_B,
                       family = Gamma(link='log'),
                       data = plaza_)
summary(regresion_todos)


predi_reg <- data_plaza_[36:49,7:18] ### contemplamos los meses
predi_reg

### Retiramos las columnas(meses) que no son significativos
# el mes que vaya 

predi_reg_t <- subset(predi_reg,select=-c(Tendencia))
predi_reg_t

pr_29 <- predict(regresion_todos,predi_reg_t,type="response")
cbind(predi_reg_t,pr_29)

####################################
## intervalo que vamos a agregar a cada uno de los datos 

Data_P <- cbind(pr_29, predi_reg_t)
names(Data_P) = c('Max_Precipitacion_MM',# 'Media_Precipitacion'
                  'mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                  'mes_9',"mes_A",'mes_B')

pred <- add_pi(Data_P,regresion_todos, alpha = 0.1, 
               names = c("lwr", "upr"))
conf <- add_ci(Data_P, regresion_todos, alpha = 0.1, 
               names = c("lwr", "upr"))
indice <- which.max(Data_P$Max_Precipitacion_MM)
intervalo_29 <- c(conf$lwr[indice],conf$upr[indice])

#########################################################################
############################## PLAZA_30 #################################
#########################################################################

data_plaza_ <- read.csv("C:/Users/Marcelo Sanchez/OneDrive/Escritorio/Tercer Semestre CIMAT/Computo Estadistico/Proyecto/PLAZA_30.csv")
View(data_plaza_)

plaza_ <- data_plaza_[,3:18]
plaza_$Max_Precipitacion_MM <- data_plaza_$Max_Precipitacion_MM+0.001
plaza_$Media_Precipitacion <- data_plaza_$Media_Precipitacion+0.001

##### Exploración de la variable de respuest
##### ¿La variable sigue una distribución gamma?
fit.gamma <- fitdist(plaza_$Max_Precipitacion_MM,
                     distr = "gamma", method = "mme")

plot.ts(plaza_$Max_Precipitacion_MM)
plot(fit.gamma)

regresion_todos <- glm(Max_Precipitacion_MM ~1/Media_Precipitacion+
                         1/mes_3+1/mes_4+1/mes_5+
                         1/mes_6+1/mes_7+1/mes_8+1/mes_9+
                         1/mes_A+1/mes_B,
                       family = Gamma(link='log'),
                       data = plaza_)
summary(regresion_todos)

##################### ocupamos la media ############
r_media <- lm(Media_Precipitacion~mes_1+mes_2+mes_3+mes_4+
                mes_5+mes_6+
                mes_7+mes_8+mes_9+
                mes_A+mes_B, data = plaza_)
r_media_pred <- data_plaza_[36:49,7:18]
r_media_pred <- subset(r_media_pred, select = -c(Tendencia) )
r_media_pred
Media_Precipitacion <- predict(r_media,r_media_pred,type="response")
Media_Precipitacion
####################################################

predi_reg <- data_plaza_[36:49,7:18] ### contemplamos los meses
predi_reg <- cbind(predi_reg, Media_Precipitacion)
predi_reg

### Retiramos las columnas(meses) que no son significativos
# el mes que vaya 

predi_reg_t <- subset(predi_reg,select=-c(Tendencia))
predi_reg_t

pr_30 <- predict(regresion_todos,predi_reg_t,type="response")
cbind(predi_reg_t,pr_30)

####################################
## intervalo que vamos a agregar a cada uno de los datos 

Data_P <- cbind(pr_30, predi_reg_t)
names(Data_P) = c('Max_Precipitacion_MM',
                  'mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                  'mes_9',"mes_A",'mes_B','Media_Precipitacion')

pred <- add_pi(Data_P,regresion_todos, alpha = 0.1, 
               names = c("lwr", "upr"))
conf <- add_ci(Data_P, regresion_todos, alpha = 0.1, 
               names = c("lwr", "upr"))
indice <- which.max(Data_P$Max_Precipitacion_MM)
intervalo_30 <- c(conf$lwr[indice],conf$upr[indice])

#########################################################################
############################## PLAZA_31 #################################
#########################################################################

data_plaza_ <- read.csv("C:/Users/Marcelo Sanchez/OneDrive/Escritorio/Tercer Semestre CIMAT/Computo Estadistico/Proyecto/PLAZA_31.csv")
View(data_plaza_)

plaza_ <- data_plaza_[,3:18]
plaza_$Max_Precipitacion_MM <- data_plaza_$Max_Precipitacion_MM+0.001
plaza_$Media_Precipitacion <- data_plaza_$Media_Precipitacion+0.001

##### Exploración de la variable de respuest
##### ¿La variable sigue una distribución gamma?
fit.gamma <- fitdist(plaza_$Max_Precipitacion_MM,
                     distr = "gamma", method = "mme")

plot.ts(plaza_$Max_Precipitacion_MM)
plot(fit.gamma)

regresion_todos <- glm(Max_Precipitacion_MM ~Media_Precipitacion+ 
                         mes_2+mes_4+mes_5+
                         mes_6+mes_7+mes_8+mes_9+mes_A+ mes_B,
                       family = Gamma(link='log'),
                       data = plaza_)
summary(regresion_todos)


##################### ocupamos la media ############
r_media <- lm(Media_Precipitacion~mes_1+mes_2+mes_3+mes_4+
                mes_5+mes_6+
                mes_7+mes_8+mes_9+
                mes_A+mes_B, data = plaza_)
r_media_pred <- data_plaza_[36:49,7:18]
r_media_pred <- subset(r_media_pred, select = -c(Tendencia) )
r_media_pred
Media_Precipitacion <- predict(r_media,r_media_pred,type="response")
Media_Precipitacion
####################################################

predi_reg <- data_plaza_[36:49,7:18] ### contemplamos los meses
predi_reg <- cbind(predi_reg, Media_Precipitacion)
predi_reg

### Retiramos las columnas(meses) que no son significativos
# el mes que vaya 

predi_reg_t <- subset(predi_reg,select=-c(Tendencia))
predi_reg_t

pr_31 <- predict(regresion_todos,predi_reg_t,type="response")
cbind(predi_reg_t,pr_31)

####################################
## intervalo que vamos a agregar a cada uno de los datos 

Data_P <- cbind(pr_31, predi_reg_t)
names(Data_P) = c('Max_Precipitacion_MM',
                  'mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                  'mes_9',"mes_A",'mes_B','Media_Precipitacion')

pred <- add_pi(Data_P,regresion_todos, alpha = 0.1, 
               names = c("lwr", "upr"))
conf <- add_ci(Data_P, regresion_todos, alpha = 0.1, 
               names = c("lwr", "upr"))
indice <- which.max(Data_P$Max_Precipitacion_MM)
intervalo_31 <- c(conf$lwr[indice],conf$upr[indice])

#########################################################################
############################## PLAZA_32 #################################
#########################################################################

data_plaza_ <- read.csv("C:/Users/Marcelo Sanchez/OneDrive/Escritorio/Tercer Semestre CIMAT/Computo Estadistico/Proyecto/PLAZA_32.csv")
View(data_plaza_)

plaza_ <- data_plaza_[,3:18]
plaza_$Max_Precipitacion_MM <- data_plaza_$Max_Precipitacion_MM+0.001
plaza_$Media_Precipitacion <- data_plaza_$Media_Precipitacion+0.001

##### Exploración de la variable de respuest
##### ¿La variable sigue una distribución gamma?
fit.gamma <- fitdist(plaza_$Max_Precipitacion_MM,
                     distr = "gamma", method = "mme")

plot.ts(plaza_$Max_Precipitacion_MM)
plot(fit.gamma)

regresion_todos <- glm(Max_Precipitacion_MM ~Media_Precipitacion+ mes_1+mes_5+
                         mes_7+mes_9+mes_2,
                       family = Gamma(link='log'),
                       data = plaza_)
summary(regresion_todos)

##################### ocupamos la media ############
r_media <- lm(Media_Precipitacion~mes_1+mes_2+mes_3+mes_4+
                mes_5+mes_6+
                mes_7+mes_8+mes_9+
                mes_A+mes_B, data = plaza_)
r_media_pred <- data_plaza_[36:49,7:18]
r_media_pred <- subset(r_media_pred, select = -c(Tendencia) )
r_media_pred
Media_Precipitacion <- predict(r_media,r_media_pred,type="response")
Media_Precipitacion
####################################################

predi_reg <- data_plaza_[36:49,7:18] ### contemplamos los meses
predi_reg <- cbind(predi_reg, Media_Precipitacion)
predi_reg

### Retiramos las columnas(meses) que no son significativos
# el mes que vaya 

predi_reg_t <- subset(predi_reg,select=-c(Tendencia))
predi_reg_t

pr_32 <- predict(regresion_todos,predi_reg_t,type="response")
cbind(predi_reg_t,pr_32)

####################################
## intervalo que vamos a agregar a cada uno de los datos 

Data_P <- cbind(pr_32, predi_reg_t)
names(Data_P) = c('Max_Precipitacion_MM',
                  'mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                  'mes_9',"mes_A",'mes_B','Media_Precipitacion')

pred <- add_pi(Data_P,regresion_todos, alpha = 0.1, 
               names = c("lwr", "upr"))
conf <- add_ci(Data_P, regresion_todos, alpha = 0.1, 
               names = c("lwr", "upr"))
indice <- which.max(Data_P$Max_Precipitacion_MM)
intervalo_32 <- c(conf$lwr[indice],conf$upr[indice])


#########################################################################
############################## PLAZA_33 #################################
#########################################################################

data_plaza_ <- read.csv("C:/Users/Marcelo Sanchez/OneDrive/Escritorio/Tercer Semestre CIMAT/Computo Estadistico/Proyecto/PLAZA_33.csv")
View(data_plaza_)

plaza_ <- data_plaza_[,3:18]
plaza_$Max_Precipitacion_MM <- data_plaza_$Max_Precipitacion_MM+0.001
plaza_$Media_Precipitacion <- data_plaza_$Media_Precipitacion+0.001

##### Exploración de la variable de respuest
##### ¿La variable sigue una distribución gamma?
fit.gamma <- fitdist(plaza_$Max_Precipitacion_MM,
                     distr = "gamma", method = "mme")

plot.ts(plaza_$Max_Precipitacion_MM)
plot(fit.gamma)

regresion_todos <- glm(Max_Precipitacion_MM ~Media_Precipitacion+ 
                        mes_1+mes_2
                       +mes_5+mes_6+mes_7+mes_9,
                       family = Gamma(link='log'),
                       data = plaza_)
summary(regresion_todos)

##################### ocupamos la media ############
r_media <- lm(Media_Precipitacion~mes_1+mes_2+mes_3+mes_4+
                mes_5+mes_6+
                mes_7+mes_8+mes_9+
                mes_A+mes_B, data = plaza_)
r_media_pred <- data_plaza_[36:49,7:18]
r_media_pred <- subset(r_media_pred, select = -c(Tendencia) )
r_media_pred
Media_Precipitacion <- predict(r_media,r_media_pred,type="response")
Media_Precipitacion
####################################################

predi_reg <- data_plaza_[36:49,7:18] ### contemplamos los meses
predi_reg <- cbind(predi_reg, Media_Precipitacion)
predi_reg

### Retiramos las columnas(meses) que no son significativos
# el mes que vaya 

predi_reg_t <- subset(predi_reg,select=-c(Tendencia))
predi_reg_t

pr_33 <- predict(regresion_todos,predi_reg_t,type="response")
cbind(predi_reg_t,pr_33)

####################################
## intervalo que vamos a agregar a cada uno de los datos 

Data_P <- cbind(pr_33, predi_reg_t)
names(Data_P) = c('Max_Precipitacion_MM',
                  'mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                  'mes_9',"mes_A",'mes_B','Media_Precipitacion')

pred <- add_pi(Data_P,regresion_todos, alpha = 0.1, 
               names = c("lwr", "upr"))
conf <- add_ci(Data_P, regresion_todos, alpha = 0.1, 
               names = c("lwr", "upr"))
indice <- which.max(Data_P$Max_Precipitacion_MM)
intervalo_33 <- c(conf$lwr[indice],conf$upr[indice])

#########################################################################
############################## PLAZA_34 #################################
#########################################################################

data_plaza_ <- read.csv("C:/Users/Marcelo Sanchez/OneDrive/Escritorio/Tercer Semestre CIMAT/Computo Estadistico/Proyecto/PLAZA_34.csv")
View(data_plaza_)

plaza_ <- data_plaza_[,3:18]
plaza_$Max_Precipitacion_MM <- data_plaza_$Max_Precipitacion_MM+0.001
plaza_$Media_Precipitacion <- data_plaza_$Media_Precipitacion+0.001

##### Exploración de la variable de respuest
##### ¿La variable sigue una distribución gamma?
fit.gamma <- fitdist(plaza_$Max_Precipitacion_MM,
                     distr = "gamma", method = "mme")

plot.ts(plaza_$Max_Precipitacion_MM)
plot(fit.gamma)

regresion_todos <- glm(Max_Precipitacion_MM ~Media_Precipitacion+ 
                        mes_1+mes_2
                       +mes_5+mes_6+mes_7+mes_9,
                       family = Gamma(link='log'),
                       data = plaza_)
summary(regresion_todos)

##################### ocupamos la media ############
r_media <- lm(Media_Precipitacion~mes_1+mes_2+mes_3+mes_4+
                mes_5+mes_6+
                mes_7+mes_8+mes_9+
                mes_A+mes_B, data = plaza_)
r_media_pred <- data_plaza_[36:49,7:18]
r_media_pred <- subset(r_media_pred, select = -c(Tendencia) )
r_media_pred
Media_Precipitacion <- predict(r_media,r_media_pred,type="response")
Media_Precipitacion
####################################################

predi_reg <- data_plaza_[36:49,7:18] ### contemplamos los meses
predi_reg <- cbind(predi_reg, Media_Precipitacion)
predi_reg

### Retiramos las columnas(meses) que no son significativos
# el mes que vaya 

predi_reg_t <- subset(predi_reg,select=-c(Tendencia))
predi_reg_t


pr_34 <- predict(regresion_todos,predi_reg_t,type="response")
cbind(predi_reg_t,pr_34)

####################################
## intervalo que vamos a agregar a cada uno de los datos 

Data_P <- cbind(pr_34, predi_reg_t)
names(Data_P) = c('Max_Precipitacion_MM',
                  'mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                  'mes_9',"mes_A",'mes_B','Media_Precipitacion')

pred <- add_pi(Data_P,regresion_todos, alpha = 0.1, 
               names = c("lwr", "upr"))
conf <- add_ci(Data_P, regresion_todos, alpha = 0.1, 
               names = c("lwr", "upr"))
indice <- which.max(Data_P$Max_Precipitacion_MM)
intervalo_34 <- c(conf$lwr[indice],conf$upr[indice])

#########################################################################
############################## PLAZA_35 #################################
#########################################################################

data_plaza_ <- read.csv("C:/Users/Marcelo Sanchez/OneDrive/Escritorio/Tercer Semestre CIMAT/Computo Estadistico/Proyecto/PLAZA_35.csv")
View(data_plaza_)

plaza_ <- data_plaza_[,3:18]
plaza_$Max_Precipitacion_MM <- data_plaza_$Max_Precipitacion_MM+0.001
plaza_$Media_Precipitacion <- data_plaza_$Media_Precipitacion+0.001

##### Exploración de la variable de respuest
##### ¿La variable sigue una distribución gamma?
fit.gamma <- fitdist(plaza_$Max_Precipitacion_MM,
                     distr = "gamma", method = "mme")

plot.ts(plaza_$Max_Precipitacion_MM)
plot(fit.gamma)

regresion_todos <- glm(Max_Precipitacion_MM ~Media_Precipitacion+ 
                        mes_1+mes_2
                       +mes_5+mes_6+mes_7+mes_9
                       +mes_A,
                       family = Gamma(link='log'),
                       data = plaza_)
summary(regresion_todos)

##################### ocupamos la media ############
r_media <- lm(Media_Precipitacion~mes_1+mes_2+mes_3+mes_4+
                mes_5+mes_6+
                mes_7+mes_8+mes_9+
                mes_A+mes_B, data = plaza_)
r_media_pred <- data_plaza_[36:49,7:18]
r_media_pred <- subset(r_media_pred, select = -c(Tendencia) )
r_media_pred
Media_Precipitacion <- predict(r_media,r_media_pred,type="response")
Media_Precipitacion
####################################################

predi_reg <- data_plaza_[36:49,7:18] ### contemplamos los meses
predi_reg <- cbind(predi_reg, Media_Precipitacion)
predi_reg

### Retiramos las columnas(meses) que no son significativos
# el mes que vaya 

predi_reg_t <- subset(predi_reg,select=-c(Tendencia))
predi_reg_t


pr_35 <- predict(regresion_todos,predi_reg_t,type="response")
cbind(predi_reg_t,pr_35)

####################################
## intervalo que vamos a agregar a cada uno de los datos 

Data_P <- cbind(pr_35, predi_reg_t)
names(Data_P) = c('Max_Precipitacion_MM',
                  'mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                  'mes_9',"mes_A",'mes_B','Media_Precipitacion')

pred <- add_pi(Data_P,regresion_todos, alpha = 0.1, 
               names = c("lwr", "upr"))
conf <- add_ci(Data_P, regresion_todos, alpha = 0.1, 
               names = c("lwr", "upr"))
indice <- which.max(Data_P$Max_Precipitacion_MM)
intervalo_35 <- c(conf$lwr[indice],conf$upr[indice])

#########################################################################
############################## PLAZA_36 #################################
#########################################################################

data_plaza_ <- read.csv("C:/Users/Marcelo Sanchez/OneDrive/Escritorio/Tercer Semestre CIMAT/Computo Estadistico/Proyecto/PLAZA_36.csv")
View(data_plaza_)

plaza_ <- data_plaza_[,3:18]
plaza_$Max_Precipitacion_MM <- data_plaza_$Max_Precipitacion_MM+0.001
plaza_$Media_Precipitacion <- data_plaza_$Media_Precipitacion+0.001

##### Exploración de la variable de respuest
##### ¿La variable sigue una distribución gamma?
fit.gamma <- fitdist(plaza_$Max_Precipitacion_MM,
                     distr = "gamma", method = "mme")

plot.ts(plaza_$Max_Precipitacion_MM)
plot(fit.gamma)

regresion_todos <- glm(Max_Precipitacion_MM ~Media_Precipitacion+ 
                        mes_1+mes_2
                       +mes_5+mes_6+mes_7+mes_9
                       +mes_A,
                       family = Gamma(link='log'),
                       data = plaza_)
summary(regresion_todos)

##################### ocupamos la media ############
r_media <- lm(Media_Precipitacion~mes_1+mes_2+mes_3+mes_4+
                mes_5+mes_6+
                mes_7+mes_8+mes_9+
                mes_A+mes_B, data = plaza_)
r_media_pred <- data_plaza_[36:49,7:18]
r_media_pred <- subset(r_media_pred, select = -c(Tendencia) )
r_media_pred
Media_Precipitacion <- predict(r_media,r_media_pred,type="response")
Media_Precipitacion
####################################################

predi_reg <- data_plaza_[36:49,7:18] ### contemplamos los meses
predi_reg <- cbind(predi_reg, Media_Precipitacion)
predi_reg

### Retiramos las columnas(meses) que no son significativos
# el mes que vaya 

predi_reg_t <- subset(predi_reg,select=-c(Tendencia))
predi_reg_t

pr_36 <- predict(regresion_todos,predi_reg_t,type="response")
cbind(predi_reg_t,pr_36)

####################################
## intervalo que vamos a agregar a cada uno de los datos 

Data_P <- cbind(pr_36, predi_reg_t)
names(Data_P) = c('Max_Precipitacion_MM',
                  'mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                  'mes_9',"mes_A",'mes_B','Media_Precipitacion')

pred <- add_pi(Data_P,regresion_todos, alpha = 0.1, 
               names = c("lwr", "upr"))
conf <- add_ci(Data_P, regresion_todos, alpha = 0.1, 
               names = c("lwr", "upr"))
indice <- which.max(Data_P$Max_Precipitacion_MM)
intervalo_36 <- c(conf$lwr[indice],conf$upr[indice])

#########################################################################
############################## PLAZA_37 #################################
#########################################################################

data_plaza_ <- read.csv("C:/Users/Marcelo Sanchez/OneDrive/Escritorio/Tercer Semestre CIMAT/Computo Estadistico/Proyecto/PLAZA_37.csv")
View(data_plaza_)

plaza_ <- data_plaza_[,3:18]
plaza_$Max_Precipitacion_MM <- data_plaza_$Max_Precipitacion_MM+0.001
plaza_$Media_Precipitacion <- data_plaza_$Media_Precipitacion+0.001

##### Exploración de la variable de respuest
##### ¿La variable sigue una distribución gamma?
fit.gamma <- fitdist(plaza_$Max_Precipitacion_MM,
                     distr = "gamma", method = "mme")

plot.ts(plaza_$Max_Precipitacion_MM)
plot(fit.gamma)

regresion_todos <- glm(Max_Precipitacion_MM ~Media_Precipitacion+ 
                        mes_4
                       +mes_5+mes_6+mes_7+mes_8+mes_9
                       +mes_A+mes_B,
                       family = Gamma(link='log'),
                       data = plaza_)
summary(regresion_todos)

##################### ocupamos la media ############
r_media <- lm(Media_Precipitacion~mes_1+mes_2+mes_3+mes_4+
                mes_5+mes_6+
                mes_7+mes_8+mes_9+
                mes_A+mes_B, data = plaza_)
r_media_pred <- data_plaza_[36:49,7:18]
r_media_pred <- subset(r_media_pred, select = -c(Tendencia) )
r_media_pred
Media_Precipitacion <- predict(r_media,r_media_pred,type="response")
Media_Precipitacion
####################################################

predi_reg <- data_plaza_[36:49,7:18] ### contemplamos los meses
predi_reg <- cbind(predi_reg, Media_Precipitacion)
predi_reg

### Retiramos las columnas(meses) que no son significativos
# el mes que vaya 

predi_reg_t <- subset(predi_reg,select=-c(Tendencia))
predi_reg_t

pr_37 <- predict(regresion_todos,predi_reg_t,type="response")
cbind(predi_reg_t,pr_37)

####################################
## intervalo que vamos a agregar a cada uno de los datos 

Data_P <- cbind(pr_37, predi_reg_t)
names(Data_P) = c('Max_Precipitacion_MM',
                  'mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                  'mes_9',"mes_A",'mes_B','Media_Precipitacion')

pred <- add_pi(Data_P,regresion_todos, alpha = 0.1, 
               names = c("lwr", "upr"))
conf <- add_ci(Data_P, regresion_todos, alpha = 0.1, 
               names = c("lwr", "upr"))
indice <- which.max(Data_P$Max_Precipitacion_MM)
intervalo_37 <- c(conf$lwr[indice],conf$upr[indice])

#########################################################################
############################## PLAZA_38 #################################
#########################################################################

data_plaza_ <- read.csv("C:/Users/Marcelo Sanchez/OneDrive/Escritorio/Tercer Semestre CIMAT/Computo Estadistico/Proyecto/PLAZA_38.csv")
View(data_plaza_)

plaza_ <- data_plaza_[,3:18]
plaza_$Max_Precipitacion_MM <- data_plaza_$Max_Precipitacion_MM+0.001
plaza_$Media_Precipitacion <- data_plaza_$Media_Precipitacion+0.001

##### Exploración de la variable de respuest
##### ¿La variable sigue una distribución gamma?
fit.gamma <- fitdist(plaza_$Max_Precipitacion_MM,
                     distr = "gamma", method = "mme")

plot.ts(plaza_$Max_Precipitacion_MM)
plot(fit.gamma)

regresion_todos <- glm(Max_Precipitacion_MM ~ Media_Precipitacion
                       +mes_5+mes_9,
                       family = Gamma(link='log'),
                       data = plaza_)
summary(regresion_todos)

##################### ocupamos la media ############
r_media <- lm(Media_Precipitacion~mes_1+mes_2+mes_3+mes_4+
                mes_5+mes_6+
                mes_7+mes_8+mes_9+
                mes_A+mes_B, data = plaza_)
r_media_pred <- data_plaza_[36:49,7:18]
r_media_pred <- subset(r_media_pred, select = -c(Tendencia) )
r_media_pred
Media_Precipitacion <- predict(r_media,r_media_pred,type="response")
Media_Precipitacion
####################################################

predi_reg <- data_plaza_[36:49,7:18] ### contemplamos los meses
predi_reg <- cbind(predi_reg, Media_Precipitacion)
predi_reg

### Retiramos las columnas(meses) que no son significativos
# el mes que vaya 

predi_reg_t <- subset(predi_reg,select=-c(Tendencia))
predi_reg_t

pr_38 <- predict(regresion_todos,predi_reg_t,type="response")
cbind(predi_reg_t,pr_38)

####################################
## intervalo que vamos a agregar a cada uno de los datos 

Data_P <- cbind(pr_38, predi_reg_t)
names(Data_P) = c('Max_Precipitacion_MM',
                  'mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                  'mes_9',"mes_A",'mes_B','Media_Precipitacion')

pred <- add_pi(Data_P,regresion_todos, alpha = 0.1, 
               names = c("lwr", "upr"))
conf <- add_ci(Data_P, regresion_todos, alpha = 0.1, 
               names = c("lwr", "upr"))
indice <- which.max(Data_P$Max_Precipitacion_MM)
intervalo_38 <- c(conf$lwr[indice],conf$upr[indice])

#########################################################################
############################## PLAZA_39 #################################
#########################################################################

data_plaza_ <- read.csv("C:/Users/Marcelo Sanchez/OneDrive/Escritorio/Tercer Semestre CIMAT/Computo Estadistico/Proyecto/PLAZA_39.csv")
View(data_plaza_)

plaza_ <- data_plaza_[,3:18]
plaza_$Max_Precipitacion_MM <- data_plaza_$Max_Precipitacion_MM+0.001
plaza_$Media_Precipitacion <- data_plaza_$Media_Precipitacion+0.001

##### Exploración de la variable de respuest
##### ¿La variable sigue una distribución gamma?
fit.gamma <- fitdist(plaza_$Max_Precipitacion_MM,
                     distr = "gamma", method = "mme")

plot.ts(plaza_$Max_Precipitacion_MM)
plot(fit.gamma)

regresion_todos <- glm(Max_Precipitacion_MM ~Media_Precipitacion+ 
                        mes_2+mes_4
                       +mes_5+mes_6+mes_7+mes_8+mes_9+mes_A+mes_B,
                       family = Gamma(link='log'),
                       data = plaza_)
summary(regresion_todos)

##################### ocupamos la media ############
r_media <- lm(Media_Precipitacion~mes_1+mes_2+mes_3+mes_4+
                mes_5+mes_6+
                mes_7+mes_8+mes_9+
                mes_A+mes_B, data = plaza_)
r_media_pred <- data_plaza_[36:49,7:18]
r_media_pred <- subset(r_media_pred, select = -c(Tendencia) )
r_media_pred
Media_Precipitacion <- predict(r_media,r_media_pred,type="response")
Media_Precipitacion
####################################################

predi_reg <- data_plaza_[36:49,7:18] ### contemplamos los meses
predi_reg <- cbind(predi_reg, Media_Precipitacion)
predi_reg

### Retiramos las columnas(meses) que no son significativos
# el mes que vaya 

predi_reg_t <- subset(predi_reg,select=-c(Tendencia))
predi_reg_t

pr_39 <- predict(regresion_todos,predi_reg_t,type="response")
cbind(predi_reg_t,pr_39)

####################################
## intervalo que vamos a agregar a cada uno de los datos 

Data_P <- cbind(pr_39, predi_reg_t)
names(Data_P) = c('Max_Precipitacion_MM',
                  'mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                  'mes_9',"mes_A",'mes_B','Media_Precipitacion')

pred <- add_pi(Data_P,regresion_todos, alpha = 0.1, 
               names = c("lwr", "upr"))
conf <- add_ci(Data_P, regresion_todos, alpha = 0.1, 
               names = c("lwr", "upr"))
indice <- which.max(Data_P$Max_Precipitacion_MM)
intervalo_39 <- c(conf$lwr[indice],conf$upr[indice])

#########################################################################
############################## PLAZA_40 #################################
#########################################################################

data_plaza_ <- read.csv("C:/Users/Marcelo Sanchez/OneDrive/Escritorio/Tercer Semestre CIMAT/Computo Estadistico/Proyecto/PLAZA_40.csv")
View(data_plaza_)

plaza_ <- data_plaza_[,3:18]
plaza_$Max_Precipitacion_MM <- data_plaza_$Max_Precipitacion_MM+0.001
plaza_$Media_Precipitacion <- data_plaza_$Media_Precipitacion+0.001

##### Exploración de la variable de respuest
##### ¿La variable sigue una distribución gamma?
fit.gamma <- fitdist(plaza_$Max_Precipitacion_MM,
                     distr = "gamma", method = "mme")

plot.ts(plaza_$Max_Precipitacion_MM)
plot(fit.gamma)

regresion_todos <- glm(Max_Precipitacion_MM ~Media_Precipitacion+ 
                        mes_4
                       +mes_5+mes_8+mes_9,
                       family = Gamma(link='log'),
                       data = plaza_)
summary(regresion_todos)

##################### ocupamos la media ############
r_media <- lm(Media_Precipitacion~mes_1+mes_2+mes_3+mes_4+
                mes_5+mes_6+
                mes_7+mes_8+mes_9+
                mes_A+mes_B, data = plaza_)
r_media_pred <- data_plaza_[36:49,7:18]
r_media_pred <- subset(r_media_pred, select = -c(Tendencia) )
r_media_pred
Media_Precipitacion <- predict(r_media,r_media_pred,type="response")
Media_Precipitacion
####################################################

predi_reg <- data_plaza_[36:49,7:18] ### contemplamos los meses
predi_reg <- cbind(predi_reg, Media_Precipitacion)
predi_reg

### Retiramos las columnas(meses) que no son significativos
# el mes que vaya 

predi_reg_t <- subset(predi_reg,select=-c(Tendencia))
predi_reg_t

pr_40 <- predict(regresion_todos,predi_reg_t,type="response")
cbind(predi_reg_t,pr_40)

####################################
## intervalo que vamos a agregar a cada uno de los datos 

Data_P <- cbind(pr_40, predi_reg_t)
names(Data_P) = c('Max_Precipitacion_MM',
                  'mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                  'mes_9',"mes_A",'mes_B','Media_Precipitacion')

pred <- add_pi(Data_P,regresion_todos, alpha = 0.1, 
               names = c("lwr", "upr"))
conf <- add_ci(Data_P, regresion_todos, alpha = 0.1, 
               names = c("lwr", "upr"))
indice <- which.max(Data_P$Max_Precipitacion_MM)
intervalo_40 <- c(conf$lwr[indice],conf$upr[indice])

#########################################################################
############################## PLAZA_41 #################################
#########################################################################

data_plaza_ <- read.csv("C:/Users/Marcelo Sanchez/OneDrive/Escritorio/Tercer Semestre CIMAT/Computo Estadistico/Proyecto/PLAZA_41.csv")
View(data_plaza_)

plaza_ <- data_plaza_[,3:18]
plaza_$Max_Precipitacion_MM <- data_plaza_$Max_Precipitacion_MM+0.001
plaza_$Media_Precipitacion <- data_plaza_$Media_Precipitacion+0.001

##### Exploración de la variable de respuest
##### ¿La variable sigue una distribución gamma?
fit.gamma <- fitdist(plaza_$Max_Precipitacion_MM,
                     distr = "gamma", method = "mme")

plot.ts(plaza_$Max_Precipitacion_MM)
plot(fit.gamma)

regresion_todos <- glm(Max_Precipitacion_MM ~Media_Precipitacion+ 
                        mes_1+mes_3+mes_4
                       +mes_5+mes_6+mes_7+mes_8+mes_9+mes_A+mes_B,
                       family = Gamma(link='log'),#log
                       data = plaza_)
summary(regresion_todos)

##################### ocupamos la media ############
r_media <- lm(Media_Precipitacion~mes_1+mes_2+mes_3+mes_4+
                mes_5+mes_6+
                mes_7+mes_8+mes_9+
                mes_A+mes_B, data = plaza_)
r_media_pred <- data_plaza_[36:49,7:18]
r_media_pred <- subset(r_media_pred, select = -c(Tendencia) )
r_media_pred
Media_Precipitacion <- predict(r_media,r_media_pred,type="response")
Media_Precipitacion
####################################################

predi_reg <- data_plaza_[36:49,7:18] ### contemplamos los meses
predi_reg <- cbind(predi_reg, Media_Precipitacion)
predi_reg

### Retiramos las columnas(meses) que no son significativos
# el mes que vaya 

predi_reg_t <- subset(predi_reg,select=-c(Tendencia))
predi_reg_t

pr_41 <- predict(regresion_todos,predi_reg_t,type="response")
cbind(predi_reg_t,pr_41)

####################################
## intervalo que vamos a agregar a cada uno de los datos 

Data_P <- cbind(pr_41, predi_reg_t)
names(Data_P) = c('Max_Precipitacion_MM',
                  'mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                  'mes_9',"mes_A",'mes_B','Media_Precipitacion')

pred <- add_pi(Data_P,regresion_todos, alpha = 0.1, 
               names = c("lwr", "upr"))
conf <- add_ci(Data_P, regresion_todos, alpha = 0.1, 
               names = c("lwr", "upr"))
indice <- which.max(Data_P$Max_Precipitacion_MM)
intervalo_41 <- c(conf$lwr[indice],conf$upr[indice])

#########################################################################
############################## PLAZA_42 #################################
#########################################################################

data_plaza_ <- read.csv("C:/Users/Marcelo Sanchez/OneDrive/Escritorio/Tercer Semestre CIMAT/Computo Estadistico/Proyecto/PLAZA_42.csv")
View(data_plaza_)

plaza_ <- data_plaza_[,3:18]
plaza_$Max_Precipitacion_MM <- data_plaza_$Max_Precipitacion_MM+0.001
plaza_$Media_Precipitacion <- data_plaza_$Media_Precipitacion+0.001

##### Exploración de la variable de respuest
##### ¿La variable sigue una distribución gamma?
fit.gamma <- fitdist(plaza_$Max_Precipitacion_MM,
                     distr = "gamma", method = "mme")

plot.ts(plaza_$Max_Precipitacion_MM)
plot(fit.gamma)

regresion_todos <- glm(Max_Precipitacion_MM ~Media_Precipitacion+ 
                        mes_1
                       +mes_5+mes_9+mes_A,
                       family = Gamma(link='log'),#log
                       data = plaza_)
summary(regresion_todos)

##################### ocupamos la media ############
r_media <- lm(Media_Precipitacion~mes_1+mes_2+mes_3+mes_4+
                mes_5+mes_6+
                mes_7+mes_8+mes_9+
                mes_A+mes_B, data = plaza_)
r_media_pred <- data_plaza_[36:49,7:18]
r_media_pred <- subset(r_media_pred, select = -c(Tendencia) )
r_media_pred
Media_Precipitacion <- predict(r_media,r_media_pred,type="response")
Media_Precipitacion
####################################################

predi_reg <- data_plaza_[36:49,7:18] ### contemplamos los meses
predi_reg <- cbind(predi_reg, Media_Precipitacion)
predi_reg

### Retiramos las columnas(meses) que no son significativos
# el mes que vaya 

predi_reg_t <- subset(predi_reg,select=-c(Tendencia))
predi_reg_t

pr_42 <- predict(regresion_todos,predi_reg_t,type="response")
cbind(predi_reg_t,pr_42)

####################################
## intervalo que vamos a agregar a cada uno de los datos 

Data_P <- cbind(pr_42, predi_reg_t)
names(Data_P) = c('Max_Precipitacion_MM',
                  'mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                  'mes_9',"mes_A",'mes_B','Media_Precipitacion')

pred <- add_pi(Data_P,regresion_todos, alpha = 0.1, 
               names = c("lwr", "upr"))
conf <- add_ci(Data_P, regresion_todos, alpha = 0.1, 
               names = c("lwr", "upr"))
indice <- which.max(Data_P$Max_Precipitacion_MM)
intervalo_42 <- c(conf$lwr[indice],conf$upr[indice])

#########################################################################
############################## PLAZA_43 #################################
#########################################################################

data_plaza_ <- read.csv("C:/Users/Marcelo Sanchez/OneDrive/Escritorio/Tercer Semestre CIMAT/Computo Estadistico/Proyecto/PLAZA_43.csv")
View(data_plaza_)

plaza_ <- data_plaza_[,3:18]
plaza_$Max_Precipitacion_MM <- data_plaza_$Max_Precipitacion_MM+0.001
plaza_$Media_Precipitacion <- data_plaza_$Media_Precipitacion+0.001

##### Exploración de la variable de respuest
##### ¿La variable sigue una distribución gamma?
fit.gamma <- fitdist(plaza_$Max_Precipitacion_MM,
                     distr = "gamma", method = "mme")

plot.ts(plaza_$Max_Precipitacion_MM)
plot(fit.gamma)

regresion_todos <- glm(Max_Precipitacion_MM ~ Media_Precipitacion+
                       mes_4+mes_5+mes_6+mes_7+mes_8+mes_9+mes_A+mes_B,
                       family = Gamma(link='log'),#log
                       data = plaza_)
summary(regresion_todos)

##################### ocupamos la media ############
r_media <- lm(Media_Precipitacion~mes_1+mes_2+mes_3+mes_4+
                mes_5+mes_6+
                mes_7+mes_8+mes_9+
                mes_A+mes_B, data = plaza_)
r_media_pred <- data_plaza_[36:49,7:18]
r_media_pred <- subset(r_media_pred, select = -c(Tendencia) )
r_media_pred
Media_Precipitacion <- predict(r_media,r_media_pred,type="response")
Media_Precipitacion
####################################################

predi_reg <- data_plaza_[36:49,7:18] ### contemplamos los meses
predi_reg <- cbind(predi_reg, Media_Precipitacion)
predi_reg

### Retiramos las columnas(meses) que no son significativos
# el mes que vaya 

predi_reg_t <- subset(predi_reg,select=-c(Tendencia))
predi_reg_t

pr_43 <- predict(regresion_todos,predi_reg_t,type="response")
cbind(predi_reg_t,pr_43)

####################################
## intervalo que vamos a agregar a cada uno de los datos 

Data_P <- cbind(pr_43, predi_reg_t)
names(Data_P) = c('Max_Precipitacion_MM',
                  'mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                  'mes_9',"mes_A",'mes_B','Media_Precipitacion')

pred <- add_pi(Data_P,regresion_todos, alpha = 0.1, 
               names = c("lwr", "upr"))
conf <- add_ci(Data_P, regresion_todos, alpha = 0.1, 
               names = c("lwr", "upr"))
indice <- which.max(Data_P$Max_Precipitacion_MM)
intervalo_43 <- c(conf$lwr[indice],conf$upr[indice])

#########################################################################
############################## PLAZA_44 #################################
#########################################################################

data_plaza_ <- read.csv("C:/Users/Marcelo Sanchez/OneDrive/Escritorio/Tercer Semestre CIMAT/Computo Estadistico/Proyecto/PLAZA_44.csv")
View(data_plaza_)

plaza_ <- data_plaza_[,3:18]
plaza_$Max_Precipitacion_MM <- data_plaza_$Max_Precipitacion_MM+0.001
plaza_$Media_Precipitacion <- data_plaza_$Media_Precipitacion+0.001

##### Exploración de la variable de respuest
##### ¿La variable sigue una distribución gamma?
fit.gamma <- fitdist(plaza_$Max_Precipitacion_MM,
                     distr = "gamma", method = "mme")

plot.ts(plaza_$Max_Precipitacion_MM)
plot(fit.gamma)

regresion_todos <- glm(Max_Precipitacion_MM ~ Media_Precipitacion+
                         mes_4+mes_5,
                       family = Gamma(link='log'),#log
                       data = plaza_)
summary(regresion_todos)

##################### ocupamos la media ############
r_media <- lm(Media_Precipitacion~mes_1+mes_2+mes_3+mes_4+
                mes_5+mes_6+
                mes_7+mes_8+mes_9+
                mes_A+mes_B, data = plaza_)
r_media_pred <- data_plaza_[36:49,7:18]
r_media_pred <- subset(r_media_pred, select = -c(Tendencia) )
r_media_pred
Media_Precipitacion <- predict(r_media,r_media_pred,type="response")
Media_Precipitacion
####################################################

predi_reg <- data_plaza_[36:49,7:18] ### contemplamos los meses
predi_reg <- cbind(predi_reg, Media_Precipitacion)
predi_reg

### Retiramos las columnas(meses) que no son significativos
# el mes que vaya 

predi_reg_t <- subset(predi_reg,select=-c(Tendencia))
predi_reg_t


pr_44 <- predict(regresion_todos,predi_reg_t,type="response")
cbind(predi_reg_t,pr_44)

####################################
## intervalo que vamos a agregar a cada uno de los datos 

Data_P <- cbind(pr_44, predi_reg_t)
names(Data_P) = c('Max_Precipitacion_MM',
                  'mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                  'mes_9',"mes_A",'mes_B','Media_Precipitacion')

pred <- add_pi(Data_P,regresion_todos, alpha = 0.1, 
               names = c("lwr", "upr"))
conf <- add_ci(Data_P, regresion_todos, alpha = 0.1, 
               names = c("lwr", "upr"))
indice <- which.max(Data_P$Max_Precipitacion_MM)
intervalo_44 <- c(conf$lwr[indice],conf$upr[indice])

#########################################################################
############################## PLAZA_45 #################################
#########################################################################

data_plaza_ <- read.csv("C:/Users/Marcelo Sanchez/OneDrive/Escritorio/Tercer Semestre CIMAT/Computo Estadistico/Proyecto/PLAZA_45.csv")
View(data_plaza_)

plaza_ <- data_plaza_[,3:18]
plaza_$Max_Precipitacion_MM <- data_plaza_$Max_Precipitacion_MM+0.001
plaza_$Media_Precipitacion <- data_plaza_$Media_Precipitacion+0.001

##### Exploración de la variable de respuest
##### ¿La variable sigue una distribución gamma?
fit.gamma <- fitdist(plaza_$Max_Precipitacion_MM,
                     distr = "gamma", method = "mme")

plot.ts(plaza_$Max_Precipitacion_MM)
plot(fit.gamma)

regresion_todos <- glm(Max_Precipitacion_MM ~ Tendencia+
                         mes_5+mes_6+mes_7+mes_9+mes_A,
                       family = Gamma(link='log'),#log
                       data = plaza_)
summary(regresion_todos)


predi_reg <- data_plaza_[36:49,7:18] ### contemplamos los meses
predi_reg

#Media_Precipitacion <- Funcion_media(plaza_)
#predi_reg <- cbind(predi_reg, Media_Precipitacion)

predi_reg$Tendencia <- c(72,73,74,75,76,77,78,79,80,81,82,83,84,85)

predi_reg_t <- predi_reg
predi_reg_t

pr_45 <- predict(regresion_todos,predi_reg_t,type="response")
cbind(predi_reg_t,pr_45)

####################################
## intervalo que vamos a agregar a cada uno de los datos 

Data_P <- cbind(pr_45, predi_reg_t)
names(Data_P) = c('Max_Precipitacion_MM','Tendencia',
                  'mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                  'mes_9',"mes_A",'mes_B')

pred <- add_pi(Data_P,regresion_todos, alpha = 0.1, 
               names = c("lwr", "upr"))
conf <- add_ci(Data_P, regresion_todos, alpha = 0.1, 
               names = c("lwr", "upr"))
indice <- which.max(Data_P$Max_Precipitacion_MM)
intervalo_45 <- c(conf$lwr[indice],conf$upr[indice])

#########################################################################
############################## PLAZA_46 #################################
#########################################################################

data_plaza_ <- read.csv("C:/Users/Marcelo Sanchez/OneDrive/Escritorio/Tercer Semestre CIMAT/Computo Estadistico/Proyecto/PLAZA_46.csv")
View(data_plaza_)

plaza_ <- data_plaza_[,3:18]
plaza_$Max_Precipitacion_MM <- data_plaza_$Max_Precipitacion_MM+0.001
plaza_$Media_Precipitacion <- data_plaza_$Media_Precipitacion+0.001

##### Exploración de la variable de respuest
##### ¿La variable sigue una distribución gamma?
fit.gamma <- fitdist(plaza_$Max_Precipitacion_MM,
                     distr = "gamma", method = "mme")

plot.ts(plaza_$Max_Precipitacion_MM)
plot(fit.gamma)

regresion_todos <- glm(Max_Precipitacion_MM ~Media_Precipitacion+ mes_1+mes_2+
                         mes_7,
                       family = Gamma(link='log'),#log
                       data = plaza_)
summary(regresion_todos)

##################### ocupamos la media ############
r_media <- lm(Media_Precipitacion~mes_1+mes_2+mes_3+mes_4+
                mes_5+mes_6+
                mes_7+mes_8+mes_9+
                mes_A+mes_B, data = plaza_)
r_media_pred <- data_plaza_[36:49,7:18]
r_media_pred <- subset(r_media_pred, select = -c(Tendencia) )
r_media_pred
Media_Precipitacion <- predict(r_media,r_media_pred,type="response")
Media_Precipitacion
####################################################

predi_reg <- data_plaza_[36:49,7:18] ### contemplamos los meses
predi_reg <- cbind(predi_reg, Media_Precipitacion)
predi_reg

### Retiramos las columnas(meses) que no son significativos
# el mes que vaya 

predi_reg_t <- subset(predi_reg,select=-c(Tendencia))
predi_reg_t

pr_46 <- predict(regresion_todos,predi_reg_t,type="response")
cbind(predi_reg_t,pr_46)

####################################
## intervalo que vamos a agregar a cada uno de los datos 

Data_P <- cbind(pr_46, predi_reg_t)
names(Data_P) = c('Max_Precipitacion_MM',
                  'mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                  'mes_9',"mes_A",'mes_B','Media_Precipitacion')

pred <- add_pi(Data_P,regresion_todos, alpha = 0.1, 
               names = c("lwr", "upr"))
conf <- add_ci(Data_P, regresion_todos, alpha = 0.1, 
               names = c("lwr", "upr"))
indice <- which.max(Data_P$Max_Precipitacion_MM)
intervalo_46 <- c(conf$lwr[indice],conf$upr[indice])

#########################################################################
############################## PLAZA_47 #################################
#########################################################################

data_plaza_ <- read.csv("C:/Users/Marcelo Sanchez/OneDrive/Escritorio/Tercer Semestre CIMAT/Computo Estadistico/Proyecto/PLAZA_47.csv")
View(data_plaza_)

plaza_ <- data_plaza_[,3:18]
plaza_$Max_Precipitacion_MM <- data_plaza_$Max_Precipitacion_MM+0.001
plaza_$Media_Precipitacion <- data_plaza_$Media_Precipitacion+0.001

##### Exploración de la variable de respuest
##### ¿La variable sigue una distribución gamma?
fit.gamma <- fitdist(plaza_$Max_Precipitacion_MM,
                     distr = "gamma", method = "mme")

plot.ts(plaza_$Max_Precipitacion_MM)
plot(fit.gamma)

regresion_todos <- glm(Max_Precipitacion_MM ~Media_Precipitacion+
                         mes_5+
                         mes_8+mes_9,
                       family = Gamma(link='log'),#log
                       data = plaza_)
summary(regresion_todos)

##################### ocupamos la media ############
r_media <- lm(Media_Precipitacion~mes_1+mes_2+mes_3+mes_4+
                mes_5+mes_6+
                mes_7+mes_8+mes_9+
                mes_A+mes_B, data = plaza_)
r_media_pred <- data_plaza_[36:49,7:18]
r_media_pred <- subset(r_media_pred, select = -c(Tendencia) )
r_media_pred
Media_Precipitacion <- predict(r_media,r_media_pred,type="response")
Media_Precipitacion
####################################################

predi_reg <- data_plaza_[36:49,7:18] ### contemplamos los meses
predi_reg <- cbind(predi_reg, Media_Precipitacion)
predi_reg

### Retiramos las columnas(meses) que no son significativos
# el mes que vaya 

predi_reg_t <- subset(predi_reg,select=-c(Tendencia))
predi_reg_t

pr_47 <- predict(regresion_todos,predi_reg_t,type="response")
cbind(predi_reg_t,pr_47)

####################################
## intervalo que vamos a agregar a cada uno de los datos 

Data_P <- cbind(pr_47, predi_reg_t)
names(Data_P) = c('Max_Precipitacion_MM',
                  'mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                  'mes_9',"mes_A",'mes_B','Media_Precipitacion')

pred <- add_pi(Data_P,regresion_todos, alpha = 0.1, 
               names = c("lwr", "upr"))
conf <- add_ci(Data_P, regresion_todos, alpha = 0.1, 
               names = c("lwr", "upr"))
indice <- which.max(Data_P$Max_Precipitacion_MM)
intervalo_47 <- c(conf$lwr[indice],conf$upr[indice])

#########################################################################
############################## PLAZA_48 #################################
#########################################################################

data_plaza_ <- read.csv("C:/Users/Marcelo Sanchez/OneDrive/Escritorio/Tercer Semestre CIMAT/Computo Estadistico/Proyecto/PLAZA_48.csv")
View(data_plaza_)

plaza_ <- data_plaza_[,3:18]
plaza_$Max_Precipitacion_MM <- data_plaza_$Max_Precipitacion_MM+0.001
plaza_$Media_Precipitacion <- data_plaza_$Media_Precipitacion+0.001

##### Exploración de la variable de respuest
##### ¿La variable sigue una distribución gamma?
fit.gamma <- fitdist(plaza_$Max_Precipitacion_MM,
                     distr = "gamma", method = "mme")

plot.ts(plaza_$Max_Precipitacion_MM)
plot(fit.gamma)

regresion_todos <- glm(Max_Precipitacion_MM ~Media_Precipitacion+
                         mes_3+mes_4+
                         mes_7+mes_8+mes_9,
                       family = Gamma(link='log'),#log
                       data = plaza_)
summary(regresion_todos)

##################### ocupamos la media ############
r_media <- lm(Media_Precipitacion~mes_1+mes_2+mes_3+mes_4+
                mes_5+mes_6+
                mes_7+mes_8+mes_9+
                mes_A+mes_B, data = plaza_)
r_media_pred <- data_plaza_[36:49,7:18]
r_media_pred <- subset(r_media_pred, select = -c(Tendencia) )
r_media_pred
Media_Precipitacion <- predict(r_media,r_media_pred,type="response")
Media_Precipitacion
####################################################

predi_reg <- data_plaza_[36:49,7:18] ### contemplamos los meses
predi_reg <- cbind(predi_reg, Media_Precipitacion)
predi_reg

### Retiramos las columnas(meses) que no son significativos
# el mes que vaya 

predi_reg_t <- subset(predi_reg,select=-c(Tendencia))
predi_reg_t

pr_48 <- predict(regresion_todos,predi_reg_t,type="response")
cbind(predi_reg_t,pr_48)

####################################
## intervalo que vamos a agregar a cada uno de los datos 

Data_P <- cbind(pr_48, predi_reg_t)
names(Data_P) = c('Max_Precipitacion_MM',
                  'mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                  'mes_9',"mes_A",'mes_B','Media_Precipitacion')

pred <- add_pi(Data_P,regresion_todos, alpha = 0.1, 
               names = c("lwr", "upr"))
conf <- add_ci(Data_P, regresion_todos, alpha = 0.1, 
               names = c("lwr", "upr"))
indice <- which.max(Data_P$Max_Precipitacion_MM)
intervalo_48 <- c(conf$lwr[indice],conf$upr[indice])

#########################################################################
############################## PLAZA_49 #################################
#########################################################################

data_plaza_ <- read.csv("C:/Users/Marcelo Sanchez/OneDrive/Escritorio/Tercer Semestre CIMAT/Computo Estadistico/Proyecto/PLAZA_49.csv")
View(data_plaza_)

plaza_ <- data_plaza_[,3:18]
plaza_$Max_Precipitacion_MM <- data_plaza_$Max_Precipitacion_MM+0.001
plaza_$Media_Precipitacion <- data_plaza_$Media_Precipitacion+0.001

##### Exploración de la variable de respuest
##### ¿La variable sigue una distribución gamma?
fit.gamma <- fitdist(plaza_$Max_Precipitacion_MM,
                     distr = "gamma", method = "mme")

plot.ts(plaza_$Max_Precipitacion_MM)
plot(fit.gamma)

regresion_todos <- glm(Max_Precipitacion_MM ~Media_Precipitacion+
                         mes_6+
                         mes_7+mes_8+mes_A,
                       family = Gamma(link='log'),#log
                       data = plaza_)
summary(regresion_todos)

##################### ocupamos la media ############
r_media <- lm(Media_Precipitacion~mes_1+mes_2+mes_3+mes_4+
                mes_5+mes_6+
                mes_7+mes_8+mes_9+
                mes_A+mes_B, data = plaza_)
r_media_pred <- data_plaza_[36:49,7:18]
r_media_pred <- subset(r_media_pred, select = -c(Tendencia) )
r_media_pred
Media_Precipitacion <- predict(r_media,r_media_pred,type="response")
Media_Precipitacion
####################################################

predi_reg <- data_plaza_[36:49,7:18] ### contemplamos los meses
predi_reg <- cbind(predi_reg, Media_Precipitacion)
predi_reg

### Retiramos las columnas(meses) que no son significativos
# el mes que vaya 

predi_reg_t <- subset(predi_reg,select=-c(Tendencia))
predi_reg_t

pr_49 <- predict(regresion_todos,predi_reg_t,type="response")
cbind(predi_reg_t,pr_49)

####################################
## intervalo que vamos a agregar a cada uno de los datos 

Data_P <- cbind(pr_49, predi_reg_t)
names(Data_P) = c('Max_Precipitacion_MM',
                  'mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                  'mes_9',"mes_A",'mes_B','Media_Precipitacion')

pred <- add_pi(Data_P,regresion_todos, alpha = 0.1, 
               names = c("lwr", "upr"))
conf <- add_ci(Data_P, regresion_todos, alpha = 0.1, 
               names = c("lwr", "upr"))
indice <- which.max(Data_P$Max_Precipitacion_MM)
intervalo_49 <- c(conf$lwr[indice],conf$upr[indice])

#########################################################################
############################## PLAZA_50 #################################
#########################################################################

data_plaza_ <- read.csv("C:/Users/Marcelo Sanchez/OneDrive/Escritorio/Tercer Semestre CIMAT/Computo Estadistico/Proyecto/PLAZA_50.csv")
View(data_plaza_)

plaza_ <- data_plaza_[,3:18]
plaza_$Max_Precipitacion_MM <- data_plaza_$Max_Precipitacion_MM+0.001
plaza_$Media_Precipitacion <- data_plaza_$Media_Precipitacion+0.001

##### Exploración de la variable de respuest
##### ¿La variable sigue una distribución gamma?
fit.gamma <- fitdist(plaza_$Max_Precipitacion_MM,
                     distr = "gamma", method = "mme")

plot.ts(plaza_$Max_Precipitacion_MM)
plot(fit.gamma)

regresion_todos <- glm(Max_Precipitacion_MM ~Media_Precipitacion+
                         mes_2+mes_4,
                       family = Gamma(link='log'),#log
                       data = plaza_)
summary(regresion_todos)

##################### ocupamos la media ############
r_media <- lm(Media_Precipitacion~mes_1+mes_2+mes_3+mes_4+
                mes_5+mes_6+
                mes_7+mes_8+mes_9+
                mes_A+mes_B, data = plaza_)
r_media_pred <- data_plaza_[36:49,7:18]
r_media_pred <- subset(r_media_pred, select = -c(Tendencia) )
r_media_pred
Media_Precipitacion <- predict(r_media,r_media_pred,type="response")
Media_Precipitacion
####################################################

predi_reg <- data_plaza_[36:49,7:18] ### contemplamos los meses
predi_reg <- cbind(predi_reg, Media_Precipitacion)
predi_reg

### Retiramos las columnas(meses) que no son significativos
# el mes que vaya 

predi_reg_t <- subset(predi_reg,select=-c(Tendencia))
predi_reg_t

pr_50 <- predict(regresion_todos,predi_reg_t,type="response")
cbind(predi_reg_t,pr_50)

####################################
## intervalo que vamos a agregar a cada uno de los datos 

Data_P <- cbind(pr_50, predi_reg_t)
names(Data_P) = c('Max_Precipitacion_MM',
                  'mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                  'mes_9',"mes_A",'mes_B','Media_Precipitacion')

pred <- add_pi(Data_P,regresion_todos, alpha = 0.1, 
               names = c("lwr", "upr"))
conf <- add_ci(Data_P, regresion_todos, alpha = 0.1, 
               names = c("lwr", "upr"))
indice <- which.max(Data_P$Max_Precipitacion_MM)
intervalo_50 <- c(conf$lwr[indice],conf$upr[indice])

#########################################################################
############################## PLAZA_51 #################################
#########################################################################

data_plaza_ <- read.csv("C:/Users/Marcelo Sanchez/OneDrive/Escritorio/Tercer Semestre CIMAT/Computo Estadistico/Proyecto/PLAZA_51.csv")
View(data_plaza_)

plaza_ <- data_plaza_[,3:18]
plaza_$Max_Precipitacion_MM <- data_plaza_$Max_Precipitacion_MM+0.001
plaza_$Media_Precipitacion <- data_plaza_$Media_Precipitacion+0.001

##### Exploración de la variable de respuest
##### ¿La variable sigue una distribución gamma?
fit.gamma <- fitdist(plaza_$Max_Precipitacion_MM,
                     distr = "gamma", method = "mme")

plot.ts(plaza_$Max_Precipitacion_MM)
plot(fit.gamma)

regresion_todos <- glm(Max_Precipitacion_MM ~Media_Precipitacion+ 
                       mes_2+mes_4+
                         mes_5+mes_6+
                         mes_7+mes_8+mes_9+mes_A+mes_B,
                       family = Gamma(link='log'),#log
                       data = plaza_)
summary(regresion_todos)

##################### ocupamos la media ############
r_media <- lm(Media_Precipitacion~mes_1+mes_2+mes_3+mes_4+
                mes_5+mes_6+
                mes_7+mes_8+mes_9+
                mes_A+mes_B, data = plaza_)
r_media_pred <- data_plaza_[36:49,7:18]
r_media_pred <- subset(r_media_pred, select = -c(Tendencia) )
r_media_pred
Media_Precipitacion <- predict(r_media,r_media_pred,type="response")
Media_Precipitacion
####################################################

predi_reg <- data_plaza_[36:49,7:18] ### contemplamos los meses
predi_reg <- cbind(predi_reg, Media_Precipitacion)
predi_reg

### Retiramos las columnas(meses) que no son significativos
# el mes que vaya 

predi_reg_t <- subset(predi_reg,select=-c(Tendencia))
predi_reg_t

pr_51 <- predict(regresion_todos,predi_reg_t,type="response")
cbind(predi_reg_t,pr_51)

####################################
## intervalo que vamos a agregar a cada uno de los datos 

Data_P <- cbind(pr_51, predi_reg_t)
names(Data_P) = c('Max_Precipitacion_MM',
                  'mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                  'mes_9',"mes_A",'mes_B','Media_Precipitacion')

pred <- add_pi(Data_P,regresion_todos, alpha = 0.1, 
               names = c("lwr", "upr"))
conf <- add_ci(Data_P, regresion_todos, alpha = 0.1, 
               names = c("lwr", "upr"))
indice <- which.max(Data_P$Max_Precipitacion_MM)
intervalo_51 <- c(conf$lwr[indice],conf$upr[indice])


#########################################################################
############################## PLAZA_52 #################################
#########################################################################

data_plaza_ <- read.csv("C:/Users/Marcelo Sanchez/OneDrive/Escritorio/Tercer Semestre CIMAT/Computo Estadistico/Proyecto/PLAZA_52.csv")
View(data_plaza_)

plaza_ <- data_plaza_[,3:18]
plaza_$Max_Precipitacion_MM <- data_plaza_$Max_Precipitacion_MM+0.001
plaza_$Media_Precipitacion <- data_plaza_$Media_Precipitacion+0.001

##### Exploración de la variable de respuest
##### ¿La variable sigue una distribución gamma?
fit.gamma <- fitdist(plaza_$Max_Precipitacion_MM,
                     distr = "gamma", method = "mme")

plot.ts(plaza_$Max_Precipitacion_MM)
plot(fit.gamma)

regresion_todos <- glm(Max_Precipitacion_MM ~ Media_Precipitacion+
                         mes_1+mes_3+mes_4+
                         mes_5,
                       family = Gamma(link='inverse'),#log
                       data = plaza_)
summary(regresion_todos)

##################### ocupamos la media ############
r_media <- lm(Media_Precipitacion~mes_1+mes_2+mes_3+mes_4+
                mes_5+mes_6+
                mes_7+mes_8+mes_9+
                mes_A+mes_B, data = plaza_)
r_media_pred <- data_plaza_[36:49,7:18]
r_media_pred <- subset(r_media_pred, select = -c(Tendencia) )
r_media_pred
Media_Precipitacion <- predict(r_media,r_media_pred,type="response")
Media_Precipitacion
####################################################

predi_reg <- data_plaza_[36:49,7:18] ### contemplamos los meses
predi_reg <- cbind(predi_reg, Media_Precipitacion)
predi_reg

### Retiramos las columnas(meses) que no son significativos
# el mes que vaya 

predi_reg_t <- subset(predi_reg,select=-c(Tendencia))
predi_reg_t

pr_52 <- predict(regresion_todos,predi_reg_t,type="response")
cbind(predi_reg_t,pr_52)

####################################
## intervalo que vamos a agregar a cada uno de los datos 

Data_P <- cbind(pr_52, predi_reg_t)
names(Data_P) = c('Max_Precipitacion_MM',
                  'mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                  'mes_9',"mes_A",'mes_B','Media_Precipitacion')

pred <- add_pi(Data_P,regresion_todos, alpha = 0.1, 
               names = c("lwr", "upr"))
conf <- add_ci(Data_P, regresion_todos, alpha = 0.1, 
               names = c("lwr", "upr"))
indice <- which.max(Data_P$Max_Precipitacion_MM)
intervalo_52 <- c(conf$lwr[indice],conf$upr[indice])

#########################################################################
############################## PLAZA_53 #################################
#########################################################################

data_plaza_ <- read.csv("C:/Users/Marcelo Sanchez/OneDrive/Escritorio/Tercer Semestre CIMAT/Computo Estadistico/Proyecto/PLAZA_53.csv")
View(data_plaza_)

plaza_ <- data_plaza_[,3:18]
plaza_$Max_Precipitacion_MM <- data_plaza_$Max_Precipitacion_MM+0.001
plaza_$Media_Precipitacion <- data_plaza_$Media_Precipitacion+0.001

##### Exploración de la variable de respuest
##### ¿La variable sigue una distribución gamma?
fit.gamma <- fitdist(plaza_$Max_Precipitacion_MM,
                     distr = "gamma", method = "mme")

plot.ts(plaza_$Max_Precipitacion_MM)
plot(fit.gamma)

regresion_todos <- glm(Max_Precipitacion_MM ~ Media_Precipitacion+
                      mes_6+mes_8+mes_9+
                       mes_A+mes_B,
                       family = Gamma(link='log'),#log
                       data = plaza_)
summary(regresion_todos)

##################### ocupamos la media ############
r_media <- lm(Media_Precipitacion~mes_1+mes_2+mes_3+mes_4+
                mes_5+mes_6+
                mes_7+mes_8+mes_9+
                mes_A+mes_B, data = plaza_)
r_media_pred <- data_plaza_[36:49,7:18]
r_media_pred <- subset(r_media_pred, select = -c(Tendencia) )
r_media_pred
Media_Precipitacion <- predict(r_media,r_media_pred,type="response")
Media_Precipitacion
####################################################

predi_reg <- data_plaza_[36:49,7:18] ### contemplamos los meses
predi_reg <- cbind(predi_reg, Media_Precipitacion)
predi_reg

### Retiramos las columnas(meses) que no son significativos
# el mes que vaya 

predi_reg_t <- subset(predi_reg,select=-c(Tendencia))
predi_reg_t

pr_53 <- predict(regresion_todos,predi_reg_t,type="response")
cbind(predi_reg_t,pr_53)

####################################
## intervalo que vamos a agregar a cada uno de los datos 

Data_P <- cbind(pr_53, predi_reg_t)
names(Data_P) = c('Max_Precipitacion_MM',
                  'mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                  'mes_9',"mes_A",'mes_B','Media_Precipitacion')

pred <- add_pi(Data_P,regresion_todos, alpha = 0.1, 
               names = c("lwr", "upr"))
conf <- add_ci(Data_P, regresion_todos, alpha = 0.1, 
               names = c("lwr", "upr"))
indice <- which.max(Data_P$Max_Precipitacion_MM)
intervalo_53 <- c(conf$lwr[indice],conf$upr[indice])

#########################################################################
############################## PLAZA_54 #################################
#########################################################################

data_plaza_ <- read.csv("C:/Users/Marcelo Sanchez/OneDrive/Escritorio/Tercer Semestre CIMAT/Computo Estadistico/Proyecto/PLAZA_54.csv")
View(data_plaza_)

plaza_ <- data_plaza_[,3:18]
plaza_$Max_Precipitacion_MM <- data_plaza_$Max_Precipitacion_MM+0.001
plaza_$Media_Precipitacion <- data_plaza_$Media_Precipitacion+0.001

##### Exploración de la variable de respuest
##### ¿La variable sigue una distribución gamma?
fit.gamma <- fitdist(plaza_$Max_Precipitacion_MM,
                     distr = "gamma", method = "mme")

plot.ts(plaza_$Max_Precipitacion_MM)
plot(fit.gamma)

regresion_todos <- glm(Max_Precipitacion_MM ~ 
                         mes_5+mes_6+mes_7+mes_8+mes_9+
                         mes_A,
                       family = Gamma(link='log'),#log
                       data = plaza_)
summary(regresion_todos)


predi_reg <- data_plaza_[36:49,7:18] ### contemplamos los meses
predi_reg

#Media_Precipitacion <- Funcion_media(plaza_)
#predi_reg <- cbind(predi_reg, Media_Precipitacion)

#predi_reg$Tendencia <- c(72,73,74,75,76,77,78,79,80,81,82,83,84,85)

predi_reg_t <- subset(predi_reg,select=-c(Tendencia))
predi_reg_t

pr_54 <- predict(regresion_todos,predi_reg_t,type="response")
cbind(predi_reg_t,pr_54)

####################################
## intervalo que vamos a agregar a cada uno de los datos 

Data_P <- cbind(pr_54, predi_reg_t)
names(Data_P) = c('Max_Precipitacion_MM',
                  'mes_1',"mes_2",'mes_3',"mes_4",'mes_5',"mes_6",'mes_7',"mes_8",
                  'mes_9',"mes_A",'mes_B')

pred <- add_pi(Data_P,regresion_todos, alpha = 0.1, 
               names = c("lwr", "upr"))
conf <- add_ci(Data_P, regresion_todos, alpha = 0.1, 
               names = c("lwr", "upr"))
indice <- which.max(Data_P$Max_Precipitacion_MM)
intervalo_54 <- c(conf$lwr[indice],conf$upr[indice])
#######################################################################
#######################################################################

#### En esta ultima seccion vamos a organizar nuestros datos
####  de forma bonita.


#########################################################################
############################## PLAZA_28 #################################
#########################################################################

#data_plaza_ <- read_csv("C:/Users/KM Reyes Maya/Downloads/Tercer semestre/Computo Estadistico/Proyecto/Datos_by_Plaza/PLAZA_01.csv")
data_plaza_ <- read.csv("C:/Users/Marcelo Sanchez/OneDrive/Escritorio/Tercer Semestre CIMAT/Computo Estadistico/Proyecto/PLAZA_28.csv")
#View(data_plaza_)

data_plaza_[1,5]

l_28<-l16<-l17<-l18<-l19<-l20<-l20<-l21<-c()
for(i in 1:length(data_plaza_$Ano)) {
  #anio <- 2016
  if(i == 1){
    l_28 <- rbind(l_28, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2016){
    l16 <- rbind(l16, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2017){
    l17 <- rbind(l17, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2018){
    l18 <- rbind(l18, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2019){
    l19 <- rbind(l19, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2020){
    l20 <- rbind(l20, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2021){
    l21 <- rbind(l21, data_plaza_[i,5])
  }
}
l_28 <- cbind(l_28,max(l16),max(l17),max(l18),max(l19),max(l20),max(l21))
l_28

#########################################################################
############################## PLAZA_29 #################################
#########################################################################

#data_plaza_ <- read_csv("C:/Users/KM Reyes Maya/Downloads/Tercer semestre/Computo Estadistico/Proyecto/Datos_by_Plaza/PLAZA_01.csv")
data_plaza_ <- read.csv("C:/Users/Marcelo Sanchez/OneDrive/Escritorio/Tercer Semestre CIMAT/Computo Estadistico/Proyecto/PLAZA_29.csv")
#View(data_plaza_)

data_plaza_[1,5]

l_29<-l16<-l17<-l18<-l19<-l20<-l20<-l21<-c()
for(i in 1:length(data_plaza_$Ano)) {
  #anio <- 2016
  if(i == 1){
    l_29 <- rbind(l_29, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2016){
    l16 <- rbind(l16, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2017){
    l17 <- rbind(l17, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2018){
    l18 <- rbind(l18, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2019){
    l19 <- rbind(l19, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2020){
    l20 <- rbind(l20, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2021){
    l21 <- rbind(l21, data_plaza_[i,5])
  }
}
l_29 <- cbind(l_29,max(l16),max(l17),max(l18),max(l19),max(l20),max(l21))
l_29
#########################################################################
############################## PLAZA_30 #################################
#########################################################################

#data_plaza_ <- read_csv("C:/Users/KM Reyes Maya/Downloads/Tercer semestre/Computo Estadistico/Proyecto/Datos_by_Plaza/PLAZA_01.csv")
data_plaza_ <- read.csv("C:/Users/Marcelo Sanchez/OneDrive/Escritorio/Tercer Semestre CIMAT/Computo Estadistico/Proyecto/PLAZA_30.csv")
#View(data_plaza_)

data_plaza_[1,5]

l_30<-l16<-l17<-l18<-l19<-l20<-l20<-l21<-c()
for(i in 1:length(data_plaza_$Ano)) {
  #anio <- 2016
  if(i == 1){
    l_30 <- rbind(l_30, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2016){
    l16 <- rbind(l16, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2017){
    l17 <- rbind(l17, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2018){
    l18 <- rbind(l18, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2019){
    l19 <- rbind(l19, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2020){
    l20 <- rbind(l20, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2021){
    l21 <- rbind(l21, data_plaza_[i,5])
  }
}
l_30 <- cbind(l_30,max(l16),max(l17),max(l18),max(l19),max(l20),max(l21))
l_30

#########################################################################
############################## PLAZA_31 #################################
#########################################################################

#data_plaza_ <- read_csv("C:/Users/KM Reyes Maya/Downloads/Tercer semestre/Computo Estadistico/Proyecto/Datos_by_Plaza/PLAZA_01.csv")
data_plaza_ <- read.csv("C:/Users/Marcelo Sanchez/OneDrive/Escritorio/Tercer Semestre CIMAT/Computo Estadistico/Proyecto/PLAZA_31.csv")
#View(data_plaza_)

data_plaza_[1,5]

l_31<-l16<-l17<-l18<-l19<-l20<-l20<-l21<-c()
for(i in 1:length(data_plaza_$Ano)) {
  #anio <- 2016
  if(i == 1){
    l_31 <- rbind(l_31, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2016){
    l16 <- rbind(l16, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2017){
    l17 <- rbind(l17, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2018){
    l18 <- rbind(l18, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2019){
    l19 <- rbind(l19, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2020){
    l20 <- rbind(l20, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2021){
    l21 <- rbind(l21, data_plaza_[i,5])
  }
}
l_31<- cbind(l_31,max(l16),max(l17),max(l18),max(l19),max(l20),max(l21))

#########################################################################
############################## PLAZA_32 #################################
#########################################################################

#data_plaza_ <- read_csv("C:/Users/KM Reyes Maya/Downloads/Tercer semestre/Computo Estadistico/Proyecto/Datos_by_Plaza/PLAZA_01.csv")
data_plaza_ <- read.csv("C:/Users/Marcelo Sanchez/OneDrive/Escritorio/Tercer Semestre CIMAT/Computo Estadistico/Proyecto/PLAZA_32.csv")
#View(data_plaza_)

data_plaza_[1,5]

l_32<-l16<-l17<-l18<-l19<-l20<-l20<-l21<-c()
for(i in 1:length(data_plaza_$Ano)) {
  #anio <- 2016
  if(i == 1){
    l_32 <- rbind(l_32, data_plaza_[1,5])
  } else if( data_plaza_[i,2] == 2016){
    l16 <- rbind(l16, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2017){
    l17 <- rbind(l17, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2018){
    l18 <- rbind(l18, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2019){
    l19 <- rbind(l19, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2020){
    l20 <- rbind(l20, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2021){
    l21 <- rbind(l21, data_plaza_[i,5])
  }
}
l_32<- cbind(l_32,max(l16),max(l17),max(l18),max(l19),max(l20),max(l21))
l_32
#########################################################################
############################## PLAZA_33 #################################
#########################################################################

#data_plaza_ <- read_csv("C:/Users/KM Reyes Maya/Downloads/Tercer semestre/Computo Estadistico/Proyecto/Datos_by_Plaza/PLAZA_01.csv")
data_plaza_ <- read.csv("C:/Users/Marcelo Sanchez/OneDrive/Escritorio/Tercer Semestre CIMAT/Computo Estadistico/Proyecto/PLAZA_33.csv")
#View(data_plaza_)

data_plaza_[1,5]

l_33<-l16<-l17<-l18<-l19<-l20<-l20<-l21<-c()
for(i in 1:length(data_plaza_$Ano)) {
  #anio <- 2016
  if(i == 1){
    l_33 <- rbind(l_33, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2016){
    l16 <- rbind(l16, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2017){
    l17 <- rbind(l17, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2018){
    l18 <- rbind(l18, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2019){
    l19 <- rbind(l19, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2020){
    l20 <- rbind(l20, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2021){
    l21 <- rbind(l21, data_plaza_[i,5])
  }
}
l_33<- cbind(l_33,max(l16),max(l17),max(l18),max(l19),max(l20),max(l21))


#########################################################################
############################## PLAZA_34 #################################
#########################################################################

#data_plaza_ <- read_csv("C:/Users/KM Reyes Maya/Downloads/Tercer semestre/Computo Estadistico/Proyecto/Datos_by_Plaza/PLAZA_01.csv")
data_plaza_ <- read.csv("C:/Users/Marcelo Sanchez/OneDrive/Escritorio/Tercer Semestre CIMAT/Computo Estadistico/Proyecto/PLAZA_34.csv")
#View(data_plaza_)

data_plaza_[1,5]

l_34<-l16<-l17<-l18<-l19<-l20<-l20<-l21<-c()
for(i in 1:length(data_plaza_$Ano)) {
  #anio <- 2016
  if(i == 1){
    l_34 <- rbind(l_34, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2016){
    l16 <- rbind(l16, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2017){
    l17 <- rbind(l17, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2018){
    l18 <- rbind(l18, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2019){
    l19 <- rbind(l19, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2020){
    l20 <- rbind(l20, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2021){
    l21 <- rbind(l21, data_plaza_[i,5])
  }
}
l_34<- cbind(l_34,max(l16),max(l17),max(l18),max(l19),max(l20),max(l21))


#########################################################################
############################## PLAZA_35 #################################
#########################################################################

#data_plaza_ <- read_csv("C:/Users/KM Reyes Maya/Downloads/Tercer semestre/Computo Estadistico/Proyecto/Datos_by_Plaza/PLAZA_01.csv")
data_plaza_ <- read.csv("C:/Users/Marcelo Sanchez/OneDrive/Escritorio/Tercer Semestre CIMAT/Computo Estadistico/Proyecto/PLAZA_35.csv")
#View(data_plaza_)

data_plaza_[1,5]

l_35<-l16<-l17<-l18<-l19<-l20<-l20<-l21<-c()
for(i in 1:length(data_plaza_$Ano)) {
  #anio <- 2016
  if(i == 1){
    l_35 <- rbind(l_35, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2016){
    l16 <- rbind(l16, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2017){
    l17 <- rbind(l17, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2018){
    l18 <- rbind(l18, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2019){
    l19 <- rbind(l19, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2020){
    l20 <- rbind(l20, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2021){
    l21 <- rbind(l21, data_plaza_[i,5])
  }
}
l_35<- cbind(l_35,max(l16),max(l17),max(l18),max(l19),max(l20),max(l21))


#########################################################################
############################## PLAZA_36 #################################
#########################################################################

#data_plaza_ <- read_csv("C:/Users/KM Reyes Maya/Downloads/Tercer semestre/Computo Estadistico/Proyecto/Datos_by_Plaza/PLAZA_01.csv")
data_plaza_ <- read.csv("C:/Users/Marcelo Sanchez/OneDrive/Escritorio/Tercer Semestre CIMAT/Computo Estadistico/Proyecto/PLAZA_36.csv")
#View(data_plaza_)

data_plaza_[1,5]

l_36<-l16<-l17<-l18<-l19<-l20<-l20<-l21<-c()
for(i in 1:length(data_plaza_$Ano)) {
  #anio <- 2016
  if(i == 1){
    l_36 <- rbind(l_36, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2016){
    l16 <- rbind(l16, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2017){
    l17 <- rbind(l17, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2018){
    l18 <- rbind(l18, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2019){
    l19 <- rbind(l19, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2020){
    l20 <- rbind(l20, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2021){
    l21 <- rbind(l21, data_plaza_[i,5])
  }
}
l_36<- cbind(l_36,max(l16),max(l17),max(l18),max(l19),max(l20),max(l21))


#########################################################################
############################## PLAZA_37 #################################
#########################################################################

#data_plaza_ <- read_csv("C:/Users/KM Reyes Maya/Downloads/Tercer semestre/Computo Estadistico/Proyecto/Datos_by_Plaza/PLAZA_01.csv")
data_plaza_ <- read.csv("C:/Users/Marcelo Sanchez/OneDrive/Escritorio/Tercer Semestre CIMAT/Computo Estadistico/Proyecto/PLAZA_37.csv")
#View(data_plaza_)

data_plaza_[1,5]

l_37<-l16<-l17<-l18<-l19<-l20<-l20<-l21<-c()
for(i in 1:length(data_plaza_$Ano)) {
  #anio <- 2016
  if(i == 1){
    l_37 <- rbind(l_37, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2016){
    l16 <- rbind(l16, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2017){
    l17 <- rbind(l17, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2018){
    l18 <- rbind(l18, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2019){
    l19 <- rbind(l19, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2020){
    l20 <- rbind(l20, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2021){
    l21 <- rbind(l21, data_plaza_[i,5])
  }
}
l_37<- cbind(l_37,max(l16),max(l17),max(l18),max(l19),max(l20),max(l21))


#########################################################################
############################## PLAZA_38 #################################
#########################################################################

#data_plaza_ <- read_csv("C:/Users/KM Reyes Maya/Downloads/Tercer semestre/Computo Estadistico/Proyecto/Datos_by_Plaza/PLAZA_01.csv")
data_plaza_ <- read.csv("C:/Users/Marcelo Sanchez/OneDrive/Escritorio/Tercer Semestre CIMAT/Computo Estadistico/Proyecto/PLAZA_38.csv")
#View(data_plaza_)

data_plaza_[1,5]

l_38<-l16<-l17<-l18<-l19<-l20<-l20<-l21<-c()
for(i in 1:length(data_plaza_$Ano)) {
  #anio <- 2016
  if(i == 1){
    l_38 <- rbind(l_38, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2016){
    l16 <- rbind(l16, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2017){
    l17 <- rbind(l17, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2018){
    l18 <- rbind(l18, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2019){
    l19 <- rbind(l19, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2020){
    l20 <- rbind(l20, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2021){
    l21 <- rbind(l21, data_plaza_[i,5])
  }
}
l_38<- cbind(l_38,max(l16),max(l17),max(l18),max(l19),max(l20),max(l21))


#########################################################################
############################## PLAZA_39 #################################
#########################################################################

#data_plaza_ <- read_csv("C:/Users/KM Reyes Maya/Downloads/Tercer semestre/Computo Estadistico/Proyecto/Datos_by_Plaza/PLAZA_01.csv")
data_plaza_ <- read.csv("C:/Users/Marcelo Sanchez/OneDrive/Escritorio/Tercer Semestre CIMAT/Computo Estadistico/Proyecto/PLAZA_39.csv")
#View(data_plaza_)

data_plaza_[1,5]

l_39<-l16<-l17<-l18<-l19<-l20<-l20<-l21<-c()
for(i in 1:length(data_plaza_$Ano)) {
  #anio <- 2016
  if(i == 1){
    l_39 <- rbind(l_39, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2016){
    l16 <- rbind(l16, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2017){
    l17 <- rbind(l17, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2018){
    l18 <- rbind(l18, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2019){
    l19 <- rbind(l19, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2020){
    l20 <- rbind(l20, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2021){
    l21 <- rbind(l21, data_plaza_[i,5])
  }
}
l_39<- cbind(l_39,max(l16),max(l17),max(l18),max(l19),max(l20),max(l21))

#########################################################################
############################## PLAZA_40 #################################
#########################################################################

#data_plaza_ <- read_csv("C:/Users/KM Reyes Maya/Downloads/Tercer semestre/Computo Estadistico/Proyecto/Datos_by_Plaza/PLAZA_01.csv")
data_plaza_ <- read.csv("C:/Users/Marcelo Sanchez/OneDrive/Escritorio/Tercer Semestre CIMAT/Computo Estadistico/Proyecto/PLAZA_40.csv")
#View(data_plaza_)

data_plaza_[1,5]

l_40<-l16<-l17<-l18<-l19<-l20<-l20<-l21<-c()
for(i in 1:length(data_plaza_$Ano)) {
  #anio <- 2016
  if(i == 1){
    l_40 <- rbind(l_40, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2016){
    l16 <- rbind(l16, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2017){
    l17 <- rbind(l17, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2018){
    l18 <- rbind(l18, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2019){
    l19 <- rbind(l19, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2020){
    l20 <- rbind(l20, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2021){
    l21 <- rbind(l21, data_plaza_[i,5])
  }
}
l_40<- cbind(l_40,max(l16),max(l17),max(l18),max(l19),max(l20),max(l21))


#########################################################################
############################## PLAZA_41 #################################
#########################################################################

#data_plaza_ <- read_csv("C:/Users/KM Reyes Maya/Downloads/Tercer semestre/Computo Estadistico/Proyecto/Datos_by_Plaza/PLAZA_01.csv")
data_plaza_ <- read.csv("C:/Users/Marcelo Sanchez/OneDrive/Escritorio/Tercer Semestre CIMAT/Computo Estadistico/Proyecto/PLAZA_41.csv")
#View(data_plaza_)

data_plaza_[1,5]

l_41<-l16<-l17<-l18<-l19<-l20<-l20<-l21<-c()
for(i in 1:length(data_plaza_$Ano)) {
  #anio <- 2016
  if(i == 1){
    l_41 <- rbind(l_41, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2016){
    l16 <- rbind(l16, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2017){
    l17 <- rbind(l17, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2018){
    l18 <- rbind(l18, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2019){
    l19 <- rbind(l19, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2020){
    l20 <- rbind(l20, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2021){
    l21 <- rbind(l21, data_plaza_[i,5])
  }
}
l_41<- cbind(l_41,max(l16),max(l17),max(l18),max(l19),max(l20),max(l21))


#########################################################################
############################## PLAZA_42 #################################
#########################################################################

#data_plaza_ <- read_csv("C:/Users/KM Reyes Maya/Downloads/Tercer semestre/Computo Estadistico/Proyecto/Datos_by_Plaza/PLAZA_01.csv")
data_plaza_ <- read.csv("C:/Users/Marcelo Sanchez/OneDrive/Escritorio/Tercer Semestre CIMAT/Computo Estadistico/Proyecto/PLAZA_42.csv")
#View(data_plaza_)

data_plaza_[1,5]

l_42<-l16<-l17<-l18<-l19<-l20<-l20<-l21<-c()
for(i in 1:length(data_plaza_$Ano)) {
  #anio <- 2016
  if(i == 1){
    l_42 <- rbind(l_42, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2016){
    l16 <- rbind(l16, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2017){
    l17 <- rbind(l17, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2018){
    l18 <- rbind(l18, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2019){
    l19 <- rbind(l19, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2020){
    l20 <- rbind(l20, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2021){
    l21 <- rbind(l21, data_plaza_[i,5])
  }
}
l_42<- cbind(l_42,max(l16),max(l17),max(l18),max(l19),max(l20),max(l21))


#########################################################################
############################## PLAZA_43 #################################
#########################################################################

#data_plaza_ <- read_csv("C:/Users/KM Reyes Maya/Downloads/Tercer semestre/Computo Estadistico/Proyecto/Datos_by_Plaza/PLAZA_01.csv")
data_plaza_ <- read.csv("C:/Users/Marcelo Sanchez/OneDrive/Escritorio/Tercer Semestre CIMAT/Computo Estadistico/Proyecto/PLAZA_43.csv")
#View(data_plaza_)

data_plaza_[1,5]

l_43<-l16<-l17<-l18<-l19<-l20<-l20<-l21<-c()
for(i in 1:length(data_plaza_$Ano)) {
  #anio <- 2016
  if(i == 1){
    l_43 <- rbind(l_43, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2016){
    l16 <- rbind(l16, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2017){
    l17 <- rbind(l17, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2018){
    l18 <- rbind(l18, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2019){
    l19 <- rbind(l19, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2020){
    l20 <- rbind(l20, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2021){
    l21 <- rbind(l21, data_plaza_[i,5])
  }
}
l_43<- cbind(l_43,max(l16),max(l17),max(l18),max(l19),max(l20),max(l21))


#########################################################################
############################## PLAZA_44 #################################
#########################################################################

#data_plaza_ <- read_csv("C:/Users/KM Reyes Maya/Downloads/Tercer semestre/Computo Estadistico/Proyecto/Datos_by_Plaza/PLAZA_01.csv")
data_plaza_ <- read.csv("C:/Users/Marcelo Sanchez/OneDrive/Escritorio/Tercer Semestre CIMAT/Computo Estadistico/Proyecto/PLAZA_44.csv")
#View(data_plaza_)

data_plaza_[1,5]

l_44<-l16<-l17<-l18<-l19<-l20<-l20<-l21<-c()
for(i in 1:length(data_plaza_$Ano)) {
  #anio <- 2016
  if(i == 1){
    l_44 <- rbind(l_44, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2016){
    l16 <- rbind(l16, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2017){
    l17 <- rbind(l17, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2018){
    l18 <- rbind(l18, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2019){
    l19 <- rbind(l19, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2020){
    l20 <- rbind(l20, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2021){
    l21 <- rbind(l21, data_plaza_[i,5])
  }
}
l_44<- cbind(l_44,max(l16),max(l17),max(l18),max(l19),max(l20),max(l21))

#########################################################################
############################## PLAZA_45 #################################
#########################################################################

#data_plaza_ <- read_csv("C:/Users/KM Reyes Maya/Downloads/Tercer semestre/Computo Estadistico/Proyecto/Datos_by_Plaza/PLAZA_01.csv")
data_plaza_ <- read.csv("C:/Users/Marcelo Sanchez/OneDrive/Escritorio/Tercer Semestre CIMAT/Computo Estadistico/Proyecto/PLAZA_45.csv")
#View(data_plaza_)

data_plaza_[1,5]

l_45<-l16<-l17<-l18<-l19<-l20<-l20<-l21<-c()
for(i in 1:length(data_plaza_$Ano)) {
  #anio <- 2016
  if(i == 1){
    l_45 <- rbind(l_45, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2016){
    l16 <- rbind(l16, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2017){
    l17 <- rbind(l17, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2018){
    l18 <- rbind(l18, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2019){
    l19 <- rbind(l19, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2020){
    l20 <- rbind(l20, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2021){
    l21 <- rbind(l21, data_plaza_[i,5])
  }
}
l_45<- cbind(l_45,max(l16),max(l17),max(l18),max(l19),max(l20),max(l21))


#########################################################################
############################## PLAZA_46 #################################
#########################################################################

#data_plaza_ <- read_csv("C:/Users/KM Reyes Maya/Downloads/Tercer semestre/Computo Estadistico/Proyecto/Datos_by_Plaza/PLAZA_01.csv")
data_plaza_ <- read.csv("C:/Users/Marcelo Sanchez/OneDrive/Escritorio/Tercer Semestre CIMAT/Computo Estadistico/Proyecto/PLAZA_46.csv")
#View(data_plaza_)

data_plaza_[1,5]

l_46<-l16<-l17<-l18<-l19<-l20<-l20<-l21<-c()
for(i in 1:length(data_plaza_$Ano)) {
  #anio <- 2016
  if(i == 1){
    l_46 <- rbind(l_46, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2016){
    l16 <- rbind(l16, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2017){
    l17 <- rbind(l17, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2018){
    l18 <- rbind(l18, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2019){
    l19 <- rbind(l19, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2020){
    l20 <- rbind(l20, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2021){
    l21 <- rbind(l21, data_plaza_[i,5])
  }
}
l_46<- cbind(l_46,max(l16),max(l17),max(l18),max(l19),max(l20),max(l21))


#########################################################################
############################## PLAZA_47 #################################
#########################################################################

#data_plaza_ <- read_csv("C:/Users/KM Reyes Maya/Downloads/Tercer semestre/Computo Estadistico/Proyecto/Datos_by_Plaza/PLAZA_01.csv")
data_plaza_ <- read.csv("C:/Users/Marcelo Sanchez/OneDrive/Escritorio/Tercer Semestre CIMAT/Computo Estadistico/Proyecto/PLAZA_47.csv")
#View(data_plaza_)

data_plaza_[1,5]

l_47<-l16<-l17<-l18<-l19<-l20<-l20<-l21<-c()
for(i in 1:length(data_plaza_$Ano)) {
  #anio <- 2016
  if(i == 1){
    l_47 <- rbind(l_47, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2016){
    l16 <- rbind(l16, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2017){
    l17 <- rbind(l17, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2018){
    l18 <- rbind(l18, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2019){
    l19 <- rbind(l19, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2020){
    l20 <- rbind(l20, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2021){
    l21 <- rbind(l21, data_plaza_[i,5])
  }
}
l_47<- cbind(l_47,max(l16),max(l17),max(l18),max(l19),max(l20),max(l21))


#########################################################################
############################## PLAZA_48 #################################
#########################################################################

#data_plaza_ <- read_csv("C:/Users/KM Reyes Maya/Downloads/Tercer semestre/Computo Estadistico/Proyecto/Datos_by_Plaza/PLAZA_01.csv")
data_plaza_ <- read.csv("C:/Users/Marcelo Sanchez/OneDrive/Escritorio/Tercer Semestre CIMAT/Computo Estadistico/Proyecto/PLAZA_48.csv")
#View(data_plaza_)

data_plaza_[1,5]

l_48<-l16<-l17<-l18<-l19<-l20<-l20<-l21<-c()
for(i in 1:length(data_plaza_$Ano)) {
  #anio <- 2016
  if(i == 1){
    l_48 <- rbind(l_48, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2016){
    l16 <- rbind(l16, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2017){
    l17 <- rbind(l17, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2018){
    l18 <- rbind(l18, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2019){
    l19 <- rbind(l19, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2020){
    l20 <- rbind(l20, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2021){
    l21 <- rbind(l21, data_plaza_[i,5])
  }
}
l_48<- cbind(l_48,max(l16),max(l17),max(l18),max(l19),max(l20),max(l21))


#########################################################################
############################## PLAZA_49 #################################
#########################################################################

#data_plaza_ <- read_csv("C:/Users/KM Reyes Maya/Downloads/Tercer semestre/Computo Estadistico/Proyecto/Datos_by_Plaza/PLAZA_01.csv")
data_plaza_ <- read.csv("C:/Users/Marcelo Sanchez/OneDrive/Escritorio/Tercer Semestre CIMAT/Computo Estadistico/Proyecto/PLAZA_49.csv")
#View(data_plaza_)

data_plaza_[1,5]

l_49<-l16<-l17<-l18<-l19<-l20<-l20<-l21<-c()
for(i in 1:length(data_plaza_$Ano)) {
  #anio <- 2016
  if(i == 1){
    l_49 <- rbind(l_49, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2016){
    l16 <- rbind(l16, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2017){
    l17 <- rbind(l17, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2018){
    l18 <- rbind(l18, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2019){
    l19 <- rbind(l19, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2020){
    l20 <- rbind(l20, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2021){
    l21 <- rbind(l21, data_plaza_[i,5])
  }
}
l_49<- cbind(l_49,max(l16),max(l17),max(l18),max(l19),max(l20),max(l21))



#########################################################################
############################## PLAZA_50 #################################
#########################################################################

#data_plaza_ <- read_csv("C:/Users/KM Reyes Maya/Downloads/Tercer semestre/Computo Estadistico/Proyecto/Datos_by_Plaza/PLAZA_01.csv")
data_plaza_ <- read.csv("C:/Users/Marcelo Sanchez/OneDrive/Escritorio/Tercer Semestre CIMAT/Computo Estadistico/Proyecto/PLAZA_50.csv")
#View(data_plaza_)

data_plaza_[1,5]

l_50<-l16<-l17<-l18<-l19<-l20<-l20<-l21<-c()
for(i in 1:length(data_plaza_$Ano)) {
  #anio <- 2016
  if(i == 1){
    l_50 <- rbind(l_50, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2016){
    l16 <- rbind(l16, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2017){
    l17 <- rbind(l17, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2018){
    l18 <- rbind(l18, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2019){
    l19 <- rbind(l19, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2020){
    l20 <- rbind(l20, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2021){
    l21 <- rbind(l21, data_plaza_[i,5])
  }
}
l_50<- cbind(l_50,max(l16),max(l17),max(l18),max(l19),max(l20),max(l21))

#########################################################################
############################## PLAZA_51 #################################
#########################################################################

#data_plaza_ <- read_csv("C:/Users/KM Reyes Maya/Downloads/Tercer semestre/Computo Estadistico/Proyecto/Datos_by_Plaza/PLAZA_01.csv")
data_plaza_ <- read.csv("C:/Users/Marcelo Sanchez/OneDrive/Escritorio/Tercer Semestre CIMAT/Computo Estadistico/Proyecto/PLAZA_51.csv")
#View(data_plaza_)

data_plaza_[1,5]

l_51<-l16<-l17<-l18<-l19<-l20<-l20<-l21<-c()
for(i in 1:length(data_plaza_$Ano)) {
  #anio <- 2016
  if(i == 1){
    l_51 <- rbind(l_51, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2016){
    l16 <- rbind(l16, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2017){
    l17 <- rbind(l17, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2018){
    l18 <- rbind(l18, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2019){
    l19 <- rbind(l19, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2020){
    l20 <- rbind(l20, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2021){
    l21 <- rbind(l21, data_plaza_[i,5])
  }
}
l_51<- cbind(l_51,max(l16),max(l17),max(l18),max(l19),max(l20),max(l21))


#########################################################################
############################## PLAZA_52 #################################
#########################################################################

#data_plaza_ <- read_csv("C:/Users/KM Reyes Maya/Downloads/Tercer semestre/Computo Estadistico/Proyecto/Datos_by_Plaza/PLAZA_01.csv")
data_plaza_ <- read.csv("C:/Users/Marcelo Sanchez/OneDrive/Escritorio/Tercer Semestre CIMAT/Computo Estadistico/Proyecto/PLAZA_52.csv")
#View(data_plaza_)

data_plaza_[1,5]

l_52<-l16<-l17<-l18<-l19<-l20<-l20<-l21<-c()
for(i in 1:length(data_plaza_$Ano)) {
  #anio <- 2016
  if(i == 1){
    l_52 <- rbind(l_52, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2016){
    l16 <- rbind(l16, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2017){
    l17 <- rbind(l17, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2018){
    l18 <- rbind(l18, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2019){
    l19 <- rbind(l19, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2020){
    l20 <- rbind(l20, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2021){
    l21 <- rbind(l21, data_plaza_[i,5])
  }
}
l_52<- cbind(l_52,max(l16),max(l17),max(l18),max(l19),max(l20),max(l21))


#########################################################################
############################## PLAZA_53 #################################
#########################################################################

#data_plaza_ <- read_csv("C:/Users/KM Reyes Maya/Downloads/Tercer semestre/Computo Estadistico/Proyecto/Datos_by_Plaza/PLAZA_01.csv")
data_plaza_ <- read.csv("C:/Users/Marcelo Sanchez/OneDrive/Escritorio/Tercer Semestre CIMAT/Computo Estadistico/Proyecto/PLAZA_53.csv")
#View(data_plaza_)

data_plaza_[1,5]

l_53<-l16<-l17<-l18<-l19<-l20<-l20<-l21<-c()
for(i in 1:length(data_plaza_$Ano)) {
  #anio <- 2016
  if(i == 1){
    l_53 <- rbind(l_53, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2016){
    l16 <- rbind(l16, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2017){
    l17 <- rbind(l17, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2018){
    l18 <- rbind(l18, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2019){
    l19 <- rbind(l19, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2020){
    l20 <- rbind(l20, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2021){
    l21 <- rbind(l21, data_plaza_[i,5])
  }
}
l_53<- cbind(l_53,max(l16),max(l17),max(l18),max(l19),max(l20),max(l21))


#########################################################################
############################## PLAZA_54 #################################
#########################################################################

#data_plaza_ <- read_csv("C:/Users/KM Reyes Maya/Downloads/Tercer semestre/Computo Estadistico/Proyecto/Datos_by_Plaza/PLAZA_01.csv")
data_plaza_ <- read.csv("C:/Users/Marcelo Sanchez/OneDrive/Escritorio/Tercer Semestre CIMAT/Computo Estadistico/Proyecto/PLAZA_54.csv")
#View(data_plaza_)

data_plaza_[1,5]

l_54<-l16<-l17<-l18<-l19<-l20<-l20<-l21<-c()
for(i in 1:length(data_plaza_$Ano)) {
  #anio <- 2016
  if(i == 1){
    l_54 <- rbind(l_54, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2016){
    l16 <- rbind(l16, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2017){
    l17 <- rbind(l17, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2018){
    l18 <- rbind(l18, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2019){
    l19 <- rbind(l19, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2020){
    l20 <- rbind(l20, data_plaza_[i,5])
  } else if( data_plaza_[i,2] == 2021){
    l21 <- rbind(l21, data_plaza_[i,5])
  }
}
l_54<- cbind(l_54,max(l16),max(l17),max(l18),max(l19),max(l20),max(l21))



######################################################################
######################################################################
######################################################################
############### Mostramos todos los resultados #######################
######################################################################
######################################################################
######################################################################

resultados <- cbind(pr_28,pr_29,pr_30,pr_31,pr_32,pr_33,pr_34,pr_35,
                    pr_36,pr_37,pr_38,pr_39,pr_40,pr_41,pr_42,pr_43,
                    pr_44,pr_45,pr_46,pr_47,pr_48,pr_49,pr_50,pr_51,
                    pr_52,pr_53,pr_54)

resultados

lista_max_22 <- c()
for (i in 1:27) {
  lista_max_22[i] <- max(resultados[,i])
}

lista_max_22

intervalos <-data.frame( rbind(intervalo_28, intervalo_29, intervalo_30
                    , intervalo_31, intervalo_32, intervalo_33
                    , intervalo_34, intervalo_35, intervalo_36
                    , intervalo_37, intervalo_38, intervalo_39
                    , intervalo_40, intervalo_41, intervalo_42
                    , intervalo_43, intervalo_44, intervalo_45
                    , intervalo_46, intervalo_47, intervalo_48
                    , intervalo_49, intervalo_50, intervalo_51
                    , intervalo_52, intervalo_53, intervalo_54) )

intervalos[,2]

#########################################################################
#########################################################################
######################## Resultados Finales #############################
#########################################################################
#########################################################################

l_final_resultados <- rbind(l_28,l_29,l_30,l_31,l_32,
                            l_33,l_34,l_35,l_36,l_37,l_38,l_39,l_40,
                            l_41,l_42,l_43,l_44,l_45,l_46,l_47,l_48,
                            l_49,l_50,l_51,l_52,l_53,l_54)
l_final_resultados <- cbind(l_final_resultados, lista_max_22,
                            intervalos[,1], intervalos[,2])
l_final_resultados


rownames(l_final_resultados) <- c('Mexico Oriente', 'Mexico Reforma',
                                  'Mexico Satelite', 'Mexico Valle', 'Monclova', 'Monterrey Centro',
                                  'Monterrey Norte', 'Monterrey Oriente','Monterrey Sur', 'Morelia',
                                  'Nogales', 'Oaxaca', 'Obregon','Pachuca', 'Piedras Negras',
                                  'Puebla', 'Queretaro', 'Reynosa','Saltillo', 'San Luis Potosi',
                                  'Tampico', 'Tijuana', 'Toluca', 'Tuxtla', 'Vallarta',
                                  'Villahermosa', 'Zacatecas')

colnames(l_final_resultados) <- c('2015','2016', 
                                  '2017','2018','2019','2020',
                                  '2021','2022', 'intervalo_INF',
                                  'intervalo_SUP')
l_final_resultados

write.csv(l_final_resultados,file = "Resultados_2mitad.csv")
#write.csv(Datos_Plaza_TE, "PLAZA_54.csv")
#read.csv("Resultados_finales.csv")

#write.csv(resultados, "Resultados_sin_media.csv")

