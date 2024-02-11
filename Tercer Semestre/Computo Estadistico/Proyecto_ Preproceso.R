################################################################################
################################################################################
########################## PREPROCESO  DE LOS DATOS ############################ 

#### Este script hizo el preproceso para cada estaci'on meteorologica
#### Reduce las instancias por año y meses
#### Reporta el Max. valor precipitacion por mes por año
#### Reporta la media mensual de la precipitación

library(readr)
library(tidyverse)


#data <- read_csv("C:/Users/KM Reyes Maya/Downloads/Datos_Precipitacion.csv")
data <- read_csv("C:/Users/KM Reyes Maya/Downloads/Datos_Precipitacion.csv")

data
data_ordenado <- data[ order(data$Plaza),  ]
lista_estaciones <- c('10VCZ Boca del Rio', '10VCZ Coatzacoalcos',
                      '10VCZ Cordoba', '10VCZ Poza Rica', '10VCZ Xalapa',
                      'Acapulco', 'Aguascalientes', 'Cancun', 'Chihuahua',
                      'Ciudad Juarez', 'Colima','Cuernavaca', 'Culiacan',
                      'Durango', 'Guadalajara Altos', 'Guadalajara Chapala',
                      'Guadalajara Tequila', 'Hermosillo', 'La Paz', 'Laguna',
                      'Laredo', 'Leon', 'Los Mochis', 'Matamoros', 'Merida',
                      'Mexicali', 'Mexico Ajusco', 'Mexico Oriente', 'Mexico Reforma',
                      'Mexico Satelite', 'Mexico Valle', 'Monclova', 'Monterrey Centro',
                      'Monterrey Norte', 'Monterrey Oriente','Monterrey Sur', 'Morelia',
                      'Nogales', 'Oaxaca', 'Obregon','Pachuca', 'Piedras Negras',
                      'Puebla', 'Queretaro', 'Reynosa','Saltillo', 'San Luis Potosi',
                      'Tampico', 'Tijuana', 'Toluca', 'Tuxtla', 'Vallarta',
                      'Villahermosa', 'Zacatecas')
p <- length(lista_estaciones)
p

#data_esta <- data_ordenado[data_ordenado$Plaza == lista_estaciones[3],]
#data_esta <- data_esta[ order(data_esta$Semana),]
#View(data_esta)
#length( data_esta$Semana )

########################
## En la siguientes lineas lo que realizamos es una verificación de las
##  semanas que hay en cada estacion
########################

l_tamano <- c( rep(0, times = p ) )
for (i in 1:p ) {
  data_esta <- data_ordenado[data_ordenado$Plaza == lista_estaciones[i],]
  #print(length( data_esta$Semana ) )
  l_tamano[i] <- length( data_esta$Semana )
}
l_tamano

##############################################################################
##############################################################################

## Vamos a agurapar por año
## Vamos a agrupar por mes
## Cada mes vamos a tomar el MÁXIMO DE LAS SEMANAS SELECCIONADAS

Anno <- function(datos){
  tamano <- length(datos)
  l <- c( rep(0, times = tamano ) )
  for (i in 1:tamano) {
    ii = as.numeric( substring(datos[i],3,4) )
    if( ii == 15){l[i] = '2015'}
    else if ( ii == 16){ l[i] = '2016' } 
    else if ( ii == 17){ l[i] = '2017' }
    else if ( ii == 18){ l[i] = '2018' }
    else if ( ii == 19){ l[i] = '2019' }
    else if ( ii == 20){ l[i] = '2020' } 
    else if ( ii == 21){ l[i] = '2021' } 
  }
  return(l)
}

Group_b_ano <- function(data_ordenado, lista_estaciones, i){
  data_esta <- data_ordenado[data_ordenado$Plaza == lista_estaciones[i],]
  #print(length( data_esta$Semana ) )
  dat <- data_esta
  ano <- Anno(dat$Semana)
  pro <- data.frame(
    dat$Semana,
    ano,
    dat$Plaza,
    dat$Precipitacion_MM)
  dat_ano <- pro
  names (dat_ano) = c("Semana","Anno", "Plaza","Precipitacion_MM")
    return(dat_ano)
}

Group_by_mes<- function(data_ordenado){
  dat <- data_ordenado
  Mes <- Gruop_by_mesanno(dat$Semana)
  pro <- data.frame(
    dat$Semana,
    dat$Anno,
    Mes,
    dat$Plaza,
    dat$Precipitacion_MM)
  dat_mes <- pro 
  names (dat_mes) = c("Semana","Anno","Mes","Plaza","Precipitacion_MM")
  return(dat_mes)
}

Gruop_by_mesanno <- function(datos){
  tamano <- length(datos)
  l <- c( rep(0, times = tamano ) )
  if(tamano<=303){
    for (i in 1:tamano) {
      ii = as.numeric( substring(datos[i],5,6) )
      if(ii <= 4){ l[i] = '1'}
      else if (5 <= ii && ii<= 8){ l[i] = '2' } 
      else if (9 <= ii && ii <=12){ l[i] = '3'}
      else if (13 <= ii && ii <=17){ l[i] = '4'}
      else if (18 <= ii && ii <=21){ l[i] = '5'}
      else if (22 <= ii && ii <=25){ l[i] = '6'}
      else if (26 <= ii && ii <=30){ l[i] = '7'}
      else if (31 <= ii && ii <=34){ l[i] = '8'}
      else if (35 <= ii && ii <=39){ l[i] = '9'}
      else if (40 <= ii && ii <=43){ l[i] = '10'}#10
      else if (44 <= ii && ii <=48){ l[i] = '11'}#11
      else if (49 <= ii && ii <=53){ l[i] = '12'}#12
    }
  }
  return(l)
}



#### Seleccionamos la estación que queremos procesar
Data_P <- Group_b_ano(data_ordenado, lista_estaciones, 1)
annos <- sort(as.numeric(unique(Data_P$Anno)))

#### Recorremos loa años
Data_plaza_anual = Data_P[Data_P$Anno == annos[1],]
Data_plaza_anual
for (i in 2:length(annos)){
  temp_ano = Data_P[Data_P$Anno == annos[i],]
  Data_plaza_anual = rbind(Data_plaza_anual,temp_ano)
}
Data_plaza_anual #### Establecemos las obs. por año

#### Recorremos los meses
Data_plaza_A_M = Group_by_mes(Data_plaza_anual)
Data_plaza_A_M #### Establecemos las obs por mese y años

######################################
#### El objetivo es analizar el máximo por mes
#### Tomaremos el máximo por mes


by_anno = Data_plaza_A_M[Data_plaza_A_M$Anno == annos[1] ,] #semna 2015
meses = as.numeric(unique(by_anno$Mes))
list_max <-c( rep(0, times = length(meses) ) )
list_media <-c(rep(0,times = length(meses)))
list_mes <-meses
for (i in 1:length(meses)){
  temp_dat =  by_anno[by_anno$Mes == meses[i],]
  maximo <- max(temp_dat$Precipitacion_MM)
  media <- mean(temp_dat$Precipitacion_MM)
  list_max[i] = maximo
  list_media[i] = media
}
Datos_plaza_= data.frame(
  temp_dat$Anno,
  list_mes,
  temp_dat$Plaza,
  list_max,
  list_media)
names (Datos_plaza_) = c("Ano", "Mes",'Plaza',"Max_Precipitacion_MM","Media_Precipitacion")
Datos_plaza_

for (i in 2:length(annos)){
  by_anno = Data_plaza_A_M[Data_plaza_A_M$Anno == annos[i] ,]
  
  meses = sort(as.numeric(unique(by_anno$Mes)))
  list_max <-c( rep(0, times = length(meses) ) )
  list_mes <- meses
  list_media <-c(rep(0,times = length(meses)))
  Anno <- c(rep(annos[i],times = length(meses)))
  Plaza <- c(rep(Data_plaza_A_M$Plaza[1],times = length(meses)))
  
  for (i in 1:length(meses)){
    temp_dat = by_anno[by_anno$Mes == meses[i],]
    maximo <- max(temp_dat$Precipitacion_MM)
    media <- mean(temp_dat$Precipitacion_MM)
    list_max[i] = maximo
    list_media[i] = media
  }
  dat_b_am_temp = data.frame(
    Anno,
    list_mes,
    Plaza,
    list_max,
    list_media)
  names (dat_b_am_temp) = c("Ano", "Mes",'Plaza',"Max_Precipitacion_MM","Media_Precipitacion")
  Datos_plaza_= rbind(Datos_plaza_,dat_b_am_temp)
}
Datos_plaza_ ##############Tenemos listos nuestros datos para la estación

##########################################################################################
##########################################################################################
#### Agregamos la variables 
#### Agregamos tendencia

Tendencia = c(1:71)
Tendencia

Datos_Plaza =data.frame(
  Datos_plaza_$Ano,
  Datos_plaza_$Mes,
  Datos_plaza_$Plaza,
  Datos_plaza_$Max_Precipitacion_MM,
  Datos_plaza_$Media_Precipitacion,
  Tendencia)
names (Datos_Plaza) = c("Ano","Mes",'Plaza',"Max_Precipitacion_MM",
                        "Media_Precipitacion","Tendencia")
Datos_Plaza

##### Revisamos la Estacionalidad
Kobe <- function(data_ordenado){
  dat <- data_ordenado
  mes = dat$Mes
  ### en esta parte se agrego.
  for (i in 1:length(mes)) {
    if(mes[i] == 10){mes[i]='A'}
    else if(mes[i] == 11){mes[i]='B'}
    else if(mes[i] == 12){mes[i]='C'}
  }
  
  pro <- data.frame(
    dat$Ano,
    dat$Mes,
    dat$Plaza,
    dat$Max_Precipitacion_MM,
    dat$Media_Precipitacion,
    dat$Tendencia,
    mes
  )
  names (pro) = c("Ano", "Mess","Plaza","Max_Precipitacion_MM",
                  "Media_Precipitacion","Tendencia","mes")
  dat_mes <- pro %>%
    mutate(mes = paste('mes', mes, sep = '_'),
           valor_mes = 1) %>%
    spread(key = mes, value = valor_mes, fill = 0)
  return(dat_mes)
}

Datos_Plaza
Datos_Plaza_TE = Kobe(Datos_Plaza)
Datos_Plaza_TE

library(xlsx)
write.csv(Datos_Plaza_TE, "C:/Users/KM Reyes Maya/Downloads/Tercer semestre/Computo Estadistico/Proyecto/PLAZA_01.csv")
#PLAZA_01 28-54 ya se agrego los meses A, B y C
