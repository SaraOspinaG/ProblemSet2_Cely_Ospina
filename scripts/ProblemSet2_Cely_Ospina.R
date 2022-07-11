###################################
## Big Data - Problem Set 2 #######
# Maria Camila Cely , Sara Ospina #
###### Julio 2022 #################
###################################

#Motivation: targeted questions that rapidly and cheaply measure the effectiveness of new policies and interventions
#Predictions: household level only 

#####################
# 1. Data Acquisition
#####################

## clean environment
rm(list=ls())

## Llamar/instalar las librerías, usamos pacman para llamar e instalar si es necesario las librerias que necesitamos
require(pacman)
p_load(tidyverse,    #Para limpiar los datos
       caret,        #Para la clasificación y regresiones
       rio,          #Para importar datos
       modelsummary, # msummary
       gamlr,        # cv.gamlr >la que nos permite hacer lasso #NOTA ESTO ES LO QUE HAY QUE TENER CUIDADO, CARET Y GAMLR TIENEN FUNCIONES CON EL MISMO NOMBRE Y DISTINTO RESULTADO, REVISAR
       class,
       ggplot2,
       skimr,
       rvest,
       dplyr,
       stargazer,
       gtsummary,
       expss,
       fastAdaboost,
       randomForest) #PLOAD PERMITE REPLICAR MAS FACIL PORQUE DE UNA VEZ INSTALA LAS LIBRERIAS SI UNO NO LAS TIENE

predict<- stats::predict  #con esto soluciono el problema de que haya mas de una libreria con este comando


#####################
##cargar los datos

##Establecer el directorio
#setwd
#setwd("C:/Users/SARA/Documents/ESPECIALIZACIÓN/BIG DATA/GITHUB/ProblemSet2_Cely_Ospina")
setwd("C:/Users/Camila Cely/Documents/GitHub/ProblemSet2_Cely_Ospina")

##traer las bases de train y de test
train_hogares<-readRDS("stores/train_hogares.Rds")
train_personas<-readRDS("stores/train_personas.Rds")
test_hogares<-readRDS("stores/test_hogares.Rds")
test_personas<-readRDS("stores/test_personas.Rds")

#####################
##1. Limpieza de datos: 
###################

#cuales son las variables que aparecen tanto en train como en test###########################################

#########     hogares  ##################################

intersect(names(test_hogares), names(train_hogares))
#[1] "id"       "Clase"    "Dominio"  "P5000"    "P5010"    "P5090"    "P5100"    "P5130"    "P5140"    "Nper"    
#[11] "Npersug"  "Li"       "Lp"       "Fex_c"    "Depto"    "Fex_dpto"

#Dominio - area metropolitana - cabecera (sirve para efectos fijos por localizacion)*****
#P5000 - de cuantos cuartos en total dispone este hogar
#P5090 - (Si la vivienda ocupada es propia u otro) *********************************
#P5100 - cuota de amortizacion mensual (cuota para reducir deuda) ******************
#P5130 - cuanto tendria que pagar de arriendo si pagara ****************************
#       (esto es para saber cuanto se "ahorran" los que viven en casa propia)
#P5140 - cuanto pagan mensualmente por arriendo ************************************
#Nper - personas en el hogar********************************************************
#Npersug - personas en la unidad de gasto (??)
#Li - linea de indigencia
#Lp - linea de pobreza (creo que esta la vamos a necesitar para la predicción del ingreso)*********************** 
#Fex_c - factor de expansion anualizado
#Fex_dpto - factor expansion departamental 

#Puse con ********** las que considero mas relevantes 
#Dominio #P5090 #P5100 #P5130 #P5140 #Nper

#ahora analisis de esas variables
#por ahora el analisis lo voy a hacer en train porque esa es la base que trabajaremos, entiendo, y en test solo se prueba

#aqui le voy a poner label a las variables

var_lab(train_hogares$P5090) = "Tipo de ocupacion de la vivienda"
val_lab(train_hogares$P5090) = num_lab("
             1 Propia 
             2 Propia_pagando 
             3 Arriendo
             4 Usufructo
             5 Posesion_sin_titulo
             6 Otra_posesion
")
var_lab(train_hogares$P5100) = "Cuota de amortizacion mensual"
var_lab(train_hogares$P5130) = "Valor estimado de arriendo mensual"
var_lab(train_hogares$P5140) =  "Valor de arriendo mensual"
var_lab(train_hogares$Nper) = "Num de personas en el hogar"

var_lab(test_hogares$P5090) = "Tipo de ocupacion de la vivienda"
var_lab(test_hogares$P5100) = "Cuota de amortizacion mensual"
var_lab(test_hogares$P5130) = "Valor estimado de arriendo mensual"
var_lab(test_hogares$P5140) =  "Valor de arriendo mensual"
var_lab(test_hogares$Nper) = "Num de personas en el hogar"
var_lab(test_hogares$Lp) = "Linea de pobreza del hogar"


#Summary para ver estad descr
train_h2 <- select(filter(train_hogares),c(Dominio, P5090, P5100, P5130, P5140, Nper, Lp)) #aqui estoy haciendo una sub-base con nuestras variables de interes para hacer las estad descr

# summarize the data with our package
table1 <- summary(train_h2)  #lo queria hacer con gtsummary pero no me ha podido instalar bien, sale lo mismo pero mas feo
table1

#revisar el paquete para cuando vayamos a exportar para poner en el pdf

#   Dominio              P5090           P5100               P5130               P5140                Nper              Lp        
#Length:164960      Min.   :1.000   Min.   :       98   Min.   :       98   Min.   :       20   Min.   : 1.000   Min.   :167222  
#Class :character   1st Qu.:1.000   1st Qu.:   300000   1st Qu.:   200000   1st Qu.:   250000   1st Qu.: 2.000   1st Qu.:275594  
#Mode  :character   Median :3.000   Median :   500000   Median :   350000   Median :   380000   Median : 3.000   Median :280029  
#                   Mean   :2.461   Mean   :   889276   Mean   :   507928   Mean   :   439160   Mean   : 3.295   Mean   :271605  
#                   3rd Qu.:3.000   3rd Qu.:   900000   3rd Qu.:   500000   3rd Qu.:   500000   3rd Qu.: 4.000   3rd Qu.:285650  
#                   Max.   :6.000   Max.   :280000000   Max.   :800000000   Max.   :300000000   Max.   :28.000   Max.   :303817  
#                                   NA's   :159344      NA's   :64344       NA's   :100616                                       

#Primero hay que volver Dominio as factor (la voy a poner en hogares y en personas de una vez)

train_hogares$Dominio <- as.factor(train_hogares$Dominio)       
class(train_hogares$Dominio)

test_hogares$Dominio <- as.factor(test_hogares$Dominio)       
class(test_hogares$Dominio) #la puse as factor tambien en test, si no toca borramos estas lineas

train_personas$Dominio <- as.factor(train_personas$Dominio)       
class(train_personas$Dominio)

test_personas$Dominio <- as.factor(test_personas$Dominio)       
class(test_personas$Dominio)

#Dominio                   P5090           P5100               P5130               P5140                Nper       
#RESTO URBANO:17169   Min.   :1.000   Min.   :       98   Min.   :       98   Min.   :       20   Min.   : 1.000  
#RURAL       :15354   1st Qu.:1.000   1st Qu.:   300000   1st Qu.:   200000   1st Qu.:   250000   1st Qu.: 2.000  
#BOGOTA      :10567   Median :3.000   Median :   500000   Median :   350000   Median :   380000   Median : 3.000  
#MEDELLIN    : 8878   Mean   :2.461   Mean   :   889276   Mean   :   507928   Mean   :   439160   Mean   : 3.295  
#BARRANQUILLA: 6668   3rd Qu.:3.000   3rd Qu.:   900000   3rd Qu.:   500000   3rd Qu.:   500000   3rd Qu.: 4.000  
#CALI        : 6644   Max.   :6.000   Max.   :280000000   Max.   :800000000   Max.   :300000000   Max.   :28.000  
#(Other)     :99680                   NA's   :159344      NA's   :64344       NA's   :100616   

#Ahora tenemos que ver que hacer con las que tienen missing values
                                    #amortizacion mensual  #arriendo estimado  #arriendo mensual


sum(train_hogares$P5090 == '1' | train_hogares$P5090 == '2' | train_hogares$P5090 == '4' | train_hogares$P5090 == '5' | train_hogares$P5090 == '6')
#100616 ---- estos son los missings de arriendo mensual (toda la gente que no paga arriendo)

sum(train_hogares$P5090 == '2')
#5616 (propia pagando)  164960 - 5616 = 159344 ---- los missings de amortizacion son los que no estan pagando credito

sum(train_hogares$P5090 == '3')
#64344 (arriendo) ----- estos son exactamente los missings de P5130, no estiman arriendo porque saben el exacto


#Por lo tanto= estos missings no son realmente problematicos porque no son reales missings, solo que cada quien responde lo que le corresponde segun su vivienda

#Bajo esa logica, hay que crear una unica variable que capture esto
#Los de P5090=
#1 = arriendo estimado
#2 = arriendo estimado
#3 = arriendo
#4 = arriendo estimado
#5 = arriendo estimado
#6 = arriendo estimado

       
class(train_hogares$P5140)
class(train_hogares$P5090)
class(train_hogares$P5130) #nos estan saliendo estas tres variables como "labelled" "numeric" 

train_hogares$P5140 <- as.numeric(train_hogares$P5140)
train_hogares$P5090 <- as.numeric(train_hogares$P5090)
train_hogares$P5130 <- as.numeric(train_hogares$P5130) #aqui ya quedan numeric

test_hogares$P5140 <- as.numeric(test_hogares$P5140)
test_hogares$P5090 <- as.numeric(test_hogares$P5090)
test_hogares$P5130 <- as.numeric(test_hogares$P5130) #hago lo mismo pero en test

train_hogares <- train_hogares %>% 
  mutate(valor_arriendo = if_else(train_hogares$P5090==3, train_hogares$P5140, train_hogares$P5130))
#aqui lo que hice fue, si la gente realmente paga arriendo, pongame el valor real del arriendo; si no, pongame el arriendo estimado

is.na(train_hogares$valor_arriendo) 
sum(is.na(train_hogares$valor_arriendo)) #vemos que en esta variable ya no hay missing values porque logramos completar con la info existente

#voy a hacer lo mismo para test################# (ver si es necesario)
test_hogares <- test_hogares %>% 
  mutate(valor_arriendo = if_else(test_hogares$P5090==3, test_hogares$P5140, test_hogares$P5130))


#ahora voy a hacer otro subset para ver como quedo esto
train_h3 <- select(filter(train_hogares),c(Dominio, P5090, valor_arriendo, Nper)) 

table2 <- summary(train_h3)  
table2

#         Dominio          P5090       valor_arriendo           Nper       
#RESTO URBANO:17169   Min.   :1.000   Min.   :       20   Min.   : 1.000  
#RURAL       :15354   1st Qu.:1.000   1st Qu.:   200000   1st Qu.: 2.000  
#BOGOTA      :10567   Median :3.000   Median :   350000   Median : 3.000  
#MEDELLIN    : 8878   Mean   :2.461   Mean   :   481105   Mean   : 3.295  
#BARRANQUILLA: 6668   3rd Qu.:3.000   3rd Qu.:   500000   3rd Qu.: 4.000  
#CALI        : 6644   Max.   :6.000   Max.   :800000000   Max.   :28.000  
#(Other)     :99680   
#######VEMOS QUE AHORA SI YA NO TENEMOS MISSINGS****************


###Pero tenemos outliers aun

lower_arriendo<-0.07
upper_arriendo<-0.99

lower_bound_atr <- quantile(train_hogares$valor_arriendo, lower_arriendo) #los valores inferiores son malos registros, ejemplo, arriendo de 99 pesos
lower_bound_atr

upper_bound_atr <- quantile(train_hogares$valor_arriendo, upper_arriendo) #este valor lo escogimos entre sara y yo, el valor mas alto queda en 2.000.000
upper_bound_atr


#duplico base para quitar los outliers sin modificar la original
train_hogares_f<- train_hogares

train_hogares_f <- train_hogares_f %>% subset(valor_arriendo >= lower_bound_atr) 
train_hogares_f <- train_hogares_f %>% subset(valor_arriendo <= upper_bound_atr) 


#ahora voy a hacer otro subset para ver como quedo esto
train_h4 <- select(filter(train_hogares_f),c(Dominio, P5090, valor_arriendo, Nper)) 
table3 <- summary(train_h4)  
table3


#Dominio          P5090       valor_arriendo         Nper       
#RESTO URBANO:16473   Min.   :1.000   Min.   :  40000   Min.   : 1.000  
#RURAL       :12903   1st Qu.:1.000   1st Qu.: 250000   1st Qu.: 2.000  
#BOGOTA      : 9185   Median :3.000   Median : 400000   Median : 3.000  
#MEDELLIN    : 8438   Mean   :2.494   Mean   : 434245   Mean   : 3.304  
#CALI        : 6383   3rd Qu.:3.000   3rd Qu.: 500000   3rd Qu.: 4.000  
#BARRANQUILLA: 6252   Max.   :6.000   Max.   :2000000   Max.   :28.000  
#(Other)     :92722 


#### YA QUEDO MEJOR VALOR_ARRIENDO



#ahora lo hago en TEST con los mismos valores upper y lower
#guarde elementos llamados lower_bound_a y upper_bound_a para que quede coordinado y no toque cambiar los numeros a mano


test_hogares_f<- test_hogares

lower_bound_ate <- quantile(test_hogares$valor_arriendo, lower_arriendo) #los valores inferiores son malos registros, ejemplo, arriendo de 99 pesos
lower_bound_ate

upper_bound_ate <- quantile(test_hogares$valor_arriendo, upper_arriendo) #este valor lo escogimos entre sara y yo, el valor mas alto queda en 2.000.000
upper_bound_ate
#notar que los valores quedan un poquito diferentes porque las bases son distintas, pero quedan valores cercanos


test_hogares_f <- test_hogares_f %>% subset(valor_arriendo >= lower_bound_ate) 
test_hogares_f <- test_hogares_f %>% subset(valor_arriendo <= upper_bound_ate) 

#COMPARAMOS BOXPLOTS PARA VER DISTRIBUCION DE VALOR_ARRIENDO ENTRE LA ORIGINAL Y LA NUEVA BASE

#train 

boxplot(train_hogares$valor_arriendo, #ORIGINAL
        ylab = "valor arriendo"
)


boxplot(train_hogares_f$valor_arriendo, #NUEVA
        ylab = "valor arriendo"
)



#test

boxplot(test_hogares$valor_arriendo, #ORIGINAL
        ylab = "valor arriendo"
)


boxplot(test_hogares_f$valor_arriendo, #NUEVA
        ylab = "valor arriendo"
)


## Ya quedo. Notar que en esta parte de hogares, fuimos haciendo en paralelo las cosas en train y en test



##################################################################
# Ahora la base personas 
#cuales son las variables que aparecen tanto en train como en test
################   personas   ####################################

intersect(names(test_personas), names(train_personas)) #aqui salen muchas mas 


var_lab(train_personas$P6020) = "Sexo"
var_lab(train_personas$P6050) = "Parentesco Jefe de hogar"
var_lab(train_personas$P6040) = "Edad"
var_lab(train_personas$Pet) = "Poblacion en edad de trabajar"
var_lab(train_personas$Oc) = "Ocupado"
var_lab(train_personas$P6210) = "Maximo nivel educativo"
var_lab(train_personas$P6090) = "Afiliado al sistema de salud"
var_lab(train_personas$P7510s3) = "Recibio dinero del gobierno"
var_lab(train_personas$P7510s5) = "Recibio dinero de productos financieros"

var_lab(test_personas$P6020) = "Sexo"
var_lab(test_personas$P6050) = "Parentesco Jefe de hogar"
var_lab(test_personas$P6040) = "Edad"
var_lab(test_personas$Pet) = "Poblacion en edad de trabajar"
var_lab(test_personas$Oc) = "Ocupado"
var_lab(test_personas$P6210) = "Maximo nivel educativo"
var_lab(test_personas$P6090) = "Afiliado al sistema de salud"
var_lab(test_personas$P7510s3) = "Recibio dinero del gobierno"
var_lab(test_personas$P7510s5) = "Recibio dinero de productos financieros"

#Ahora analizar el comportamiento de esas variables
train_p <- select(filter(train_personas),c(Dominio, P6020, P6050, P6040, Pet, Oc, P6210, P6090)) #aqui estoy haciendo una sub-base con nuestras variables de interes para hacer las estad descr
tablep1 <- summary(train_p) 
tablep1
#Vemos que en Pet, Oc y nivel educativo hay muchos missings
#pero la verdad creo que no importa por ahora porque lo que yo quiero es ver esto para el jefe de hogar

#analicemos jefes de hogar ###########

summary(train_personas$P6050)

#voy a crear una variable de solo jefe de hogar, toma valor de 1 para jefe de hogar y 0 para el resto
train_personas <- train_personas %>% 
  mutate(jefe_hogar = if_else(train_personas$P6050==1, 1, 0))

summary(train_personas$jefe_hogar) #30% de los encuestados son jefes de hogar #ademas= todos los hogares de la muestra tienen jefe de hogar

train_personas_f<- train_personas #duplico para poder sacar una submuestra de personas que solo incluya a los jefes de hogar#############################################

train_personas_f <- train_personas_f %>% subset(jefe_hogar == 1) #aqui saque base con solo jefes de hogar


train_p2 <- select(filter(train_personas_f),c(Dominio, P6020, P6050, P6040, Pet, Oc, P6210, P6090)) #aqui estoy haciendo una sub-base con nuestras variables de interes para hacer las estad descr
tablep2 <- summary(train_p2) 
tablep2
#vemos que mejora la situacion de los missings, ya no hay missings en nivel educativo, hay 1 solo missing en Pet
#para Oc continua habiendo missings pero es que Oc no toma valor de cero para nada por default, el valor de desempleados esta en otra variable

#La relacion la vamos a encontrar es analizando Oc - Des - Ina

train_p3 <- select(filter(train_personas_f),c(Oc, Des, Ina)) #aqui estoy haciendo una sub-base con nuestras variables de interes para hacer las estad descr
tablep3 <- summary(train_p3) 
tablep3

#Oc = 117234 individuos (71% de los jefes de hogar)
#Des = 7645 individuos (4,63% de los jefes de hogar) #intuicion= estos nos ayudan a predecir pobres
#Ina = 40080 individuos (24,29% de los jefes de hogar) #que querra decir que el jefe de hogar esta inactivo... pobre o no pobre?
#total = 164959
#por lo tanto vemos que en realidad hay UN missing value (total individuos 164960) #no es representativo, se puede cambiar por cero

#como no podemos usar las tres variables porque habria multicolinearidad, voy a ponerle valor de 0 a todos los missings de Oc
#porque a partir del analisis anterior sabemos que realmente esos missings corresponden a gente que no esta trabajando
#y NO son verdaderos missings

summary(train_personas_f$Oc)
class(train_personas_f$Oc)
head(train_personas_f$Oc)

train_personas_f$Oc[is.na(train_personas_f$Oc)] <- 0 #aqui lo que hice fue cambiar los NA de Oc por valor de cero
head(train_personas_f$Oc)  #comprobar que quedo bien

#voy a crearle variable de mujer, toma valor de 1 si el individuo es mujer, 0 si no
train_personas_f <- train_personas_f %>% 
  mutate(mujer = if_else(train_personas_f$P6020==2, 1, 0))

#educacion es factor
sum(is.na(train_personas_f$P6210)) #no hay missing values en educ
train_personas_f$P6210 <- as.factor(train_personas_f$P6210)       
class(train_personas_f$P6210)


#Ajustamos el comportamiento de afiliado para que no sabe, no responda sea igual a 0

summary(train_personas_f$P6090) #solo hay UN NA porque los que responden esta pregunta son jefes de hogar, asumimos que los NAs de la base original eran los hijos
#por lo tanto no hay problema en cambiar este unico NA por valor de 0

#veamos un histograma de como se comporta

hist(train_personas_f$P6090) #los valores distintos a 1 son muy pocos, en todo caso vamos a unificarlos para volverla dummy

#aqui vamos a cambiar el valor de 9 por el valor de 0
train_personas_f <- train_personas_f %>% 
  mutate(P6090 = if_else(train_personas_f$P6090==1, 1, 0))

#vemos como queda
summary(train_personas_f$P6090) #94% de los encuestados están ailiados al sistema de salud, es probable que personas pobres estén afiliadas

#por ultimo, cambiamos el NA por cero
train_personas_f$P6090[is.na(train_personas_f$P6090)] <- 0 #aqui lo que hice fue cambiar los NA por valor de cero
summary(train_personas_f$P6090) #ya no tiene NAs


#ahora veamos como queda

train_p4 <- select(filter(train_personas_f),c(P6040, Oc, P6210, mujer, P6090)) #quite Pet para centrarnos en Oc
tablep4 <- summary(train_p4) 
tablep4

#P6040             Oc                                 P6210           mujer            P6090       
#Min.   : 11.0   Min.   :0.0000   Ninguno                    : 8603   Min.   :0.0000   Min.   :0.0000  
#1st Qu.: 37.0   1st Qu.:0.0000   Preescolar                 :   13   1st Qu.:0.0000   1st Qu.:1.0000  
#Median : 49.0   Median :1.0000   Básica primaria (1o - 5o)  :46619   Median :0.0000   Median :1.0000  
#Mean   : 49.6   Mean   :0.7107   Básica secundaria (6o - 9o):21616   Mean   :0.4164   Mean   :0.9402  
#3rd Qu.: 61.0   3rd Qu.:1.0000   Media (10o - 13o)          :43028   3rd Qu.:1.0000   3rd Qu.:1.0000  
#Max.   :108.0   Max.   :1.0000   Superior o universitaria   :45061   Max.   :1.0000   Max.   :1.0000  
#                                 No sabe, no informa        :   20                                    



#VEMOS= el promedio de edad de jefes de hogar es 49 a?os, 71% estan empleados, 41% son mujeres
#otra cosa= vemos que hay outliers en edad (P6040), aun mas teniendo en cuenta que en esta base tenemos solo jefe de hogar


#analisis de distribucion de la edad
boxplot(train_personas_f$P6040, #original
        ylab = "edad"
)

#vamos a ponerle limites inferior y superior principalmente porque hay unos valores por encima de 100
lower_bound_ed<-0.002
upper_bound_ed<-0.998

lower_bound_etr <- quantile(train_personas_f$P6040, lower_bound_ed)  
lower_bound_etr #18

upper_bound_etr <- quantile(train_personas_f$P6040, upper_bound_ed) 
upper_bound_etr #93 #de esta manera sacamos esos valores locos de mas de 100 a?os

train_personas_f <- train_personas_f %>% subset(P6040 >= lower_bound_etr) 

train_personas_f <- train_personas_f %>% subset(P6040 <= upper_bound_etr) #aqui modificamos la base para sacar a estas personas outliers de edad
#quedan 164512

boxplot(train_personas_f$P6040, #nueva version
        ylab = "edad"
)
#SOLUCIONADOS LOS OUTLIERS DE EDAD


#Ahora voy a hacer exactamente lo mismo pero en test, de corrido

test_personas <- test_personas %>% 
  mutate(jefe_hogar = if_else(test_personas$P6050==1, 1, 0)) #crear jefe hogar

test_personas_f<- test_personas

test_personas_f <- test_personas_f %>% subset(jefe_hogar == 1) #aqui saque base con solo jefes de hogar #66168


test_personas_f <- test_personas_f %>% 
  mutate(P6090 = if_else(test_personas_f$P6090==1, 1, 0)) #salud

summary(test_personas_f$P6090) #continua habiendo 1 NA
test_personas_f$P6090[is.na(test_personas_f$P6090)] <- 0 #solucionado


test_personas_f$Oc[is.na(test_personas_f$Oc)] <- 0 #cambiar NA de Oc

test_personas_f <- test_personas_f %>% 
  mutate(mujer = if_else(test_personas_f$P6020==2, 1, 0)) #crear variable mujer

test_personas_f$P6210 <- as.factor(test_personas_f$P6210)        #educ as factor


lower_bound_ete <- quantile(test_personas_f$P6040, lower_bound_ed)  
lower_bound_ete #18

upper_bound_ete <- quantile(test_personas_f$P6040, upper_bound_ed) 
upper_bound_ete #93 

test_personas_f <- test_personas_f %>% subset(P6040 >= lower_bound_ete) 
test_personas_f <- test_personas_f %>% subset(P6040 <= upper_bound_ete) #quitar outliers de edad 

#65981



#################################
#Unir Train
############
Personas <- select(filter(train_personas_f),c(id, mujer, Oc, P6210, P6040, P6090, P7510s3, P7510s5)) 
summary(Personas) #aqui vemos que los missing values que no hemos abordado son los de las variables de subsidios y de productos financieros
#mas adelante lo ajustamos

summary(train_hogares_f)

####Base de Hogares + Personas con todas las variables
train_hogares_personas <- left_join(train_hogares_f, Personas)
summary(train_hogares_personas) 

train_final <- train_hogares_personas %>% drop_na(c("mujer", "Oc", "P6210")) #374 NAs que se generan realizando el cruce   ###########AQUI CREAMOS TRAIN_FINAL
summary(train_final) 

#vemos en cuales variables de train_final hay NAs

is.na(train_final)
colSums(is.na(train_final))
colSums(is.na(train_final))>0
colnames(train_final)[colSums(is.na(train_final))>0] #"P5100"   "P5130"   "P5140"   "P7510s3" "P7510s5"
#no hay problema con esas variables de missings porque P5100, P5130 y P5140 estan conectadas entre si y juntas crean valor_arriendo, que no tiene NAs

#con respecto a "P7510s3" "P7510s5", vamos a ajustarlas=

#Ajustar si recibió subsidios en train
train_final <- train_final %>% 
  mutate(P7510s3 = if_else(train_final$P7510s3==1, 1, 0))
train_final$P7510s3[is.na(train_final$P7510s3)] <- 0 


#Ajustar si tiene productos financieros en train
train_final <- train_final %>% 
  mutate(P7510s5 = if_else(train_final$P7510s5==1, 1, 0))
train_final$P7510s5[is.na(train_final$P7510s5)] <- 0 

is.na(train_final)
colSums(is.na(train_final))
colSums(is.na(train_final))>0
colnames(train_final)[colSums(is.na(train_final))>0] #ahora si nos salen solo las de arriendo con missings, ya quedo bien

############################
#Unir Test
PersonasT <- select(filter(test_personas_f),c(id, mujer, Oc, P6210, P6040, P6090, P7510s3, P7510s5)) 
summary(PersonasT)

summary(test_personas_f)

####Base de Hogares + Personas con todas las variables
test_hogares_personas <- left_join(test_hogares_f, PersonasT)
summary(test_hogares_personas) 

test_final <- test_hogares_personas %>% drop_na(c("mujer", "Oc", "P6210")) #se generan 156 missings en el cruce   ##############AQUI CREAMOS TEST_FINAL

#Ajustar si recibió subsidios en test
test_final <- test_final %>% 
  mutate(P7510s3 = if_else(test_final$P7510s3==1, 1, 0))
test_final$P7510s3[is.na(test_final$P7510s3)] <- 0 


#Ajustar si tiene productos financieros en test
test_final <- test_final %>% 
  mutate(P7510s5 = if_else(test_final$P7510s5==1, 1, 0))
test_final$P7510s5[is.na(test_final$P7510s5)] <- 0 

summary(test_final) 

is.na(test_final)
colSums(is.na(test_final))
colSums(is.na(test_final))>0
colnames(test_final)[colSums(is.na(test_final))>0] #nuevamente solo nos quedan NAs en "P5100" "P5130" "P5140", ya quedo bien


############################consideraciones variables explicativas ##########
#SOBRE VARIABLE P5090
#para poder sacar conclusiones sobre esta variable (tipo de posesion de la vivienda) hay que volverla dummys

#recordar esto:
#val_lab(train_hogares$P5090) = num_lab("
#             1 Propia 
#             2 Propia pagando 
#             3 Arriendo
#             4 Usufructo
#             5 Posesion sin titulo
#             6 Otra_posesion
#")


#en train

train_final <- train_final %>% 
  mutate(viv_propia = if_else(train_final$P5090==1, 1, 0))

train_final <- train_final %>% 
  mutate(viv_propiapagando = if_else(train_final$P5090==2, 1, 0))

train_final <- train_final %>% 
  mutate(viv_arrendada = if_else(train_final$P5090==3, 1, 0))

train_final <- train_final %>% 
  mutate(viv_usufr = if_else(train_final$P5090==4, 1, 0))

train_final <- train_final %>% 
  mutate(viv_sintitulo = if_else(train_final$P5090==5, 1, 0))

train_final <- train_final %>% 
  mutate(viv_otra = if_else(train_final$P5090==6, 1, 0))

#en test

test_final <- test_final %>% 
  mutate(viv_propia = if_else(test_final$P5090==1, 1, 0))

test_final <- test_final %>% 
  mutate(viv_propiapagando = if_else(test_final$P5090==2, 1, 0))

test_final <- test_final %>% 
  mutate(viv_arrendada = if_else(test_final$P5090==3, 1, 0))

test_final <- test_final %>% 
  mutate(viv_usufr = if_else(test_final$P5090==4, 1, 0))

test_final <- test_final %>% 
  mutate(viv_sintitulo = if_else(test_final$P5090==5, 1, 0))

test_final <- test_final %>% 
  mutate(viv_otra = if_else(test_final$P5090==6, 1, 0))




#lo mismo lo voy a hacer con la de educacion #P6210
#la cosa es que educacion trae nombres rarisimos entonces creo que la voy a hacer a mano

#En train
train_final <- train_final %>% mutate(educ= train_final$P6210)


colnames(train_final)
#              "Ninguno"                     "Preescolar"                 
#[40] "Básica primaria (1o - 5o)"   "Básica secundaria (6o - 9o)" "Media (10o - 13o)"          
#[43] "Superior o universitaria"    "No sabe, no informa"      

train_final <- train_final %>% 
  mutate(sin_educ = if_else(train_final$P6210=="Ninguno", 1, 0))

train_final <- train_final %>% 
  mutate(preescolar = if_else(train_final$P6210=="Preescolar", 1, 0))

train_final <- train_final %>% 
  mutate(primaria = if_else(train_final$P6210=="Básica primaria (1o - 5o)", 1, 0))

train_final <- train_final %>% 
  mutate(secundaria = if_else(train_final$P6210=="Básica secundaria (6o - 9o)", 1, 0))

train_final <- train_final %>% 
  mutate(bachillerato_completo = if_else(train_final$P6210=="Media (10o - 13o)", 1, 0))

train_final <- train_final %>% 
  mutate(superior = if_else(train_final$P6210=="Superior o universitaria", 1, 0))

train_final <- train_final %>% 
  mutate(educ_ns_nr = if_else(train_final$P6210=="No sabe, no informa", 1, 0))


#en test
test_final <- test_final %>% 
  mutate(sin_educ = if_else(test_final$P6210=="Ninguno", 1, 0))

test_final <- test_final %>% 
  mutate(preescolar = if_else(test_final$P6210=="Preescolar", 1, 0))

test_final <- test_final %>% 
  mutate(primaria = if_else(test_final$P6210=="Básica primaria (1o - 5o)", 1, 0))

test_final <- test_final %>% 
  mutate(secundaria = if_else(test_final$P6210=="Básica secundaria (6o - 9o)", 1, 0))

test_final <- test_final %>% 
  mutate(bachillerato_completo = if_else(test_final$P6210=="Media (10o - 13o)", 1, 0))

test_final <- test_final %>% 
  mutate(superior = if_else(test_final$P6210=="Superior o universitaria", 1, 0))

test_final <- test_final %>% 
  mutate(educ_ns_nr = if_else(test_final$P6210=="No sabe, no informa", 1, 0))



#############################
#2. Estadistica descriptiva general
###############################


#querremos mostrar las variables que nos interesan en TRAIN
#como ya no tenemos hogares y personas por separado, sacamos este analisis en las bases final


colnames(train_final)

#[1] "id"             "Clase"          "Dominio"        "P5000"          "P5010"          "P5090"         
#[7] "P5100"          "P5130"          "P5140"          "Nper"           "Npersug"        "Ingtotug"      
#[13] "Ingtotugarr"    "Ingpcug"        "Li"             "Lp"             "Pobre"          "Indigente"     
#[19] "Npobres"        "Nindigentes"    "Fex_c"          "Depto"          "Fex_dpto"       "valor_arriendo"
#[25] "mujer"          "Oc"             "P6210"          "P6040"          "P6090"          "P7510s3"       
#[31] "P7510s5"     


var_lab(train_final$P5000) = "Num total de cuartos"
var_lab(train_final$P5010) = "Num total de cuartos donde se duerme"
var_lab(train_final$Npersug) = "Num personas por unidad de gasto"
var_lab(train_final$Ingtotug) = "Ingreso total unidad de gasto"
var_lab(train_final$Ingtotugarr) = "Ingreso total unidad de gasto con imputacion de arriendo"
var_lab(train_final$Ingpcug) = "Ingreso per capita unidad de gasto con imputacion de arriendo"
var_lab(train_final$Li) = "Linea indigencia"
var_lab(train_final$Li) = "Linea pobreza"
var_lab(train_final$Pobre) = "Hogar clasificado como pobre (1)"
var_lab(train_final$Npobres) = "Total pobres en el hogar"
var_lab(train_final$Nindigentes) = "Total indigentes en el hogar"
var_lab(train_final$Fex_c) = "Factor de expansion anualizado"
var_lab(train_final$Fex_dpto) = "Factor de expansion departamental"
var_lab(train_final$mujer) = "Jefe de hogar mujer (1)"
var_lab(train_final$P6210) = "Nivel educativo max jefe de hogar"
var_lab(train_final$P6040) = "Edad jefe de hogar"
var_lab(train_final$P6090) = "Entidad salud jefe de hogar (1=si)"
var_lab(train_final$P7510s3) = "Jefe de hogar recibe ayudas institucionales"
var_lab(train_final$P7510s5) = "Jefe de hogar recibe dinero de productos financieros"

dim(train_final) #151982     31

#paquete gtsummary
#The default output from tbl_summary() is meant to be publication ready.

summary(train_final$Pobre) #mean= 0.1923
class(train_final$Pobre) #19% de las observaciones de hogares denotan que son pobres, pero R no lo lee porque es labelled numeric y no le he podido cambiar la clase

#voy a intentar crear una nueva variable con los mismos valores

train_final <- train_final %>% 
  mutate(hogarpobre = if_else(train_final$Pobre==1, 1, 0))

class(train_final$hogarpobre) #esta ya queda solo como numeric
var_lab(train_final$hogarpobre) = "Hogar clasificado como pobre (1)" #ahora si se pudo

#tabla general
train_final %>%
  select(hogarpobre, mujer, P6210, P6040, valor_arriendo, Ingtotugarr) %>%
  tbl_summary() 

#tabla discriminando si el jefe de hogar es hombre o mujer
train_final %>%
  select(hogarpobre, mujer, P6210, P6040, valor_arriendo, Ingtotugarr) %>%
  tbl_summary(by=mujer) 

summary(train_final$valor_arriendo)
hist(train_final$valor_arriendo)



#aqui se le pueden añadir mas tablas si quisieramos pero creo que debido a que el documento es tan acotado, podemos por ahora dejar con estas basicas

##
#Prueba de como se mide la pobreza
#aqui vamos a ver si midiendo si un hogar es pobre "a mano" podemos llegar a los mismos resultados que el dane


train_final <- train_final %>% 
  mutate(hogar_pobre_pr = if_else(train_final$Ingtotugarr<=(train_final$Lp * train_final$Nper), 1, 0))
#notar que tocaba multiplicar linea de pobreza por numero de personas


summary(train_final$hogarpobre)

compare<- select(filter(train_final),c(hogarpobre, hogar_pobre_pr)) 
table_compare <- summary(compare)  
table_compare 

#hogarpobre     hogar_pobre_pr  
#Min.   :0.0000   Min.   :0.0000  
#1st Qu.:0.0000   1st Qu.:0.0000  
#Median :0.0000   Median :0.0000  
#Mean   :0.1923   Mean   :0.1928 #ahora sí nos da
#3rd Qu.:0.0000   3rd Qu.:0.0000  
#Max.   :1.0000   Max.   :1.0000  


### ####### ####### #######
#por ultimo voy a hacer exactamente lo mismo pero en test_final
###### ######## ####### ######

colnames(test_final)
# "id"             "Clase"          "Dominio"        "P5000"          "P5010"          "P5090"         
#[7] "P5100"          "P5130"          "P5140"          "Nper"           "Npersug"        "Li"            
#[13] "Lp"             "Fex_c"          "Depto"          "Fex_dpto"       "valor_arriendo" "mujer"         
#[19] "Oc"             "P6210"          "P6040"          "P6090"          "P7510s3"        "P7510s5" 

##NOTAR que en test no esta la variable de Ingtotugarr, entonces hay que aprender a predecirla con lo que tenemos
    
var_lab(test_final$P5000) = "Num total de cuartos"
var_lab(test_final$P5010) = "Num total de cuartos donde se duerme"
var_lab(test_final$Npersug) = "Num personas por unidad de gasto"

#var_lab(train_final$Ingtotug) = "Ingreso total unidad de gasto" #NO EXISTE
#(train_final$Ingtotugarr) = "Ingreso total unidad de gasto con imputacion de arriendo" #NO EXISTE
#var_lab(train_final$Ingtpcug) = "Ingreso per capita unidad de gasto con imputacion de arriendo" #NO EXISTE

var_lab(test_final$Li) = "Linea indigencia"
var_lab(test_final$Li) = "Linea pobreza"

#var_lab(train_final$Pobre) = "Hogar clasificado como pobre (1)" #NO EXISTE
#var_lab(train_final$Npobres) = "Total pobres en el hogar" #NO EXISTE
#var_lab(train_final$Nindigentes) = "Total indigentes en el hogar"#NO EXISTE

var_lab(test_final$Fex_c) = "Factor de expansion anualizado"
var_lab(test_final$Fex_dpto) = "Factor de expansion departamental"
var_lab(test_final$mujer) = "Jefe de hogar mujer (1)"
var_lab(test_final$P6210) = "Nivel educativo max jefe de hogar"
var_lab(test_final$P6040) = "Edad jefe de hogar"
var_lab(test_final$P6090) = "Entidad salud jefe de hogar (1=si)"
var_lab(test_final$P7510s3) = "Jefe de hogar recibe ayudas institucionales"
var_lab(test_final$P7510s5) = "Jefe de hogar recibe dinero de productos financieros"

dim(test_final) #60845    24

#no hago el analisis de hogares pobres precisamente porque en esta base no los tenemos

#tabla general
test_final %>%
  select(mujer, P6210, P6040, valor_arriendo) %>%
  tbl_summary() 

#tabla discriminando si el jefe de hogar es hombre o mujer
test_final %>%
  select(mujer, P6210, P6040, valor_arriendo) %>%
  tbl_summary(by=mujer) #las proporciones de todo estan dando parecidas, excepto valor arriendo, que en esta muestra mas pequeña esta dando mas caro para las mujeres (40.000 pesos mas caro en promedio, alrededor de 11% mas caro)

summary(test_final$valor_arriendo)
hist(test_final$valor_arriendo) #dan parecidos


#####Tener Pobre como factor para que los modelos que corra con caret funcionen
train_final <- train_final %>% mutate(hogar_es_pobre= train_final$hogarpobre)
train_final <- train_final %>% mutate(hogar_es_pobre=factor(hogar_es_pobre, levels=c(1,0), labels=c("Si", "No")))

#################################################################################
#3. Dividir las muestras #######################################################
#################################################################################

##Primero partimos la base de entrenamiento, se parte en 80% - 20% por ser la forma en la que se comporta la variable de pobreza 
set.seed(123)
split1<-createDataPartition(train_final$hogarpobre, p = .8)[[1]]
length(split1)

other<-train_final[-split1,]
training <-train_final[split1,]

##Ahora partimos para obtener las bases de evaluacion y prueba
split2<- createDataPartition(other$hogarpobre, p= 1/3) [[1]]
evaluation<-other[split2,]
testing<-other[-split2,]

####NOTAR ENTONCES QUE AQUI TENEMOS LO SIGUIENTE:

#TRAINING:    121586 obs
#EVALUATION:   10132 obs
#TESTING:      20264 obs  #recordar que en este testing SI HAY la variable de pobres

#comprobamos las proporciones de hogares pobres en cada muestra
summary(training$hogarpobre) #19,3%
summary(evaluation$hogarpobre) #18,3%
summary(testing$hogarpobre) #19,0%    #podemos concluir que si estan semejantes


#Por otro lado
#TEST_FINAL:   60845 obs  #en esta es donde haremos la prueba final final no va mas


#######################################################################################
#DE AQUI EN ADELANTE VIENEN LOS MODELOS PLANTEADOS

################################################################################################
##4. Modelos de Clasificación ###################################################################
################################################################################################


###################
##Predecir pobreza

##Classification Models: (este es si pobre 1 o 0)#######################################
#en clasificacion nuestra variable Y es si es pobre o no es pobre


#############################
###   RandomForests   ########
#############################

#OBJETIVO= meter muchas variables y que nos indique cuales son las mejores

#install.packages("randomForest")


fiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...))

ctrl<- trainControl(method = "cv", 
                    number = 5,
                    summaryFunction = fiveStats,  
                    classProbs = TRUE,
                    verbose = FALSE,
                    savePredictions = T)

set.seed(123)

forest <- train(
  hogar_es_pobre ~ P5000 + P5010 + valor_arriendo + mujer + Oc + P6040 + P6090 + P7510s3 + P7510s5 + preescolar + primaria + secundaria + bachillerato_completo + superior + educ_ns_nr + viv_propia + viv_propiapagando + viv_arrendada + viv_usufr + viv_sintitulo ,
  data = training,
  method = "rf",
  trControl = ctrl,
  family = "binomial",
  metric="Sens",)

#como esto se demoro literal dos horas corriendo, voy a guardar una copia hasta aca
save(forest, file = "C:/Users/Camila Cely/Documents/GitHub/ProblemSet2_Cely_Ospina/stores/forest.Rdata")
save.image()

#variable importance 
varImp(forest,scale=TRUE)
####################################IMPORTANTE= ESTA ES LA PRIMERA SALIDA QUE NOS INDICA LAS MEJORES VARIABLES QUE PREDICEN POBREZA

#P6040                 100.00000   #Edad del jefe de hogar   
#valor_arriendo         72.28647   #cuanto pagan de arriendo
#P5000                  28.00644   #cuantos cuartos tienen
#P5010                  18.04168   #en cuantos cuartos duermen
#mujer                  14.78857   #si el jefe de hogar es mujer #de acuerdo con nuestra intuicion y con las estad descr
#primaria                9.16831   #si el jefe de hogar tiene nivel educativo muy bajo
#Oc                      8.04550   #si el jefe de hogar esta empleado
#P6090                   7.10394   #si el jefe de hogar esta afiliado a salud
#secundaria              7.00427   # (educ)
#P7510s3                 6.24572   #si el jefe de hogar recibe ayudas del gobierno
#bachillerato_completo   6.18634   # (educ)
#superior                5.84915   # (educ)                       #notar que salen en orden
#viv_arrendada           5.67787   #si la vivienda es arrendada
#viv_propia              4.76958   #si la vivienda es propia     ## ver cual de las dos usar, diria que arrendada
#viv_sintitulo           4.68723
#viv_usufr               4.43618
#viv_propiapagando       1.30497
#P7510s5                 0.44975   #vemos que la de recibir dinero de productos financieros no ayuda mucho
#preescolar              0.01527   #estas dos ultimas no sirven de mucho porque hay demasiado pocas observaciones
#educ_ns_nr              0.00000

#Predecir
pred_rf<-predict(forest,testing)

#comparar
confusionMatrix(testing$hogar_es_pobre,pred_rf)

#Confusion Matrix and Statistics

#Reference
#Prediction    Si    No
#Si  1330  2532
#No  1756 14646  #notar que predice como pobres a muchisimos no-pobres

#Accuracy : 0.7884         
#95% CI : (0.7827, 0.794)
#No Information Rate : 0.8477         
#P-Value [Acc > NIR] : 1              

#Kappa : 0.2571         

#Mcnemar's Test P-Value : <2e-16         

#           Sensitivity : 0.43098       #hace relativamente buen trabajo 
#           Specificity : 0.85260        
#        Pos Pred Value : 0.34438        
#        Neg Pred Value : 0.89294        
#            Prevalence : 0.15229        
#        Detection Rate : 0.06563        
#  Detection Prevalence : 0.19058        
#     Balanced Accuracy : 0.64179        

#     'Positive' Class : Si          








#########################
###   AdaBoost   ########
#########################

#install.packages("fastAdaboost")

#le voy a lanzar un monton de variables a ver cuales pone como importantes
#sin embargo recordar que tienen que estar en test_final tambien
intersect(names(training), names(test_final))

#[1] "id"             "Clase"          "Dominio"        "P5000"          "P5010"          "P5090"         
#[7] "P5100"          "P5130"          "P5140"          "Nper"           "Npersug"        "Li"            
#[13] "Lp"             "Fex_c"          "Depto"          "Fex_dpto"       "valor_arriendo" "mujer"         
#[19] "Oc"             "P6210"          "P6040"          "P6090"          "P7510s3"        "P7510s5"

class(training$Dominio) #factor

#prueba con pocas explicativas para ver si corre bien el codigo ##############

adaboost <- train(
    hogar_es_pobre ~ mujer + viv_propia ,
    data = training,
    method = "adaboost",
    trControl = ctrl,
    family = "binomial",
    metric = "Sens",
    #preProcess = c("center", "scale")
  )

#predecimos
pred_ada<-predict(adaboost,testing)

#comparamos
confusionMatrix(testing$hogar_es_pobre,pred_ada)

##Reference
#Prediction    Si    No
#Si  2904   958
#No 10124  6278

#Accuracy : 0.4531        
#95% CI : (0.4462, 0.46)
#No Information Rate : 0.6429        
#P-Value [Acc > NIR] : 1             

#Kappa : 0.0706        

#Mcnemar's Test P-Value : <2e-16        

#            Sensitivity : 0.2229        
#            Specificity : 0.8676        
#         Pos Pred Value : 0.7519        
#         Neg Pred Value : 0.3828        
#             Prevalence : 0.6429        
#         Detection Rate : 0.1433        
#   Detection Prevalence : 0.1906        
#      Balanced Accuracy : 0.5453        

#       'Positive' Class : Si            






  
  
  
  
  ##prueba forests codigo complementario #######pendiente
  
  ctrl<- trainControl(method = "cv",
                      number = 5,
                      summaryFunction = fiveStats,
                      classProbs = TRUE,
                      verbose=FALSE,
                      savePredictions = T)
  
  modelo1 <- decision_tree() %>%
    set_engine("rpart") %>%
    set_mode("classification")
  
  
  
  
################################################
###   Soluciones a desbalance de clases  ########
#################################################
  
  #como vimos, los hogares pobres son alrededor del 20% en las bases que tenemos, entonces vamos a hacer un remuestreo
  








#######Modelos Logit######

#Logit con cross validation
#Vamos a combinar las dos para tener todas las estadísticas, pero nos vamos a centrar en la sensibilidad: 
fiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...))
ctrl<- trainControl(method = "cv", 
                    number = 5,
                    summaryFunction = fiveStats,  
                    classProbs = TRUE,
                    verbose = FALSE,
                    savePredictions = T)

#Uso siempre la misma semilla para que sea comparable
#1.1
set.seed(123)
mylogit_caret <- train(
  hogar_es_pobre ~  mujer + superior + P6040, 
  data = training,
  method = "glm",
  trControl = ctrl,
  family = "binomial",
  preProcess = c("center", "scale")
)
mylogit_caret

#ROC        Sens         Spec       Accuracy   Kappa      
#0.6917659  0.006468293  0.9993067  0.8074203  0.009263736
#Accuracy es VN+VP/VN+VP+FN+FP = tenemos 81% de efectividad, aleatorio daría parecido, no está tan bien
#La sensibilidad está muy bajita comparado con el ejemplo que vimos en clase 



#1.2
set.seed(123)
mylogit_caret2 <- train(
  hogar_es_pobre ~ viv_propia + mujer + superior + P6040 + P5090 + valor_arriendo , 
  data = training,
  method = "glm",
  trControl = ctrl,
  family = "binomial",
  preProcess = c("center", "scale")
)
mylogit_caret2

#ROC        Sens       Spec       Accuracy   Kappa    
#0.7664873  0.1288993  0.9780297  0.8139177  0.1519466
#Accuracy es VN+VP/VN+VP+FN+FP = tenemos 81% de efectividad, aleatorio daría parecido, no está tan bien
#La sensibilidad está muy bajita comparado con el ejemplo que vimos en clase 

#1.3
set.seed(123)
mylogit_caret3 <- train(
  hogar_es_pobre ~viv_propia + mujer + superior + P6040 + P5090 + valor_arriendo + P7510s3 + P7510s5, 
  data = training,
  method = "glm",
  trControl = ctrl,
  family = "binomial",
  preProcess = c("center", "scale")
)
mylogit_caret3

#ROC        Sens       Spec       Accuracy   Kappa    
#0.7664873  0.1288993  0.9780297  0.8139177  0.1519466
#Accuracy es VN+VP/VN+VP+FN+FP = tenemos 81% de efectividad, aleatorio daría parecido, no está tan bien
#La sensibilidad está muy bajita comparado con el ejemplo que vimos en clase 




##########Logit con Elastic Net - sensibility
lambda_grid <- 10^seq(-4, 0.01, length = 100)
lambda_grid

set.seed(123)
mylogit_enet_sens <- train(
  hogar_es_pobre ~ valor_arriendo+mujer +superior + P6040 + P6090 + P7510s3 + P7510s5, 
  data = training,
  method = "glmnet",
  trControl = ctrl,
  family = "binomial",
  metric = "Sens",
  tuneGrid = ,
  preProcess = c("center", "scale")
)

help(train)

mylogit_enet_sens
#Sens was used to select the optimal model using the largest value.
#The final values used for the model were alpha = 0.55 and lambda = 0.0002121025.
#  alpha  lambda        ROC        Sens         Spec       Accuracy   Kappa     
#0.10   0.0002121025  0.7631357  0.110345370  0.9814348  0.8130788  0.13278064
#Podemos ver que la sensibilidad mejora considerablemente


##########Logit con Lasso - sensibility

install.packages("glmnet")

lambda_grid <- 10^seq(-4, 0.01, length = 100)
lambda_grid

set.seed(123)
mylogit_lasso_sens <- train(
  hogar_es_pobre ~ valor_arriendo+mujer +superior + P6040 + P6090 + P7510s3 + P7510s5, 
  data = training,
  method = "glmnet",
  trControl = ctrl,
  family = "binomial",
  metric = "Sens",
  tuneGrid = expand.grid(alpha = 0, lambda=lambda_grid),
  preProcess = c("center", "scale")
)

mylogit_lasso_sens
#Tuning parameter 'alpha' was held constant at a value of 0
#Sens was used to select the optimal model using the largest value.
#The final values used for the model were alpha = 0 and lambda = 0.0105987.
#Sensibility = 8.613159e-02


##########Logit con lasso - ROC
set.seed(123)
mylogit_lasso_roc <- train(
  hogar_es_pobre ~ valor_arriendo+mujer +superior + P6040 + P6090 + P7510s3 + P7510s5, 
  data = training,
  method = "glmnet",
  trControl = ctrl,
  family = "binomial",
  metric = "ROC",
  tuneGrid = expand.grid(alpha = 0, lambda=lambda_grid),
  preProcess = c("center", "scale")
)

mylogit_lasso_roc
#Tuning parameter 'alpha' was held constant at a value of 0
#ROC was used to select the optimal model using the largest value.
#The final values used for the model were alpha = 0 and lambda = 0.009654893.




######Logit con Lasso y cambio en cutoff
#Para el cambio de cutoofs lo hago en la muestra de evaluation

evalResults <- data.frame(hogar_es_pobre = evaluation$hogar_es_pobre)
evalResults$Roc <- predict(mylogit_lasso_roc,
                          newdata = evaluation,
                          type = "prob") [,1]

library(pROC)
rfROC <- roc(evalResults$hogar_es_pobre, evalResults$Roc, levels = rev(levels(evalResults$hogar_es_pobre)))
rfROC

#Aquí queremos encontrar el treshold que esta mas cerca a la esquina superior izquierda
rfThresh <- coords(rfROC, x = "best", best.method = "closest.topleft")
rfThresh
#threshold specificity sensitivity
#0.1978883   0.6511965    0.744887

evalResults <- evalResults %>% mutate(hat_def_05=ifelse(evalResults$Roc>0.5,"Si","No"),
                                      hat_def_rfThresh=ifelse(evalResults$Roc>rfThresh$threshold,"Si","No"))


with(evalResults,table(hogar_es_pobre,hat_def_05))
#hogar_es_pobre   No   Si
#             Si 1706  152
#             No 8167  107


with (evalResults,table(hogar_es_pobre,hat_def_rfThresh))
#hogar_es_pobre   No   Si
#             Si  474 1384
#             No 5388 2886




#Con el nuevo cutoff estamos obteniendo mas verdaderos si, tenemos mas falsos positivos pero menos falsos negativos. 
#Lo que mas nos importa es predecir bien los pobres


evalResults2 <- data.frame(hogar_es_pobre = evaluation$hogar_es_pobre)
evalResults2$Roc <- predict(mylogit_enet_sens,
                           newdata = evaluation,
                           type = "prob") [,1]

evalResults2 <- evalResults2 %>% mutate(hat_def_06=ifelse(evalResults2$Roc>0.5,"Si","No"),
                                      hat_def_rfThresh2=ifelse(evalResults2$Roc>rfThresh$threshold,"Si","No"))


library(pROC)
rfROC2 <- roc(evalResults2$hogar_es_pobre, evalResults2$Roc, levels = rev(levels(evalResults2$hogar_es_pobre)))
rfROC2

with(evalResults2,table(hogar_es_pobre,hat_def_06))
#hat_def_06
#hogar_es_pobre   No   Si
#             Si 1665  193
#             No 8128  146
with (evalResults2,table(hogar_es_pobre,hat_def_rfThresh2))
#hat_def_rfThresh2
#hogar_es_pobre   No   Si
#             Si  503 1355
#             No 5469 2805






#Logit con Lasso y resampleo
#Arbol normal


##5. Regresiones
#Ingreso

###hago algo, guardo y hago commit   










