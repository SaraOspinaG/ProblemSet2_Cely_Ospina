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
       fastAdaboost) #PLOAD PERMITE REPLICAR MAS FACIL PORQUE DE UNA VEZ INSTALA LAS LIBRERIAS SI UNO NO LAS TIENE

predict<- stats::predict  #con esto soluciono el problema de que haya mas de una libreria con este comando



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

##1. Limpieza de datos: 
###################

#cuales son las variables que aparecen tanto en train como en test
#########hogares

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
             2 Propia pagando 
             3 Arriendo
             4 Usufructo
             5 Posesion sin titulo
             6 Otra
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
test_hogares$P5130 <- as.numeric(test_hogares$P5130)#hago lo mismo pero en test

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

lower_bound <- quantile(train_hogares$valor_arriendo, 0.07) #los valores inferiores son malos registros, ejemplo, arriendo de 99 pesos
lower_bound

upper_bound <- quantile(train_hogares$valor_arriendo, 0.99) #este valor lo escogi revisando el boxplot original pero esta sujeto a cambios, ver con sara
upper_bound


#duplico base
train_hogares_f<- train_hogares

train_hogares_f <- train_hogares_f %>% subset(valor_arriendo >= lower_bound) 
train_hogares_f <- train_hogares_f %>% subset(valor_arriendo <= upper_bound) 


#ahora voy a hacer otro subset para ver como quedo esto
train_h4 <- select(filter(train_hogares_f),c(Dominio, P5090, valor_arriendo, Nper)) 
table3 <- summary(train_h4)  
table3

#         Dominio          P5090      valor_arriendo         Nper       
#RESTO URBANO:16490   Min.   :1.00   Min.   :  40000   Min.   : 1.000  
#RURAL       :12913   1st Qu.:1.00   1st Qu.: 250000   1st Qu.: 2.000  
#BOGOTA      : 9466   Median :3.00   Median : 400000   Median : 3.000  
#MEDELLIN    : 8568   Mean   :2.49   Mean   : 448287   Mean   : 3.303  
#CALI        : 6422   3rd Qu.:3.00   3rd Qu.: 530000   3rd Qu.: 4.000  
#BARRANQUILLA: 6313   Max.   :6.00   Max.   :4000000   Max.   :28.000  
#(Other)     :93047 

#### YA QUEDO MEJOR VALOR_ARRIENDO


#entonces ahora hay que hacer exactamente lo mismo en test (logica= en la vida real primero hariamos esta limpieza y luego dividiriamos la base en train y test)
lower_boundtest <- quantile(test_hogares$valor_arriendo, 0.07) 
lower_boundtest


upper_boundtest <- quantile(test_hogares$valor_arriendo, 0.99) 
upper_boundtest #dan valores ligeramente diferentes a los de train precisamente porque no son los mismos datos

#duplico base
test_hogares_f<- test_hogares

test_hogares_f <- test_hogares_f %>% subset(valor_arriendo >= lower_boundtest) 
test_hogares_f <- test_hogares_f %>% subset(valor_arriendo <= upper_boundtest) 

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



#cuales son las variables que aparecen tanto en train como en test
################personas

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

#analicemos jefes de hogar###########

summary(train_personas$P6050)

#Ajustamos el comportamiento de afiliado para que no sabe, no responda sea 0
train_personas <- train_personas %>% 
  mutate(P6090 = if_else(train_personas$P6090==1, 1, 0))
summary(train_personas$P6090) #93% de los encuestados están ailiados al sistema de salud, es probable que personas pobres estén afiliadas


#voy a crear una variable de solo jefe de hogar
train_personas <- train_personas %>% 
  mutate(jefe_hogar = if_else(train_personas$P6050==1, 1, 0))

summary(train_personas$jefe_hogar) #30% de los encuestados son jefes de hogar

train_personas_f<- train_personas

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
#por lo tanto vemos que en realidad hay UN missing value (total individuos 164960) #no es representativo

#como no podemos usar las tres variables porque habria multicolinearidad, voy a ponerle valor de 0 a todos los missings de Oc
#porque a partir del analisis anterior sabemos que realmente esos missings corresponden a gente que no esta trabajando
#y no son verdaderos missings

summary(train_personas_f$Oc)
class(train_personas_f$Oc)
head(train_personas_f$Oc)

train_personas_f$Oc[is.na(train_personas_f$Oc)] <- 0 #aqui lo que hice fue cambiar los NA de Oc por valor de cero
head(train_personas_f$Oc)  #comprobar que quedo bien

#voy a crearle variable de mujer
train_personas_f <- train_personas_f %>% 
  mutate(mujer = if_else(train_personas_f$P6020==2, 1, 0))

#educacion es factor
sum(is.na(train_personas_f$P6210)) #no hay missing values en educ
train_personas_f$P6210 <- as.factor(train_personas_f$P6210)       
class(train_personas_f$P6210)


#ahora veamos como queda

train_p4 <- select(filter(train_personas_f),c(P6040, Oc, P6210, mujer)) #quite Pet para centrarnos en Oc
tablep4 <- summary(train_p4) 
tablep4

#    P6040             Oc                           P6210                  mujer       
#Min.   : 11.0   Min.   :0.0000   Ninguno                    : 8603   Min.   :0.0000  
#1st Qu.: 37.0   1st Qu.:0.0000   Preescolar                 :   13   1st Qu.:0.0000  
#Median : 49.0   Median :1.0000   B?sica primaria (1o - 5o)  :46619   Median :0.0000  
#Mean   : 49.6   Mean   :0.7107   B?sica secundaria (6o - 9o):21616   Mean   :0.4164  
#3rd Qu.: 61.0   3rd Qu.:1.0000   Media (10o - 13o)          :43028   3rd Qu.:1.0000  
#Max.   :108.0   Max.   :1.0000   Superior o universitaria   :45061   Max.   :1.0000  
#No sabe, no informa        :   20                   

#VEMOS= el promedio de edad de jefes de hogar es 49 a?os, 71% estan empleados, 41% son mujeres
#otra cosa= vemos que hay outliers en edad, aun mas teniendo en cuenta que en esta base tenemos solo jefe de hogar

boxplot(train_personas$P6040, #original
        ylab = "edad"
)

lower_bound_j <- quantile(train_personas_f$P6040, 0.002) 
lower_bound_j #18

upper_bound_j <- quantile(train_personas_f$P6040, 0.998) 
upper_bound_j #87 #de esta manera sacamos esos valores locos de mas de 100 a?os

train_personas_f <- train_personas_f %>% subset(P6040 >= lower_bound_j) 

train_personas_f <- train_personas_f %>% subset(P6040 <= upper_bound_j)

boxplot(train_personas_f$P6040, #original
        ylab = "edad"
)
#SOLUCIONADOS LOS OUTLIERS DE EDAD


#Ahora voy a hacer exactamente lo mismo pero en test

test_personas <- test_personas %>% 
  mutate(P6090 = if_else(test_personas$P6090==1, 1, 0)) #salud

test_personas <- test_personas %>% 
  mutate(jefe_hogar = if_else(test_personas$P6050==1, 1, 0)) #jefe hogar

test_personas_f<- test_personas

test_personas_f <- test_personas_f %>% subset(jefe_hogar == 1) #aqui saque base con solo jefes de hogar

test_personas_f$Oc[is.na(test_personas_f$Oc)] <- 0

test_personas_f <- test_personas_f %>% 
  mutate(mujer = if_else(test_personas_f$P6020==2, 1, 0))

test_personas_f$P6210 <- as.factor(test_personas_f$P6210)       

lower_bound_jtest <- quantile(test_personas_f$P6040, 0.002) 
upper_bound_jtest <- quantile(test_personas_f$P6040, 0.998) 

test_personas_f <- test_personas_f %>% subset(P6040 >= lower_bound_jtest)
test_personas_f <- test_personas_f %>% subset(P6040 <= upper_bound_jtest) 



####Base de Hogares + Personas con todas las variables
train_hogares_personas <- train_hogares_f %>% left_join(train_personas_f,by="id")
summary(train_hogares_personas) 


is.na(train_hogares_personas)
colSums(is.na(train_hogares_personas))
colSums(is.na(train_hogares_personas))>0
colnames(train_hogares_personas)[colSums(is.na(train_hogares_personas))>0]

is.na(train_hogares_personas$mujer) 
sum(is.na(train_hogares_personas$mujer)) #no hay missing values








#2. Estadistica descriptiva
###############################






#cuales son las variables Y

#Ingtot (ingreso total)
#Pobre (1 es pobre) 

##^recordar que ninguna de las 2 esta en test




##Transformamos las variables categóricas que vamos a usar para que sean de tipo factor (esto puede variar con las variables que escojamos)

train_hogares <- train_hogares %>%
                 mutate_at(.vars = c("Clase","Dominio","P5090", "Pobre", "Indigente"), .funs = factor)

train_personas <- train_personas %>%
                  mutate_at(.vars = c("P6020", "P6090", "P6050", "P6210", "Pet", "Oc"), .funs = factor)





##Volver dummies las variables categoricas >> aqui no entiendo si toca hacerlo para las dos bases 


fem<-ifelse(train_personas$P6020==2,1,0)
jefe<-ifelse(train_personas$P6050==1,1,0)
ocu<-ifelse(is.na(train_personas$Oc)==1,1,0)
edad<-train_personas$P6040
Ingtot<- train_hogares$Ingtot
menoreshogar<-ifelse(train$edad<18,1,0)
afiliado<-ifelse(train_personas$P6090==2,1,0) #aqui falta que el 9 debería ser na creo
#Estrato1: 1,2y3 son bajos según el DNP pero no se si necesariamente pobre (bajo y bajo-bajo son 1 y 2, 3 es medio-bajo)
#maxeduc P6210
#vivienda propia P9050
#falta incluir la de educación y estrat, no estoy segura como incluir mas de una 


#Se resumen las variables que se encuentran por persona para que estén en unidades compatibles con la de hogares
DB<-train_personas %>% group_by(id) %>% summarise(femhog = sum(fem),
                                        ocuhog = sum(ocu),
                                        menoreshog= sum(menores),
                                        Ingtothog = sum(Ingtot),
                                        afiliadoshog = sum(afiliado),
                                        estratohog=mean(estrato),
                                        maxeduchog= sum(maxeduc)
) 

##se unen las bases para tener una sola de entrenamiento
train_hogares <-train_hogares %>% left_join(DB,by="id")


##Escalar variables (?)

##Dividir la base en 3: Entrenamiento, evaluacion y prueba

##Primero partimos la base de entrenamiento, se parte en 80% - 20% por ser la forma en la que se comporta la variable de pobreza 
set.seed(123)
split1<-createDataPartition(DB$Pobre = .8)[[1]]
length(split1)

other<-DB[-split1]
training <-DB[split1]

##Ahora partimos para obtener las bases de evaluacion y prueba
split2<- createDataPartition(other$Pobre, p=1/3) [[1]]
evaluation<-other[split2]
testing<-other[-split2]


##Estadísticas descriptivas: (SE HACE ANTES O DESPUES DE DIVIDIR LA BASE EN 3?)

skim(train_hogares)
stargazer(train[c( )], type = "text")


###################
##Predecir pobreza


##Classification Models: (este es pobre 1 o 0)#######################################

#########################
###   AdaBoost   ########
#########################

#install.packages("fastAdaboost")

#en clasificacion nuestra variable Y es si es pobre o no es pobre

summary(train_hogares_f$Pobre) #19% pobres


#le voy a lanzar un monton de variables a ver cuales pone como importantes
set.seed(123)
adaboost <- train(
  Pobre ~amount+installment+age+ historygood + historypoor + purposeusedcar+ purposegoods.repair + purposeedu + foreigngerman + rentTR
  data = train_hogares_total, #aqui poner la base final
  method = "adaboost",
  trControl = ctrl,
  family = "binomial",
  metric = "Sens",
  #preProcess = c("center", "scale")
)








##modelos de ROC, AUC, False positives, False negatives 

#A detailed explanation of the final chosen model. The explanation must include how the model was trained, hyper-parameters selection, and other relevant information.
#∗ Include comparisons to at least 5 other models. You can compare them in terms of ROC, AUC, False Positives, or False Negatives.
#∗ Describe the variables that you used in the model and a measure of their relative importance in the prediction.
#∗ Describe any sub-sampling approach used to address class imbalances.


##Income regression Models (este es predecir el ingreso)###################################
#A detailed explanation of the final chosen model. The explanation must include how the model was trained, hyper-parameters selection, and other relevant information.
#∗ Include comparisons to at least 5 other models. Compare them in terms of MSE.
#∗ Convert the predicted income to a binary indicator and show the performance in terms of the ROC, AUC, False Positives, or False Negatives.
#∗ Describe the variables that you used in the model and a measure of theirrelative importance in the prediction.
