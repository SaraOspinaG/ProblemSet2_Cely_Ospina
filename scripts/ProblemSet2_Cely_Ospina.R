###################################
## Big Data - Problem Set 2 #######
# Maria Camila Cely , Sara Ospina #
###### Julio 2022 #################
###################################


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
       gamlr,        # cv.gamlr >la que nos permite hacer lasso
       class,
       ggplot2,
       skimr,
       rvest,
       dplyr,
       stargazer)       


##cargar los datos

##Establecer el directorio
setwd("C:/Users/SARA/Documents/ESPECIALIZACIÓN/BIG DATA/GITHUB/ProblemSet2_Cely_Ospina")
#setwd(aqui pon el tuuyo)

##Aquí no estoy segura si las bases antes de trabajarlas van en la carpeta de stores o en una carpeta de Data
##traer las bases de train y de test
train_hogares<-readRDS("stores/train_hogares.Rds")
train_personas<-readRDS("stores/train_personas.Rds")
test_hogares<-readRDS("stores/test_hogares.Rds")
test_personas<-readRDS("stores/test_personas.Rds")

##1. Limpieza de datos: 
###################

##escoger las variables que vamos a usar en el modelo, propongo estas pero revisar: 


#De la base de hogares: (revisar que estén en train y test)
#Ingtot (ingreso total ) (no está en TEST entonces no se) <<<<<<<esta es la Y para el ejecricio de predicción del ingreso,creo
#Nper (personas en el hogar)(esta no estoy segura)
#Clase (si está en cabecera o no, no se si aquí tambien podemos correr efectos fijos o algo así)
#Dominio (cual cabecera)
#P5090 (Si la vivienda ocupada es propia u otro)
#Pobre (1 es pobre) Esta variable esta en la de train, no en la de test <<<<<<<esta es la Y para el ejecricio de pobreza,creo

#De la base personas: no se si aquí se sacarían promedios o sumas para unirla con la de hogares (revisar que estén en train y test)
#Estrato1 (estrato socioeconómico)
#P6020 (sexo)
#P6040 (edad)
#P6050 (parentesco con el jee de hogar - identificar si es jefe de hogar) (no se si es necesario porque se mide la pobreza del hogar)
#P6090 (esta afiliado o beneficiario de salud)
#Pet (población en edad de trabajar)
#Oc (Ocupado)
#P6210 (max nivel educativo)

##Aquí no se si deberiamos quitar variables que no vamos a usar, eso me quedo de duda del pasado, algo como un drop en stata


##Transformamos las variables categóricas que vamos a usar para que sean de tipo factor (esto puede variar con las variables que escojamos)

train_hogares <- train_hogares %>%
                 mutate_at(.vars = c("Clase","Dominio","P5090", "Pobre", "Indigente"), .funs = factor)

train_personas <- train_personas %>%
                  mutate_at(.vars = c("Estrato1", "sexo", "P6090", "P6050", "P6210", "Pet", "Oc"), .funs = factor)


##Revisar NA por variables y quitar las que no sean necesarias

#calclular la cantidad de na por cada variable, creo que si son mas de x porcentaje se quita la variable 



##Volver dummies las variables categoricas >> aqui no entiendo si toca hacerlo para las dos bases 


fem<-ifelse(train_personas$P6020==2,1,0)
jefe<-ifelse(train$P6050==1,1,0)
ocu<-ifelse(is.na(train_personas$Oc),0,1)
edad<-train_personas$P6040
Ingtot<- train_hogares$Ingtot
menoreshogar<-ifelse(train$edad<18,1,0)
afiliado<-ifelse(train_personas$P6090==2,1,0) #aqui falta que el 9 debería ser na creo
#estrato
#maxeduc
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



##Estadísticas descriptivas


###################
##Predecir pobreza


##Classification Models: (este es pobre 1 o 0)

##modelos de ROC, AUC, False positives, False negatives 

#A detailed explanation of the final chosen model. The explanation must include how the model was trained, hyper-parameters selection, and other relevant information.
#∗ Include comparisons to at least 5 other models. You can compare them in terms of ROC, AUC, False Positives, or False Negatives.
#∗ Describe the variables that you used in the model and a measure of their relative importance in the prediction.
#∗ Describe any sub-sampling approach used to address class imbalances.


##Income regression Models (este es predecir el ingreso)
#A detailed explanation of the final chosen model. The explanation must include how the model was trained, hyper-parameters selection, and other relevant information.
#∗ Include comparisons to at least 5 other models. Compare them in terms of MSE.
#∗ Convert the predicted income to a binary indicator and show the performance in terms of the ROC, AUC, False Positives, or False Negatives.
#∗ Describe the variables that you used in the model and a measure of theirrelative importance in the prediction.
