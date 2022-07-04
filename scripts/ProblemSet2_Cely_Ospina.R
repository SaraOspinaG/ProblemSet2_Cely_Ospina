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

## Llamar/instalar las librerías 
require(pacman)
p_load(tidyverse,    #Limpiar datos
       caret, 
       rio, 
       modelsummary, # msummary
       gamlr,        # cv.gamlr >la que nos permite hacer lasso
       class,
       ggplot2,
       skimr,
       rvest,
       dplyr,
       stargazer)       


##cargar los datos

setwd("C:\Users\SARA\Documents\ESPECIALIZACIÓN\BIG DATA\GITHUB\ProblemSet2_Cely_Ospina\scripts")

##Aquí no estoy segura si las bases antes de trabajarlas van en la carpeta de stores o en una carpeta de Data
##traer las bases de train y de test
train_hogares<-readRDS("stores/train_hogares.Rds")
train_personas<-readRDS("stores/train_personas.Rds")
test_hogares<-readRDS("stores/test_hogares.Rds")
test_personas<-readRDS("stores/test_personas.Rds")


##Unir las bases de train y de test para hogares y personas 

train<-merge(train_hogares,train_personas, by="id")
test<-merge(test_hogares,test_personas, by="id")

##Volver factor las variables categóricas



##Revisar NA por variables y escoger las variables que vamos a usar en el modelo



##Volver dummies las variables categoricas 

##Escalar variables (?)



##Estadísticas descriptivas


###################
##Predecir pobreza


##Classification Models: 

##modelos de ROC, AUC, False positives, False negatives 

#A detailed explanation of the final chosen model. The explanation must include how the model was trained, hyper-parameters selection, and other relevant information.
#∗ Include comparisons to at least 5 other models. You can compare them in terms of ROC, AUC, False Positives, or False Negatives.
#∗ Describe the variables that you used in the model and a measure of their relative importance in the prediction.
#∗ Describe any sub-sampling approach used to address class imbalances.


##Income regression Models
#A detailed explanation of the final chosen model. The explanation must include how the model was trained, hyper-parameters selection, and other relevant information.
#∗ Include comparisons to at least 5 other models. Compare them in terms of MSE.
#∗ Convert the predicted income to a binary indicator and show the performance in terms of the ROC, AUC, False Positives, or False Negatives.
#∗ Describe the variables that you used in the model and a measure of theirrelative importance in the prediction.
