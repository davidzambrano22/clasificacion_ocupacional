library(RTextTools)
library(tm)
library(readr)
library(readxl)
library(haven)
library(foreign)

library(dplyr)
library(remotes)
library(readxl)

#Se carga el dataset con las denominaciones y descripciones de Cuoc
path.denominations <- "base_cuoc/cuoc.xlsx"
Base_pruebav1 <- read_excel(path.denominations, sheet = "Denominación")
  
nrows.Base_pruebav1 <- dim(Base_pruebav1)[1]


#Se carga la información de las vacantes y la descripción
data_computrabajo <- as_tibble(read.csv("datasets/Computrabajo/computrabajo_2023-01-31T00-45-05.csv"))[1:20,] %>%
  select(titulo, descripcion) 
View(data_computrabajo)

#Se crean los vectores con información de los titulos y las descripciones y a partir de ellos se crea un dataframe
titulos.vector <- c(Base_pruebav1$`Nombre Denominación`, data_computrabajo$titulo)
descripciones.vector <- c(Base_pruebav1$descripcion, data_computrabajo$descripcion)
cod.5 <- c(Base_pruebav1$Ocupación, rep(0, 20))

base <- data.frame(titulos = titulos.vector, descripciones = descripciones.vector, codigo = cod.5)
View(base)

#Se realiza limpieza y se crea  el documento matriz-termino
doc_matrix <- create_matrix(cbind(base$titulos), language="spanish", removeNumbers=TRUE, removeSparseTerms=.998, removePunctuation=TRUE, stripWhitespace=TRUE, toLower=TRUE, removeStopwords=TRUE, stemWords=TRUE,)


#En la matriz las lineas equivalen a cada texto, en las columnas se encuentran todas las palabras que 
#se encuentran en todos los textos. En la casilla aparece la frecuencia con que una palabra aparece en 
#un texto
matriz<-as.matrix(doc_matrix)
View(matriz)
dim(matriz)


#CREA LOS CONTAINERS <- a list of objects that will be fed to the machine learning algorithms in the next step.
container <- create_container(doc_matrix, base$codigo, trainSize=1:14752,
                              testSize=14753:14772, virgin=FALSE)


################################################
# Entrenamiento
###############################################
#SVM <- train_model(container,"SVM")
SLDA <- train_model(container,"SLDA")
BOOSTING <- train_model(container,"BOOSTING")
BAGGING <- train_model(container,"BAGGING")
RF <- train_model(container,"RF")
NNET <- train_model(container,"NNET")


################################################
# TESTING
###############################################
SVM_CLASSIFY <- classify_model(container, SVM)
SLDA_CLASSIFY <- classify_model(container, SLDA)
BOOSTING_CLASSIFY <- classify_model(container, BOOSTING)
BAGGING_CLASSIFY <- classify_model(container, BAGGING)
RF_CLASSIFY <- classify_model(container, RF)
NNET_CLASSIFY <- classify_model(container, NNET)



analytics <- create_analytics(container,
                              cbind(SLDA_CLASSIFY,
                                    RF_CLASSIFY, NNET_CLASSIFY,
                                    BAGGING_CLASSIFY))

summary(analytics)


# CREATE THE data.frame SUMMARIES
topic_summary <- analytics@label_summary
alg_summary <- analytics@algorithm_summary
ens_summary <-analytics@ensemble_summary
doc_summary <- analytics@document_summary


write.csv(analytics@document_summary, "DocumentSummaryp2.csv")
write.csv(analytics@ensemble_summary, "ensemble_summary2.csv")
write.csv(analytics@algorithm_summary, "algorithm_summary2.csv")

SVM <- cross_validate(container, 4, "SVM")
SLDA <- cross_validate(container, 4, "SLDA")
RF <- cross_validate(container, 4, "RF")
NNET <- cross_validate(container, 4, "NNET")
BOOSTING <- cross_validate(container, 4, "BOOSTING")
BAGGING <- cross_validate(container, 4, "BAGGING")

