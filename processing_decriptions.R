library(tokenizers)
library(dplyr)
library(tidyverse)
library(tm)
library(readxl)
source("text_processing.R")

#Lee y procesa los títulos  las vacantes de computrabajo en un dataframe

data_computrabajo <- as_tibble(read.csv("datasets/Computrabajo/computrabajo_2023-01-31T00-45-05.csv")) %>%
  select(descripcion) 
# View(data_computrabajo)

#Lee y procesa los títulos las vacantes de elempleo en un dataframe

data_elempleo <- as_tibble(read.csv("datasets/Elempleo/elempleo_2023-02-01T19-39-03.csv")) %>%
  select(descripcion_oferta) %>%
  mutate(descripcion = descripcion_oferta) %>% select(descripcion)
# View(data_elempleo)

#Concatena los dos dataframes

data <- rbind(data_computrabajo, data_elempleo)%>%
  mutate(tokenized_description = lapply(descripcion, text_processing))
# View(data)

n_records <- dim(data)[1]

#Lee las denominaciones de los trabajos y los guarda en un dataframe
path.denominations <- "base_cuoc/20220829 Base CUOC Vs 2022.xlsx"
denominations <- read_excel(path.denominations, sheet = "Denominación") %>%
  mutate(denominaciones = lapply(`Nombre Denominación`, text_processing))
names <- denominations$`denominaciones`


# Create corpus ---------------------------------------------

#Create a dataframe with the encoded strings
corpus <- Corpus(VectorSource(c(data$tokenized_description, names)))
corpus <- tm_map(corpus, stemDocument)

dtm <- as.matrix(DocumentTermMatrix(corpus))

offer_vectors <- as.data.frame(dtm[0:n_records,])
denominations_vectors <- as.data.frame(dtm[(n_records+1):dim(dtm)[1],])
rownames(denominations_vectors) <- 1:length(names)
# View(denominations_vectors[1:50,])


# Match word vectors ------------------------------------------------------

positions.sim <- list()
similarities <- list()
for (row_o in 1:dim(head(offer_vectors,20))[1]){
  cat("---------->", row_o, "\n")
  similarity <- 0
  max.sim.position <- 0
  for (row_d in 1:dim(denominations_vectors)[1]){
    vec_o <- offer_vectors[row_o, 1:dim(offer_vectors)[2]]
    vec_d <- denominations_vectors[row_d,1:dim(denominations_vectors)[2]]
    sim <- cosine_similarity(vec_o, vec_d)
    cat(sprintf("Similarity %1$d-%2$d=",row_o, row_d),similarity, "\n")
    if (sim > similarity){
      similarity <- sim
      max.sim.position <- row_d
    }
  }
  positions.sim[[row_o]] <- max.sim.position
  similarities[[row_o]] <- similarity 
}



