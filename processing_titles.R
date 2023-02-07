library(tokenizers)
library(dplyr)
library(tidyverse)
library(tm)
library(readxl)
library(lsa)
source("text_processing.R")

#Lee y procesa los títulos  las vacantes de computrabajo en un dataframe

data_computrabajo <- as_tibble(read.csv("datasets/Computrabajo/computrabajo_2023-01-31T00-45-05.csv")) %>%
  select(titulo) 
# View(data_computrabajo)

#Lee y procesa los títulos las vacantes de elempleo en un dataframe

# data_elempleo <- as_tibble(read.csv("datasets/Elempleo/elempleo_2023-02-01T19-39-03.csv")) %>%
#   select(titulo) 
# View(data_elempleo)

#Se selecciona las primeras 20 ofertas para trabajar

data <- data_computrabajo[1:20,] %>%
  mutate(tokenized_title = lapply(titulo, text_processing))
# View(data)

n_records <- dim(data)[1]


#Lee las denominaciones de los trabajos y los guarda en un dataframe
path.denominations <- "base_cuoc/cuoc.xlsx"
denominations <- read_excel(path.denominations, sheet = "Denominación") %>%
  mutate(denominaciones = lapply(`Nombre Denominación`, text_processing))
names <- denominations$`denominaciones` #Extrae los nombres de las denominaciones

# Create corpus ---------------------------------------------

#Create a dataframe with the encoded strings

n_jobs <- 10
corpus <- Corpus(VectorSource(c(data$tokenized_title[1:n_jobs], names))) #Concatena titulos de las ofertas con los nombres de las denominaciones en un corpus
corpus <- tm_map(corpus, stemDocument)

dtm <- as.matrix(DocumentTermMatrix(corpus))#Regresa una matriz que contiene los vectores numéricos de ofertas y denominaciones
dtm <- str_replace_all(dtm, "[^01 ]","1")
View(dtm)

#Se separan dataframes de vectores y ofertas y denominaciones
offer_vectors <- as.data.frame(dtm[1:n_jobs,])
denominations_vectors <- as.data.frame(dtm[(n_jobs + 1):dim(dtm)[1],]) 
  
rownames(denominations_vectors) <- 1:dim(denominations_vectors)[1]

# Match word vectors asynchronously------------------------------------------------------
library(furrr)
library(progressr)


positions.sim <- list()
similarities <- list()


n_offers <- 1:10
options(future.globals.maxSize = 1000000000)
plan(multisession, workers = 2)

with_progress({
  p <- progressor(steps = length(n_offers))
  results <- future_map(n_offers, get_similarities, p = p)
})

get_similarities <- function(job_index, p){
  p()
  similarity <- 0
  max.sim.position <- 0
  
  for (row_d in 1:dim(denominations_vectors)[1]){
    vec_o <- offer_vectors[job_index, 1:dim(offer_vectors)[2]]
    vec_d <- denominations_vectors[row_d,1:dim(denominations_vectors)[2]]
    sim <- dist(rbind(vec_o, vec_d))
    if (sim > similarity){
      similarity <- sim
      max.sim.position <- row_d
    }
  }
  return(c(similarity, max.sim.position))
}

# Match word vectors asynchronously in a for loop--------------------------

#Itera sobre cada oferta de empleo y sobre cada denominación para hallar similaridad coseno
#Devuelve la similaridad más alta encontrada para cada empleo
#Devuelve la posici[on que corresponde a la denominación para la que la similitud con el empleo es más alta


positions.sim <- list()
similarities <- list()
for (row_o in 1:dim(offer_vectors)[1]){
  cat("---------->", row_o, "\n")
  similarity <- 0
  max.sim.position <- 0
  for (row_d in 1:dim(denominations_vectors)[1]){
    vec_o <- offer_vectors[row_o, 1:dim(offer_vectors)[2]]
    vec_d <- denominations_vectors[row_d,1:dim(denominations_vectors)[2]]
    sim <- dist(rbind(vec_o, vec_d))
    cat(sprintf("Similarity %1$d-%2$d=",row_o, row_d),similarity, "\n")
    if (sim > similarity){
      similarity <- sim
      max.sim.position <- row_d
    }
  }
  positions.sim[[row_o]] <- max.sim.position
  similarities[[row_o]] <- similarity 
}


# impute job code ---------------------------------------------------------

jobs_codes <- data.frame(similaridad = unlist(similarities)
                         , position = unlist(positions.sim)) %>%
  mutate(titulo = data$tokenized_title[1:20],
         codigo_ESCO = lapply(position, get.code),
         Nombre_denominación = lapply(position, get.code.name))
View(jobs_codes)


# Testing vectorized matching ------------------------------------------------------------------

ofertas_codigos <- data.frame(Index = 1:dim(data)[1]) %>%
                                mutate(titulo = data$tokenized_title, titulo.vectorizado = do.call("paste", offer_vectors)) 
ofertas_codigos$similarity <- lapply(ofertas_codigos$titulo.vectorizado, match.offer)

sample_str <- ofertas_codigos$titulo.vectorizado[64]
str_replace_all(sample_str, "[^01 ]","1")

View(ofertas_codigos)

ofertas_codigos$titulo.vectorizado <- ofertas_codigos$titulo.vectorizado %>% 
  lapply(fix_count_flag)


