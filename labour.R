library(labourR)
library(dplyr)
library(readxl)
source("text_processing.R")

data_computrabajo <- as_tibble(read.csv("datasets/Computrabajo/computrabajo_2023-01-31T00-45-05.csv")) %>%
  select(titulo) %>% 
  mutate(tokenized_title = lapply(titulo, text_processing))


corpus_frame <- data.frame(id = 1:dim(data_computrabajo)[1]) %>% 
  mutate(text = data_computrabajo$tokenized_title)


classification <- classify_occupation(corpus_frame, isco_level = 3, lang = "es", num_leaves = 5)
rownames(classification) <- classification$id


#Lee las denominaciones de los trabajos y los guarda en un dataframe
path.denominations <- "base_cuoc/20220829 Base CUOC Vs 2022.xlsx"
denominations <- read_excel(path.denominations, sheet = "Denominación")
names <- denominations$`Grupo Primario`

# comparación de códigos --------------------------------------------------

positions.sim <- list()
similarities <- list()
for (n in 1:length(classification$iscoGroup)){
  cat("---------->", n, "\n")
  code <-  classification$iscoGroup[n]
  code <- as.numeric(str_split_fixed(code, pattern = "", n = nchar(code)))
  similarity <- 0 
  sim.position <- 1
  for (i in 1:length(names)){
    occupation <- names[i]
    occupation <- as.numeric(str_split_fixed(occupation, pattern = "", n = nchar(occupation)))[1:3]
    sim <- cosine_similarity(code, occupation)
    if (sim > similarity){
      similarity <- sim
      sim.position <- i
    }
    cat(sprintf("Similarity %1$d-%2$d=",n, i),similarity, "\n")
  }
  positions.sim[[n]] <- sim.position
  similarities[[n]] <- similarity 
}

# impute job code ---------------------------------------------------------

classification <- classification[1:133,] %>% mutate(posicion.sim = unlist(positions.sim)) %>%
  mutate(codigo.ocupacion = lapply(posicion.sim, get.code),
         nombre.ocupacion = lapply(posicion.sim, get.code.name)) %>%
  transform(id = as.numeric(id)) %>% arrange(id) %>%
  mutate(descripcion = data_computrabajo[corpus_frame$id %in% classification$id,]$titulo)

classification$iscoGroup <- classification$preferredLabel <- classification$posicion.sim <-  NULL

View(classification[,c("id","descripcion","codigo.ocupacion","nombre.ocupacion")])



