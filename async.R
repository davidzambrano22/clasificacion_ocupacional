library(furrr)
library(progressr)
source("text_processing.R")


positions.sim <- list()
similarities <- list()


options(future.globals.maxSize = 1000000000)
#plan(multisession, workers = 2)

results <- future_map(args_set, get_similarities)

get_similarities <- function(args){
  results_1 <- future_map(args, ~calculate.similarities(args[[1]], args[[2]]))
  return(results_1)
}

calculate.similarities <- function(job_index, den.index){
  vec_o <- offer_vectors[job_index, 1:dim(offer_vectors)[2]]
  vec_d <- denominations_vectors[den.index, 1:dim(denominations_vectors)[2]]
  sim <- dist(rbind(vec_o, vec_d))
  return(sim)
}



#Formas de crear la lista de argumentos-----------------------------------

args_set <- list()

#Esta lista devuelve un total de 
n <- 1
for (i in 1:10){
  for (j in 1:length(names)){
    args_set[[n]] <- list(i, j)
    print(sprintf("job = %1$d, den = %2$d", i, j))
    n <- n+1
  }
}

args_set[[1]]


args_sets <- list()
for (i in 1:10){
  args_sets[[i]] <- list(i, c(1:length(names)))
}
args_sets
