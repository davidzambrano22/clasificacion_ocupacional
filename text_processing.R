library(tokenizers)
library(tm)
library(SnowballC)


# Función para el procesamiento de texto ----------------------------------


filepath <- "vacias.txt"
palabras <- scan(filepath, character(), quote = "")
palabras_wboundaries <- gsub("(.*)","\\\\b\\1\\\\b",palabras)
regex_filtro <- paste(palabras_wboundaries, collapse = "|")


#Procesa los textos para crear los vectores de conteos
text_processing <- function(text){
  text <- gsub(regex_filtro,"",text)
  text <- iconv(text, to="ASCII//TRANSLIT")
  palabras <- tokenize_words(text) #Tokeniza y quita caracteres especiales
  palabras <- str_c(unlist(palabras),collapse = ", ") #Vuelve la lista un vector y la colapsa en cadena
  text <- gsub(",", " ", palabras) #Remueve comas
  text <- str_replace_all(text, "[^A-Za-z\\s]", "")
  text <- gsub("[0-9]+", "", text) #Remueve números
  text <- removeWords(text, words = stopwords("spanish"))
  
  corpus <- Corpus(VectorSource(text))
  # corpus <- tm_map(corpus, content_transformer(tolower))
  # corpus <- tm_map(corpus, function(x) iconv(enc2utf8(x), sub = "byte"))
  corpus <- tm_map(corpus, stemDocument, language = "spanish")
  
  lemmatized_text <- sapply(corpus, as.character)
  return(lemmatized_text)
}


# Funciones de similaridad ------------------------------------------------

#Cosine similarity
cosine_similarity <- function(v1, v2) {
  dot_product <- sum(v1 * v2)
  magnitude_v1 <- sqrt(sum(v1^2))
  magnitude_v2 <- sqrt(sum(v2^2))
  return (dot_product / (magnitude_v1 * magnitude_v2))
}




# Funciones para mapear códigos  ------------------------------------------

#Devuelve el nombre de los códigos ESCO
get.code.name<- function(position){
  if (position == 0){
    return(NULL)
  }
  return(denominations$`Nombre Denominación`[position])
}

#Devuelve los códigos ESCO
get.code<- function(position){
  if (position == 0){
    return(NULL)
  }
  return(denominations$Ocupación[position])
}



# Funciones para proceso vectorizado --------------------------------------

library(stringr)

#Computa la similaridad entre vectores numéricos de ofertas y denominaciones
match.offer <- function(X){
  X <- as.vector(str_split_fixed(X, pattern = "", n = nchar(X)))
  X <- as.numeric(X[! X %in% " "])
  similarity <- 0
  for (deno.vector in 1:dim(denominations_vectors)[1]){
    vect <- unlist(denominations_vectors[deno.vector,])
    sim <- cosine_similarity(X, vect)
    if (sim > similarity){similarity <- sim}
  }
  return(similarity)
}

#Reemplaza los dígitos superiores a 1 por 1
fix_count_flag <- function(str) {
  return (str_replace_all(str, "[^01 ]","1"))
}