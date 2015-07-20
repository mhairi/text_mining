library(stringr)
library(dplyr)
library(slam)

###################### Cleaning ####################

stopwords <-  function(language){
  if (language == 'english'){
    return(c('and', 'or', 'if', 'i', 'in', 'an'))  
  }
  if (language == 'french'){
    return(c('et', 'je')) 
  }
}

clean_lower <- function(text_vector){
  tolower(text_vector)
}

clean_whitespace <- function(text_vector){
 str_replace_all(text_vector, '//s', ' ')
}

clean_numbers <- function(text_vector){
 str_replace_all(text_vector,  '[0-9]', '')
}
  
clean_punctuation <- function(text_vector){
 str_replace_all(text_vector, '[[:punct:]]', '') 
}

clean_stopwords <- function(text_vector, words = stopwords('english')){
  suppressWarnings(str_replace_all(text_vector, words, ''))
  # Warns about recycling the final argument of str_replace_all
}

############### Tokenise ##################################
  

tokenise <- function(text_vector, method = 'remove_punctuation'){
  # Turn text into word tokens
  
  # Make sure this is character, not factor
  text_vector <- as.character(text_vector)
  
  # Tokenise, ignoring punctuation
  if (method == 'remove_punctuation'){
   return(str_split(text_vector, boundary('word')))
  }
  
  # Tokenise, keeping punctuation associated with word
  if (method == 'keep_punctuation'){
   return(str_split(text_vector, ' ')) 
  }
}



################# Word count ##########################


word_count <- function(tokens){
data.frame(words = tokens %>% unlist) %>%
  group_by(words) %>%
  summarise(count = n()) %>%
  arrange(-count)
}



############## Term Document Matrix ######################

term_document_matrix <- function(tokens){
 
  # What are the unique documents?
  documents <- as.character(1:length(tokens))
  
  # What are the unique terms?
  terms <- tokens %>% unlist %>% unique %>% sort
  
  # How big does this matrix need to be
  ndocs <-  length(documents)
  nterms <- length(terms)
  
  # Build up i, j and v for each document
  i <- numeric(0)
  j <- numeric(0)
  v <- numeric(0)
  
  for (doc in seq_along(tokens)){
    # Word count within a document
    temp_df <- word_count(tokens[doc])
    
    # Get values of j, i
    temp_i <- match(temp_df$words, terms)
    temp_j <- rep(doc, nrow(temp_df))
    temp_v <- temp_df$count
    
    # Add to full list  
    i <- c(i, temp_i)
    j <- c(j, temp_j)
    v <- c(v, temp_v)
  }
  
  return( 
  simple_triplet_matrix(i, j, v,
                        nrow     = nterms,
                        ncol     = ndocs,
                        dimnames = list(terms     = terms,
                                        documents = documents))
  )
}


##############
# Trying out #
##############

test_data <- c('an example of text',
               'more text input you might have',
               'text!!!',
               '2 sentences of text. In this document')

test_data2 <- as.factor(test_data)

test_data %>%
  clean_lower %>%
  clean_whitespace %>%
  clean_numbers %>%
  clean_punctuation %>%
  clean_stopwords %>%
  tokenise

test_tokens <- 
test_data %>%
  clean_lower %>%
  clean_whitespace %>%
  clean_numbers %>%
  clean_punctuation %>%
  clean_stopwords %>%
  tokenise

tokenise(test_data, method = 'keep_punctuation')
tokenise(test_data2)

word_count(test_tokens)
word_count(test_data %>% tokenise)

term_document_matrix(test_tokens) %>% as.matrix

######## Larger Data ###############

test_data3 <- read.csv('reviews.csv')

tokens <-
test_data3$text %>%
  clean_lower %>%
  clean_whitespace %>%
  clean_punctuation %>%
  clean_stopwords %>%
  tokenise

tokens %>% word_count

tokens %>% term_document_matrix
  
  