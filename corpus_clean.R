setwd('~/Review - Homo E/homo_review') 
library(easypackages) 
libraries("litsearchr","tidyverse", "dplyr", "readr", "revtools", "RColorBrewer", "ggplot2", "tidytext")
##importing OG corpus & setting up for scoring 
mendeley <-litsearchr::import_results(directory = 'mendeleylib', verbose = TRUE)
mendeley <-litsearchr::remove_duplicates(mendeley, field = "title", method = "string_osa") 
train <- mendeley %>% select(-(doi:n_duplicates))  
train <- train %>% select(-(volume:end_page)) 
train<- train %>% select(-(source_type))
write.csv(train, './training_files.csv') 
##append data wanted in excel 
train_data <- read.csv('./training_files.csv')
train_data <- train_data %>% select(-(X)) 
##score in excel & add new papers found from WoS and references 
#reading in full corpus  
corpus<- read.csv('./corpus.csv') 
#identifying location labels 
titles<- corpus[c('title')] 
abstracts<- corpus[c('abstract')]  
#write titles & abstracts into csv, then copy into plain .txt
write.csv(titles, './titles.csv')  
write.csv(abstracts, './abstracts.csv')  
#read in plain text and tokenize   
titles_orig <- readLines('./titles.txt') 
titles_df <- tibble(line = 1:160, text = titles_orig) 
titles_tidy <- titles_df %>% unnest_tokens(word, text)  
#x&y have no common variables = no stop_words in titles to remove
title_count <- titles_tidy %>% count(word, sort = TRUE)  
write.csv(title_count, './title_count.csv') 
#repeat for abstracts
abstracts_orig <-readLines('./abstracts.txt') 
abstracts_df <- tibble(line = 1:150, text = abstracts_orig) 
abstracts_tidy <- abstracts_df %>% unnest_tokens(word, text)  
data(stop_words) 
abstracts_tidy <- abstracts_tidy %>% anti_join(stop_words) 
abstracts_count <- abstracts_tidy %>% count(word, sort = TRUE)  
write.csv(abstracts_count, './abstracts_count.csv') 
