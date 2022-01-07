setwd('~/Review - Homo E/homo_review') 
library(easypackages) 
libraries("litsearchr","tidyverse", "dplyr", "readr", "revtools", "RColorBrewer", "ggplot2", "tidytext",'pdftools', 'tm')
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
corpus<- read.csv('./fatcorpus_hypothesis.csv') 
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
## also extracted environmental terms from these csv files (manual) and re-checked terms in papers for bigrams & trigrams etc.##
#location labelling - country$X, gives all the terms for each group
locations <- read.csv("./locations.csv")  
locations <- locations %>% select(-('continent')) 
no_country <- locations %>% filter(country =='0')
algeria <- locations %>% filter(country =='algeria')
armenia <- locations %>% filter(country =='armenia')
azerbaijan <- locations %>% filter(country =='azerbaijan')
china <- locations %>% filter(country =='china')
dagestan<- locations %>% filter(country =='dagestan')
egypt<- locations %>% filter(country =='egypt')
ethiopia<- locations %>% filter(country =='ethiopia')
france<- locations %>% filter(country =='france')
georgia<- locations %>% filter(country =='georgia')
germany<- locations %>% filter(country =='germany')
greece<- locations %>% filter(country =='greece')
india<- locations %>% filter(country =='india')
indonesia<- locations %>% filter(country =='indonesia')
iran<- locations %>% filter(country =='iran')
israel<- locations %>% filter(country =='israel')
italy<- locations %>% filter(country =='italy')
kenya<- locations %>% filter(country =='kenya')
malawi<- locations %>% filter(country =='malawi')
malaysia<- locations %>% filter(country =='malaysia')
myanmar<- locations %>% filter(country =='myanmar')
netherlands<- locations %>% filter(country =='netherlands')
pakistan<- locations %>% filter(country =='pakistan')
philippines<- locations %>% filter(country =='philippines')
romania<- locations %>% filter(country =='romania')
russia <- locations %>% filter(country =='russia ')
saudi_arabia <- locations %>% filter(country =='	
saudi_arabia')
south_africa <- locations %>% filter(country =='south_africa')
spain<- locations %>% filter(country =='spain')
syria<- locations %>% filter(country =='syria')
taiwan<- locations %>% filter(country =='taiwan')
tanzania <- locations %>% filter(country =='tanzania')
turkey <- locations %>% filter(country =='	
turkey ')
uk <- locations %>% filter(country =='uk')
vietnam <- locations %>% filter(country =='vietnam')
yemen<- locations %>% filter(country =='yemen') 
 
## testing pdf_text function
testpdf<- pdf_text('./testpdf.pdf')  
fileConn<-file("output.txt")
writeLines(c(testpdf), fileConn)
close(fileConn) 
#download all skinny corpus pdfs into one location 
#list all pdf files and convert to txt (have to set wd to pdf directory for pdf_text to run)
setwd('~/Review - Homo E/homo_review/skinnypdfs')
skinny <- list.files(pattern = "pdf$")  
skinnytexts <- lapply(skinny, pdf_text)   
#reset wd & read in skinny corpus, simple search terms
setwd('~/Review - Homo E/homo_review') 
skinnycorpus <- read.csv('./skinnycorpus.csv') 
searchterms<- c("arid",
                "boreal",
                "bush*",
                "C4-dominated",
                "climate*",
                "cold",
                "conifer",
                "cool",
                "deciduous",
                "dense",
                "desert",
                "dry*",
                "envrionment*",
                "flexibility",
                "fluctuating",
                "forest*",
                "grass*",
                "herb*",
                "humid",
                "instability",
                "lake margin",
                "lakeshore",
                "mangrove",
                "marsh*",
                "meadow*",
                "mediterranean",
                "mesic",
                "moist",
                "moor*",
                "mosaic", 
                "mud",
                "open",
                "parkland",
                "plain",
                "rainforest", 
                "riparian",
                "savanna*",
                "scrub*",
                "seasonal*",
                "shrub*",
                "stepp*",
                "swamp*",
                "temperate",
                "tropical",
                "variabl*",
                "veget*",
                "warm",
                "wet",
                "wetland",
                "wood*",
                "xeric"
)
# search skinny texts for terms
searchmatrix <- sapply(searchterms, grepl, skinnytexts) 
# get list of what papers for each term 
write.csv(searchmatrix, './searchmatrix.csv') 
# manually check hits(full term, location, context (+ve/-ve)) & complete matrix

## setting up skinny texts for tm package use 
skincorp <- Corpus(URISource(skinny),
                   readerControl = list(reader = readPDF)) 
skincorp <- tm_map(skincorp, removePunctuation, ucp = TRUE)
skinny.tdm <- TermDocumentMatrix(skincorp, 
                                 control = 
                                   list(stopwords = TRUE,
                                        tolower = TRUE,
                                        stemming = TRUE,
                                        removeNumbers = TRUE,
                                        bounds = list(global = c(3, Inf)))) 

