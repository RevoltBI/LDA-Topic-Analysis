
# Information -------------------------------------------------------------

#LDA topic model over user comments and replies. 


# Load Data ---------------------------------------------------------------

source("DataCleaning.R")

# Transformation functions ------------------------------------------------


#Do some exchanges - change date for #Date an CZK for #Money and @email for #email and tel.num for #Telephone Number

TextPreparation<-function(x) {x%>%str_to_lower%>%
               str_replace_all("\\\n"," ")%>%
               iconv(from="UTF-8",to="ASCII//TRANSLIT")%>%
               str_replace_all("[0-9]*(.)?(kc|,-|czk)"," #Penize ")%>%
               str_replace_all("[a-zA-Z0-9._-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,4}"," #Email ")%>%
               str_replace_all("(\\+420)? ?[1-9][0-9]{2} ?[0-9]{3} ?[0-9]{3}"," #TelefonniCislo ") %>%
               str_replace_all("(0?[1-9]|[12][0-9]|3[01])\\. ?(0?[1-9]|1[0-2])\\. ?20[0-9]{2} |(0?[1-9]|[12][0-9]|3[01])\\. ?(0?[1-9]|1[0-2])"," #Datum ")%>%
               str_replace_all("\\d{7}"," #ČisloSmlouvy ")%>%
               str_replace_all("\\d{3} ?\\d{2}( |\\.|\\,)"," #PSČ ")%>%
               str_replace_all("[^[:alnum:][:space:]#]"," ")%>%
               str_replace_all("[:digit:]"," ") }

#The same as above but truncates the email so it keeps just the last message. 
EmailPreparation<-function(x){
                   Text<- TextPreparation(x)

                    Result<-sapply(Text,function(BodyText){
                                str_trunc(BodyText,
                                      ifelse(
                                        is.na(
                                          str_locate(BodyText,"-----|\\[image: Inline image|from:")[,1]),
                                              str_length(BodyText),
                                                  str_locate(BodyText,"-----|\\[image: Inline image|from:")[,1])
                                              )
                                                          })
}
              
Tokenizer<-function(x){x%>%str_split("[[:space:]]+")  }    

# Text2Vec Vocab creation function  -----------------------------------------




create_vocabulary<-function(dataframe,preprocessor,tokenizer){
  
  library(text2vec)
  library(data.table)
  library(dplyr)
  
  if (!("text" %in% names(dataframe))|!("index" %in% names(dataframe)))
    stop("Make sure you have a 'text' column and an 'index' column in your dataframe ")
    
  
  #Initialize Data.Table
  setDT(dataframe)
  setkey(dataframe, Index)
  
}

#get the stopwords

cz_stopwords<-read_table("stopwords.txt",col_names = FALSE)[,1]%>%c%>%unlist

custom_terms<-c("sivakova","")

               # define preprocessing function and tokenization fucntion
               
               TextData = itoken(Messages$Text, 
                                 preprocessor = TextPreparation, 
                                 tokenizer = Tokenizer, 
                                 ids = Messages$Index, 
                                 progressbar = TRUE)
               
               vocab <- create_vocabulary(TextData, 
                                          stopwords = cz_stopwords,
                                          ngram = c(1L, 1L))
               
               vocab$vocab <- vocab$vocab %>%filter(nchar(terms)>2)


# Pruning and dtm creation ------------------------------------------------

               pruned_vocab <- prune_vocabulary(vocab, 
                                               term_count_min = 10, 
                                               doc_proportion_max = 0.6,
                                               doc_proportion_min = 0.001)
               
               vectorizer = vocab_vectorizer(pruned_vocab)
               
               dtm <- create_dtm(TextData, vectorizer)
               
               #create tables of top terms for stats
               TopTerms<-data.table(vocab)[1][[1]][[1]]%>%arrange(desc(doc_counts))
               
               PrunedTopTerms<-data.table(pruned_vocab)[1][[1]][[1]]%>%arrange(desc(doc_counts))

# LDA Model Fitting -------------------------------------------------------

lda_model = LatentDirichletAllocation$new(n_topics = 10, vocabulary = pruned_vocab,
                                          doc_topic_prior = 1,
                                          topic_word_prior = 0.2)
               
doc_topic_distr = lda_model$fit_transform(dtm, n_iter =1000, check_convergence_every_n = 5)
# run LDAvis visualisation if needed (make sure LDAvis package installed)
library(LDAvis)
lda_model$plot()

save.image()


# Analysis ----------------------------------------------------------------

#Look at particular topic member
Messages[doc_topic_distr[,1]>4,]%>%View

Topicfreq<-as.data.frame(apply(doc_topic_distr,1,which.max))
names(Topicfreq)[1]<-"topic"
View(group_by(Topicfreq,topic)%>%summarise(count=n()))



