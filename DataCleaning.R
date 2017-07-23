library(readr)
library(dtplyr)
library(dplyr)
library(stringr)

Messages <- read_csv("~/R/LDA-Topic-Analysis/Files/Messages.csv", 
                     locale = locale(), na = "NA", comment = "#")

#Anonymize dataset

Data<-Messages%>%filter(AgentId!="",MessageType=="Email",Direction=="I")%>%
                  mutate(Spam=str_detect(SubjectField,"SPAM")|str_detect(BodyText,"Trouble viewing this email|Pokud se Vám tento e-mail"),
                         Index=row_number())%>%filter(!Spam)%>%
  #zkousim podle emailove adresy odfitlrovat interni komunikaci.
                  mutate(isInternal=if_else(ContactId=="" | str_detect(RemoteAddress,"digi.tv|teleperformance.com|MAILER-DAEMON|MicrosoftExchange"),
                                            TRUE,
                                            FALSE
                                              ))%>%
                  filter(isInternal==FALSE)

#Removing other potential spam and internal messages
Data<-Data[!(duplicated(Data[,c("SubjectField","BodyText")]) | duplicated(Data[,c("SubjectField","BodyText")], fromLast=TRUE)),]

Messages<-Data%>%select(Index,SubjectField,Text)
  
rm(Data)



 






