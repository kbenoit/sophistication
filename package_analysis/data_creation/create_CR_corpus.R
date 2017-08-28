#################reading in congressional speeches from 1995 - 2008 and matching with metadata

library(quanteda)
library(dplyr)
library(data.table)
require(bit64)
library(lubridate)
setwd("C:/Users/kevin/Documents/GitHub/sophistication/R_package/package_analysis/data_creation/data_corpus_CR/")


##read in speech metadata

speech_meta<-list.files("speech")

speech.df<-fread(paste0("speech/", speech_meta[1]), data.table = FALSE)

for(i in 2:7){
  speech.df<-rbind(speech.df, fread(paste0("speech/", speech_meta[i]), data.table = FALSE))
}

##drop data we don't care about 

speech.df<-speech.df[c('speechID', "date", 'speakerID')]






##read in speeches

speeches<-list.files("speeches")

speeches.df<-fread(paste0("speeches/", speeches[1]), data.table = FALSE)

for(i in 2:7){
  speeches.df<-rbind(speeches.df, fread(paste0("speeches/", speeches[i]), data.table = FALSE))
}


##convert  IDs

speech.df$speechID <-as.numeric(speech.df$speechID)

speeches.df$speechID <-as.numeric(speeches.df$speechID)



##merge

df.merged<-full_join(speech.df, speeches.df)


##read in speaker metadata

speakers<-list.files("speaker")

speakers.df<-fread(paste0("speaker/", speakers[1]), data.table = FALSE)

for(i in 2:7){
  speakers.df<-rbind(speakers.df, fread(paste0("speaker/", speakers[i]), data.table = FALSE))
}

speakers.df$speakerID<-speakers.df$id
speakers.df$id<-NULL
##merge

df.full<-left_join(df.merged, speakers.df)

##split date var into year and session
df.full$year<-as.numeric(substr(as.character(df.full$date), 1,2))
df.full$session<-as.numeric(substr(as.character(df.full$date), 4,6))
df.full$date<-NULL


##delete variables used to merge
df.full$speakerID<-NULL

##create DF of just docvars

docvars.df<-df.full[c('state', 'name', 'chamber', 'district', 'party', 'year', 'session')]

##create corpus

speeches


data_corpus_CR <- corpus(df.full$speech,
                                docnames = df.full$speechID,
                                docvars = docvars.df,
                                 notes = "All speeches given in Congress from 1995 to 2008",
                                 source = "http://www.icpsr.umich.edu/icpsrweb/ICPSR/studies/33501#")


summary(data_corpus_CR, 3)
cat(data_corpus_CR[10])

save(data_corpus_CR, file = "C:/Users/kevin/Documents/GitHub/sophistication/R_package/data/data_corpus_CR.RData", compress = "bzip2")



