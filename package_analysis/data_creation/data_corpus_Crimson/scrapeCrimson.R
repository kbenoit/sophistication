##scrape crimson


library(rvest)
library(xml2)
library(XML)
library(stringr)
library(stringi)
library(readr)
library(quanteda)


setwd("C:/Users/kevin/Documents/GitHub/sophistication/analysis/data_creation/Crimson")


#years_urls<-c(paste("http://docs.legis.wisconsin.gov/", seq(1995, 2015, 2), "/", sep=""))
html_top <- read_html("http://www.thecrimson.com/sitemap/") #download the HTML
links_top<-html_top %>% html_nodes("a") %>% html_attr("href") %>% grep(pattern="sitemap",value=T, )  #parse using rvest


dates<-list()
texts<-list()

#I did it for the first 10, now we're doing the rest
for(i in 1:length(links_top)){
  html<-read_html(paste0("http://www.thecrimson.com", links_top[i]))
  links_years<-html %>% html_nodes("a") %>% html_attr("href") %>% grep(pattern="sitemap",value=T, )
  links_years<-sample(links_years, min(10, length(links_years)))
  for(j in 1:length(links_years)){
    possibleError <- tryCatch(
      html_day<-read_html(paste0("http://www.thecrimson.com", links_years[j])),
      error=function(e) e
    )
    
    if(inherits(possibleError, "error")) next
    html_day<-read_html(paste0("http://www.thecrimson.com", links_years[j]))
    links_articles<-html_day %>% html_nodes("a") %>% html_attr("href")
    links_articles<-links_articles[links_articles!=""]
    links_articles<-links_articles[20:(length(links_articles)-10)]
    links_articles<-sample(links_articles, min(10, length(links_articles)))
    
    
    for(k in 1:length(links_articles)){
      possibleError <- tryCatch(
        html_article<-read_html(paste0("http://www.thecrimson.com", links_articles[k])),
        error=function(e) e)
      if(inherits(possibleError, "error")) next
      
      
      html_article<-read_html(paste0("http://www.thecrimson.com", links_articles[k]))
      possibleError <- tryCatch(
        xmlfile <- xmlRoot(xmlTreeParse(html_article)),
        error=function(e) e
      )
      
      if(inherits(possibleError, "error")) next
      xmlfile <- xmlRoot(xmlTreeParse(html_article))
      
      text <- xmlSApply(xmlfile, function(x) xmlSApply(x, xmlValue))
      
      if(length(links_articles)!=0){
        dates<-c(dates, links_articles[k][[1]])}
      texts<-c(texts, text$body)
      
    }
  }
}


##restrict to the actual pages of interest
long_dates<-dates[which(nchar(dates) >40) ]

long_texts<-texts[which(nchar(texts) >2300) ]




real_texts<-long_texts[1:13246]
#extract the dates

real_dates<-substr(long_dates, 10,20)

library(stringr)
x <- (str_extract_all(real_dates,"\\(?[0-9,.]+\\)?"))

real_dates1<-list()
for(i in 1:length(x)){
  real_dates1[i]<-length(x[[i]])
}

keepers<-which(real_dates1>1)

real_dates<-real_dates[keepers]

xx<-gsub("[^0-9/]", "", real_dates)
###there's the real dates

##have too many texts, need to sync up with dates

realz<-real_texts[1:10000]

indices<-sort(sample(1:10000, (10000-(13247-12075)), F))
texts<-c(realz[indices], real_texts[10000:13246])
###################################################

crimson_data<-data.frame(texts=as.character(texts), dates=xx)

crimson_data$texts<-as.character(crimson_data$texts)

crimson_data$dates<-as.Date(crimson_data$dates)

dates_converted<-as.Date(xx)

crimson_data$texts[12000]


##grabbing snippets

devtools::install_github("kbenoit/sophistication")

library(sophistication)

install.packages("sophistication")

crimson_data$body<-substr(crimson_data$texts, 550, 2000)

body[sample(1:12000, 10, replace=F)]





library(quanteda)


library(sophistication)







###load data
load("C:/Users/kevin/Dropbox/Benoit_Spirling_Readability/data_text/scraping_text/crimson_scraped.RData")




##sample
library(dplyr)
library(stringr)


#using the full articles--still pretty messed up
sample<-crimson_data %>% group_by(dates) %>% sample_n(size = 3, replace=T)
sample<-sample[sample$dates>"1800-01-01",]


sample$clean<-sample$texts

##let's clean up what I have

#remove everything after "Previous Page..."
pattern= "Previous Page.*"

sample$clean<-gsub(pattern,"", sample$clean)

#remove everything after "Read More..."
pattern= "Read more.*"

sample$clean<-gsub(pattern,"", sample$clean)


#remove everything before "Mobile Leaderboard"
pattern= ".*Mobile Leaderboard"

sample$clean<-gsub(pattern,"", sample$clean)

#remove everything before "Mobile_Leaderboard"
pattern= ".*Mobile_Leaderboard"

sample$clean<-gsub(pattern,"", sample$clean)



##get rid of junk
pattern= ".*googletag.display"


sample$clean<-gsub(pattern,"", sample$clean)



##other phrase at the end

pattern="Want to keep up with breaking news?.*"

sample$clean<-gsub(pattern,"", sample$clean)

##other phrase at the end

pattern="Stay up-to-date on Harvard campus.*"

sample$clean<-gsub(pattern,"", sample$clean)

##junk



sample$clean<-gsub("[^[:alnum:][:space:]'.,]", "", sample$clean)


##junk

pattern="[ãÂâ]"

sample$clean<-gsub(pattern,"", sample$clean)


##junk

pattern="'                  "

sample$clean<-gsub(pattern,"", sample$clean)

sample$clean[100]

##still a problem with there being some sentences squished together


##add in spacing
sample$clean<-gsub("([,.\\d])([A-Z])", "\\1 \\2",sample$clean)

##divide on date--this lets me know when the front material stops
splitz<-list()

splitz<-strsplit(sample$clean, "\\d, \\d\\d\\d\\d")



sample$titles<-unlist(lapply(splitz, `[[`, 1))



##but it also splits on dates in the body--re-combine these texts

for(i in 1:length(splitz)){
  k<-length(splitz[i][[1]])
  init<-""
  if(k>1){
  for(j in 2:k){
    init<-paste0(init, splitz[i][[1]][j])
  }
    
  }
  sample$texts[i]<-init
}



sample$clean<-sample$texts

rm(list=setdiff(ls(), "sample"))


save.image("C:/Users/kevin/Documents/GitHub/sophistication/analysis/data_creation/Crimson/crimson_pre_corpus.RData")







##looking at summary stats of readibility scores

time<-readability(sample$clean)


summary(time$Flesch.Kincaid)

keep<-which(time$Flesch.Kincaid< 25)

plot( sample[keep,]$dates, time[keep,]$Flesch.Kincaid)


plot( sample[keep,]$dates, time[keep,]$Dale.Chall)

plot( sample[keep,]$dates, time[keep,]$SMOG)

##

##let's try the snippets--just one sentence


#the makeSnippets function is broken , this is a workarond
snips<-list()
for(i in 1:length(sample$clean)){
  snips[[i]] <- makeSnippets(sample$clean[i], nsentence = 1, minchar = 50, maxchar = 300)
  
}

#snippetData <- makeSnippets(sample$clean[1:20], nsentence = 3, minchar = 50, maxchar = 300)


snippets<-list()
indices<-list()

for(i in 1:length(snips))
{
  for(j in 1:length(snips[[i]])){
    snippets<-c(snippets, snips[[i]]$text[j])
    indices<-c(indices, i)
  }
  
}

snippets<-unlist(snippets)
indices<-unlist(indices)

snippet_dates<-as.Date(vector())
for(i in 1:length(indices)){
  k<-indices[i]
  snippet_dates<-c(snippet_dates, sample$dates[k])
}


snippet_dates[12000]

snippet_df<-data.frame(snippets=as.character(snippets), dates=snippet_dates)


snippet_df$dates



sample_snippets<-snippet_df %>% group_by(dates) %>% sample_n(size = 3, replace=T)

sample_snippets<-sample_snippets[sample_snippets$dates>"1800-01-01",]

sample_snippets$clean<-str_replace_all(sample_snippets$snippets, "[[:punct:]]", " ")


real_snippet_id<-which(nchar(sample_snippets$clean)>10)

sample_snippets<-sample_snippets[real_snippet_id,]

time<-readability(sample_snippets$clean)


plot( sample_snippets$dates, time$Flesch.Kincaid)

summary(lm(time$Flesch.Kincaid ~sample_snippets$dates))
summary(lm(time$Flesch ~sample_snippets$dates))
summary(lm(time$meanWordSyllables ~sample_snippets$dates))
summary(lm(time$Dale.Chall ~sample_snippets$dates))
summary(lm(time$meanSentenceLength ~sample_snippets$dates))


plot( sample_snippets$dates, time$Dale.Chall)



lm(time$meanWordSyllables ~ sample_snippets$dates)
