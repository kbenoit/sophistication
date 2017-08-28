## read in executive orders




#################scraping executive orders
install.packages("xml2")
library(rvest)
library(xml2)
library(XML)
library(stringr)
library(stringi)
library(readr)
library(quanteda)
library(data.table)
library(readtext)
##update for 2017
setwd("C:/Users/kevin/Dropbox/Benoit_Spirling_Readability/data_text/executive_orders/eo_texts/")
years<-seq(2017, 2017, 1)

#years<-seq(1826, 2016, 1)

###create folders to house text
for(i in years){
    dir.create(file.path("C:/Users/kevin/Dropbox/Benoit_Spirling_Readability/data_text/executive_orders/eo_texts/", i), showWarnings = FALSE)
}


url_list<-c(paste("http://www.presidency.ucsb.edu/executive_orders.php?year=", years, "&Submit=DISPLAY", sep=""))
for(i in 1:length(url_list)){

    possibleError <- tryCatch(
        link_list<-read_html(url_list[i]) %>% html_nodes("a") %>% html_attr("href"),
        error=function(e) e
    )

    if(inherits(possibleError, "error")){
        next
    }
    link_list<-link_list[grepl(pattern = "/ws/index.php", link_list)] # get only endings
    link_endings<-gsub(pattern = "../", replacement = "", link_list)
    numbers<-substr(link_endings, 15, 19)
    full_link<-c(paste("http://www.presidency.ucsb.edu/ws/", link_endings, sep=""))
    for(j in 1:length(full_link)){
        lnk=full_link[j]
        text<-read_html(lnk) %>% html_nodes("span p") %>% html_text()
        text<-capture.output(cat(text))
        write(text, file=paste0(years[i], "/", numbers[j], ".txt"))
        Sys.sleep(20)

    }
}


##
## read in scraped proclamations
##

data_corpus_eo <- corpus(readtext(paste0(getOption("ROOT_DROPBOX"), "data_text/executive_orders/eo_texts/*/*txt")),
                         metacorpus = list(notes = "All presidential executive orders",
                                           source = "http://www.presidency.ucsb.edu/executive_orders.php"))
docvars(data_corpus_eo, "Year") <- as.integer(substring(docnames(data_corpus_eo), 1, 4))
data_corpus_eo <- corpus_subset(data_corpus_eo, ntoken(data_corpus_eo) > 0)

devtools::use_data(data_corpus_eo, overwrite = TRUE)




