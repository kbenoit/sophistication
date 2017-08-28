##
## create cleaned SCOTUS corpus
##

load(paste0(getOption("ROOT_DROPBOX"), "data_text/SCOTUS/justiaData/opinionMetaData.Rdata"))
load(paste0(getOption("ROOT_DROPBOX"), "data_text/SCOTUS/justiaData/opinionTextData.Rdata"))

ops.data$opinionType <- factor(ops.data$opinionType)
ops.data$Year <- as.integer(ops.data$Year)

data_corpus_SCOTUS <- corpus(ops.df$text,
                             docnames = ops.df$docs,
                             docvars = ops.data[c('title', 'author', 'opinionType',"Year", 'dateDecision', 'Age', 'ChiefJustice')],
                             metacorpus = list(notes = "All decisions written by SCOTUS justices, 1790 to 2012",
                                               source = "https://supreme.justia.com/, scraped by Tom Clark"))

## remove any texts of zero length
data_corpus_SCOTUS <- corpus_subset(data_corpus_SCOTUS, ntoken(data_corpus_SCOTUS) > 0)

## clean up things that will mess up the sentence segmentation
texts(data_corpus_SCOTUS) <- stringi::stri_replace_all_regex(texts(data_corpus_SCOTUS), "A.\\w{0,1}D.", "AD")
texts(data_corpus_SCOTUS) <- stringi::stri_replace_all_fixed(texts(data_corpus_SCOTUS), " v.", "v")
texts(data_corpus_SCOTUS) <- stringi::stri_replace_all_fixed(texts(data_corpus_SCOTUS), " c.", "c")
# for things like "GO.", "TH.", "J."
texts(data_corpus_SCOTUS) <- stringi::stri_replace_all_regex(texts(data_corpus_SCOTUS), "(\\p{Lu}{1,2})\\.", "$1")
# for enumerations followed by ., such as 2. 3. etc
texts(data_corpus_SCOTUS) <- stringi::stri_replace_all_regex(texts(data_corpus_SCOTUS), "(\\d{1,2})\\.", "$1) ")

devtools::use_data(data_corpus_SCOTUS, overwrite = TRUE)

# copy it to the Dropbox folder
system2("mv", c("data/data_corpus_SCOTUS.rda", paste0(getOption("ROOT_DROPBOX"), "data_pkg/")))
