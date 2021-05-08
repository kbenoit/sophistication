##
## create data on presidential proclamations
##

require(readtext)

## read the texts
data_corpus_pp <- corpus(readtext(paste0(getOption("ROOT_DROPBOX"), "data_text/presidential_proclamations/procl_texts/*"), verbosity = 0))

## set the document variable Year from the directory name
docvars(data_corpus_pp, "Year") <- as.integer(substring(docnames(data_corpus_pp), 1, 4))

## remove any texts of zero length
data_corpus_pp <- corpus_subset(data_corpus_pp, ntoken(data_corpus_pp) > 0)

## clean up things that will mess up the sentence segmentation
texts(data_corpus_pp) <- stringi::stri_replace_all_regex(as.character(data_corpus_pp), "A.\\w{0,1}D.", "AD")
# for things like "GO.", "TH.", "J."
texts(data_corpus_pp) <- stringi::stri_replace_all_regex(as.character(data_corpus_pp), "(\\p{Lu}{1,2})\\.", "$1")
# for enumerations followed by ., such as 2. 3. etc
texts(data_corpus_pp) <- stringi::stri_replace_all_regex(as.character(data_corpus_pp), "(\\d{1,2})\\.", "$1) ")

## save the corpus
devtools::use_data(data_corpus_pp, overwrite = TRUE)

