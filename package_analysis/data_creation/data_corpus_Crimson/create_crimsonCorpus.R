# code to create the crimson corpus
#
# KM

require(quanteda)

##load in data from scrapeCrimson.R

load("package_analysis/data_creation/data_corpus_Crimson/crimson_pre_corpus.RData")


# sample$clean <- stringi::stri_conv(sample$clean, from = "windows-1252", to = "UTF-8")

data_corpus_Crimson <- corpus(sample[, -which(names(sample)=="texts")], textField = "clean",
                              notes = "A selection of articles from the Harvard Crimson archive",
                              source = "https://www.thecrimson.com/sitemap/")

# clean up non-ASCII format text otherwise causing CHECK error
# data_corpus_Crimson[["titles"]] <- stringi::stri_replace_all_regex(data_corpus_Crimson[["titles"]],
#                                                                    "Ingl.s", "Inglés")

docvars(data_corpus_Crimson, "titles") <-
    stringi::stri_replace_all_regex(docvars(data_corpus_Crimson, "titles"), "\xc3", "é")

Encoding(docvars(data_corpus_Crimson, "titles")) <- "UTF-8"

# inspect
summary(data_corpus_Crimson, 30)

# remove nearly or entirely empty documents
data_corpus_Crimson <- subset(data_corpus_Crimson, ntoken(data_corpus_Crimson) > 25)

save(data_corpus_Crimson, file = "data/data_corpus_Crimson.RData", compress = "bzip2")

