# code to create the fifth grade corpus
#
# KM

require(quanteda)

## load in data from text files
fifthTfile <- textfile("package_analysis/data_creation/Fifth_Grade/raw/*.txt", encoding="UTF-8")
data_corpus_fifthgrade <- corpus(fifthTfile,
                      notes = "A selection of texts at the fifth grade level",
                      source = "https://www.ncsu.edu/project/lancet/fifth.htm")
summary(data_corpus_fifthgrade, 3)
cat(data_corpus_fifthgrade[10])

save(data_corpus_fifthgrade, file = "data/data_corpus_fifthgrade.RData", compress = "bzip2")

