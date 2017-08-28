# code to create the party broadcasts corpus
#
# KB

require(quanteda)

data_corpus_partybroadcasts <-
    corpus(textfile("package_analysis/data_creation/Party_Broadcasts/*.txt",
                    docvarsfrom = "filenames", docvarnames = c("party", "date")),
           source = "http://www.politicsresources.net/area/uk/peb.htm",
           notes = "A selection of UK political party election broadcasts")

# tidy up the docvars
docvars(data_corpus_partybroadcasts, "date") <-
    as.Date(docvars(data_corpus_partybroadcasts, "date"))
docvars(data_corpus_partybroadcasts, "party") <-
    as.factor(docvars(data_corpus_partybroadcasts, "party"))

# inspect
summary(data_corpus_partybroadcasts, 5)

save(data_corpus_partybroadcasts, file = "data/data_corpus_partybroadcasts.RData",
     compress = "bzip2")


