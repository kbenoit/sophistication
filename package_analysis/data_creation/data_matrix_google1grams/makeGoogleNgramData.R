## load in Google n-gram unigrams
##
## kept out of Dropbox because of size
## KB

rm(list = ls())
setwd("~/Documents/Google ngrams/unigrams")

require(data.table)
require(Matrix)
require(quanteda)


## intermediate step: create the entire set as a data.table

filenames <- list.files()
dt <- data.table()
for (f in filenames[grep(".*-[n-z]$", filenames)]) {
    cat("Processing letter ", substring(f, nchar(f)), ":\n", sep = "")
    this.letter.dt <- fread(f, col.names = c("token", "year", "tf"), drop = 4,
                            na.strings = NULL, stringsAsFactors = TRUE)
    dt <- rbind(dt, this.letter.dt)
}

# saveRDS(dt, file = "google_1grams_a-m.rds")
# saveRDS(dt, file = "google_1grams_n-z.rds")
# dt <- rbind(readRDS("google_1grams_a-m.rds"), dt)
# saveRDS(dt, file = "google_1grams_a-z.rds")



## create the sparse matrix

require(data.table)
require(Matrix)
require(bit64)
totalcounts <- fread("~/Documents/Google ngrams/unigrams/googlebooks-eng-all-totalcounts-20120701.txt",
                     col.names = c("year", "totalfreq"), drop = 3:4)
save(totalcounts, file = "~/Dropbox/Papers/Benoit_Spirling_Readability/data/Google_ngrams/totalcounts.RData")
dt <- readRDS("~/Documents/Google ngrams/unigrams/google_1grams_a-z.rds")
# start at 1970, discard any term with <5 term frequency
dt <- dt[year >= 1790 & tf >= 5]
# refactor the terms
dt[, token := factor(token)]
setkey(dt, year)
setkey(totalcounts, year)
dt <- totalcounts[dt]
dt[, tf := log(tf / totalfreq)]
dt <- dt[, totalfreq := NULL]
names(dt) <- c("year", "token", "logRelTf")
gc()
google1gramsSparseLogRelTf <- sparseMatrix(i = as.integer(dt[, token]),
                                      j = dt[, year] - 1790 + 1,
                                      x = dt[, logRelTf],
                                      dimnames = list(token = levels(dt[, token]), year = sort(unique(dt[, year]))))
object.size(google1gramsSparse)
object.size(dt)

save(google1gramsSparse,
     file = "~/Dropbox/Papers/Benoit_Spirling_Readability/data_text/Google_ngrams/google1gramsSparse.RData")

save(google1gramsSparseLogRelTf,
     file = "~/Dropbox/Papers/Benoit_Spirling_Readability/data_text/Google_ngrams/google1gramsSparseLogRelTf.RData")


## prune the years and tokens
# collapse by decade (by replacing year with decade)
colnames(google1gramsSparse) <- floor(as.integer(colnames(google1gramsSparse)) / 10) * 10
# remove POS tags and numbers from tokens
rownames(google1gramsSparse) <- gsub("(_.+)|(\\.[[:digit:]]+)$", "", rownames(google1gramsSparse))
# remove any tokens with punctuation or digits remaining
google1gramsSparse <- google1gramsSparse[!grepl("[[:punct:][:digit:]]", rownames(google1gramsSparse)), ]
# lowercase the tokens
rownames(google1gramsSparse) <- tolower(google1gramsSparse)
# remove any tokens not regular English characters
google1gramsSparse <- google1gramsSparse[grepl("^[a-z]+$", rownames(google1gramsSparse)), ]
# collapse
google1gramsSparse <- quanteda::compress(new("dfmSparse", google1gramsSparse))
# remove words not in English
google1gramsSparse <- google1gramsSparse[rownames(google1gramsSparse) %in% names(quanteda::englishSyllables), ]
names(dimnames(google1gramsSparse)) <- NULL

# convert to matrix
# data_matrix_google1grams <- as.matrix(google1gramsSparse)

# make relative to "the"
# data_matrix_google1grams <-
#     data_matrix_google1grams / rep(data_matrix_google1grams["the", ], each = nrow(data_matrix_google1grams))

data_matrix_google1grams <- as.matrix(google1gramsSparse)
save(data_matrix_google1grams, file = "R_package/data/data_matrix_google1grams.RData",
     compress = "bzip2", compression_level = 9)


