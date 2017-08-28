
# set the appropriate pathname depending on user
path <- ifelse(Sys.info()["user"] == "kbenoit",
               "~/Dropbox/Papers/Benoit_Spirling_Readability/",
               "c:/users/as9934/dropbox/Benoit_Spirling_Readability/")


## tests with CF data

require(sophistication)
## put the main snippets together from the corpus
# make the snippets
snippetData <- makeSnippets(SOTUCorpus, nsentence = 1, minchar = 120, maxchar = 300)
# clean the text snippets
snippetData <- cleanSnippets(snippetData, readability.limits = c(10, 100), measure = "Flesch")
snippetData <- addSnippetVariables(snippetData)

# without covariates
results1 <- computeBT(file = paste0(path, "data_CF_results/f921916.csv"))
# with covariates
results2 <- computeBT(file = paste0(path, "data_CF_results/f921916.csv"),
                     covars = snippetData,
                     formula = ~ Flesch + Types + Tokens)

