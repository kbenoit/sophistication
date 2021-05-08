
#' inspect gold snippet pairs
#'
#' Opens gold snippet pairs in a browser for easy inspection.
#' @param goldPairs a snippet gold object produced by [pairs_gold_make()]
#' @param filename optional filename where the html output will be written; if none
#' is supplied, writes to an R [tempfile]
#' @param ... additional arguments passed to [xtable][xtable::xtable]
#' @importFrom xtable xtable
#' @importFrom utils browseURL
#' @export
pairs_gold_browse <- function(goldPairs, filename = NULL, ...) {

    if (is.null(filename))
        filename <- tempfile(, fileext = ".html")

    goldPairs[["READDIFF"]] <- round(goldPairs$read1 - goldPairs$read2, 2)
    goldTable <- goldPairs[, c("read1", "text1", "READDIFF", "text2", "read2")]
    names(goldTable) <- c("read_A", "Text A", "READdiff", "Text B", "read_B")

    # make the cell entries bold that are easier
    for (r in 1:nrow(goldTable)) {
        goldTable[r, goldPairs$"easier_gold"[r]*2] <- paste("<strong>", goldTable[r, goldPairs$"easier_gold"[r]*2], "</strong>")
    }
    goldTable[, 3] <- paste("<center>", goldTable[, 3], "</center>")

    temp_xtable <- xtable::xtable(goldTable, ...)
    print(temp_xtable, type = "html", sanitize.text.function = identity, file = filename,
          html.table.attributes = getOption("xtable.html.table.attributes", "border=1 cellpadding=5 cellspacing=3 style=\"border-collapse:collapse; font-family:arial;\""))
    utils::browseURL(filename)
}


#' inspect gold snippet pairs
#'
#' Opens gold snippet pairs in a browser for easy inspection.
#' @param pairs snippet pairs created by gold object produced by [pairs_regular_make()]
#' @param filename optional filename where the html output will be written; if none
#' is supplied, writes to an R [tempfile]
#' @param ... additional arguments passed to [xtable][xtable::xtable]
#' @importFrom xtable xtable
#' @importFrom utils browseURL
#' @export
pairs_regular_browse <- function(pairs, filename = NULL, ...) {

    if (is.null(filename))
        filename <- tempfile(, fileext = ".html")

    pairs <- pairs[, c("docID1", "snippetID1", "text1", "text2", "snippetID2", "docID2")]
    pairs[, c("snippetID1", "snippetID2")] <- format(pairs[, c("snippetID1", "snippetID2")], nsmall = 0)

    temp_xtable <- xtable::xtable(pairs, ...)
    print(temp_xtable, type = "html", sanitize.text.function = identity, file = filename,
          html.table.attributes = getOption("xtable.html.table.attributes", "align=justify border=1 cellpadding=5 cellspacing=3 style=\"border-collapse:collapse; font-family:arial;\""))
    utils::browseURL(filename)
}


#' inspect the sentences in a corpus
#'
#' Inspect the sentences in a corpus, in a browser.  Useful for inspecting how sentences are segmented, to see what problems are occurring in the segmentation.
#' @param x the corpus to be inspected.
#' @param n max number of sentences; default is 1000
#' @details
#' This function segments the corpus by sentences, and displays the results in a browser for inspection.
#' @examples
#' \dontrun{
#' corpus_sentences_browse(data_corpus_pp)
#' }
#' @importFrom xtable xtable
#' @importFrom utils browseURL
#' @export
corpus_sentences_browse <- function(x, n = 1000) {
    UseMethod("corpus_sentences_browse")
}

#' @noRd
#' @export
corpus_sentences_browse.corpus <- function(x, n = 1000) {
    filename <- tempfile(, fileext = ".html")

    temp_corpus_sentences <- corpus_reshape(x, to = "sentences")
    df <- data.frame(
        sentences = as.character(temp_corpus_sentences),
        document = docid(temp_corpus_sentences)
    )

    temp_xtable <- xtable::xtable(df[1:n, ])
    print(temp_xtable, type = "html", sanitize.text.function = identity, file = filename,
          html.table.attributes = getOption("xtable.html.table.attributes", "align=justify border=1 cellpadding=5 cellspacing=3 style=\"border-collapse:collapse; font-family:arial;\""))
    utils::browseURL(filename)
}

