#' create text segments for comparisons
#'
#' Create text segments of a defined length, for comparisons.
#' @param x input objects, texts or a quanteda corpus
#' @param nsentences length in sentences of each snippet
#' @param minchar minimum length in characters of a snippet
#' @param maxchar maximum length in characters of a snippet
#' @param ... not used
#' @return a data.frame of snippets, with a unique indentifier
#' @export
snippets_make <- function(x, ...) {
    UseMethod("snippets_make")
}

#' @rdname snippets_make
#' @export
#' @importFrom quanteda corpus docnames
snippets_make.character <- function(x, ...) {
    snippets_make(quanteda::corpus(x), ...)
}

#' @rdname snippets_make
#' @importFrom data.table data.table
#' @importFrom quanteda corpus_reshape
#' @export
snippets_make.corpus <- function(x, nsentences = 1, minchar = 100, maxchar = 350, ...) {
    # to prevent "no visible binding" errors in R CMD CHECK
    text <- snippetID <- docID <- completeSet <- . <- docIDnum <- NULL
    `:=` <- setkey <- .N <- .GRP <- NULL

    x$docname <- docnames(x)
    x <- quanteda::corpus_reshape(x, to = "sentence")
    dt <- data.table(text = as.character(x), docID = docvars(x, "docname"))
    # create a snippetID to be used in selecting the snippets
    dt[, snippetID := rep(1:.N, each = nsentences, length.out = .N), by = docID]
    # flag any subsets that are not at least nsentence in length
    dt[, completeSet := .N==nsentences, by = .(docID, snippetID)]
    # remove the shorter snippets
    dt <- dt[completeSet == TRUE]
    # concatenate the texts by snippetID, if multiple sentences
    dt <- dt[, list(text = paste(text, collapse = "  ")), by = .(docID, snippetID)]
    # create a numeric document ID, to help with dyad sampling
    dt[, docIDnum := .GRP, by = .(docID)]
    # unique ID for each snippet
    dt[, snippetID := docIDnum * 10e4 + snippetID]
    # remove the (short) snippetID
    dt[, docIDnum := NULL]
    # dt[, completeSet := NULL]
    # remove snippets outside of length criteria
    dt <- dt[nchar(text) >= minchar & nchar(text) <= maxchar]
    dt <- as.data.frame(dt)
    class(dt) <- c("snippet", class(dt))
    dt
}
