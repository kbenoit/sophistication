#' compute text-based variables from text or snippet data
#'
#' Compute additional variables to snippet data created by
#' \code{\link{snippets_make}}.  These are based on tokens, types, and various
#' readability measures.
#' @param x snippet data from \code{\link{snippets_make}} consisting of the
#'   fields \code{text}, \code{docID}, and \code{snippetID}
#' @param readability_measure additional readability measures passed through in
#'   the \code{measure} argument passed to
#'   \link[quanteda]{textstat_readability}. Because our standard input will be
#'   constituent elements rather than indexes, this defaults to \code{NULL}
#'   indicating that no compound measures will be used.
#' @param text_field the name of the text field, if a \link{data.frame}, default
#'   is \code{"text"}
#' @param normalize if \code{TRUE}, return proportions of words/sentences as
#'   appropriate, instead of raw counts
#' @param ... arguments passed through to \code{covars_make_character}
#' @return the data.frame of snippets \code{x} with added variables.  Note:
#'
#' \code{W_wl.Dale.Chall} is the proportion of words \emph{not} in the Dale-Chall word list.
#'
#' @import stringi
#' @importFrom data.table data.table
#' @import quanteda
#' @export
covars_make <- function(x, ...) {
    UseMethod("covars_make")
}

#' @rdname covars_make
#' @export
covars_make.snippet <- function(x, ...) {
    covars_make(x[["text"]], ...)
}

#' @rdname covars_make
#' @export
covars_make.data.frame <- function(x, text_field = "text", ...) {
    covars_make(x[[text_field]], ...)
}

#' @rdname covars_make
#' @export
covars_make.corpus <- function(x, ...) {
    covars_make(texts(x), ...)
}

#' @rdname covars_make
#' @export
covars_make.character <- function(x, readability_measure = NULL, normalize = TRUE, ...) {
    # include Dale.Chall?
    dc <- any(grepl("^Dale\\.Chall", readability_measure))
    # always include some measures
    readability_measure <- unique(c("meanSentenceLength",
                                    "meanWordSyllables", 
                                    "Dale.Chall.old", readability_measure))
    # return the data frame plus the computed variables
    result <- quanteda::textstat_readability(x, measure = readability_measure, intermediate = TRUE)
    # eliminate Dale.Chall.old if was not in readability_measure
    if (!dc) result[["Dale.Chall.old"]] <- NULL
    # remove document field
    result[["document"]] <- NULL
    # normalize result if needed
    if (normalize) {
        result[["meanWordChars"]] <- result[["C"]] / result[["W"]]
        result[["meanSentenceChars"]] <- result[["C"]] / result[["St"]]
        result[["meanSentenceSyllables"]] <- result[["Sy"]] / result[["St"]]
        result[, c("W3Sy", "W2Sy", "W_1Sy", "W6C", "W7C", "W_wl.Dale.Chall", "Wlt3Sy")] <-
            result[, c("W3Sy", "W2Sy", "W_1Sy", "W6C", "W7C", "W_wl.Dale.Chall", "Wlt3Sy")] / result$W
        # result$W <- result$St <- result$C <- result$Sy <- NULL
    }
    result
}

#' @rdname covars_make
#' @param dependency logical; if \code{TRUE} parse dependencies
#' @param verbose logical; if \code{TRUE} print status messages
#' @details \code{covars_make_all} calls \code{covars_make},
#'   \code{\link{covars_make_baselines}}, and \code{\link{covars_make_pos}},
#'   returning them as a data.frame.
#' @importFrom data.table data.table setkey
#' @export
covars_make_all <- function(x, ..., dependency = TRUE, verbose = FALSE) {
    `:=` <- doc_id <- NULL

    if (verbose) message("   ...computing readability statistics")
    covars1 <- covars_make(x, ...)
    if (verbose) message("   ...computing frequency baseline statistics")
    covars2 <- covars_make_baselines(x)
    if (verbose) message("   ...computing part-of-speech measures")
    covars3 <- covars_make_pos(x, dependency = dependency)

    namestmp <- row.names(covars1)
    covars1 <- data.table(covars1)
    covars1[, doc_id := namestmp]

    namestmp <- row.names(covars2)
    covars2 <- data.table(covars2)
    covars2[, doc_id := paste0("text", namestmp)]

    covars3 <- data.table(covars3)

    setkey(covars1, doc_id)
    setkey(covars2, doc_id)
    setkey(covars3, doc_id)
    covarsall <- covars1[covars2[covars3]]

    as.data.frame(covarsall)
}
