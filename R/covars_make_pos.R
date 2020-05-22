#' calculate part-of-speech information from text snippet data
#'
#' Add additional variables consisting of part-of-speech (POS) frequencies to
#' snippets.
#' @param x snippet data from \code{\link{snippets_make}} consisting of the
#'   fields \code{text}, \code{docID}, and \code{snippetID}
#' @param text_field the name of the text field, if a \link{data.frame}, default
#'   is \code{"text"}
#' @param normalize if \code{TRUE}, convert pos tag counts to rates
#' @param ... used to pass the \code{tagset} argument to
#'   \code{\link[spacyr]{spacy_parse}}, for example \code{tagset = "penn"} to specify the Penn Treebank tagset scheme, instead of the Google
#'   universal tagset default.  See \code{\link[spacyr]{spacy_parse}}.
#' @details Note that this requires spaCy to be installed (along with Python).
#'   See the installation instructions at
#'   \url{http://github.com/kbenoit/spacyr}.
#' @return the data.frame of added variables, consisting of the frequencies of
#'   parts of speech in each text
#' @export
#' @importFrom utils installed.packages
#' @examples
#' \dontrun{
#' # some examples here
#' }
covars_make_pos <- function(x, ...) {
    UseMethod("covars_make_pos")
}

#' @rdname covars_make_pos
#' @export
covars_make_pos.snippet <- function(x, ...) {
    covars_make_pos(x[["text"]], ...)
}

#' @rdname covars_make_pos
#' @export
covars_make_pos.corpus <- function(x, ...) {
    covars_make_pos(texts(x), ...)
}
#' @rdname covars_make_pos
#' @export
covars_make_pos.data.frame <- function(x,  text_field = "text", ...) {
    covars_make_pos(x[[text_field]], ...)
}

#' @rdname covars_make_pos
#' @param dependency logical; if \code{TRUE} parse dependencies
#' @importFrom data.table data.table setkey setnames
#' @import spacyr
#' @export
covars_make_pos.character <- function(x, text_field = "text", dependency = TRUE, normalize = TRUE, ...) {
    if (!("spacyr" %in% installed.packages()[, "Package"])) {
        stop("you must first install spacyr to tag parts of speech")
    }
    
    suppressMessages(spacy_initialize())
    result <- spacy_parse(x, lemma = FALSE, pos = TRUE, tag = TRUE,
                          dependency = dependency, entity = TRUE)
    orig_docid <- result$doc_id
    
    result <- subset(result, !(pos %in% c("PUNCT", "SPACE")))

    if (!dependency) {
        result$dep_rel <- ""
    }

    doc_id <- sentence_id <- tag <- pos <- dep_rel <- n_namedentities <- `:=` <-
        baseline_year <- .N <- n_sentence <- n_noun <- n_verb <- n_adjective <-
        n_adverb <- n_clause <- NULL

    # count named entities
    ne <- data.table(entity_extract(result))
    ne <- ne[, .N, by = doc_id]
    setnames(ne, "N", "n_namedentities")
    setkey(ne, doc_id)
    result <- data.table(result)
    orig_docid <- result[, unique(doc_id)]
    result_bydoc <- result[, list(max(sentence_id),
                                  sum(pos == "NOUN"),
                                  sum(pos == "VERB"),
                                  sum(pos == "ADJ"),
                                  sum(pos == "ADV"),
                                  sum(stringi::stri_detect_fixed(dep_rel, "cl")),
                                  .N),
                           by = doc_id]
    setnames(result_bydoc,
             c("doc_id", "n_sentence", "n_noun", "n_verb", "n_adjective", "n_adverb", "n_clause", "ntoken"))
    if (normalize) {
        result_bydoc[, c("pr_sentence", "pr_noun", "pr_verb", "pr_adjective", "pr_adverb", "pr_clause") :=
                         list(n_sentence/ntoken, n_noun/ntoken, n_verb/ntoken, n_adjective/ntoken, n_adverb/ntoken, n_clause/ntoken)]
    }
    setkey(result_bydoc, doc_id)

    result_bydoc <- ne[result_bydoc]
    # change missing ne to zero
    result_bydoc[is.na(n_namedentities), n_namedentities := 0]

    if (!dependency) result_bydoc[, c("n_clause", "pr_clause") := NULL]

    # spacyr::spacy_finalize()
    result <- as.data.frame(result_bydoc)[match(result_bydoc[, doc_id], orig_docid), ]
    row.names(result) <- NULL
    result
}



