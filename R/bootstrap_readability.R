#' compute bootstrapped SEs for readability statistics
#'
#' Function to compute bootstrapped mean and SEs for readability statistics.  This is a wrapper around
#' [textstat_readability()], that redraws the corpus using sentence-level bootstrapping from the
#' original texts.
#' @param x character or [corpus] input object for which readability will be computed
#' @param ... additional arguments passed to [textstat_readability()]
#' @param n bootstrap replicates
#' @param verbose if `TRUE` show status messages
#' @return list consisting of three data.frame objects: the computed values on
#'   the original texts; the computed mean of the replicates; and the standard
#'   deviations of the replicates
#' @import quanteda
#' @importFrom stats sd
#' @examples
#' data(data_corpus_inaugural, package = "quanteda")
#' bootstrap_readability(data_corpus_inaugural[50:58], n = 10,
#'     measure = c("Flesch", "Flesch.Kincaid"), verbose = TRUE)
#' @export
bootstrap_readability <- function(x, n = 100, ..., verbose = FALSE) {
    UseMethod("bootstrap_readability")
}

#' @noRd
#' @export
bootstrap_readability.character <- function(x, n = 100, ..., verbose = FALSE) {
    bootstrap_readability(corpus(x), n = n, ..., verbose = verbose)
}

#' @noRd
#' @export
bootstrap_readability.corpus <- function(x, n = 100, ..., verbose = FALSE) {
    if (verbose)
        message("Bootstrapping readability statistics for ", ndoc(x), " documents:")

    # initialize return object
    result <- list()

    # original texts
    if (verbose)
        message("   ...computing values from original texts")
    result$original <- textstat_readability(x, ...)[, -1]

    # segment into sentences
    if (verbose)
        message("   ...segmenting the texts into sentences for resampling")
    x_sentences <- corpus_reshape(x, to = "sentences")
    # makes result compatible for pre and post v2
    docvars(x_sentences, "_document") <- attr(x_sentences, "docvars")$"docid_"

    # initialize replicates array
    replicates_array <- array(NA, dim = c(dim(result$original), n))

    # compute replicates
    if (verbose)
        message("   ...computing values from bootstrapped replicates\n     ", appendLF = FALSE)
    for (i in seq_len(n)) {
        message(" ", i, appendLF = FALSE)
        x_recombined <- corpus_reshape(corpus_sample(x_sentences, replace = TRUE, by = "_document"), to = "documents")
        replicates_array[, , i] <- as.matrix(textstat_readability(x_recombined, ...)[, -1])
    }
    message("")

    # compute summary statistics
    if (verbose)
        message("   ...computing summary statistics")
    result$bs_mean <- (apply(replicates_array, c(1,2), mean))
    result$bs_sd <- (apply(replicates_array, c(1,2), sd))

    # reattach dimension names to bs elements
    dimnames(result$bs_mean) <- dimnames(result$bs_sd) <- dimnames(result$original)

    if (verbose)
        message("   ...finished.")

    result
}
