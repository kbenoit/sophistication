
#' form pairs of snippets for comparison
#'
#' Form pairs of snippets for comparison.  By default, form the simple chained
#' minimum-spannning tree of (\emph{n} - 1), where \emph{n} is the number
#' snippets, and each snippet is paired with its neighbour.  If \code{n.pairs} >
#' \emph{n}, then randomly form addition pairs not already in the MST, where
#' pairs are combinations without respect to order.  Note that the maximum
#' number of unique pairings is the combination \emph{n} choose 2, or
#' \emph{n}(\emph{n} - 1)/2.
#' @param x snippet data.frame
#' @param n.sample subset of observations of \code{x} to sample before forming
#'   pairwise comparisons
#' @param n.pairs number of pairwise comparisons to form
#' @param seed seed to use for random procedures; set this to ensure
#'   replicability
#' @return data.frame of paired snippets, consisting of the elements
#'   \code{text1}, \code{docID1}, \code{snippetID1}, \code{text2},
#'   \code{docID2}, and \code{snippetID2}.
#' @importFrom utils combn
#' @export
pairs_regular_make <-  function(x, n.sample = NULL, n.pairs = nrow(x)-1, seed = NULL) {

    if (is.numeric(seed)) set.seed(seed) else
        if (!is.null(seed)) stop("seed must be integer")

    if (!is.null(n.sample))
        x <- x[sample(nrow(x), size = n.sample), ]

    if (n.pairs < nrow(x) - 1)
        warning("not an MST when n.pairs < number of items-1")

    # forms the unique pairwise indexes
    pairs <- t(utils::combn(1:nrow(x), 2))

    # get the MST single-line connected set
    MSTindex <- cumsum(c(1, (nrow(x)-1):2))
    # extract these from pairs
    MSTpairs <- pairs[MSTindex, ]
    pairs <- pairs[-MSTindex, ]
    # randomly shuffle the non-MST pairs
    pairs <- pairs[sample(1:nrow(pairs)), ]

    # move the MST items to the front of pairIndex
    pairs <- rbind(MSTpairs, pairs)
    # take just the first n.pairs
    pairs <- pairs[1:n.pairs, ]

    result <- data.frame(cbind(x[pairs[, 1], ], x[pairs[, 2], ]),
                         stringsAsFactors = FALSE, row.names = NULL)
    # original names plus 1, 2
    names(result) <- paste0(names(x), rep(1:2, each = length(names(x))))

    result
}

#' bridge different pair datasets
#'
#' Form bridges between datasets by pairing across different sets.  Currently,
#' this function samples the first snippet from \code{bridge_size} pairs
#' randomly drawn from each input dataset, and then forms all pairwise
#' combinations between all of these. This is overkill, perhaps, but ensures
#' good linkage between the pairs drawn both within and across datasets.
#'
#' So for instance, with three datasets of 10 pairs each, 3 pairs would be drawn
#' from each dataset, for 9 total pairs, forming 9 choose 2 =
#' @param ... data.frames of pairs created by \code{\link{pairs_regular_make}} or
#'   \code{\link{pairs_gold_make}}
#' @param bridge_size how many snippets to pair from one set to another
#' @return a new paired dataset of all combinations of bridging pairs between
#'   input datasets
#' @export
pairs_bridge <- function(..., bridge_size = 3) {
    datasets <- list(...)

    # bridge the different datasets
    bridgeSize <- min(bridge_size, sapply(datasets, nrow))

    # join the rows
    bridgingText <- do.call(rbind, lapply(datasets, function(d) d[sample(nrow(d), size = bridgeSize), c("snippetID1", "docID1", "text1")]))
    names(bridgingText) <- gsub("1", "", names(bridgingText))

    # form pairwise combinations
    pairs_regular_make(bridgingText, n.pairs = choose(nrow(bridgingText), 2))
}


# function(x, n.snippets = nrow(x), n.pairs = n.snippets-1, seed = NULL) {
#
#     if (is.numeric(seed))
#         set.seed(seed)
#     else
#         if (!is.null(seed)) stop("seed must be integer")
#
#     # randomly sample n snippets
#     x <- x[sample(1:nrow(x), n.snippets), ]
#
#     if (n.pairs < n.snippets - 1)
#         stop("n.pairs must be at least the number of n.snippets.")
#
#     # get the set guaranteed to produce the MST
#     pairedIndex <- design.pairs(x, n.pairs, seed)
#
#     data.frame(snippetID1 = x[pairedIndex[, 1], "snippetID"],
#                text1 = x[pairedIndex[, 1], "text"],
#                snippetID2 = x[pairedIndex[, 2], "snippetID"],
#                text2 = x[pairedIndex[, 2], "text"])
#
# }
#
#
# ## function to create a minimum spanning tree btwn snippets
# ## imports mst from ape package
# design.pairs <- function(x, n = nrow(x)-1, seed = NULL) {
#     if (is.numeric(seed))
#         set.seed(seed)
#     else
#         if (!is.null(seed)) stop("seed must be integer")
#     X <- matrix(runif(n * 10), n, 10)
#     d <- stats::dist(X)
#     PC <- stats::prcomp(X)
#     M <- ape::mst(d)
#     M[upper.tri(M)] <- 0 # convert upper triangle to zeros (symmetric, anyway)
#     out.frame <- which(M==1, arr.ind = TRUE) # identify all the relevant comparisons
#     rownames(out.frame) <- paste("comparison", 1:nrow(out.frame), sep="_")
#     colnames(out.frame) <- c("snippet1", "snippet2")
#     out.frame
# }
#
