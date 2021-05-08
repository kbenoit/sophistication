#' create gold snippet pairs for CrowdFlower
#'
#' Create test questions for CrowdFlower from sample of snippet pairs, where the
#' readability is the most different between pairs.
#' @param x snippet pairs
#' @param n.pairs how many gold pairs to form
#' @param metric measure from [quanteda.textstats::textstat_readability()] to use to
#'   compute difference, default is `"Flesch"`
#' @param min.diff.quantile use this quantile range of difference in the metric
#'   only.  This is a relative measure that defaults to the upper and lower
#'   deciles: `c(.1, .9))`
#' @param min.diff.absolute form gold pairs with only at least this absolute
#'   value difference in the readability measures of the two texts.  Only one of
#'   `min.diff.quantile` and `min.diff.absolute` may be specified.
#' @param screeners if `TRUE`, add screener text with random instructions
#' @param verbose if `TRUE`, output status messages during processing
#' @param seed seed to use for random procedures; set this to ensure
#'   replicability
#' @return a data.frame of gold pairings, with reasons
#' @importFrom stats quantile
#' @importFrom data.table setorder
#' @importFrom quanteda.textstats textstat_readability
#' @export
pairs_gold_make <- function(x, n.pairs, metric = "Flesch", min.diff.quantile = c(.1, .9),
                     min.diff.absolute = NULL, screeners = FALSE, verbose = TRUE, seed = NULL) {

    screener <- text1 <- text2 <- readdiff <- read1 <- read2 <- easier_gold <- NULL
    `:=` <- NULL

    # error checks
    if (!all(c("text1", "text2") %in% names(x)))
        stop("input must contain two snippet texts")
    if (n.pairs > nrow(x))
        stop("n.pairs cannot be greater than the number of snippet pairs supplied")
    if (length(min.diff.quantile) != 2)
        stop("min.diff.quantile must be length 2")

    if (!missing(min.diff.quantile) & !missing(min.diff.absolute)) {
        warning("ignoring min.diff.absolute since min.diff.quantile also specified")
        min.diff.absolute <- NULL
    }

    if (is.numeric(seed)) set.seed(seed) else
        if (!is.null(seed)) stop("seed must be integer")

    if (verbose) message("Starting the creation of gold questions...")

    dtGold <- data.table(x)

    # add readability
    if (verbose) message("   computing ", metric, " readability measure")
    dtGold[, c("read1", "read2") := list(textstat_readability(text1, metric)[[metric]], 
                                         textstat_readability(text2, metric)[[metric]])]
    # compute difference and sort in descending order of absolute difference
    dtGold[, readdiff := abs(read1 - read2)]

    setorder(dtGold, -readdiff)

    lowerQuantileDiff <- round(stats::quantile(dtGold[, readdiff], min.diff.quantile[1]), 2)
    upperQuantileDiff <- round(stats::quantile(dtGold[, readdiff], min.diff.quantile[2]), 2)

    # restore sign
    dtGold[, readdiff := (read1 - read2)]

    # select top n.pairs, or randomly select if screeners
    if (!screeners) {
        if (verbose) message("   selecting top different ", n.pairs, " pairs")
        dtGold <- dtGold[sample(1:min(nrow(dtGold), n.pairs*3), size = n.pairs)]

        # apply thresholds of difference
        beforeRows <- nrow(dtGold)
        if (!is.null(min.diff.absolute)) {
            if (verbose) message("   applying min.diff.absolute threshold of ", min.diff.absolute)
            dtGold <- dtGold[abs(readdiff) >= min.diff.absolute]
        } else {
            if (verbose) message("   applying min.diff.quantile thresholds of ", lowerQuantileDiff, ", ", upperQuantileDiff)
            dtGold <- dtGold[readdiff <= lowerQuantileDiff | readdiff >= upperQuantileDiff]
        }
        afterRows <- nrow(dtGold)
        if (afterRows != beforeRows & verbose) {
            message("   removed ", beforeRows - afterRows, " pair",
                    ifelse(beforeRows - afterRows > 1, "s", ""),
                    " that did not meet the minimum threshold of readability differences",
                    sep = "")
        }

    } else {
        if (verbose) message("   randomly sampling ", n.pairs, " pairs")
        dtGold <- dtGold[sample(1:nrow(dtGold), n.pairs)]
    }

    # add gold reasons and answers
    if (verbose) message("   creating gold_reason text")
    dtGold[, "_golden" := "TRUE"]
    dtGold[, "easier_gold" := ifelse((readdiff > 0 & screeners == FALSE) | (readdiff < 0 & screeners == TRUE), 1, 2)]
    if (screeners == FALSE) {
        dtGold[, "easier_gold_reason" :=
                   paste("Text",  ifelse(easier_gold == 1, "A", "B"),
                         "is \"easier\" to read because it contains some combination of shorter sentences, more commonly used and more easily understood terms, and is generally less complicated and easier to read and grasp its point.")]
    } else {
        dtGold[, "easier_gold_reason" :=
                   paste("This question is a \"screener\" in which embedded instructions explicitly asked you to code Text",
                         ifelse(easier_gold == 1, "A", "B"), "as \"easier\".")]
    }

    # for screeners, randomly add text to counter-intuitive result
    if (screeners) {
        if (verbose) message("   adding screener instructions to texts")
        instructionsPre <- "Code THIS text as easier.  "
        instructionsPost <- "  Code THIS text as easier."

        # create or draw indexes for vectorized screener text
        text1index <- dtGold[, easier_gold] == 1
        text2index <- !text1index
        preindex <- sample(c("pre", "post"), n.pairs, replace = TRUE) == "pre"
        postindex <- !preindex

        dtGold[text1index & preindex, text1 := paste0(instructionsPre, text1)]
        dtGold[text1index & postindex, text1 := paste0(text1, instructionsPost)]

        dtGold[text2index & preindex, text2 := paste0(instructionsPre, text2)]
        dtGold[text2index & postindex, text2 := paste0(text2, instructionsPost)]
    }

    # remove intermediate values
    # dtGold[, c("read1", "read2", "readdiff") := NULL]

    if (screeners) dtGold[, screener := TRUE]

    if (verbose) message("   ...finished.")
    as.data.frame(dtGold, stringsAsFactors = FALSE)
}

