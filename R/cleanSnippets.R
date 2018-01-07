
#' clean up snippets
#'
#' Remove snippets that have unusual text, such as numbers.
#' @param x snippet data created by \code{\link{snippets_make}}
#' @param readability.limits Two-element numeric vector used to filter out snippets based on readability scores.  Any snippets with values outside this range will be dropped.
#' @param ... additional arguments passed to \link[quanteda]{textstat_readability}, such as `measure`
#' @param verbose if \code{TRUE} output status messages
#' @importFrom quanteda textstat_readability
#' @export
snippets_clean <- function(x, readability.limits = NULL, verbose = TRUE, ...) {

    text <- NULL

    if (!("snippet" %in% class(x)))
        stop("cleanSnippets only works on snippet data.")

    if (verbose) message("Cleaning ", format(nrow(x), big.mark=","), " snippets...")

    dt <- data.table(x)

    # readability filters
    if (!is.null(readability.limits)) {
        if (!is.numeric(readability.limits) & length(readability.limits) != 2)
            stop("readability.limits must be a two-element numeric vector")

        if (verbose) message("   computing readability...", appendLF = FALSE)
        readblty <- quanteda::textstat_readability(x$text, ...)
        # make sure smaller is before lower
        readability.limits <- sort(readability.limits)
        excludedRows <- (readblty < readability.limits[1] | readblty > readability.limits[2])
        if (sum(excludedRows)) {
            dt <- dt[-which(excludedRows)]
        }
        if (verbose)
            message("removed ", format(sum(excludedRows), big.mark=","), " snippets exceeding readability.limits")
    }

    # remove any snippets with long figures, or more than 2 years
    figureRows <- grep("\\d{1,3}(,\\d{3}){1,}(\\.\\d{2})*|(\\d{4}.*){2,}", dt[, text])
    if (verbose)
        message("   removed ", format(length(figureRows), big.mark=","),
                " snippets containing numbers of at least 1,000")
    dt <- dt[-figureRows]

    # remove any snippets with long ........
    figureRows <- grep("\\.{4,}", dt[, text])
    if (length(figureRows)) {
        if (verbose)
            message("   removed ", format(length(figureRows), big.mark=","), " snippets containing long elipses ....")
        dt <- dt[-figureRows]
    }

    # remove any snippets with all caps titles of two words or more
    figureRows <- grep("([A-Z]{3,}\\s){2,}", dt[, text])
    if (length(figureRows)) {
        if (verbose)
            message("   removed ", format(length(figureRows), big.mark=","), " snippets containing ALL CAPS titles")
        dt <- dt[-figureRows]
    }

    if (verbose) message("   ...finished.")
    dt <- as.data.frame(dt)
    class(dt) <- c("snippet", class(dt))
    dt
}

# remove any snippets < minCharLength

# if (verbose)
#     cat("Removed", nrow(dt[ncharNearest < round(minCharLength/nearestNcharMatch)]), "snippets less than", minCharLength, "characters.\n")
# dt <- dt[ncharNearest >= round(minCharLength/nearestNcharMatch)]
#
# # remove any snippets > maxCharLength
# if (verbose)
#     cat("Removed", nrow(dt[ncharNearest > round(maxCharLength/nearestNcharMatch)]), "snippets greater than", maxCharLength, "characters.\n")
# dt <- dt[ncharNearest <= round(maxCharLength/nearestNcharMatch)]
