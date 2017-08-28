
#' format Crowdflower data for BT analysis
#'
#' Format Crowdflower results for analysis by the BradleyTerry2 package.  Can
#' accept covariates computed by  \code{\link{covars_make}}.
#' @param x data.frame of results, if already loaded
#' @param file character containing the file with the Crowdflower results (.csv
#'   format). One of \code{x} or \code{file} must be specified.
#' @param format the format of the data: \describe{
#'   \item{\code{"chameleons"}}{similar to
#'   \code{\link[BradleyTerry2]{chameleons}} a list of three data frames:
#'   \code{easier} and \code{harder}, each with a single column \code{ID} with a
#'   unique identifier for the snippet that won or lost, and of the same row
#'   dimensionality since each row corresponds to a single pairwise comparison;
#'   and \code{predictors}, a data.frame of predictors associated with each
#'   \code{ID} where the row.name corresponds to an ID in the in the
#'   \code{easier} and \code{harder} data.frames.}
#'   \item{\code{"binomial"}}{similar to extended example for
#'   \code{\link[BradleyTerry2]{baseball}} in \code{\link[BradleyTerry2]{BTm}}.}
#'   }
#' @param remove_gold if \code{TRUE}, remove "gold" sentences from analysis
#' @param remove_screeners if \code{TRUE}, remove "screener" sentences from
#'   analysis
#' @param covars logical; if \code{TRUE} then add covariates for each snippet,
#'   taken directly from the Crowdflower saved data.  Additional arguments to
#'   \code{\link{covars_make}} can be passed through \code{...}
#' @param covars_baseline logical; if \code{TRUE}, add summary baseline
#'   frequencies compared to Google and Brown corpora speech computed by
#'   \code{\link{covars_make_baselines}}
#' @param covars_pos logical; if \code{TRUE}, add frequencies of parts of speech
#'   computed by \code{\link{covars_make_pos}}
#' @param normalize if \code{TRUE} return appropriately normalized covariates,
#'   including parts of speech if applicable
#' @param ... additional arguments passed to \code{\link{covars_make}}
#' @return a data.frame suitable for analysis by \link[BradleyTerry2]{BTm}
#' @importFrom reshape2 melt
#' @importFrom utils read.csv
#' @export
#' @examples
#' # compute abilities for the BT model from CF data
#' \dontrun{
#' require(BradleyTerry2)
#'
#' ## compute BT model without covariates
#' # in binomial format
#' inputdata1a <- bt_input_make(file = "data/CF_results/f921916.csv", format = "binomial")
#' BTmodel1a <- BTm(cbind(win1, win2), snippet1, snippet2, data = inputdata1a)
#' BTabils1a <- BTabilities(BTmodel1a)
#' head(BTabils1a)
#' # in "chameleons" format
#' inputdata1b <- bt_input_make(file = "data/CF_results/f921916.csv", format = "chameleons")
#' BTmodel1b <- BTm(player1 = easier, player2 = harder, id = "ID", data = inputdata1b)
#' BTabils1b <- BTabilities(BTmodel1b)
#' head(BTabils1b)
#'
#' ## compute BT model with covariates
#' inputdata2 <- bt_input_make(file = "data/CF_results/f921916.csv",
#'                             covars = TRUE, readability_measure = "Flesch")
#' BTmodel2 <- BTm(player1 = easier, player2 = harder,
#'                  formula = ~ W[ID] + St[ID] + C[ID] + Sy[ID] + Flesch[ID] + (1|ID),
#'                  id = "ID", data = inputdata2)
#' BTabils2 <- BTabilities(BTmodel2)
#' head(BTabils2[order(BTabils2[, 1], decreasing = TRUE), ], 10)
#'
#' ## compute BT model with covariates and POS
#' options(PYTHON_PATH = "/usr/local/bin")  # needed on Ken's system
#' inputdata3 <- bt_input_make(file = "data/CF_results/f921916.csv",
#'                             covars = TRUE, covars_pos = TRUE,
#'                             readability_measure = "Flesch")
#' BTmodel3 <- BTm(player1 = easier, player2 = harder,
#'                  formula = ~ W[ID] + St[ID] + C[ID] + Sy[ID] + Flesch[ID] +
#'                              ADJ[ID] + VERB[ID] + NOUN[ID] + (1|ID),
#'                  id = "ID", data = inputdata3)
#' BTabils3 <- BTabilities(BTmodel3)
#' head(BTabils3[order(BTabils3[, 1], decreasing = TRUE), ], 10)
#'
#' }
bt_input_make <- function(x = NULL, file = NULL, format = c("chameleons", "binomial"),
                     remove_gold = TRUE, remove_screeners = remove_gold, covars = FALSE,
                     covars_baseline = FALSE, covars_pos = FALSE, normalize = TRUE,
                     ...) {

    format <- match.arg(format)

    # to prevent no visible binding warnings during check
    snippet1 <- snippet2 <- snippetid1 <- snippetid2 <- value <- win1 <- win2 <-
        X_golden <- screener <- coded_easier <- NULL

    # some error checks
    if (is.null(file) & !is.data.frame(x))
        stop("x must be a data.frame")
    if (!is.null(file) & !is.null(x))
        cat("you should only specify x or file, not both")

    # read file if specified
    if (is.null(x)) {
        if (!file.exists(file))
            stop("file ", file, " not found")
        x <- subset(utils::read.csv(file, stringsAsFactors = FALSE),
                    select = c("snippetid1", "snippetid2", "easier", "X_golden", "screener",
                               "text1", "text2"))
        # assign CF ID for job
        # x$CFjobID <- as.integer(substring(gsub("\\.csv$", "", basename(file)), 2))
    }

    # remove gold and/or screeners if specified
    x$screener[is.na(x$screener)] <- FALSE
    if (remove_screeners)
        x <- subset(x, screener == FALSE)
    if (remove_gold)
        x <- subset(x, X_golden == "false")
    # remove variables no longer needed
    x$"X_golden" <- x$screener <- NULL

    # get snippet info, factor levels of IDs
    snippetData <- data.frame(snippetid = c(x$snippetid1, x$snippetid2),
                              text = c(x$text1, x$text2),
                              # CFjobid = x$CFjobID[1],
                              stringsAsFactors = FALSE)
    snippetData <- snippetData[!duplicated(snippetData$snippetid), ]
    factor_levels <- sort(snippetData$snippetid)

    # get the covars directly from the CF results
    if (covars | covars_pos | covars_baseline)
        predictors <- data.frame(snippetid = snippetData$snippetid)
    if (covars)
        predictors <- cbind(predictors, covars_make(snippetData, normalize = normalize, ...))
    if (covars_baseline)
        predictors <- cbind(predictors, covars_make_baselines(snippetData))
    if (covars_pos)
        predictors <- cbind(predictors, covars_make_pos(snippetData, normalize = normalize))


    if (format == "chameleons") {

        # identify the easier and harder items
        names(x)[which(names(x) == "easier")] <- "coded_easier"
        x <- within(x, easier <- ifelse(coded_easier == 1, snippetid1, snippetid2))
        x <- within(x, harder <- ifelse(coded_easier == 2, snippetid1, snippetid2))

        # make the two result variables a common factor
        x <- within(x, easier <- factor(easier, levels = factor_levels))
        x <- within(x, harder <- factor(harder, levels = factor_levels))

        # for this format, make predictor row.names the ID
        if (covars) {
            row.names(predictors) <- predictors$snippetid
            predictors$snippetid <- NULL
            # sort the predictors data.frame to match the factor level order from easier, harder
            # necessary because otherwise BTm with chameleons format scrambles covariate rows!!
            predictors <- predictors[match(levels(x$easier), row.names(predictors)), ]
        }

        btinput <- list(easier = data.frame(ID = x$easier),
                        harder = data.frame(ID = x$harder),
                        predictors = if (covars) predictors else NULL)
    }

    if (format == "binomial") {
        # format the input matrix for BT
        x_melted <- reshape2::melt(x, id = c("snippetid1", "snippetid2"))
        x_melted <- within(x_melted, easier <- ifelse(value == 1, snippetid1, snippetid2))
        x_melted <- within(x_melted, harder <- ifelse(value == 2, snippetid1, snippetid2))
        btinput <- BradleyTerry2::countsToBinomial(table(x_melted$easier, x_melted$harder))
        names(btinput)[1:2] <- c("snippet1", "snippet2")

        # add in covariates, if specified
        if (covars) {
            row.names(predictors) <- covars$snippetID
            predictors$snippetID <- NULL
            # covars$text <- NULL

            # add in the covariates by replacing the winner and loser IDs with
            # data.frames of IDs and covariates, similar to the baseball example
            # from ?BTm
            btinput$snippet1 <- data.frame(snippetID = btinput$snippet1,
                                           predictors[btinput$snippet1, ])
            btinput$snippet2 <- data.frame(snippetID = btinput$snippet2,
                                           predictors[btinput$snippet2, ])
        }
    }

    btinput
}



