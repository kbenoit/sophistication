#' predict readability score from a fitted BT model
#'
#' Predicts the \eqn{lambda} for a given text in \code{newdata}, from a fitted
#' Bradley-Terry model object \code{object}.
#'
#' @param object a fitted \code{\link[BradleyTerry2]{BTm}} model object
#' @param newdata an optional data frame in which to look for texts whose
#'   readability values will be predicted.  This can be a data.frame with the
#'   same covariates as \code{object}, or a corpus or character object.  If
#'   omitted, the fitted values from \code{object} are used.
#' @param reference_top,reference_bottom the \eqn{lambda} values of a text
#'   against which each predicted text will be compared for difficulty or rescaled.  The
#'   default value for \code{reference_bottom} is the \eqn{lambda} applied to all of
#'   \code{\link{data_corpus_fifthgrade}}, and is used as the baseline value to calculate
#'   the probability that a given text is easier, as well as the anchor value of 100 to which 
#'   texts are rescaled. The default value for \code{reference_top} is the \eqn{lambda}
#'   of the most difficult text in the State of the Union corpus, and is used as the anchor value 
#'   of 0 to which texts are rescaled (See \code{f999866.csv}.)
#' @param bootstrap_n number of bootstrap replicates for computing intervals
#' @param baseline_year a scalar or vector of the baseline years to choose for
#'   reference: a year ending in 0 from 1790-2000
#' @param verbose logical; if \code{TRUE} print status messages
#' @return a data.frame with the rows named to the text names, and the columns
#'   consisting of: \describe{ \item{\code{lambda}}{estimated lambda for each
#'   text} \item{\code{prob}}{the probability that the text is easier than the
#'   reference lambda, the default of which is \eqn{lambda} applied to all of
#'   \code{\link{data_corpus_fifthgrade}}} \item{\code{scaled}}{a rescaled lambda on a scale of
#'   "ease" ranging from 0-100, where 100 and 0 are determined by the fifth grade texts 
#'   and the hardest text from the State of the Union corpus, respectively, unless
#'   specified by the user} }
#' @import BradleyTerry2
#' @importFrom data.table as.data.table
#' @export
#' @examples
#' \dontrun{
#' load("analysis_article/output/fitted_BT_model.Rdata")
#'
#' head(predict_readability(BT_best))
#' ##           lambda       prob   scaled
#' ## 100014 -3.296731 0.24593816 60.51612
#' ## 100028 -3.190470 0.26617180 64.26088
#' ## 100029 -3.719532 0.17607128 45.61617
#' ## 100033 -4.703668 0.07396423 10.93416
#' ## 100034 -3.289739 0.24723716 60.76252
#' ## 100045 -2.780185 0.35346383 78.71976
#'
#' txts <- c(fifthgrade = paste(texts(data_corpus_fifthgrade), collapse = "  "),
#'           data_corpus_inaugural[c(1:2, 9:10, 54:58)])
#' predict_readability(BT_best, newdata = txts)
#' ##                    lambda       prob    scaled
#' ## fifthgrade      -2.128336 0.51199792 102.84175
#' ## 1789-Washington -5.494969 0.03493749 -96.46991
#' ## 1793-Washington -2.852801 0.33705102  59.95195
#' ## 1821-Monroe     -3.629638 0.18949402  13.96156
#' ## 1825-Adams      -4.138627 0.12321942 -16.17163
#' ## 2001-Bush       -2.273380 0.47575815  94.25482
#' ## 2005-Bush       -2.583155 0.39967525  75.91551
#' ## 2009-Obama      -2.529601 0.41259115  79.08604
#' ## 2013-Obama      -2.747889 0.36087883  66.16295
#' ## 2017-Trump      -2.359702 0.45428669  89.14440
#'
#' years <- c(2000, as.integer(substring(names(txts)[-1], 1, 4)))
#' predict_readability(BT_best, newdata = txts, baseline_year = years)
#' ##                    lambda       prob    scaled
#' ## fifthgrade      -2.128338 0.51199736 102.84162
#' ## 1789-Washington -5.494972 0.03493741 -96.47004
#' ## 1793-Washington -2.852803 0.33705052  59.95182
#' ## 1821-Monroe     -3.629640 0.18949368  13.96143
#' ## 1825-Adams      -4.138629 0.12321918 -16.17177
#' ## 2001-Bush       -2.273383 0.47575759  94.25469
#' ## 2005-Bush       -2.583158 0.39967471  75.91538
#' ## 2009-Obama      -2.529603 0.41259061  79.08591
#' ## 2013-Obama      -2.747891 0.36087832  66.16282
#' ## 2017-Trump      -2.359704 0.45428614  89.14426
#' 
#' names(txts) <- gsub("ington", "", names(txts))
#' pr <- predict_readability(BT_best, newdata = txts[c(1:3, 9:10)], bootstrap_n = 100)
#' format(pr, digits = 4)
#' ##            lambda    prob scaled lambda_lo lambda_hi  prob_lo prob_hi scaled_lo scaled_hi
#' ## fifthgrade -2.172 0.50105 100.15    -2.210    -2.135 0.491591 0.51032     98.81   101.455
#' ## 1789-Wash  -5.676 0.02931 -23.35    -6.917    -4.870 0.008664 0.06336    -67.06     5.076
#' ## 1793-Wash  -3.560 0.20036  51.22    -4.524    -2.609 0.087218 0.39358     17.25    84.765
#' ## 2013-Obama -2.791 0.35107  78.35    -2.914    -2.645 0.323485 0.38483     74.00    83.469
#' ## 2017-Trump -2.381 0.44904  92.79    -2.511    -2.213 0.417178 0.49080     88.22    98.704
#'
#' predict_readability(BT_best, newdata = "The cat in the hat ate green eggs and ham.")
#' ##      lambda      prob   scaled
#' ## 1 -1.125721 0.7408932 137.0248
#'
#' }
predict_readability <- function(object, newdata, reference_top = -2.1763368548, 
                                reference_bottom = -3.865467, bootstrap_n = 0, 
                                baseline_year = 2000,
                                verbose = FALSE) {
    UseMethod("predict_readability")
}

#' @export
#' @importFrom scales rescale
#' @importFrom data.table data.table
#' @importFrom utils packageVersion
#' @importFrom stats coef
predict_readability.BTm <- function(object, newdata, reference_top = -2.1763368548, 
                                    reference_bottom = -3.865467, bootstrap_n = 0, 
                                    baseline_year = 2000,
                                    verbose = FALSE) {

    `:=` <- NA
    prob <- lambda <- scaled <- resample <- NULL

    time1 <- proc.time()
    if (verbose)
        message("Starting predict_readability (sophistication v", packageVersion("sophistication"), ")...")

    if (missing(newdata) && bootstrap_n > 0)
        warning("bootstrap_n ignored; only new texts can be bootstrapped")

    if (verbose)
        message("   ...using ", deparse(substitute(object)), " as fitted BT model", appendLF = FALSE)

    # extract the coefficients
    coeffs <- coef(object)
    # strip the "[ID]" from the coef names
    names(coeffs) <- gsub("[ID]", "", names(coeffs), fixed = TRUE)

    if (missing(newdata)) {
        if (verbose) message(" and as newdata")
        if (bootstrap_n > 0) {
            warning("bootstrapping can only be applied to new data")
            bootstrap_n <- 0
        }
        newdata_docnames <- levels(object$data$easier$ID)
        # get newdata from object, if not supplied
        newdata <- as.data.table(object$data[names(coeffs)])
        newdata[, c("_docid", "resample") := list(levels(object$data$easier$ID), 0)]

    } else {
        if (verbose) message("; ", deparse(substitute(newdata)), " as newdata")
        # otherwise compute covars on newdata supplied as character or corpus
        if (is.corpus(newdata)) newdata <- texts(newdata)
        if (is.character(newdata)) {
            newdata_docnames <- names(newdata)
            newdata <- get_covars_from_newdata(newdata, bootstrap_n, 
                                               names(coeffs), 
                                               baseline_year = baseline_year,
                                               verbose = verbose)
        } else {
            stop("newdata must be a character or corpus object")
        }
    }

    # make coefficient name from newdata fitting same as fitted data
    # this just means that the prediction will work
    names(newdata)[grep("^google_min", names(newdata))] <- 
        grep("^google_min", names(coeffs), value = TRUE)
    
    # error checks
    if (!all(whichfound <- names(coeffs) %in% names(newdata))) {
        warning("dropping coefficients for which variables are not found in newdata:\n", names(coeffs)[!whichfound])
        coeffs <- coeffs[whichfound]
    }
    
    # select just the ID vars and coefficients
    newdata <- newdata[, c("_docid", "resample", names(coeffs)), with = FALSE]
    # compute the predictions, output as a named vector
    if (verbose) message("   ...computing predicted values")
    newdata$lambda <- apply(newdata[, names(coeffs), with = FALSE], 1, function(z) sum(z * coeffs))
    # compute the probability that a text is easier than the reference text
    newdata[, prob := exp(lambda) / (exp(lambda) + exp(reference_top))]

    # compute the rescaled lambda
    # use the references for the 0 and 100 endpoints
    newdata[, scaled := scales::rescale(lambda, to = c(0, 100), from = c(reference_bottom, reference_top))]

    # combine into data.frame
    result <- as.data.frame(newdata[resample == 0, list(lambda, prob, scaled)])

    # add CIs if any
    if (bootstrap_n > 0) {
        result <- cbind(result,
                        newdata[resample > 0,
                                list(lambda_lo = quantile(lambda, 0.025), lambda_hi = quantile(lambda, 0.975)), by = "_docid"][, 2:3, with = FALSE])
        result <- cbind(result,
                        newdata[resample > 0,
                                list(prob_lo = quantile(prob, 0.025), prob_hi = quantile(prob, 0.975)), by = "_docid"][, 2:3, with = FALSE])
        result <- cbind(result,
                        newdata[resample > 0,
                                list(scaled_lo = quantile(scaled, 0.025), scaled_hi = quantile(scaled, 0.975)), by = "_docid"][, 2:3, with = FALSE])
    }

    rownames(result) <- newdata_docnames
    if (verbose)
        message("   ...finished; elapsed time: ", format((proc.time() - time1)[3], digits = 3), " seconds.")
    result
}

# get_covars_from_newdata ----

get_covars_from_newdata <- function(x, bootstrap_n = 0, covar_selection = NULL, 
                                    baseline_year = 2000, verbose = FALSE) {
    UseMethod("get_covars_from_newdata")
}

get_covars_from_newdata.character <- function(x, bootstrap_n = 0, 
                                              covar_selection = NULL, 
                                              baseline_year = 2000,
                                              verbose = FALSE) {
    get_covars_from_newdata(corpus(x), bootstrap_n, covar_selection, 
                            baseline_year, verbose = verbose)
}

get_covars_from_newdata.corpus <- function(x, bootstrap_n = 0, 
                                           covar_selection = NULL, 
                                           baseline_year = 2000,
                                           verbose = FALSE) {

    .SD <- .N <- `:=` <- NULL

    covars <- get_covars_new(x, baseline_year = baseline_year, verbose = verbose)
    # covars <- covars_make_all(dt$texts, dependency = FALSE, verbose = verbose)
    # covars_select <- c(covar_selection, c("C", "St", "n_noun", "ntoken", "W"))
    # dt <- cbind(dt, covars[, covars_select])

    result <- computed_aggregated_covars(covars, 0)

    if (verbose && bootstrap_n > 0) 
        message("   ...recombining bootstrapped sentence-level quantities")
    for (i in seq_len(bootstrap_n)) {
        result <- rbind(result,
                        computed_aggregated_covars(covars[, .SD[sample(.N, replace = TRUE)], by = "doc_id"], i))
    }

    setnames(result, "doc_id", "_docid")
    result
}

computed_aggregated_covars <- function(y, i) {
    `:=` <- .N <- NA
    C <- W <- n_noun <- google_min <- St <- resample <- n_chars <- n_token <- NULL

    y <- y[, list(C = sum(n_chars, na.rm = TRUE), W = sum(n_token, na.rm = TRUE), St = .N,
                  n_noun = sum(n_noun, na.rm = TRUE), google_min = min(google_min, na.rm = TRUE)),
                  by = "doc_id"]

    y[, c("meanWordChars", "pr_noun", "meanSentenceChars",   "google_min") :=
           list  ( C / W ,  n_noun/W,             C / St , min(google_min))]
    y[, resample := i]
    y
}


## compute all covariates based on spacy parse
## needs an entire text, not parsed into sentences
get_covars_new <- function(x, baseline_year = 2000, verbose = FALSE) {
    UseMethod("get_covars_new")
}

get_covars_new.character <- function (x, baseline_year = 2000, verbose = FALSE) {
    get_covars_new(corpus(x))
}

get_covars_new.corpus <- function(x, baseline_year = 2000, verbose = FALSE) {
    google_min <- pos <- `:=` <- nchars <- token <- sentence_id <- years <- NULL
    doc_id <- .N <- NULL

    if (verbose) message("   ...tagging parts of speech\n   ...")
    suppressMessages(
        spacyr::spacy_initialize(python_executable = getOption("PYTHON_EXECUTABLE"))
    )
    result <- data.table(spacyr::spacy_parse(texts(x), tag = FALSE, lemma = FALSE, entity = FALSE, dependency = FALSE))
    # remove punctuation
    result <- result[pos != "PUNCT" & pos != "SPACE"]
    
    # if years is a vector, repeat for each token
    if (length(baseline_year) > 1)
        baseline_year <- rep(baseline_year,
                             result[, list(years = length(sentence_id)), by = doc_id][, years])

    if (verbose) message("   ...computing word lengths in characters")
    result[, nchars := stringi::stri_length(token)]

    if (verbose) message("   ...computing baselines from Google frequencies")
    bl_google <- 
        suppressWarnings(make_baselines_google(result$token, baseline_word = "the",
                                               baseline_year = baseline_year)[, 2])
    result[, google_min := bl_google]

    if (verbose) message("   ...aggregating to sentence level")
    result[,
           list(doc_id = doc_id[1],
                n_noun = sum(pos == "NOUN", na.rm = TRUE),
                n_chars = sum(nchars, na.rm = TRUE),
                google_min = min(google_min, na.rm = TRUE),
                n_token = .N),
           by = c("sentence_id", "doc_id")]
}
