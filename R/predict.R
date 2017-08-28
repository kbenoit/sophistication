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
#'   against which each predicted text will be compared for difficulty.  The
#'   default value is the \eqn{lambda} applied to all of
#'   \code{\link{data_corpus_fifthgrade}} (\code{reference_top}), and for the
#'   bottom, the "hardest" text represented by \code{reference_top}.  The
#'   reference values are also used to score the 100 for the rescaled lambda.
#'   The default values come from the snippets we fit to the model taken from
#'   the SOTU corpus.  (See \code{f999866.csv}.)
#' @param bootstrap_n number of bootstrap replicates for computing intervals
#' @param verbose logical; if \code{TRUE} print status messages
#' @return a data.frame with the rows named to the text names, and the columns
#'   consisting of: \describe{ \item{\code{lambda}}{estimated lambda for each
#'   text} \item{\code{prob}}{the probability that the text is easier than the
#'   reference lambda)} \item{\code{scaled}}{a rescaled lambda on a scale of
#'   "ease" ranging from 0-100, where 100 is set to the difficulty of the
#'   reference value (set by default at the fifth-grade reading level).  0 will
#'   be set at the difficulty level of the most difficult text in the training
#'   data.} }
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
#' ## fifthgrade      -2.168784 0.50188816 100.26616
#' ## 1789-Washington -5.678335 0.02925544 -23.41412
#' ## 1793-Washington -3.607503 0.19291702  49.56418
#' ## 1821-Monroe     -3.666829 0.18384785  47.47347
#' ## 1825-Adams      -4.167703 0.12011246  29.82216
#' ## 2001-Bush       -2.317610 0.46474035  95.02139
#' ## 2005-Bush       -2.652254 0.38321669  83.22817
#' ## 2009-Obama      -2.594972 0.39684336  85.24685
#' ## 2013-Obama      -2.779760 0.35356091  78.73473
#' ## 2017-Trump      -2.370844 0.45152595  93.14536
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
predict_readability <- function(object, newdata, reference_top = -2.1763368548, reference_bottom = -3.865467, bootstrap_n = 0, verbose = FALSE) {
    UseMethod("predict_readability")
}

#' @export
#' @importFrom scales rescale
#' @importFrom data.table data.table
#' @importFrom utils packageVersion
predict_readability.BTm <- function(object, newdata, reference_top = -2.1763368548, reference_bottom = -3.865467, bootstrap_n = 0, verbose = FALSE) {

    `:=` <- NA
    prob <- lambda <- scaled <- resample <- NULL

    time1 <- proc.time()
    if (verbose)
        message("Starting predict_readability (sophistication v", packageVersion("sophistication"), ")...")

    if (bootstrap_n > 0)
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
            newdata <- get_covars_from_newdata(newdata, bootstrap_n, names(coeffs), verbose = verbose)
        } else {
            stop("newdata must be a character or corpus object")
        }
    }

    # error checks
    if (!all(whichfound <- names(coeffs) %in% names(newdata))) {
        warning("dropping coefficients for which variables are not found in newdata:\n", names(coeffs)[!whichfound])
        coeffs <- coeffs[, whichfound, with = FALSE]
    }

    # select just the ID vars and coefficients
    newdata <- newdata[, c("_docid", "resample", names(coeffs)), with = FALSE]
    # compute the predictions, output as a named vector
    if (verbose) message("   ...computing predicted values")
    newdata$lambda <- apply(newdata[, names(coeffs), with = FALSE], 1, function(z) sum(z * coeffs))
    # compute the predictions, output as a named vector
    newdata[, prob := exp(lambda) / (exp(lambda) + exp(reference))]

    # compute the rescaled lambda
    # use the reference for the 100, and the hardest text from the training data as 0
    #hardest_lambda_from_trainingdata <- min(as.matrix(as.data.frame(object$data[names(coeffs)])) %*% coeffs)
    #newdata[, scaled := scales::rescale(lambda, to = c(0, 100), from = c(hardest_lambda_from_trainingdata, reference))]
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

get_covars_from_newdata <- function(x, bootstrap_n = 0, covar_selection = NULL, verbose = FALSE) {
    UseMethod("get_covars_from_newdata")
}

get_covars_from_newdata.character <- function(x, bootstrap_n = 0, covar_selection = NULL, verbose = FALSE) {
    get_covars_from_newdata(corpus(x), bootstrap_n, covar_selection, verbose = verbose)
}

get_covars_from_newdata.corpus <- function(x, bootstrap_n = 0, covar_selection = NULL, verbose = FALSE) {

    .SD <- .N <- `:=` <- NULL

    # if (bootstrap_n > 0) {
    #     if (verbose) message("   ...segmenting texts into sentences for bootstrapping ", bootstrap_n, " resamples")
    #     x <- corpus_segment(x, "sentences")
    #     x <- corpus_subset(x, stringi::stri_length(texts(x)) > 3)
    #     dt <- data.table(x$documents)
    # } else {
    #     if (verbose) message("   ...using whole document, without sentence segmentation")
    #     dt <- data.table(cbind(x$documents,
    #                            "_document" = docnames(x),
    #                            "_docid" = seq_len(ndoc(x)),
    #                            "_segid" = 1))
    # }

    covars <- get_covars_new(x, verbose = verbose)
    # covars <- covars_make_all(dt$texts, dependency = FALSE, verbose = verbose)
    # covars_select <- c(covar_selection, c("C", "St", "n_noun", "ntoken", "W"))
    # dt <- cbind(dt, covars[, covars_select])

    result <- computed_aggregated_covars(covars, 0)

    if (verbose && bootstrap_n > 0) message("   ...recombining bootstrapped sentence-level quantities")
    for (i in seq_len(bootstrap_n)) {
        result <- rbind(result,
                        computed_aggregated_covars(covars[, .SD[sample(.N, replace = TRUE)], by = "doc_id"], i))
    }

    setnames(result, "doc_id", "_docid")
    result
}

computed_aggregated_covars <- function(y, i) {
    `:=` <- .N <- NA
    C <- W <- n_noun <- google_min_2000 <- St <- resample <- n_chars <- n_token <- NULL

    y <- y[, list(C = sum(n_chars, na.rm = TRUE), W = sum(n_token, na.rm = TRUE), St = .N,
                  n_noun = sum(n_noun, na.rm = TRUE), google_min_2000 = min(google_min_2000, na.rm = TRUE)),
                  by = "doc_id"]

    y[, c("meanWordChars", "pr_noun", "meanSentenceChars",   "google_min_2000") :=
           list  ( C / W ,  n_noun/W,             C / St , min(google_min_2000))]
    y[, resample := i]
    y
}


## compute all covariates based on spacy parse
## needs an entire text, not parsed into sentences
get_covars_new <- function(x, verbose = FALSE) {
    UseMethod("get_covars_new")
}

get_covars_new.character <- function (x, verbose = FALSE) {
    get_covars_new(corpus(x))
}

get_covars_new.corpus <- function(x, verbose = FALSE) {
    google_min_2000 <- pos <- `:=` <- nchars <- token <- google_min_2000 <- NULL
    doc_id <- .N <- NULL

    if (verbose) message("   ...tagging parts of speech")
    suppressMessages(spacyr::spacy_initialize(python_executable = getOption("PYTHON_EXECUTABLE")))
    result <- data.table(spacyr::spacy_parse(texts(x), tag = FALSE, lemma = FALSE, entity = FALSE, dependency = FALSE))
    # remove punctuation
    result <- result[pos != "PUNCT" & pos != "SPACE"]

    if (verbose) message("   ...computing word lengths in characters")
    result[, nchars := stringi::stri_length(token)]

    if (verbose) message("   ...computing baselines from Google frequencies")
    result[, google_min_2000 := compute_google_min_2000(token)]

    if (verbose) message("   ...aggregating to sentence level")
    result[,
           list(doc_id = doc_id[1],
                n_noun = sum(pos == "NOUN", na.rm = TRUE),
                n_chars = sum(nchars, na.rm = TRUE),
                google_min_2000 = min(google_min_2000, na.rm = TRUE),
                n_token = .N),
           by = c("sentence_id", "doc_id")]
}

compute_google_min_2000 <- function(toks) {
    baseline_word <- "the"
    baseline_year <- 2000
    indexToken <- match(char_tolower(as.character(toks)), rownames(data_matrix_google1grams))

    # normalize token frequencies by baseline_word
    data_matrix_google1grams <- data_matrix_google1grams /
        rep(data_matrix_google1grams[baseline_word, ], each = nrow(data_matrix_google1grams))

    indexYear <-  which(colnames(data_matrix_google1grams) == as.character(baseline_year))
    baselines <- apply(data.frame(i = indexToken, j = indexYear), 1,
                       function(x) data_matrix_google1grams[x[1], x[2]])
    baselines <- split(baselines, rep(seq_along(toks), times = lengths(toks)))
    result <- data.frame(google_mean_ = sapply(baselines, mean, na.rm = TRUE),
                         google_min_ = sapply(baselines, min, na.rm = TRUE))
    # if (!is.null(names(x))) row.names(result) <- names(x)
    names(result)[(ncol(result)-1):ncol(result)] <-
        paste0(names(result)[(ncol(result)-1):ncol(result)], baseline_year)

    result$google_min_2000[is.infinite(result$google_min_2000)] <- NA
    result$google_min_2000
}
