
#' compute mean baseline frequencies from Google ngrams
#'
#' Computes the mean frequencies of terms in a text based on closest match to
#' the Google unigram corpus, for the decade in which the text was recorded.
#' @param x data.frame of results, if already loaded
#' @param text_field the name of the text field, if a [data.frame], default
#'   is `"text"`
#' @param baseline_data `"brown"`, `"google"`, or both (the default) to indicate the
#'   Brown corpus data or Google n-grams data, respectively.
#' @param baseline_year a scalar or vector of the baseline years to choose for
#'   reference: a year ending in 0 from 1780-2000, or `NULL` to match a
#'   text to its nearest year (the year information is taken from the
#'   `textid` that is part of the Crowdflower data).  Does not apply if
#'   `baseline_data = "brown"`.
#' @param baseline_word the word against which other word frequencies will be
#'   baselined.  This defaults to "the" but can be any word found in the word
#'   frequency tables
#' @seealso [data_matrix_google1grams()],
#'   [data_integer_brownfreq()]
#' @param ... additional arguments passed through to other functions
#' @return a data.frame suitable for adding to variates for analysis by
#'   [BTm][BradleyTerry2::BTm]
#' @importFrom utils read.csv
#' @importFrom quanteda tokens
#' @export
#' @examples
#' txt <- c(d1 = quanteda::data_char_sampletext,
#'          d2 = "No if and or but.",
#'          d3 = "the")
#' covars_make_baselines(txt)
#' 
#' txt2 <- rep("The art of husbandry is ancient.", 3)
#' names(txt2) <- paste0("doc", 1:3)
#' covars_make_baselines(txt2, baseline_data = "google")
#' covars_make_baselines(txt2, baseline_data = "google",
#'                       baseline_year = c(1790, 1850, 1980))
#'
#' \dontrun{
#' head(covars_make_baselines(file = "data/CF_results/f921916.csv"))
#' head(bt_input_make(file = "data/CF_results/f921916.csv",
#'                    covars = TRUE, readability_measure = "Flesch")$predictors)
#' }
#'
covars_make_baselines <- function(x, ...) {
    UseMethod("covars_make_baselines")
}


#' @rdname covars_make_baselines
#' @export
covars_make_baselines.snippet <- function(x, ...) {
     covars_make_baselines(x[["text"]], ...)
}

#' @rdname covars_make_baselines
#' @export
covars_make_baselines.data.frame <- function(x, text_field = "text", ...) {
    covars_make_baselines(x[[text_field]], ...)
}

#' @rdname covars_make_baselines
#' @export
covars_make_baselines.corpus <- function(x, ...) {
    covars_make_baselines(as.character(x), ...)
}

#' @rdname covars_make_baselines
#' @export
covars_make_baselines.character <- function(x, baseline_data = c("brown", "google"),
                                            baseline_year = 2000, baseline_word = "the", ...) {
    baseline_data <- match.arg(baseline_data, several.ok = TRUE)
    result <- NULL

    if ("brown" %in% baseline_data) {
        result <- c(result, make_baselines_brown(x, baseline_word = baseline_word))
    }

    if ("google" %in% baseline_data) {
        result <- c(result, make_baselines_google(x, baseline_year = baseline_year,
                                                  baseline_word = baseline_word))
    }

    as.data.frame(result, row.names = names(x))
}

make_baselines_google <- function(x, baseline_year, baseline_word) {

    # check that a single baseline_year is in the matrix
    if (length(baseline_year) >= 1 && !all(baseline_year >= 1780))
        warning("baseline_year < 1790 set to 1790")

    # check that baseline_year is one per text, if not a single value
    if (length(baseline_year) > 1 && length(baseline_year) != length(x))
        stop("baseline_year must be scalar or equal in length to the number of documents")

    # round to nearest decade
    baseline_year <- floor(baseline_year / 10) * 10
    # fix 2010 and higher to 2000
    max_google_year <- max(as.integer(colnames(data_matrix_google1grams)))
    baseline_year[baseline_year > max_google_year] <- max_google_year
    # fix below 1790 to 1790
    baseline_year[baseline_year < 1790] <- 1790

    # check that baseline word exists in the matrix
    if (!baseline_word %in% rownames(data_matrix_google1grams))
        stop(baseline_year, " is not part of data_matrix_google1grams")

    toks <- quanteda::tokens(quanteda::char_tolower(x), remove_punct = TRUE, remove_numbers = TRUE,
                             split_hyphens = TRUE)
    zero_length_tokens <- lengths(toks) == 0
    if (any(zero_length_tokens)) {
        toks2 <- as.list(toks)
        toks2[which(zero_length_tokens)] <- "_NA_"
        toks <- as.tokens(toks2)
    }

    # normalize token frequencies by baseline_word, just for types found
    data_matrix_google1grams <- data_matrix_google1grams /
        rep(data_matrix_google1grams[baseline_word, ], each = nrow(data_matrix_google1grams))
    
    # get the indexes for tokens
    indexToken <- match(as.character(toks), rownames(data_matrix_google1grams))
    # get the indexes for years
    indexYear <- match(as.character(baseline_year), colnames(data_matrix_google1grams))
    if (length(indexYear) > 1) {
        indexYear <- rep(indexYear, times = quanteda::ntoken(toks))
    }

    # look up years
    baselines <- apply(data.frame(i = indexToken, j = indexYear), 1,
                       function(x) data_matrix_google1grams[x[1], x[2]])
    baselines <- split(baselines, rep(seq_along(toks), times = lengths(toks)))
    result <- data.frame(google_mean_ = sapply(baselines, mean, na.rm = TRUE),
                         google_min_ = sapply(baselines, min, na.rm = TRUE))
    
    # set row and column names
    if (!is.null(names(x))) row.names(result) <- names(x)
    yearlab <- if (length(baseline_year) > 1) {
        "local"
    } else {
        as.character(baseline_year) 
    }
    names(result)[(ncol(result)-1):ncol(result)] <-
        paste0(names(result)[(ncol(result)-1):ncol(result)], yearlab)

    result
}

make_baselines_brown <- function(x, baseline_word) {
    baseline_year <- NULL
    toks <- quanteda::tokens(quanteda::char_tolower(x), remove_punct = TRUE, remove_numbers = TRUE,
                             split_hyphens = TRUE)
    zero_length_tokens <- lengths(toks) == 0
    if (any(zero_length_tokens)) {
        toks2 <- as.list(toks)
        toks2[which(zero_length_tokens)] <- "_NA_"
        toks <- as.tokens(toks2)
    }

    if (!baseline_word %in% names(sophistication::data_integer_brownfreq))
        stop(baseline_year, " is not part of data_integer_brownfreq")
    indexToken <- match(as.character(toks), names(sophistication::data_integer_brownfreq))

    # get proportions relative to baseline_word
    data_relfreq_brown <- sophistication::data_integer_brownfreq /
        sophistication::data_integer_brownfreq[baseline_word]

    baselines <- data_relfreq_brown[indexToken]
    baselines <- split(baselines, rep(seq_along(toks), times = lengths(toks)))
    result <- data.frame(brown_mean = sapply(baselines, mean, na.rm = TRUE),
                         brown_min = sapply(baselines, min, na.rm = TRUE))

    if (!is.null(names(x))) row.names(result) <- docnames(toks)
    result
}
