.onAttach <- function(...) {
    if (requireNamespace("spacyr", quietly = TRUE)) {
        spacyr::spacy_initialize()
    } else {
        "You need to install spacyr before you can use this package."
    }
}
