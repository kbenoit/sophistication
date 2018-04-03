
#' load a file from the \code{data_pkg} Dropbox location
#'
#' Loads a data file from the \code{data_pkg} directory from a user's Dropbox
#' root folder.  This keeps very large data objects outside of the package, and
#' off of GitHub.
#' @param object the data object to be loaded (not quoted)
#' @param location the directory where the large data objects are located.  The
#'   default location is set upon loading the package, by the environment
#'   variable \code{getOption("ROOT_DROPBOX")}.
#' @return loads the object into the global environment
#' @export
#' @examples
#' \dontrun{
#' data_dropbox(data_corpus_SCOTUS)
#' }
data_dropbox <- function(object, location = paste0(getOption("ROOT_DROPBOX"), "data_pkg/")) {
    load(paste0(location, paste0(deparse(substitute(object)), ".rda")), envir = .GlobalEnv)
}
