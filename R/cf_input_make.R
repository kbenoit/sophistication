##
## make output datasets for CrowdFlower
##

#' make dataset for CrowdFlower upload
#'
#' Prepare a dataset for uploading to CrowdFlower, output as a
#' comma-separated-value (.csv) file.
#' @param ... a series of datasets to be combined into a single dataset for
#'   upload to CF
#' @param filename the filename to which the .csv file will be written
#' @return Returns an invisible copy of the data.frame also written to
#'   \code{filename} in .csv format.
#' @importFrom utils write.csv
#' @export
cf_input_make <- function(..., filename = "CFdata.csv") {
    datasets <- list(...)

    # harmonize the column names
    datasets <- lapply(datasets, function(x) {
        x[["read1"]] <- x[["read2"]] <- x[["readdiff"]] <- NULL
        if (!("_golden" %in% names(x))) x[["_golden"]] <- ""
        if (!("easier_gold" %in% names(x))) x[["easier_gold"]] <- ""
        if (!("easier_gold_reason" %in% names(x))) x[["easier_gold_reason"]] <- ""
        if (!("screener" %in% names(x))) x[["screener"]] <- ""
        x
    })

    # join the rows
    outputdf <- do.call(rbind, datasets)

    # output as a .csv file
    utils::write.csv(outputdf, file = filename, row.names = FALSE)

    # return as a data.frame
    invisible(outputdf)
}
