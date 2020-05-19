#' Import A Local Dataset
#'
#' This function allows you to import a local dataset.
#' @param path Path to local dataset.
#' @param log Whether or not to apply log(x+1) (optional). Default: FALSE
#' @param sep Separator / delimiter character (optional). Default: '\t'
#' @param header Whether or not the file has a header (optional). Default: TRUE
#' @param index Whether or not the file has an index column (optional). Default: FALSE
#' @param transpose Whether or not to transpose the data matrix (optional). IMPORTANT NOTE: gnlearn works with cell x gene matrices. Default: FALSE
#' @keywords datasets
#' @export
#' @examples
#' df <- import_dataset('mydataset.tsv')

import_dataset <- function(path, log=FALSE, sep='\t', header=TRUE, index=FALSE, transpose=FALSE) {
    ext <- tools::file_ext(path)
    if (ext == 'gz') {
        file <- gzfile(path, 'rt')
    } else if (ext %in% c('bz', 'bz2')) {
        file <- bzfile(path, 'rt')
    } else if (ext == 'xz') {
        file <- xzfile(path, 'rt')
    }
    if (index) {
        df <- read.table(file, sep=sep, header=header, row.names=1, check.names=FALSE)
    } else {
        df <- read.table(file, sep=sep, header=header, check.names=FALSE)
    }
    df <- as.data.frame(lapply(df, as.double))
    df <- df[apply(df, 1, function(x) !all(x==0)),]
    df <- df[,apply(df, 2, function(x) !all(x==0))]
    if (transpose) {
        df <- as.data.frame(t(as.matrix(df)))
    }
    if (log) {
        df <- log(df+1)
    }
    return(df)
}
