HOST = 'https://gnlearn-api.herokuapp.com'

#' List Genesets Available Via RESTful API
#'
#' This function allows you to list genesets available via RESTful API.
#' @param sp.scientific Species (Scientific Name) (regex) (optional).
#' @param sp.common Species (Common Name) (regex) (optional).
#' @param dataset Dataset Name (regex) (optional).
#' @keywords genes api
#' @export
#' @examples
#' genesets <- list_genesets()
#' genesets <- list_genesets(sp.common='human')
#' genesets <- list_genesets(sp.common='mouse')

list_genesets <- function(sp.scientific=NULL, sp.common=NULL, dataset=NULL, host=HOST) {
    uri <- file.path(host, 'genesets')
    df <- jsonlite::fromJSON(uri)
    if (!is.null(sp.scientific)) {
        regex <- sp.scientific
        df <- df[with(df, grepl(regex, sp.scientific, ignore.case=TRUE)), ]
    }
    if (!is.null(sp.common)) {
        regex <- sp.common
        df <- df[with(df, grepl(regex, sp.common, ignore.case=TRUE)), ]
    }
    if (!is.null(dataset)) {
        regex <- dataset
        df <- df[with(df, grepl(regex, dataset, ignore.case=TRUE)), ]
    }
    return(df)
}

#' List Datasets Available Via RESTful API
#'
#' This function allows you to list datasets available via RESTful API.
#' @param sp.scientific Species (Scientific Name) (regex) (optional).
#' @param sp.common Species (Common Name) (regex) (optional).
#' @param bio.layer Biological Information Layer (regex) (optional).
#' @param seq.protocol Sequencing Protocol (regex) (optional).
#' @param cell.identity Biological Cell Identity (regex) (optional).
#' @param min.genes Minimum Number of Genes (optional).
#' @param max.genes Maximum Number of Genes (optional).
#' @param min.cells Minimum Number of Cells (optional).
#' @param max.cells Maximum Number of Cells (optional).
#' @keywords datasets api
#' @export
#' @examples
#' datasets <- list_datasets()
#' datasets <- list_datasets(sp.common='human')
#' datasets <- list_datasets(sp.common='mouse')

list_datasets <- function(sp.scientific=NULL, sp.common=NULL, bio.layer=NULL, seq.protocol=NULL, cell.identity=NULL,
                          min.genes=0, max.genes=Inf, min.cells=0, max.cells=Inf, host=HOST) {
    uri <- file.path(host, 'datasets')
    df <- jsonlite::fromJSON(uri)
    if (!is.null(sp.scientific)) {
        regex <- sp.scientific
        df <- df[with(df, grepl(regex, sp.scientific, ignore.case=TRUE)), ]
    }
    if (!is.null(sp.common)) {
        regex <- sp.common
        df <- df[with(df, grepl(regex, sp.common, ignore.case=TRUE)), ]
    }
    if (!is.null(bio.layer)) {
        regex <- bio.layer
        df <- df[with(df, grepl(regex, bio.layer, ignore.case=TRUE)), ]
    }
    if (!is.null(seq.protocol)) {
        regex <- seq.protocol
        df <- df[with(df, grepl(regex, seq.protocol, ignore.case=TRUE)), ]
    }
    if (!is.null(cell.identity)) {
        regex <- cell.identity
        df <- df[with(df, grepl(regex, cell.identity, ignore.case=TRUE)), ]
    }
    df <- df[df$n.genes > min.genes & df$n.genes < max.genes, ]
    df <- df[df$n.cells > min.cells & df$n.cells < max.cells, ]
    return(df)
}

#' Download A Geneset Via RESTful API
#'
#' This function allows you to download a geneset available via RESTful API.
#' @param code Download Code (indicated by list_genesets() output).
#' @keywords genes api
#' @export
#' @examples
#' df <- download_geneset(1)

download_geneset <- function(code, host=HOST) {
    uri <- file.path(host, 'genesets', code)
    df <- jsonlite::fromJSON(uri)
    url <- df$url
    if (!is.null(url)) {
        tmp <- fs::file_temp(ext='.txt')
        utils::download.file(url, destfile=tmp, quiet=TRUE, mode='wt')
        df <- read.table(tmp, sep='\t', header=TRUE, check.names=FALSE)
        return(df)
    } else {
        return(NULL)
    }
}

#' Download A Dataset Via RESTful API
#'
#' This function allows you to download a dataset available via RESTful API.
#' @param code Download Code (indicated by list_datasets() output).
#' @param log Whether or not to apply log(x+1) (optional). Default: FALSE
#' @keywords datasets api
#' @export
#' @examples
#' df <- download_dataset(1)

download_dataset <- function(code, log=FALSE, host=HOST) {
    uri <- file.path(host, 'datasets', code)
    df <- jsonlite::fromJSON(uri)
    url <- df$raw.dataset
    if (!is.null(url)) {
        tmp <- fs::file_temp(ext='.gz')
        utils::download.file(url, destfile=tmp, quiet=TRUE, mode='wb')
        tmp <- gzfile(tmp, 'rt')
        df <- read.table(tmp, sep='\t', header=TRUE, check.names=FALSE)
        df <- as.data.frame(lapply(df, as.double))
        df <- df[apply(df, 1, function(x) !all(x==0)),]
        df <- df[,apply(df, 2, function(x) !all(x==0))]
        if (log) {
            df <- log(df+1)
        }
        return(df)
    } else {
        return(NULL)
    }
}
