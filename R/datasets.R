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
#' genesets <- list.genesets()
#' genesets <- list.genesets(sp.common='human')
#' genesets <- list.genesets(sp.common='mouse')

list.genesets <- function(sp.scientific=NULL, sp.common=NULL, dataset=NULL, host=HOST) {
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
#' datasets <- list.datasets()
#' datasets <- list.datasets(sp.common='human')
#' datasets <- list.datasets(sp.common='mouse')

list.datasets <- function(sp.scientific=NULL, sp.common=NULL, bio.layer=NULL, seq.protocol=NULL, cell.identity=NULL,
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
#' @param code Download Code (indicated by list.genesets() output).
#' @keywords genes api
#' @export
#' @examples
#' df <- download.geneset(1)

download.geneset <- function(code, host=HOST) {
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
#' @param code Download Code (indicated by list.datasets() output).
#' @param log Whether or not to apply log(x+1) (optional). Default: TRUE
#' @keywords datasets api
#' @export
#' @examples
#' df <- download.dataset (1)

download.dataset  <- function(code, log=TRUE, host=HOST) {
    uri <- file.path(host, 'datasets', code)
    df <- jsonlite::fromJSON(uri)
    url <- df$raw.dataset
    if (!is.null(url)) {
        tmp <- fs::file_temp(ext='.gz')
        utils::download.file(url, destfile=tmp, quiet=TRUE, mode='wb')
        tmp <- gzfile(tmp, 'rt')
        df <- read.table(tmp, sep='\t', header=TRUE, check.names=FALSE)
        df <- as.data.frame(lapply(df, as.double))
        df <- drop.all.zeros(df)
        if (log) {
            df <- log(df+1)
        }
        return(df)
    } else {
        return(NULL)
    }
}

#' Import A Local Dataset
#'
#' This function allows you to import a local dataset.
#' @param path Path to local dataset.
#' @param log Whether or not to apply log(x+1) (optional). Default: FALSE
#' @param sep Separator / delimiter character (optional). Default: TAB
#' @param header Whether or not the file has a header (optional). Default: TRUE
#' @param index Whether or not the file has an index column (optional). Default: FALSE
#' @param transpose Whether or not to transpose the data matrix (optional). IMPORTANT NOTE: gnlearn works with cell x gene matrices. Default: FALSE
#' @keywords datasets
#' @export
#' @examples
#' df <- import.dataset('mydataset.tsv')

import.dataset <- function(path, log=FALSE, sep='\t', header=TRUE, index=FALSE, transpose=FALSE) {
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
    df <- drop.all.zeros(df)
    if (transpose) {
        df <- as.data.frame(t(as.matrix(df)))
    }
    if (log) {
        df <- log(df+1)
    }
    return(df)
}

#' Select (automatically) the most appropriate gene columns of your dataset.
#'
#' This function allows you to (automatically) select the most appropriate gene columns of your dataset.
#' @param df Dataset.
#' @param genes Geneset with features (optional).
#' @param selected.genes User-selected genes (optional).
#' @param features Features you want to select (optional).
#' @param max.genes Maximum number of selected genes (optional).
#' @param min.non.zeros Minimum number of non-zero values per gene (optional).
#' @param glasso Using the GLASSO algorithm to exclude unconnected genes (optional).
#' @param rho Non-negative regularization parameter for GLASSO.
#' @keywords genes select
#' @export
#' @examples
#' df <- select.genes(df, max.genes=100, min.non.zeros=2)
#' df <- select.genes(df, genes=genes, features=c('tumor.suppressor', 'breast.cancer'), glasso=TRUE, rho=0.5)

select.genes <- function(df, genes=NULL, selected.genes=NULL, features=NULL, max.genes=100, min.non.zeros=2, glasso=TRUE, rho=0.5) {
    if (is.null(selected.genes)) {
        selected.genes <- colnames(df)
    }
    selected.genes <- selected.genes[colSums(df!=0) >= min.non.zeros]
    if (!is.null(genes) & !is.null(features)) {
        f_genes <- c()
        for (f in features) {
            tmp <- intersect(selected.genes, genes[genes[f]==TRUE,]$name)
            f_genes <- union(f_genes, tmp)
        }
        selected.genes <- f_genes
    }
    df <- subset(df, select=selected.genes)
    if (glasso & !is.null(rho)) {
        g <- run.glasso(df, rho, unconnected.nodes=FALSE)
        selected.genes <- names(igraph::V(g))
    }
    if (length(selected.genes) > max.genes) {
        df <- df[,1:max.genes]
    }
    return(df)
}