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
    df <- df[df$n.genes >= min.genes & df$n.genes <= max.genes, ]
    df <- df[df$n.cells >= min.cells & df$n.cells <= max.cells, ]
    return(df)
}

#' List Graphs Available Via RESTful API
#'
#' This function allows you to list graphs available via RESTful API.
#' @param sp.scientific Species (Scientific Name) (regex) (optional).
#' @param sp.common Species (Common Name) (regex) (optional).
#' @param dataset Dataset used as input to learn the graph (Dataset Download Code) (optional).
#' @param bio.layer Biological Information Layer (regex) (optional).
#' @param cell.identity Biological Cell Identity (regex) (optional).
#' @param algorithm Algorithm used to learn the graph (regex) (optional).
#' @param min.nodes Minimum Number of Nodes (optional).
#' @param max.nodes Maximum Number of Nodes (optional).
#' @param min.edges Minimum Number of Edges (optional).
#' @param max.edges Maximum Number of Edges (optional).
#' @keywords graphs api
#' @export
#' @examples
#' graphs <- list.graphs()
#' graphs <- list.graphs(sp.common='human')
#' graphs <- list.graphs(sp.common='mouse')

list.graphs <- function(sp.scientific=NULL, sp.common=NULL, dataset=NULL, bio.layer=NULL, cell.identity=NULL, algorithm=NULL,
                        min.nodes=0, max.nodes=Inf, min.edges=0, max.edges=Inf, host=HOST) {
    uri <- file.path(host, 'graphs')
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
        df <- df[df$dataset == dataset, ]
    }
    if (!is.null(bio.layer)) {
        regex <- bio.layer
        df <- df[with(df, grepl(regex, bio.layer, ignore.case=TRUE)), ]
    }
    if (!is.null(cell.identity)) {
        regex <- cell.identity
        df <- df[with(df, grepl(regex, cell.identity, ignore.case=TRUE)), ]
    }
    if (!is.null(algorithm)) {
        regex <- algorithm
        df <- df[with(df, grepl(regex, algorithm, ignore.case=TRUE)), ]
    }
    df <- df[df$n.nodes >= min.nodes & df$n.nodes <= max.nodes, ]
    df <- df[df$n.edges >= min.edges & df$n.edges <= max.edges, ]
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

#' Download A Graph Via RESTful API
#'
#' This function allows you to download a graph available via RESTful API.
#' @param code Download Code (indicated by list.graphs() output).
#' @keywords graphs api
#' @export
#' @examples
#' g <- download.graph(1)

download.graph <- function(code, to=c('igraph', 'adjacency', 'edges', 'graph', 'bnlearn'), host=HOST) {
    to <- match.arg(to)
    uri <- file.path(host, 'graphs', code)
    df <- jsonlite::fromJSON(uri)
    url <- df$url
    if (!is.null(url)) {
        tmp <- fs::file_temp(ext='.rds')
        utils::download.file(url, destfile=tmp, quiet=TRUE, mode='wb')
        g <- readRDS(tmp)
        g$average <- convert.format(g$average, from='adjacency', to=to)
        R <- length(g$replicates)
        for (i in 1:R) {
            g$replicates[[i]] <- convert.format(g$replicates[[i]], from='adjacency', to=to)
        }
        return(g)
    } else {
        return(NULL)
    }
}

#' Import A Local Geneset
#'
#' This function allows you to import a local geneset.
#' @param path Path to local geneset.
#' @param sep Separator / delimiter character (optional). Default: TAB
#' @param header Whether or not the file has a header (optional). Default: TRUE
#' @param index Whether or not the file has an index column (optional). Default: FALSE
#' @keywords genesets
#' @export
#' @examples
#' df <- import.geneset('mygeneset.tsv')

import.geneset <- function(path, sep='\t', header=TRUE, index=FALSE) {
    ext <- tools::file_ext(path)
    if (ext == 'gz') {
        file <- gzfile(path, 'rt')
    } else if (ext %in% c('bz', 'bz2')) {
        file <- bzfile(path, 'rt')
    } else if (ext == 'xz') {
        file <- xzfile(path, 'rt')
    } else {
        file <- file(path, 'rt')
    }
    if (index) {
        df <- read.table(file, sep=sep, header=header, row.names=1, check.names=FALSE)
    } else {
        df <- read.table(file, sep=sep, header=header, check.names=FALSE)
    }
    df <- as.data.frame(df)
    close(file)
    return(df)
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
    } else {
        file <- file(path, 'rt')
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
    close(file)
    return(df)
}

#' Import A Local Graph
#'
#' This function allows you to import a local graph.
#' @param path Path to local graph.
#' @param sep Separator / delimiter character (optional). Default: TAB
#' @param header Whether or not the file has a header (optional). Default: TRUE
#' @param index Whether or not the file has an index column (optional). Default: TRUE
#' @param from Input format (optional): 'adjacency' or 'edges'. Default: 'adjacency'
#' @param to Output format (optional): 'adjacency', 'edges', 'graph', 'igraph', or 'bnlearn'. Default: 'igraph'
#' @keywords graphs
#' @export
#' @examples
#' g <- import.graph('mygraph.tsv')

import.graph <- function(path, sep='\t', header=TRUE, index=TRUE, from=c('adjacency','edges'),
                         to=c('igraph', 'adjacency', 'edges', 'graph', 'bnlearn')) {
    from <- match.arg(from)
    to <- match.arg(to)
    ext <- tools::file_ext(path)
    if (ext == 'gz') {
        file <- gzfile(path, 'rt')
    } else if (ext %in% c('bz', 'bz2')) {
        file <- bzfile(path, 'rt')
    } else if (ext == 'xz') {
        file <- xzfile(path, 'rt')
    } else {
        file <- file(path, 'rt')
    }
    if (from=='edges') {
        index <- FALSE
    }
    if (index) {
        df <- read.table(file, sep=sep, header=header, row.names=1, check.names=FALSE)
    } else {
        df <- read.table(file, sep=sep, header=header, check.names=FALSE)
    }
    df <- as.data.frame(df)
    if (header & !index & from=='adjacency') {
        rownames(df) <- colnames(df)
    }
    if (!header & index & from=='adjacency') {
        colnames(df) <- rownames(df)
    }
    g <- convert.format(df, from=from, to=to)
    close(file)
    return(g)
}

#' Export A Geneset To A File
#'
#' This function allows you to export a geneset to a file.
#' @param df Geneset to export.
#' @param path Output file path.
#' @param sep Separator / delimiter character (optional). Default: TAB
#' @param header Whether or not to include a header (optional). Default: TRUE
#' @param index Whether or not to include an index column (optional). Default: FALSE
#' @keywords genesets
#' @export
#' @examples
#' export.geneset(df, 'mygeneset.tsv')

export.geneset <- function(df, path, sep='\t', header=TRUE, index=FALSE) {
    ext <- tools::file_ext(path)
    if (ext == 'gz') {
        file <- gzfile(path, 'wt')
    } else if (ext %in% c('bz', 'bz2')) {
        file <- bzfile(path, 'wt')
    } else if (ext == 'xz') {
        file <- xzfile(path, 'wt')
    } else {
        file <- file(path, 'wt')
    }
    write.table(df, file, append=FALSE, quote=FALSE, sep=sep, col.names=header, row.names=index)
    close(file)
}

#' Export A Dataset To A File
#'
#' This function allows you to export a dataset to a file.
#' @param df Dataset to export.
#' @param path Output file path.
#' @param sep Separator / delimiter character (optional). Default: TAB
#' @param header Whether or not to include a header (optional). Default: TRUE
#' @param index Whether or not to include an index column (optional). Default: FALSE
#' @param transpose Whether or not to transpose the data matrix (optional). IMPORTANT NOTE: gnlearn works with cell x gene matrices. Default: FALSE
#' @keywords datasets
#' @export
#' @examples
#' export.dataset(df, 'mydataset.tsv')

export.dataset <- function(df, path, sep='\t', header=TRUE, index=FALSE, transpose=FALSE) {
    if (transpose) {
        df <- as.data.frame(t(as.matrix(df)))
    }
    ext <- tools::file_ext(path)
    if (ext == 'gz') {
        file <- gzfile(path, 'wt')
    } else if (ext %in% c('bz', 'bz2')) {
        file <- bzfile(path, 'wt')
    } else if (ext == 'xz') {
        file <- xzfile(path, 'wt')
    } else {
        file <- file(path, 'wt')
    }
    write.table(df, file, append=FALSE, quote=FALSE, sep=sep, col.names=header, row.names=index)
    close(file)
}

#' Export A Graph To A File
#'
#' This function allows you to graph a geneset to a file.
#' @param g Graph to export.
#' @param path Output file path.
#' @param sep Separator / delimiter character (optional). Default: TAB
#' @param header Whether or not to include a header (optional). Default: TRUE
#' @param index Whether or not to include an index column (optional). Default: TRUE
#' @param from Input format (optional): 'auto', 'adjacency', 'edges', 'graph', 'igraph', or 'bnlearn'. Default: 'auto'
#' @param from Output format (optional): 'adjacency' or 'edges'. Default: 'adjacency'
#' @keywords graphs
#' @export
#' @examples
#' export.graph(g, 'mygraph.tsv')

export.graph <- function(g, path, sep='\t', header=TRUE, index=TRUE,
                         from=c('auto', 'adjacency', 'edges', 'graph', 'igraph', 'bnlearn'), to=c('adjacency','edges')) {
    to <- match.arg(to)
    from <- match.arg(from)
    if (from=='auto') {
        from <- detect.format(g)
    }
    df <- convert.format(g, from=from, to=to)
    ext <- tools::file_ext(path)
    if (ext == 'gz') {
        file <- gzfile(path, 'wt')
    } else if (ext %in% c('bz', 'bz2')) {
        file <- bzfile(path, 'wt')
    } else if (ext == 'xz') {
        file <- xzfile(path, 'wt')
    } else {
        file <- file(path, 'wt')
    }
    write.table(df, file, append=FALSE, quote=FALSE, sep=sep, col.names=header, row.names=index)
    close(file)
}

#' Select (automatically) the most appropriate gene columns and cell rows of your dataset.
#'
#' This function allows you to (automatically) select the most appropriate gene columns and cell rows of your dataset.
#' @param df Dataset.
#' @param filter.first Whether to filter the cells or the genes first. Default: cells
#' @param genes Geneset with features (optional).
#' @param selected.genes User-selected genes (optional).
#' @param features Features you want to select (optional).
#' @param max.genes Maximum number of selected genes (optional).
#' @param max.cells Maximum number of selected cells (optional).
#' @param genes.criteria Criteria to be applied when it is necessary to select a maximum number of genes from the previously filtered ones. Default: 'non.zeros'
#' @param cells.criteria Criteria to be applied to select cells. Default: 'non.zeros'
#' @param min.non.zeros Minimum number of non-zero values per gene (optional).
#' @param cor Using correlation to exclude unconnected genes (optional).
#' @param cor.method Correlation method: 'spearman', 'kendall', or 'pearson'. Default: 'spearman'
#' @param cor.threshold Correlation threshold. Default: 0.1
#' @keywords select genes
#' @export
#' @examples
#' df <- filter.dataset(df, max.genes=100, max.cells=2000)

filter.dataset <- function(df, filter.first=c('cells','genes'), genes=NULL, selected.genes=NULL, features=NULL,
                           max.genes=100, max.cells=2000, genes.criteria=c('non.zeros','mean','variance','random'),
                           cells.criteria=c('non.zeros','mean','variance','random'), min.non.zeros=10,
                           cor=TRUE, cor.method=c('spearman','kendall','pearson'), cor.threshold=0.25) {
    filter.first <- match.arg(filter.first)
    genes.criteria <- match.arg(genes.criteria)
    cells.criteria <- match.arg(cells.criteria)
    cor.method <- match.arg(cor.method)

    if (filter.first=='cells') {
        df <- select.cells(df, max.cells=max.cells, selection.criteria=cells.criteria)
        df <- select.genes(df, genes=genes, selected.genes=selected.genes, features=features,
                           max.genes=max.genes, selection.criteria=genes.criteria, min.non.zeros=min.non.zeros,
                           cor=cor, cor.method=cor.method, cor.threshold=cor.threshold)
    } else {
        df <- select.genes(df, genes=genes, selected.genes=selected.genes, features=features,
                           max.genes=max.genes, selection.criteria=genes.criteria, min.non.zeros=min.non.zeros,
                           cor=cor, cor.method=cor.method, cor.threshold=cor.threshold)
        df <- select.cells(df, max.cells=max.cells, selection.criteria=cells.criteria)
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
#' @param selection.criteria Criteria to be applied when it is necessary to select a maximum number of genes from the previously filtered ones. Default: 'non.zeros'
#' @param min.non.zeros Minimum number of non-zero values per gene (optional).
#' @param cor Using correlation to exclude unconnected genes (optional).
#' @param cor.method Correlation method: 'spearman', 'kendall', or 'pearson'. Default: 'spearman'
#' @param cor.threshold Correlation threshold. Default: 0.1
#' @keywords select genes
#' @export
#' @examples
#' df <- select.genes(df, max.genes=100, min.non.zeros=2)
#' df <- select.genes(df, genes=genes, features=c('tumor.suppressor', 'breast.cancer'), cor=TRUE, cor.threshold=0.7)

select.genes <- function(df, genes=NULL, selected.genes=NULL, features=NULL,
                         max.genes=100, selection.criteria=c('non.zeros','mean','variance','random'), min.non.zeros=10,
                         cor=TRUE, cor.method=c('spearman','kendall','pearson'), cor.threshold=0.25) {
    selection.criteria <- match.arg(selection.criteria)
    cor.method <- match.arg(cor.method)
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
    if (cor) {
        A <- cor(df, method=cor.method)
        diag(A) <- 0
        A[abs(A) < cor.threshold] <- 0
        A <- drop.all.zeros(A, square.matrix='all')
        selected.genes <- colnames(A)
    }
    if (is.null(selected.genes)) {
        if (length(colnames(df)) > max.genes) {
            selected.genes <- switch(selection.criteria,
                non.zeros = {
                    nz <- as.list(colSums(df!=0))
                    selected.genes <- names(nz[order(unlist(nz), decreasing=TRUE)])[1:max.genes]
                },
                mean = {
                    m <- as.list(sapply(df, mean))
                    selected.genes <- names(m[order(unlist(m), decreasing=TRUE)])[1:max.genes]
                },
                variance = {
                    v <- as.list(sapply(df, var))
                    selected.genes <- names(v[order(unlist(v), decreasing=TRUE)])[1:max.genes]
                },
                random = {
                    selected.genes <- sample(colnames(df), max.genes, replace=FALSE)
                }
            )
            df <- subset(df, select=selected.genes)
        }
    } else if (length(selected.genes) > max.genes) {
        df <- subset(df, select=selected.genes)
        selected.genes <- switch(selection.criteria,
            non.zeros = {
                nz <- as.list(colSums(df!=0))
                selected.genes <- names(nz[order(unlist(nz), decreasing=TRUE)])[1:max.genes]
            },
            mean = {
                m <- as.list(sapply(df, mean))
                selected.genes <- names(m[order(unlist(m), decreasing=TRUE)])[1:max.genes]
            },
            variance = {
                v <- as.list(sapply(df, var))
                selected.genes <- names(v[order(unlist(v), decreasing=TRUE)])[1:max.genes]
            },
            random = {
                selected.genes <- sample(colnames(df), max.genes, replace=FALSE)
            }
        )
        df <- subset(df, select=selected.genes)
    } else {
        df <- subset(df, select=selected.genes)
    }
    df <- drop.all.zeros(df)
    return(df)
}

#' Select (automatically) the most appropriate cell rows of your dataset.
#'
#' This function allows you to (automatically) select the most appropriate cell rows of your dataset.
#' @param df Dataset.
#' @param max.cells Maximum number of selected cells (optional).
#' @param selection.criteria Criteria to be applied to select cells. Default: 'non.zeros'
#' @keywords select cells
#' @export
#' @examples
#' df <- select.cells(df, max.cells=2000)

select.cells<- function(df, max.cells=2000, selection.criteria=c('non.zeros','mean','variance','random')) {
    selection.criteria <- match.arg(selection.criteria)
    if (max.cells > nrow(df)) {
        max.cells <- nrow(df)
    }
    selected.cells <- switch(selection.criteria,
        non.zeros = {
            nz <- as.list(rowSums(df!=0))
            selected.cells <- names(nz[order(unlist(nz), decreasing=TRUE)])[1:max.cells]
        },
        mean = {
            m <- as.list(apply(df, 1, mean))
            selected.cells <- names(m[order(unlist(m), decreasing=TRUE)])[1:max.cells]
        },
        variance = {
            v <- as.list(apply(df, 1, var))
            selected.cells <- names(v[order(unlist(v), decreasing=TRUE)])[1:max.cells]
        },
        random = {
            selected.cells <- sample(rownames(df), max.cells, replace=FALSE)
        }
    )
    df <- df[selected.cells, ]
    df <- drop.all.zeros(df)
    return(df)
}

#' Split A Dataset Into Training And Test Datasets
#'
#' This function allows you to split a dataset into training and test datasets.
#' @param df Dataset.
#' @param m Size of training dataset (optional). Default: nrow(df)/2
#' @keywords training test dataframe
#' @export
#' @examples
#' splitted.df <- dataset.split(df)
#' splitted.df$train
#' splitted.df$test

dataset.split <- function(df, m=NULL) {
    if (is.null(m)) {
        m <- nrow(df)/2
    }

    df <- drop.all.zeros(df)
    nz <- as.list(colSums(df!=0))
    last.gene <- length(nz)-1
    selected.genes <- names(nz[nz>=10])
    df <- subset(df, select=selected.genes)

    R <- 1
    repeat {
        n.genes <- ncol(df)
        ixs <- sample(1:nrow(df), size=m, replace=FALSE)
        train <- df[ixs,]
        test <- df[-ixs,]
        train <- drop.all.zeros(train, rows=TRUE, columns=TRUE)
        test <- drop.all.zeros(test, rows=TRUE, columns=TRUE)
        if (dim(train)[2]==n.genes & dim(test)[2]==n.genes) {
            break
        } else {
            R <- R+1
            if (R%%100==0) {
                nz <- as.list(colSums(df!=0))
                last.gene <- length(nz)-1
                selected.genes <- names(nz[order(unlist(nz), decreasing=TRUE)])[1:last.gene]
                df <- subset(df, select=selected.genes)
                df <- drop.all.zeros(df)
            }
        }
    }
    return(list(
        train = train,
        test = test
    ))
}

#' Create a Ground Truth Graph
#'
#' This function allows you to create a ground truth graph from a geneset available via RESTful API or from a local list of edges.
#' @param x Download Code (must be of type 'TF-Target Interactions') or list of edges (data.frame or matrix).
#' @param to Output format ('adjacency', 'edges', 'igraph', or 'bn') (optional).
#' @keywords groundtruth graph
#' @export
#' @examples
#' gt <- ground.truth(2)

ground.truth <- function(x, to=c('igraph', 'adjacency', 'edges', 'graph', 'bnlearn')) {
    to <- match.arg(to)
    if (class(x)=='numeric') {
        genesets <- list.genesets()
        type <- genesets[genesets$download.code==x,]$dataset
        if (grepl('TF-Target Interactions', type, ignore.case=TRUE)) {
            geneset <- download.geneset(x)
        } else {
            message(paste('Wrong type of geneset?', '->', type))
            geneset <- download.geneset(x)
        }
    } else if (class(x) %in% c('data.frame','matrix')) {
        fmt <- detect.format(x)
        if (fmt=='adjacency') {
            g <- convert.format(x, to=to)
            return(g)
        }
    } else {
        return(NULL)
    }
    mtx <- as.adjacency(subset(geneset, select=c('from', 'to')))
    if (!is.null(geneset$type)) {
        repression <- geneset[geneset$type=='Repression',]
        for (i in 1:nrow(repression)) {
            gene.from <- repression[i,'from']
            gene.to <- repression[i,'to']
            mtx[gene.from, gene.to] <- -1
        }
    }
    g <- convert.format(mtx, to=to)
    return(g)
}

#' Plot expression histograms of genes in your dataset.
#'
#' This function allows you to plot the expression histograms of genes in your dataset.
#' @param df Dataset.
#' @param selected.genes User-selected genes (optional).
#' @param n.cols Number of column in the plot (optional). Default: 3
#' @keywords plot genes expression
#' @export
#' @examples
#' gene.histogram(df)

gene.histogram <- function(df, selected.genes=NULL, n.cols=3) {
    if (is.null(selected.genes)) {
        selected.genes <- colnames(df)
    }
    n.rows = ceiling(length(selected.genes)/n.cols)
    par(mfrow = c(n.rows, n.cols), mar = c(2, 2, 2, 2))
    for (gene in selected.genes) {
        x <- df[, gene]
        hist(x, prob=TRUE, xlab=gene, ylab='', main='', col=rgb(0.9,0.9,0.9))
        lines(density(x), lwd=2, col='darkred')
        curve(dnorm(x, mean=mean(x), sd=sd(x)), from=min(x), to=max(x), add=TRUE, lwd=2, col='darkgreen')
    }
}

#' Plot expression correlation of genes in your dataset.
#'
#' This function allows you to plot the expression correlation of genes in your dataset.
#' @param df Dataset.
#' @param selected.genes User-selected genes (optional).
#' @param method Correlation method: 'spearman', 'kendall', or 'pearson'. Default: 'spearman'
#' @keywords correlation genes expression
#' @export
#' @examples
#' gene.correlation(df)

gene.correlation <- function(df, selected.genes=NULL, method=c('spearman','kendall','pearson')) {
    method <- match.arg(method)
    if (is.null(selected.genes)) {
        selected.genes <- colnames(df)
    }
    pairs(df[, selected.genes],
       upper.panel = function(x, y, ...) {
         points(x=x, y=y, col=rgb(0.0,0.7,0.7))
         abline(coef(lm(y ~ x)), col='red', lwd=2)
       },
       lower.panel = function(x, y, ...) {
         par(usr = c(0,1,0,1))
         text(x=0.5, y=0.5, round(cor(x, y, method=method), 2), cex=2)
       }
    )
}

#' Gene Clustering
#'
#' This function allows you to cluster genes in your dataset by expression correlation.
#' @param df Dataset.
#' @param selected.genes User-selected genes (optional).
#' @param cor.method Correlation method: 'spearman', 'kendall', or 'pearson'. Default: 'spearman'
#' @keywords clustering genes expression
#' @export
#' @examples
#' gene.clustering(df)

gene.clustering <- function(df, selected.genes=NULL, cor.method=c('spearman','kendall','pearson')) {
    cor.method <- match.arg(cor.method)
    if (is.null(selected.genes)) {
        selected.genes <- colnames(df)
    }
    rho <- cor(df[, selected.genes], method=cor.method)
    diag(rho) <- NA
    palette.breaks <- seq(0,1,0.05)
    palette <- colorRampPalette(c(rgb(0.0,0.7,0.7), 'black', rgb(0.7,0.0,0.7)))
    par(oma = c(2,2,2,2))
    gplots::heatmap.2(rho, scale='none', trace='none', revC=TRUE, col=palette, breaks=palette.breaks, tracecol='yellow')
}
