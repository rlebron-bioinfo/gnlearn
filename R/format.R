#' Graph Format Conversion
#'
#' This function allows you to convert graph between different formats: adjacency matrix, list of edges, igraph and bn (bnlearn).
#' @param x Graph object.
#' @param to Output format ('adjacency', 'edges', 'igraph', or 'bn').
#' @param from Input format (optional).
#' @keywords graph adjacency edges format
#' @export
#' @examples
#' graph <- convert_format(mtx, 'igraph')

convert_format <- function(x, to, from=NULL) {
    if (is.null(from)) {
        from=detect_format(x)
    }
    obj <- switch(to,
        adjacency = {
            obj <- as_adjacency(x, from=from)
        },
        edges = {
            obj <- as_edges(x, from=from)
        },
        igraph = {
            obj <- as_igraph(x, from=from)
        },
        bn = {
            obj <- as_bn(x, from=from)
        },
        NULL
    )
    return(obj)
}

#' Graph Format Detection
#'
#' This function allows you to detect graph format.
#' @param x Graph object.
#' @keywords graph format
#' @export
#' @examples
#' detect_format(obj)

detect_format <- function(x) {
    fmt <- class(x)[1]
    fmt <- switch(fmt,
        matrix = { adjacency_or_edges(x) },
        data.frame = { adjacency_or_edges(x) },
        fmt
    )
    return(fmt)
}

adjacency_or_edges <- function(x) {
    dims = dim(x)
    if (dims[1] == dims[2] & dims[2] != 2) {
        return('adjacency')
    } else {
        return('edges')
    }
}

#' Convert Graph To Edge List
#'
#' This function allows you to convert your graph to edge list format.
#' @param x Graph object.
#' @param from Input format (optional).
#' @export
#' @examples
#' mtx <- as_edges(obj)

as_edges <- function(x, from=NULL) {
    if (is.null(from)) {
        from=detect_format(x)
    }
    mtx <- switch(from,
        adjacency = {
            g <- igraph::graph_from_adjacency_matrix(as.matrix(x), mode='directed', weighted=TRUE, diag=TRUE)
            mtx <- igraph::as_edgelist(g, names=TRUE)
        },
        edges = {
            mtx <- as.matrix(x)
        },
        igraph = {
            mtx <- igraph::as_edgelist(x, names=TRUE)
        },
        bn = {
            g <- as_igraph(as.matrix(bnlearn::arcs(x)), from='edges')
            mtx <- igraph::as_edgelist(g, names=TRUE)
        },
        NULL
    )
    return(mtx)
}

#' Convert Graph To Adjacency Matrix
#'
#' This function allows you to convert your graph to adjacency matrix format.
#' @param x Graph object.
#' @param from Input format (optional).
#' @export
#' @examples
#' mtx <- as_adjacency(obj)

as_adjacency <- function(x, from=NULL) {
    if (is.null(from)) {
        from=detect_format(x)
    }
    mtx <- switch(from,
        adjacency = {
            mtx <- as.matrix(x)
        },
        edges = {
            g <- igraph::graph_from_edgelist(as.matrix(x), directed=TRUE)
            mtx <- as.matrix(igraph::as_adjacency_matrix(g, type='both'))
        },
        igraph = {
            mtx <- as.matrix(igraph::as_adjacency_matrix(x, type='both'))
        },
        bn = {
            g <- as_igraph(as.matrix(bnlearn::arcs(x)), from='edges')
            mtx <- as.matrix(igraph::as_adjacency_matrix(g, type='both'))
        },
        NULL
    )
    return(mtx)
}

#' Convert Graph To igraph Format
#'
#' This function allows you to convert your graph to igraph format.
#' @param x Graph object.
#' @param from Input format (optional).
#' @export
#' @examples
#' g <- as_igraph(obj)

as_igraph <- function(x, from=NULL) {
    if (is.null(from)) {
        from=detect_format(x)
    }
    g <- switch(from,
        adjacency = {
            g <- igraph::graph_from_adjacency_matrix(as.matrix(x), mode='directed', weighted=TRUE, diag=TRUE)
        },
        edges = {
            g <- igraph::graph_from_edgelist(as.matrix(x), directed=TRUE)
        },
        igraph = {
            g <- x
        },
        bn = {
            g <- as_igraph(as.matrix(bnlearn::arcs(x)), from='edges')
        },
        NULL
    )
    return(g)
}

#' Convert Graph To bn Format
#'
#' This function allows you to convert your graph to bn format (bnlearn).
#' @param x Graph object.
#' @param from Input format (optional).
#' @export
#' @examples
#' g <- as_bn(obj)

as_bn <- function(x, from=NULL) {
    if (is.null(from)) {
        from=detect_format(x)
    }
    g <- switch(from,
        adjacency = {
            mtx <- subset(as.data.frame(as_edges(x, from='adjacency')), select=c(1,2))
            colnames(mtx) <- c('from', 'to')
            mtx$from <- as.character(mtx$from)
            mtx$to <- as.character(mtx$to)
            nodes <- union(mtx[[1]], mtx[[2]])
            nodes <- nodes[!duplicated(nodes)]
            g <- bnlearn::empty.graph(nodes)
            bnlearn::arcs(g, check.cycles=FALSE, check.illegal=FALSE) <- mtx
            g
        },
        edges = {
            mtx <- subset(as.data.frame(x), select=c(1,2))
            colnames(mtx) <- c('from', 'to')
            mtx$from <- as.character(mtx$from)
            mtx$to <- as.character(mtx$to)
            nodes <- union(mtx[[1]], mtx[[2]])
            nodes <- nodes[!duplicated(nodes)]
            g <- bnlearn::empty.graph(nodes)
            bnlearn::arcs(g, check.cycles=FALSE, check.illegal=FALSE) <- mtx
            g
        },
        igraph = {
            mtx <- subset(as.data.frame(as_edges(x, from='igraph')), select=c(1,2))
            colnames(mtx) <- c('from', 'to')
            mtx$from <- as.character(mtx$from)
            mtx$to <- as.character(mtx$to)
            nodes <- union(mtx[[1]], mtx[[2]])
            nodes <- nodes[!duplicated(nodes)]
            g <- bnlearn::empty.graph(nodes)
            bnlearn::arcs(g, check.cycles=FALSE, check.illegal=FALSE) <- mtx
            g
        },
        bn = {
            g <- x
        },
        NULL
    )
    return(g)
}
