#' A Test Function
#'
#' This function allows you to test your package.
#' @param message Say something.
#' @keywords test message
#' @export
#' @examples
#' test.function()
#' test.function('The way to get started is to quit talking and begin doing.')
#' test.function(message='Your time is limited, so don't waste it living someone else's life.')

test.function <- function(message='hello world!') {
    print(message)
}

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

as_adjacency <- function(x, from=NULL) {
    if (is.null(from)) {
        from=detect_format(x)
    }
    mtx <- switch(from,
        adjacency = {
            mtx <- as.matrix(x)
        },
        edges = {
            #nodes <- union(x[[1]], x[[2]])
            #nodes <- nodes[!duplicated(nodes)]
            #j <- dim(x)[1]
            #n <- length(nodes)
            #df <- as.data.frame(matrix(0, n, n))
            #colnames(df) <- as.character(nodes)
            #rownames(df) <- as.character(nodes)
            #for (i in 1:j) {
            #    row <- x[i,]
            #    f <- as.character(row[[1]])
            #    t <- as.character(row[[2]])
            #    df[f, t] = 1
            #}
            #mtx <- as.matrix(df)
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
