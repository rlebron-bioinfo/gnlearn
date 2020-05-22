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

#' Graph Plotting
#'
#' This function allows you to plot a graph, regardless of the format (adjacency matrix, list of edges, igraph, or bn).
#' @param x Graph object.
#' @param from Input format (optional).
#' @param layout igraph plot layout (optional). It will be ignored if interactive=TRUE.
#' @param from Interactive plot (optional). Default: FALSE
#' @keywords graph plot
#' @export
#' @examples
#' plot_graph(obj, interactive=FALSE)
#' plot_graph(obj, interactive=TRUE)

plot_graph <- function(x, from=NULL, layout=NULL, interactive=FALSE) {
    if (is.null(from)) {
        from=detect_format(x)
    }
    g <- as_igraph(x, from=from)
    if (is.null(layout)) {
        layout=igraph::layout_nicely(g)
    }
    if (interactive) {
        threejs::graphjs(g)
    } else {
        igraph::plot.igraph(g, layout=layout)
    }
}

#' Adjacency Matrix Plotting
#'
#' This function allows you to plot a graph as an adjacency matrix, regardless of the format (adjacency matrix, list of edges, igraph, or bn).
#' @param x Graph object.
#' @param from Input format (optional).
#' @param col Heatmap color palette (optional).
#' @keywords adjacency heatmap plot
#' @export
#' @examples
#' heatmap_adjacency(obj)

heatmap_adjacency <- function(x, from=NULL, col=NULL) {
    if (is.null(from)) {
        from=detect_format(x)
    }
    mtx <- as_adjacency(x, from=from)
    if (is.null(col)) {
        col=colorRampPalette(c('white', 'dark blue'))(100)
    }
    stats::heatmap(mtx, col=col)
}

#' Graph Comparison
#'
#' This function allows you to compare two graphs, regardless of the format (adjacency matrix, list of edges, igraph, or bn).
#' @param learned Learned graph or graph 1.
#' @param true Ground truth graph or graph 2 (reference).
#' @param arcs Whether or not to list the arcs (optional). Default: FALSE.
#' @keywords graph comparison
#' @export
#' @examples
#' comparison <- compare_graphs(obj1, obj2)

compare_graphs <- function(learned, true, arcs=FALSE) {
    learned <- as_igraph(learned)
    true <- as_igraph(true)
    learned <- igraph::simplify(learned, remove.multiple=TRUE, remove.loops=TRUE)
    true <- igraph::simplify(true, remove.multiple=TRUE, remove.loops=TRUE)
    v1 <- names(igraph::V(learned))
    v2 <- names(igraph::V(true))
    r1 <- v1[!(v1 %in% v2)]
    r2 <- v2[!(v2 %in% v1)]
    learned <- igraph::delete_vertices(learned, r1)
    true <- igraph::delete_vertices(true, r2)
    u <- igraph::union(learned, true)
    tp <- igraph::intersection(learned, true)
    fp <- igraph::difference(u, true)
    fn <- igraph::difference(u, learned)
    p <- precision(igraph::ecount(tp), igraph::ecount(fp))
    r <- recall(igraph::ecount(tp), igraph::ecount(fn))
    f1 <- f1_score(igraph::ecount(tp), igraph::ecount(fp), igraph::ecount(fn))
    #bn_learned <- as_bn(learned)
    #bn_true <- as_bn(true)
    #shd <- bnlearn::shd(bn_learned, bn_true)
    #hamming <- bnlearn::hamming(bn_learned, bn_true)
    if (arcs) {
        tp <- igraph::as_edgelist(tp)
        fp <- igraph::as_edgelist(fp)
        fn <- igraph::as_edgelist(fn)
    } else {
        tp <- igraph::ecount(tp)
        fp <- igraph::ecount(fp)
        fn <- igraph::ecount(fn)
    }
    return(list(
        tp = tp,
        fp = fp,
        fn = fn,
        precision = p,
        recall = r,
        f1_score = f1 #,
        #shd = shd,
        #hamming = hamming
    ))

}

precision <- function(tp, fp) {
    return(tp/(tp+fp))
}

recall <- function(tp, fn) {
    return(tp/(tp+fn))
}

f1_score <- function(tp, fp, fn) {
    p <- precision(tp, fp)
    r <- recall(tp, fn)
    return((2*p*r)/(p+r))
}

#' Feature Degree
#'
#' This function allows you to calculate the in-degree and out-degree of genes that have a certain feature (e.g. transcription factors or tumor suppressor genes).
#' @param x Graph object.
#' @param genes Geneset with features.
#' @param features Features you want to analyze. Default: all boolean features.
#' @keywords vertices Vertices you want to analyze. Default: all vertices.
#' @export in.degree Whether or not to analyze in-degree (optional). Default: TRUE.
#' @export out.degree Whether or not to analyze out-degree (optional). Default: TRUE.
#' @export loops Whether or not to consider loops (optional). Default: FALSE.
#' @export normalized Whether or not to normalize degrees (optional). Default: FALSE.
#' @examples
#' mtx <- feature_degree(x, genes)
#' mtx <- feature_degree(x, genes, features=c('tf', 'target', 'tumor.suppressor'))
#' mtx <- feature_degree(x, genes, features='tumor.suppressor', in.degree=TRUE, out.degree=FALSE)
#' mtx <- feature_degree(x, genes, features='essential', vertices=c('ADRB1', 'HSF2'), normalized=TRUE)

feature_degree <- function(x, genes, features=NULL, vertices=NULL, in.degree=TRUE, out.degree=TRUE, loops=FALSE, normalized=FALSE) {
    x <- as_adjacency(x)
    if (!loops) {
        diag(x) <- 0
    }
    k <- sum(c(in.degree, out.degree))
    if (k==0) {
        in.degree <- TRUE
        out.degree <- TRUE
        k = 2
    }
    if (!is.null(features)) {
        features <- features[as.logical(lapply(features, valid_feature, genes=genes))]
        n.features <- length(features)
        if (n.features==0) {
            features <- colnames(genes)
            features <- features[as.logical(lapply(features, valid_feature, genes=genes))]
            n.features <- length(features)
        }
    } else {
        features <- colnames(genes)
        features <- features[as.logical(lapply(features, valid_feature, genes=genes))]
        n.features <- length(features)
    }
    if (!is.null(vertices)) {
        vertices <- vertices[as.logical(lapply(vertices, valid_vertex, x=x))]
        n.vertices <- length(vertices)
        if (n.vertices==0) {
            vertices <- colnames(x)
            n.vertices <- length(vertices)
        }
    } else {
        vertices <- colnames(x)
        n.vertices <- length(vertices)
    }
    mtx = matrix(NA, n.vertices, k+n.features*k)
    rownames(mtx) <- vertices
    colnames(mtx) <- feature_colnames(features, in.degree, out.degree)
    for (v in vertices) {
        if (in.degree) {
            mtx[v, 'in.degree'] <- igraph::degree(as_igraph(x), v, mode='in', normalized=normalized)
        }
        if (out.degree) {
            mtx[v, 'out.degree'] <- igraph::degree(as_igraph(x), v, mode='out', normalized=normalized)
        }
        for (f in features) {
            f_genes <- intersect(colnames(x), genes[genes[f]==TRUE,]$name)
            if (!v %in% f_genes) {
                f_genes <- c(v, f_genes)
                j = 2
            } else {
                j = 1
            }
            f_x <- subset(x, select=f_genes)
            f_x <- f_x[f_genes, ]
            if (!is.null(dim(f_x))) {
                if (loops) {
                    s <- f_x[v, v]
                } else {
                    s <- 0
                }
                diag(f_x) <- 0
                f_x[v, v] <- s
                if (in.degree) {
                    f_col <- paste(f, 'in.degree', sep='|')
                    mtx[v, f_col] <- igraph::degree(as_igraph(f_x, from='adjacency'), v, mode='in')
                    if (normalized) {
                        mtx[v, f_col] <- mtx[v, f_col]/(length(f_genes)-j)
                    }
                }
                if (out.degree) {
                    f_col <- paste(f, 'out.degree', sep='|')
                    mtx[v, f_col] <- igraph::degree(as_igraph(f_x, from='adjacency'), v, mode='out')
                    if (normalized) {
                        mtx[v, f_col] <- mtx[v, f_col]/(length(f_genes)-j)
                    }
                }
            }
        }
    }
    return(mtx)
}

valid_feature <- function(genes, feature) {
    col.classes <- lapply(genes, class)
    if (feature %in% colnames(genes)) {
        if (col.classes[feature]=='logical') {
            return(TRUE)
        } else {
            return(FALSE)
        }
    } else{
        return(FALSE)
    }
}

valid_vertex <- function(x, vertex) {
    x <- as_adjacency(x)
    return(vertex %in% colnames(x))
}

feature_colnames <- function(features, in.degree, out.degree) {
    cols <- c('in.degree', 'out.degree')[c(in.degree, out.degree)]
    for (feature in features) {
        if (in.degree) {
            cols <- c(cols, paste(feature, 'in.degree', sep='|'))
        }
        if (out.degree) {
            cols <- c(cols, paste(feature, 'out.degree', sep='|'))
        }
    }
    return(cols)
}
