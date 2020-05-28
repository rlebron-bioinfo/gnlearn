#' Graph Format Conversion
#'
#' This function allows you to convert graph between different formats: adjacency matrix, list of edges, igraph and bn (bnlearn).
#' @param x Graph object.
#' @param to Output format ('adjacency', 'edges', 'igraph', or 'bn').
#' @param from Input format (optional).
#' @keywords graph adjacency edges format
#' @export
#' @examples
#' graph <- convert.format(mtx, 'igraph')

convert.format <- function(x, to, from=NULL) {
    if (is.null(from)) {
        from=detect.format(x)
    }
    obj <- switch(to,
        adjacency = {
            obj <- as.adjacency(x, from=from)
        },
        edges = {
            obj <- as.edges(x, from=from)
        },
        igraph = {
            obj <- as.igraph(x, from=from)
        },
        bn = {
            obj <- as.bn(x, from=from)
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
#' detect.format(obj)

detect.format <- function(x) {
    fmt <- class(x)[1]
    fmt <- switch(fmt,
        matrix = { adjacency.or.edges(x) },
        data.frame = { adjacency.or.edges(x) },
        fmt
    )
    return(fmt)
}

adjacency.or.edges <- function(x) {
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
#' mtx <- as.edges(obj)

as.edges <- function(x, from=NULL) {
    if (is.null(from)) {
        from=detect.format(x)
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
            g <- as.igraph(as.matrix(bnlearn::arcs(x)), from='edges')
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
#' mtx <- as.adjacency(obj)

as.adjacency <- function(x, from=NULL) {
    if (is.null(from)) {
        from=detect.format(x)
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
            g <- as.igraph(as.matrix(bnlearn::arcs(x)), from='edges')
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
#' g <- as.igraph(obj)

as.igraph <- function(x, from=NULL) {
    if (is.null(from)) {
        from=detect.format(x)
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
            g <- as.igraph(as.matrix(bnlearn::arcs(x)), from='edges')
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
#' g <- as.bn(obj)

as.bn <- function(x, from=NULL) {
    if (is.null(from)) {
        from=detect.format(x)
    }
    g <- switch(from,
        adjacency = {
            mtx <- subset(as.data.frame(as.edges(x, from='adjacency')), select=c(1,2))
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
            mtx <- subset(as.data.frame(as.edges(x, from='igraph')), select=c(1,2))
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
#' @param interactive Interactive plot (optional). Default: FALSE
#' @keywords graph plot
#' @export
#' @examples
#' plot.graph(obj, interactive=FALSE)
#' plot.graph(obj, interactive=TRUE)

graph.plot <- function(x, from=NULL, layout=NULL, interactive=FALSE) {
    if (is.null(from)) {
        from=detect.format(x)
    }
    g <- as.igraph(x, from=from)
    igraph::E(g)$color <- 'black'
    if (is.null(layout)) {
        layout=igraph::layout_nicely(g)
    }
    if (interactive) {
        threejs::graphjs(g)
    } else {
        igraph::plot.igraph(g, layout=layout)
    }
}

#' Feature Graph Plotting
#'
#' This function allows you to plot the graph of a feature (e.g. transcription factors or tumor suppressors).
#' @param x Graph object.
#' @param genes Geneset with features.
#' @param features Feature whose graph you want to plot.
#' @param from Input format (optional).
#' @param feature.color Color of the nodes with the feature (optional).
#' @param other.color Color of the other nodes (optional).
#' @param layout igraph plot layout (optional). It will be ignored if interactive=TRUE.
#' @param interactive Interactive plot (optional). Default: FALSE
#' @keywords graph feature plot
#' @export
#' @examples
#' plot.feature.graph(obj, genes, 'tf', interactive=FALSE)
#' plot.feature.graph(obj, genes, 'tumor.suppressor', interactive=TRUE)

feature.plot <- function(x, genes, feature, from=NULL, feature.color='red', other.color='green', layout=NULL, interactive=FALSE) {
    if (is.null(from)) {
        from=detect.format(x)
    }
    x <- as.edges(x, from=from)
    f_genes <- genes[genes[feature]==TRUE,]$name
    x <- x[x[,1] %in% f_genes | x[,2] %in% f_genes, ]
    g <- as.igraph(x, from='edges')
    igraph::E(g)$color <- 'black'
    igraph::V(g)$color <- ifelse(names(igraph::V(g)) %in% f_genes, 'red', 'green')
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
#' heatmap.adjacency(obj)

adjacency.heatmap <- function(x, from=NULL, col=NULL) {
    if (is.null(from)) {
        from=detect.format(x)
    }
    mtx <- as.adjacency(x, from=from)
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
#' comparison <- compare.graphs(obj1, obj2)

compare.graphs <- function(learned, true, arcs=FALSE) {
    learned <- as.igraph(learned)
    true <- as.igraph(true)
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
    f1 <- f1.score(igraph::ecount(tp), igraph::ecount(fp), igraph::ecount(fn))
    #bn_learned <- as.bn(learned)
    #bn_true <- as.bn(true)
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
        f1.score = f1 #,
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

f1.score <- function(tp, fp, fn) {
    p <- precision(tp, fp)
    r <- recall(tp, fn)
    return((2*p*r)/(p+r))
}

common.edges <- function(learned, true) {
    learned <- as.igraph(learned)
    true <- as.igraph(true)
    learned <- igraph::simplify(learned, remove.multiple=TRUE, remove.loops=TRUE)
    true <- igraph::simplify(true, remove.multiple=TRUE, remove.loops=TRUE)
    v1 <- names(igraph::V(learned))
    v2 <- names(igraph::V(true))
    r1 <- v1[!(v1 %in% v2)]
    r2 <- v2[!(v2 %in% v1)]
    learned <- igraph::delete_vertices(learned, r1)
    true <- igraph::delete_vertices(true, r2)
    return(list(learned=learned, true=true))
}

#' Feature Degree
#'
#' This function allows you to calculate the in-degree and out-degree of genes that have a certain feature (e.g. transcription factors or tumor suppressor genes).
#' @param x Graph object.
#' @param genes Geneset with features.
#' @param features Features you want to analyze. Default: all boolean features.
#' @param vertices Vertices you want to analyze. Default: all vertices.
#' @param in.degree Whether or not to analyze in-degree (optional). Default: TRUE.
#' @param out.degree Whether or not to analyze out-degree (optional). Default: TRUE.
#' @param loops Whether or not to consider loops (optional). Default: FALSE.
#' @param normalized Whether or not to normalize degrees (optional). Default: FALSE.
#' @keywords graph feature vertex degree
#' @export
#' @examples
#' mtx <- feature.degree(x, genes)
#' mtx <- feature.degree(x, genes, features=c('tf', 'target', 'tumor.suppressor'))
#' mtx <- feature.degree(x, genes, features='tumor.suppressor', in.degree=TRUE, out.degree=FALSE)
#' mtx <- feature.degree(x, genes, features='essential', vertices=c('ADRB1', 'HSF2'), normalized=TRUE)

feature.degree <- function(x, genes, features=NULL, vertices=NULL, in.degree=TRUE, out.degree=TRUE, loops=FALSE, normalized=FALSE) {
    x <- as.adjacency(x)
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
        features <- features[as.logical(lapply(features, valid.feature, genes=genes))]
        n.features <- length(features)
        if (n.features==0) {
            features <- colnames(genes)
            features <- features[as.logical(lapply(features, valid.feature, genes=genes))]
            n.features <- length(features)
        }
    } else {
        features <- colnames(genes)
        features <- features[as.logical(lapply(features, valid.feature, genes=genes))]
        n.features <- length(features)
    }
    if (!is.null(vertices)) {
        vertices <- vertices[as.logical(lapply(vertices, valid.vertex, x=x))]
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
    colnames(mtx) <- feature.colnames(features, in.degree, out.degree)
    for (v in vertices) {
        if (in.degree) {
            mtx[v, 'in.degree'] <- igraph::degree(as.igraph(x), v, mode='in', normalized=normalized)
        }
        if (out.degree) {
            mtx[v, 'out.degree'] <- igraph::degree(as.igraph(x), v, mode='out', normalized=normalized)
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
                    mtx[v, f_col] <- igraph::degree(as.igraph(f_x, from='adjacency'), v, mode='in')
                    if (normalized) {
                        mtx[v, f_col] <- mtx[v, f_col]/(length(f_genes)-j)
                    }
                }
                if (out.degree) {
                    f_col <- paste(f, 'out.degree', sep='|')
                    mtx[v, f_col] <- igraph::degree(as.igraph(f_x, from='adjacency'), v, mode='out')
                    if (normalized) {
                        mtx[v, f_col] <- mtx[v, f_col]/(length(f_genes)-j)
                    }
                }
            }
        }
    }
    return(mtx)
}

valid.feature <- function(genes, feature) {
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

valid.vertex <- function(x, vertex) {
    x <- as.adjacency(x)
    return(vertex %in% colnames(x))
}

feature.colnames <- function(features, in.degree, out.degree) {
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

#' Drop rows and/or columns with all zeros
#'
#' This function allows you to drop rows and/or columns with all zeros.
#' @param mtx Matrix or dataframe.
#' @param rows Drop rows with all zeros.
#' @param columns Drop columns with all zeros.
#' @param square.matrix Special mode for square matrices ('any' or 'all'). any: if the column or row is full of zeros, delete both; all: if the column or row is not full of zeros, keep both
#' @keywords matrix dataframe zeros
#' @export
#' @examples
#' mtx <- drop.all.zeros(mtx)
#' mtx <- drop.all.zeros(mtx, square.matrix='any')
#' mtx <- drop.all.zeros(mtx, square.matrix='all')

drop.all.zeros <- function(mtx, rows=TRUE, columns=TRUE, square.matrix='none') {
    nz.rows <- apply(mtx, 1, function(x) !all(x==0))
    nz.cols <- apply(mtx, 2, function(x) !all(x==0))
    mtx <- switch(square.matrix,
        any = mtx[nz.rows & nz.cols, nz.rows & nz.cols],
        all = mtx[nz.rows | nz.cols, nz.rows | nz.cols],
        {
            if (rows) {
                mtx <- mtx[nz.rows,]
            }
            if (columns) {
                mtx <- mtx[,nz.cols]
            }
        }
    )
    return(mtx)
}

#' Calculate The Averaged Graph
#'
#' This function allows you to calculate the averaged graph from a list of graphs.
#' @param graphs List of graphs (with the same genes and in the same order).
#' @param names Vector with gene names (in order).
#' @param threshold Minimum strength required for a coefficient to be included in the averaged graph (optional). Default: 0.5
#' @param to Output format ('adjacency', 'edges', 'igraph', or 'bn') (optional).
#' @keywords average graph
#' @export
#' @examples
#' graph <- run.aracne(df)

averaged.graph <- function(graphs, names, threshold=0.5, to='igraph') {
    R <- length(graphs)
    A <- array(data=NA, dim=c(dim(graphs[[1]]), R))
    for (i in 1:R) {
        A[, , i] <- graphs[[i]]
    }
    A <- apply(sign(abs(A)), c(1,2), mean)
    A[A < threshold] <- 0
    rownames(A) <- names
    colnames(A) <- names
    g <- convert.format(A, to=to)
    return(g)
}
