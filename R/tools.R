#' Graph Format Conversion
#'
#' This function allows you to convert graph between different formats: adjacency matrix, list of edges, igraph and bnlearn (bnlearn).
#' @param g Graph object.
#' @param to Output format (optional): 'adjacency', 'edges', 'graph', 'igraph', or 'bnlearn'. Default: 'igraph'
#' @param from Input format (optional): 'auto', 'adjacency', 'edges', 'graph', 'igraph', or 'bnlearn'. Default: 'auto'
#' @keywords graph adjacency edges format
#' @export
#' @examples
#' g <- convert.format(g, to='igraph')

convert.format <- function(g, to=c('igraph', 'adjacency', 'edges', 'graph', 'bnlearn'),
                           from=c('auto', 'adjacency', 'edges', 'graph', 'igraph', 'bnlearn')) {
    to <- match.arg(to)
    from <- match.arg(from)
    if (from=='auto') {
        from <- detect.format(g)
    }
    g <- switch(to,
        adjacency = {
            g <- as.adjacency(g, from=from)
        },
        edges = {
            g <- as.edges(g, from=from)
        },
        graph = {
            g <- as.graph(g, from=from)
        },
        igraph = {
            g <- as.igraph(g, from=from)
        },
        bnlearn = {
            g <- as.bnlearn(g, from=from)
        }
    )
    return(g)
}

#' Graph Format Detection
#'
#' This function allows you to detect graph format.
#' @param g Graph object.
#' @keywords graph format
#' @export
#' @examples
#' detect.format(g)

detect.format <- function(g) {
    fmt <- class(g)[1]
    fmt <- switch(fmt,
        matrix = { adjacency.or.edges(g) },
        data.frame = { adjacency.or.edges(g) },
        graphNEL = { 'graph' },
        graphAM = { 'graph' },
        bn = { 'bnlearn' },
        bn.fit = { 'bnlearn' },
        fmt
    )
    return(fmt)
}

adjacency.or.edges <- function(g) {
    dims = dim(g)
    if (dims[1] == dims[2]) {
        return('adjacency')
    } else {
        return('edges')
    }
}

#' Convert Graph To Adjacency Matrix
#'
#' This function allows you to convert your graph to adjacency matrix format.
#' @param g Graph object.
#' @param from Input format (optional): 'auto', 'adjacency', 'edges', 'graph', 'igraph', or 'bnlearn'. Default: 'auto'
#' @export
#' @examples
#' g <- as.adjacency(g)

as.adjacency <- function(g, from=c('auto', 'adjacency', 'edges', 'graph', 'igraph', 'bnlearn')) {
    from <- match.arg(from)
    if (from=='auto') {
        from <- detect.format(g)
    }
    g <- switch(from,
        adjacency = {
            g <- as.matrix(g)
        },
        edges = {
            g <- igraph::graph_from_data_frame(as.data.frame(g))
            attr <- NULL
            if ('weight' %in% igraph::list.edge.attributes(g)) {
                attr <- 'weight'
            }
            g <- as.matrix(igraph::as_adjacency_matrix(g, type='both', attr=attr))
        },
        graph = {
            g <- as(g, 'matrix')
        },
        igraph = {
            attr <- NULL
            if ('weight' %in% igraph::list.edge.attributes(g)) {
                attr <- 'weight'
            }
            g <- as.matrix(igraph::as_adjacency_matrix(g, type='both', attr=attr))
        },
        bnlearn = {
            g <- bnlearn::amat(g)
        }
    )
    return(g)
}

#' Convert Graph To Edge List
#'
#' This function allows you to convert your graph to edge list format.
#' @param g Graph object.
#' @param from Input format (optional): 'auto', 'adjacency', 'edges', 'graph', 'igraph', or 'bnlearn'. Default: 'auto'
#' @export
#' @examples
#' g <- as.edges(g)

as.edges <- function(g, from=c('auto', 'adjacency', 'edges', 'graph', 'igraph', 'bnlearn')) {
    from <- match.arg(from)
    if (from=='auto') {
        from <- detect.format(g)
    }
    g <- switch(from,
        adjacency = {
            g <- igraph::graph_from_adjacency_matrix(as.matrix(g), mode='directed', weighted=TRUE, diag=TRUE)
            g <- igraph::as_data_frame(g, what='edges')
        },
        edges = {
            g <- as.data.frame(g)
        },
        graph = {
            g <- as(g, 'matrix')
            g <- igraph::graph_from_adjacency_matrix(as.matrix(g), mode='directed', weighted=TRUE, diag=TRUE)
            g <- igraph::as_data_frame(g, what='edges')
        },
        igraph = {
            g <- igraph::as_data_frame(g, what='edges')
        },
        bnlearn = {
            g <- as.data.frame(bnlearn::arcs(g))
        }
    )
    return(g)
}

#' Convert Graph To graph Format
#'
#' This function allows you to convert your graph to graph format.
#' @param g Graph object.
#' @param from Input format (optional): 'auto', 'adjacency', 'edges', 'graph', 'igraph', or 'bnlearn'. Default: 'auto'
#' @export
#' @examples
#' g <- as.graph(g)

as.graph <- function(g, from=c('auto', 'adjacency', 'edges', 'graph', 'igraph', 'bnlearn')) {
    from <- match.arg(from)
    if (from=='auto') {
        from <- detect.format(g)
    }
    g <- switch(from,
        adjacency = {
            g <- igraph::graph_from_adjacency_matrix(as.matrix(g), mode='directed', weighted=TRUE, diag=TRUE)
            V <- names(igraph::V(g))
            W <- NULL
            if ('weight' %in% igraph::list.edge.attributes(g)) {
                W <- g$weight
            }
            g <- igraph::as_data_frame(g, what='edges')
            g <- as.matrix(subset(g, select=c('from', 'to')))
            g <- graph::ftM2graphNEL(g, W=W, V=V, edgemode='directed')
        },
        edges = {
            g <- igraph::graph_from_data_frame(as.data.frame(g))
            V <- names(igraph::V(g))
            W <- NULL
            if ('weight' %in% igraph::list.edge.attributes(g)) {
                W <- g$weight
            }
            g <- igraph::as_data_frame(g, what='edges')
            g <- as.matrix(subset(g, select=c('from', 'to')))
            g <- graph::ftM2graphNEL(g, W=W, V=V, edgemode='directed')
        },
        graph = {
            g <- g
        },
        igraph = {
            g <- igraph::as_graphnel(g)
        },
        bnlearn = {
            g <- bnlearn::as.graphNEL(g)
        }
    )
    return(g)
}

#' Convert Graph To igraph Format
#'
#' This function allows you to convert your graph to igraph format.
#' @param g Graph object.
#' @param from Input format (optional): 'auto', 'adjacency', 'edges', 'graph', 'igraph', or 'bnlearn'. Default: 'auto'
#' @export
#' @examples
#' g <- as.igraph(g)

as.igraph <- function(g, from=c('auto', 'adjacency', 'edges', 'graph', 'igraph', 'bnlearn')) {
    from <- match.arg(from)
    if (from=='auto') {
        from <- detect.format(g)
    }
    g <- switch(from,
        adjacency = {
            g <- igraph::graph_from_adjacency_matrix(as.matrix(g), mode='directed', weighted=TRUE, diag=TRUE)
        },
        edges = {
            g <- igraph::graph_from_data_frame(as.data.frame(g))
        },
        graph = {
            g <- as(g, 'matrix')
            g <- igraph::graph_from_adjacency_matrix(as.matrix(g), mode='directed', weighted=TRUE, diag=TRUE)
        },
        igraph = {
            g <- g
        },
        bnlearn = {
            g <- bnlearn::amat(g)
            g <- igraph::graph_from_adjacency_matrix(as.matrix(g), mode='directed', weighted=TRUE, diag=TRUE)
        }
    )
    return(g)
}

#' Convert Graph To bnlearn Format
#'
#' This function allows you to convert your graph to bnlearn format.
#' @param g Graph object.
#' @param from Input format (optional): 'auto', 'adjacency', 'edges', 'graph', 'igraph', or 'bnlearn'. Default: 'auto'
#' @export
#' @examples
#' g <- as.bnlearn(g)

as.bnlearn <- function(g, from=c('auto', 'adjacency', 'edges', 'graph', 'igraph', 'bnlearn')) {
    from <- match.arg(from)
    if (from=='auto') {
        from <- detect.format(g)
    }
    g <- switch(from,
        adjacency = {
            g <- igraph::graph_from_adjacency_matrix(as.matrix(g), mode='directed', weighted=TRUE, diag=TRUE)
            V <- names(igraph::V(g))
            W <- NULL
            if ('weight' %in% igraph::list.edge.attributes(g)) {
                W <- g$weight
            }
            g <- igraph::as_data_frame(g, what='edges')
            g <- as.matrix(subset(g, select=c('from', 'to')))
            g <- graph::ftM2graphNEL(g, W=W, V=V, edgemode='directed')
            g <- bnlearn::as.bn(g)
        },
        edges = {
            g <- igraph::graph_from_data_frame(as.data.frame(g))
            V <- names(igraph::V(g))
            W <- NULL
            if ('weight' %in% igraph::list.edge.attributes(g)) {
                W <- g$weight
            }
            g <- igraph::as_data_frame(g, what='edges')
            g <- as.matrix(subset(g, select=c('from', 'to')))
            g <- graph::ftM2graphNEL(g, W=W, V=V, edgemode='directed')
            g <- bnlearn::as.bn(g)
        },
        graph = {
            g <- bnlearn::as.bn(g)
        },
        igraph = {
            g <- igraph::as_graphnel(g)
            g <- bnlearn::as.bn(g, check.cycles=FALSE)
        },
        bnlearn = {
            g <- g
        }
    )
    return(g)
}

#' Graph Plotting
#'
#' This function allows you to plot a graph.
#' @param x Graph object.
#' @param from Input format (optional).
#' @param layout igraph plot layout (optional): 'grid', 'star', 'circle', 'sphere', or 'nicely'. Default: 'circle'
#' @param interactive Interactive plot (optional). Default: FALSE
#' @param isolated.genes Whether or not to include isolated nodes in the plot (optional). Default: FALSE
#' @keywords graph plot
#' @export
#' @examples
#' graph.plot(obj)
#' graph.plot(obj, isolated.genes=TRUE)
#' graph.plot(obj, interactive=TRUE)

graph.plot <- function(x, from=c('auto', 'adjacency', 'edges', 'graph', 'igraph', 'bnlearn'),
                       layout=c('circle','star','grid','sphere','nicely'),
                       interactive=FALSE, isolated.genes=FALSE) {
    from <- match.arg(from)
    layout <- match.arg(layout)

    if (from=='auto') {
        from=detect.format(x)
    }

    if (igraph::gsize(as.igraph(x, from=from)) == 0) {
        isolated.genes <- TRUE
    }

    if (isolated.genes) {
        g <- as.igraph(x, from=from)
    } else {
        g <- delete.isolated(as.adjacency(x, from=from), from='adjacency', to='igraph')
    }

    igraph::V(g)$color <- rgb(0.0,0.5,0.5,0.1)
    if ('weight' %in% igraph::list.edge.attributes(g)) {
        igraph::E(g)$color <- ifelse(igraph::E(g)$weight > 0, rgb(0.0,0.7,0.0,0.9), rgb(0.7,0.0,0.0,0.9))
        igraph::E(g)$width <- sapply(igraph::E(g)$weight, function(x) ceiling(abs(x))+1)
        t <- as.igraph(as.adjacency(g))
        igraph::E(t)$weight <- abs(igraph::E(t)$weight)
    } else {
        igraph::E(g)$color <- rgb(0.3,0.3,0.3,0.9)
        igraph::E(g)$width <- 2
        t <- as.igraph(as.adjacency(g))
    }

    layout <- switch(layout,
        grid = igraph::layout_on_grid(t),
        star = igraph::layout_as_star(t),
        circle = igraph::layout_in_circle(t),
        sphere = igraph::layout_on_sphere(t),
        nicely = igraph::layout_nicely(t)
    )

    if (interactive) {
        layout <- third.axis(layout)
        threejs::graphjs(g,
            layout=layout)
    } else {
        igraph::plot.igraph(g,
            vertex.label.font=2,
            vertex.label.color=rgb(0.0,0.3,0.3),
            vertex.label.family='Helvetica',
            vertex.frame.color=rgb(0.0,0.3,0.3),
            vertex.shape='circle',
            vertex.size=30,
            edge.lty='solid',
            edge.arrow.width=1,
            layout=layout)
    }
}

#' Feature Graph Plotting
#'
#' This function allows you to plot the graph of a feature (e.g. transcription factors or tumor suppressors).
#' @param x Graph object.
#' @param genes Geneset with features.
#' @param features Feature whose graph you want to plot.
#' @param from Input format (optional).
#' @param layout igraph plot layout (optional): 'grid', 'star', 'circle', 'sphere', or 'nicely'. Default: 'circle'
#' @param interactive Interactive plot (optional). Default: FALSE
#' @keywords graph feature plot
#' @export
#' @examples
#' feature.plot(obj, genes, 'tf', interactive=FALSE)
#' feature.plot(obj, genes, 'tumor.suppressor', interactive=TRUE)

feature.plot <- function(x, genes, feature, from=c('auto', 'adjacency', 'edges', 'graph', 'igraph', 'bnlearn'),
                         layout=c('circle','star','grid','sphere','nicely'), interactive=FALSE) {
    from <- match.arg(from)
    layout <- match.arg(layout)

    if (from=='auto') {
        from=detect.format(x)
    }

    if (igraph::gsize(as.igraph(x, from=from)) == 0) {
        isolated.genes <- TRUE
    }

    x <- as.edges(x, from=from)
    f_genes <- genes[genes[feature]==TRUE,]$name
    x <- x[x[,1] %in% f_genes | x[,2] %in% f_genes, ]
    g <- as.igraph(x, from='edges')
    igraph::V(g)$color <- ifelse(names(igraph::V(g)) %in% f_genes, rgb(0.5,0.0,0.5,0.1), rgb(0.0,0.5,0.5,0.1))
    igraph::V(g)$label.color <- ifelse(names(igraph::V(g)) %in% f_genes, rgb(0.3,0.0,0.3), rgb(0.0,0.3,0.3))
    igraph::V(g)$frame.color <- ifelse(names(igraph::V(g)) %in% f_genes, rgb(0.3,0.0,0.3), rgb(0.0,0.3,0.3))
    if ('weight' %in% igraph::list.edge.attributes(g)) {
        igraph::E(g)$color <- ifelse(igraph::E(g)$weight > 0, rgb(0.0,0.7,0.0,0.9), rgb(0.7,0.0,0.0,0.9))
        igraph::E(g)$width <- sapply(igraph::E(g)$weight, function(x) ceiling(abs(x))+1)
        t <- as.igraph(as.adjacency(g))
        igraph::E(t)$weight <- abs(igraph::E(t)$weight)
    } else {
        igraph::E(g)$color <- rgb(0.3,0.3,0.3,0.9)
        igraph::E(g)$width <- 2
        t <- as.igraph(as.adjacency(g))
    }

    layout <- switch(layout,
        grid = igraph::layout_on_grid(t),
        star = igraph::layout_as_star(t),
        circle = igraph::layout_in_circle(t),
        sphere = igraph::layout_on_sphere(t),
        nicely = igraph::layout_nicely(t)
    )

    if (interactive) {
        layout <- third.axis(layout)
        threejs::graphjs(g,
            layout=layout)
    } else {
        igraph::plot.igraph(g,
            vertex.label.font=2,
            vertex.label.family='Helvetica',
            vertex.shape='circle',
            vertex.size=30,
            edge.lty='solid',
            edge.arrow.width=1,
            layout=layout)
    }
}

third.axis <- function(layout) {
    if (dim(layout)[2] == 2) {
        layout <- as.data.frame(layout)
        layout[,3] <- 0
        colnames(layout) <- NULL
        layout <- as.matrix(layout)
    }
    return(layout)
}

#' Graph Comparison
#'
#' This function allows you to compare two graphs.
#' @param learned Learned graph or graph 1.
#' @param true Ground truth graph or graph 2 (reference).
#' @param marginalize Whether or not to marginalize: 'none', 'learned', 'true', or 'both'. Default: 'none'
#' @param max.steps Maximum number of steps in the path during marginalization. Default: Inf
#' @param arcs Whether or not to list the arcs. Default: FALSE.
#' @param plot Whether or not to plot the differences between the two graphs. Default: TRUE
#' @param vertical.plot Whether to draw the comparison plots horizontally. Otherwise, they will be drawn horizontally. Default: TRUE
#' @param split.plot Whether to split comparison plots. Otherwise, they will be drawn together. Default: TRUE
#' @keywords graph comparison
#' @export
#' @examples
#' comparison <- compare.graphs(obj1, obj2, plot=TRUE)
#' comparison <- compare.graphs(obj1, obj2, plot=FALSE)

compare.graphs <- function(learned, true, marginalize=c('none','learned','true','both'),
                           max.steps=Inf, arcs=FALSE, plot=TRUE, vertical.plot=TRUE, split.plot=TRUE) {
    marginalize <- match.arg(marginalize)
    learned <- as.igraph(learned)
    true <- as.igraph(true)

    learned <- igraph::simplify(learned, remove.multiple=TRUE, remove.loops=TRUE)
    true <- igraph::simplify(true, remove.multiple=TRUE, remove.loops=TRUE)

    v1 <- names(igraph::V(learned))
    v2 <- names(igraph::V(true))

    marginalize.learned <- marginalize == 'both' | marginalize == 'learned'
    marginalize.true <- marginalize == 'both' | marginalize == 'true'

    if (marginalize.learned) {
        learned <- graph.marginalization(learned, v1[(v1 %in% v2)], to='igraph')
    }

    if (marginalize.true) {
        true <- graph.marginalization(true, v2[(v2 %in% v1)], to='igraph')
    }

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

    if (plot) {
        learned.x <- igraph::difference(learned, tp)
        true.x <- igraph::difference(true, tp)
        igraph::V(tp)$color <- rgb(0.0,0.5,0.5,0.1)
        igraph::V(learned.x)$color <- rgb(0.0,0.5,0.5,0.1)
        igraph::V(true.x)$color <- rgb(0.0,0.5,0.5,0.1)

        igraph::E(tp)$color <- rgb(0.3,0.3,0.3,0.9)
        igraph::E(learned.x)$color <- rgb(0.3,0.3,0.3,0.9)
        igraph::E(true.x)$color <- rgb(0.3,0.3,0.3,0.9)

        if (vertical.plot & !split.plot) {
            par(mfrow = c(3,1), mar = c(2,2,2,2))
        } else if (!split.plot) {
            par(mfrow = c(1,3), mar = c(2,2,2,2))
        }

        igraph::plot.igraph(tp,
            main='True Positive Arches',
            vertex.label.font=2,
            vertex.label.color=rgb(0.0,0.3,0.3),
            vertex.label.family='Helvetica',
            vertex.frame.color=rgb(0.0,0.3,0.3),
            vertex.shape='circle',
            vertex.size=30,
            edge.width=2,
            edge.arrow.width=1,
            layout=igraph::layout_in_circle(tp))

        igraph::plot.igraph(learned.x,
            main='False Positive Arches',
            vertex.label.font=2,
            vertex.label.color=rgb(0.0,0.3,0.3),
            vertex.label.family='Helvetica',
            vertex.frame.color=rgb(0.0,0.3,0.3),
            vertex.shape='circle',
            vertex.size=30,
            edge.width=2,
            edge.arrow.width=1,
            layout=igraph::layout_in_circle(learned.x))

        igraph::plot.igraph(true.x,
            main='False Negative Arches',
            vertex.label.font=2,
            vertex.label.color=rgb(0.0,0.3,0.3),
            vertex.label.family='Helvetica',
            vertex.frame.color=rgb(0.0,0.3,0.3),
            vertex.shape='circle',
            vertex.size=30,
            edge.width=2,
            edge.arrow.width=1,
            layout=igraph::layout_in_circle(true.x))
    }

    bnlearn_learned <- as.bnlearn(learned)
    bnlearn_true <- as.bnlearn(true)
    shd <- bnlearn::shd(bnlearn_learned, bnlearn_true)
    hamming <- bnlearn::hamming(bnlearn_learned, bnlearn_true)

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
        f1.score = f1,
        shd = shd,
        hamming = hamming
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
    mtx = matrix(0, n.vertices, k+n.features*k)
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
#' @param graphs List of graphs.
#' @param threshold Minimum strength required for a coefficient to be included in the averaged graph (optional). Default: 0.5
#' @param to Output format ('adjacency', 'edges', 'graph', 'igraph', or 'bnlearn') (optional).
#' @keywords average graph
#' @export
#' @examples
#' graph <- averaged.graph(graphs)

averaged.graph <- function(graphs, threshold=0.5, to='igraph') {
    R <- length(graphs)
    coeff.1 <- as.data.frame(as.adjacency(graphs[[1]]))
    adj.1 <- sign(abs(coeff.1))
    for (i in 2:R) {
        coeff.2 <- as.data.frame(as.adjacency(graphs[[i]]))
        adj.2 <- sign(abs(coeff.2))
        cols.1 <- rownames(adj.1) <- colnames(adj.1) <- rownames(coeff.1) <- colnames(coeff.1)
        cols.2 <- rownames(adj.2) <- colnames(adj.2) <- rownames(coeff.2) <- colnames(coeff.2)
        coeff.1 <- coeff.1[order(cols.1), order(cols.1)]
        adj.1 <- adj.1[order(cols.1), order(cols.1)]
        coeff.2 <- coeff.2[order(cols.2), order(cols.2)]
        adj.2 <- adj.2[order(cols.2), order(cols.2)]
        int.1 <- cols.1 %in% cols.2
        int.2 <- cols.2 %in% cols.1
        coeff.int <- ((i-1) * coeff.1[int.1, int.1] / i) + (coeff.2[int.2, int.2] / i)
        adj.int <- ((i-1) * adj.1[int.1, int.1] / i) + (adj.2[int.2, int.2] / i)
        cols.int <- rownames(adj.int) <- colnames(adj.int) <- rownames(coeff.int) <- colnames(coeff.int)
        coeff.int <- coeff.int[order(cols.int), order(cols.int)]
        adj.int <- adj.int[order(cols.int), order(cols.int)]
        cols.u <- sort(union(cols.1, cols.2))
        n.cols.u <- length(cols.u)
        coeff.u <- as.data.frame(matrix(0, nrow=n.cols.u, ncol=n.cols.u))
        adj.u <- as.data.frame(matrix(0, nrow=n.cols.u, ncol=n.cols.u))
        rownames(adj.u) <- colnames(adj.u) <- rownames(coeff.u) <- colnames(coeff.u) <- cols.u
        coeff.u <- coeff.u[order(cols.u), order(cols.u)]
        adj.u <- adj.u[order(cols.u), order(cols.u)]
        coeff.u[cols.u %in% cols.1, cols.u %in% cols.1] <- coeff.1
        coeff.u[cols.u %in% cols.2, cols.u %in% cols.2] <- coeff.2
        coeff.u[cols.u %in% cols.int, cols.u %in% cols.int] <- coeff.int
        adj.u[cols.u %in% cols.1, cols.u %in% cols.1] <- adj.1
        adj.u[cols.u %in% cols.2, cols.u %in% cols.2] <- adj.2
        adj.u[cols.u %in% cols.int, cols.u %in% cols.int] <- adj.int
        coeff.1 <- coeff.u
        adj.1 <- adj.u
    }
    cols.1 <- rownames(adj.1) <- colnames(adj.1) <- rownames(coeff.1) <- colnames(coeff.1)
    coeff.1 <- coeff.1[order(colnames(coeff.1)), order(colnames(coeff.1))]
    adj.1 <- adj.1[order(colnames(adj.1)), order(colnames(adj.1))]
    coeff.1[adj.1 < threshold] <- 0
    g <- convert.format(coeff.1, to=to)
    return(g)
}

#' Rename Vertices Of A List Of Graphs
#'
#' This function allows you to rename nodes of a list of graphs.
#' @param graphs List of graphs.
#' @param names Vector with the new vertex names (in order).
#' @param to Output format ('adjacency', 'edges', 'graph', 'igraph', or 'bnlearn') (optional).
#' @keywords rename graph
#' @export
#' @examples
#' graph <- rename.graphs(graphs, names)

rename.graphs <- function(graphs, names, to='igraph') {
    R <- length(graphs)
    renamed.graphs <- list()
    for (i in 1:R) {
        g <- convert.format(graphs[[i]], to='adjacency')
        if (length(g) > 0) {
            rownames(g) <- names
            colnames(g) <- names
            g <- convert.format(g, to=to)
            renamed.graphs <- list(unlist(renamed.graphs), g)
        }
    }
    return(renamed.graphs)
}

#' Graph Communities
#'
#' This function allows you to detect how many communities are in the graph and to which community each node and edge belongs.
#' @param x Graph object.
#' @param algorithm Algorithm for finding communities: 'louvain', 'edge.betweenness', 'fast.greedy', 'label.prop', 'leading.eigen', 'optimal', 'spinglass', or 'walktrap'. Default: 'louvain'
#' @param network Whether or not to plot the network. Default: TRUE
#' @param network.layout igraph network layout (optional): 'grid', 'star', 'circle', 'sphere', or 'nicely'. Default: 'circle'
#' @param interactive.network Interactive network (optional). Default: FALSE
#' @param network.isolated Whether or not to include isolated nodes in the plot (optional). Default: TRUE
#' @param dendrogram Whether or not to plot a dendrogram (when possible). Default: FALSE
#' @param dendrogram.type Type of phylogeny to be drawn: 'fan', 'phylogram', 'cladogram', 'unrooted', or 'radial'. Default: 'fan'
#' @param from Input format (optional).
#' @keywords graph community plot
#' @export
#' @examples
#' communities <- graph.communities(g)
#' communities <- graph.communities(g, algorithm='louvain', network=TRUE, network.isolated=FALSE, dendrogram=FALSE)
#' communities <- graph.communities(g, algorithm='walktrap', network=FALSE, dendrogram=TRUE, dendrogram.type='cladogram')

graph.communities <- function(x, algorithm=c('louvain','edge.betweenness','fast.greedy','label.prop','leading.eigen','optimal','spinglass','walktrap'),
                              network=TRUE, network.layout=c('circle','star','grid','sphere','nicely'), interactive.network=FALSE, network.isolated=TRUE,
                              dendrogram=FALSE, dendrogram.type=c('fan','phylogram','cladogram','unrooted','radial'),
                              from=c('auto', 'adjacency', 'edges', 'graph', 'igraph', 'bnlearn')) {
    algorithm <- match.arg(algorithm)
    network.layout <- match.arg(network.layout)
    dendrogram.type <- match.arg(dendrogram.type)
    from <- match.arg(from)

    if (algorithm %in% c('louvain','label.prop','optimal','spinglass')) {
        dendrogram <- FALSE
    }

    algorithm <- switch(algorithm,
        louvain = igraph::cluster_louvain,
        edge.betweenness = igraph::cluster_edge_betweenness,
        fast.greedy = igraph::cluster_fast_greedy,
        label.prop = igraph::cluster_label_prop,
        leading.eigen = igraph::cluster_leading_eigen,
        optimal = igraph::cluster_optimal,
        spinglass = igraph::cluster_spinglass,
        walktrap = igraph::cluster_walktrap
    )

    if (from=='auto') {
        from=detect.format(x)
    }

    if (igraph::gsize(as.igraph(x, from=from)) == 0) {
        network.isolated <- TRUE
    }

    library(dplyr)

    if (network.isolated) {
        g <- as.igraph(x, from=from)
    } else {
        g <- delete.isolated(as.adjacency(x, from=from), from='adjacency', to='igraph')
    }

    g <- as.igraph(x, from=from)
    if ('weight' %in% igraph::list.edge.attributes(g)) {
        igraph::E(g)$color <- ifelse(igraph::E(g)$weight > 0, rgb(0.0,0.7,0.0,0.9), rgb(0.7,0.0,0.0,0.9))
        igraph::E(g)$width <- sapply(igraph::E(g)$weight, function(x) ceiling(abs(x))+1)
        t <- as.igraph(as.adjacency(g))
        igraph::E(t)$weight <- abs(igraph::E(t)$weight)
    } else {
        igraph::E(g)$color <- rgb(0.3,0.3,0.3,0.9)
        igraph::E(g)$width <- 2
        t <- as.igraph(as.adjacency(g))
    }
    c <- algorithm(igraph::as.undirected(t))
    igraph::V(g)$community <- igraph::membership(c)

    palette <- rainbow(length(unique(igraph::V(g)$community)))

    node.community <- igraph::get.data.frame(g, what='vertices')

    edge.community <- igraph::get.data.frame(g, what='edges') %>%
        inner_join(node.community %>% select(name, community), by=c('from'='name')) %>%
        inner_join(node.community %>% select(name, community), by=c('to'='name')) %>%
        mutate(community = ifelse(community.x == community.y, community.x, NA) %>% factor())
    colnames(edge.community) <- c('from', 'to', 'weight', 'from.community', 'to.community', 'community')

    if (network & interactive.network) {

        layout <- third.axis(layout)
        threejs::graphjs(g,
            vertex.color = palette[as.numeric(as.factor(igraph::vertex_attr(g, 'community')))],
            layout=network.layout)

    } else if (network & !interactive.network) {

        network.layout <- switch(network.layout,
            grid = igraph::layout_on_grid(t),
            star = igraph::layout_as_star(t),
            circle = igraph::layout_in_circle(t),
            sphere = igraph::layout_on_sphere(t),
            nicely = igraph::layout_nicely(t)
        )

        igraph::plot.igraph(g,
            vertex.color = palette[as.numeric(as.factor(igraph::vertex_attr(g, 'community')))],
            vertex.label.font=2,
            vertex.label.color='black',
            vertex.label.family='Helvetica',
            vertex.frame.color='black',
            vertex.shape='circle',
            vertex.size=30,
            edge.lty='solid',
            edge.arrow.width=1,
            layout=network.layout)

    }

    if (dendrogram) {
        igraph::plot_dendrogram(c, mode='phylo', palette=palette, type=dendrogram.type,
                                font=2, cex=1.5, edge.color='black', edge.width=3)
    }

    return(list(
        communities = unique(igraph::V(g)$community),
        node.community = node.community,
        edge.community = edge.community
    ))
}

#' Generate A Random Graph/DAG
#'
#' This function allows you to generate a random graph/DAG.
#' @param nodes Number of nodes or vector of node names.
#' @param exp.degree Number of expected degree per node.
#' @param dag Whether the graph should be a DAG or not. Default: TRUE
#' @param plot Whether or not to plot the graph. Default: TRUE
#' @param algorithm Algorithm to be used to generate the graph: 'regular', 'watts', 'er', 'power', 'bipartite', 'barabasi', or 'geometric'. Default: 'regular'
#' @param to Output format ('adjacency', 'edges', 'graph', 'igraph', or 'bnlearn').
#' @keywords generate random graph
#' @export
#' @examples
#' graph <- random.graph(LETTERS[1:15], 3, dag=TRUE)
#' graph <- random.graph(LETTERS[1:15], 3, dag=FALSE)
#' graph <- random.graph(15, 3, dag=TRUE)
#' graph <- random.graph(15, 3, dag=FALSE)

random.graph <- function(nodes, exp.degree, dag=TRUE, plot=TRUE,
                       algorithm=c('regular','watts','er','power','bipartite','barabasi','geometric'), to='igraph', ...) {
    algorithm <- match.arg(algorithm)

    n <- ifelse(length(nodes) == 1, nodes, length(nodes))

    if (dag) {
        g <- pcalg::randDAG(n, exp.degree, method=algorithm, DAG=TRUE, ...)
        g <- as.data.frame(as(g, 'matrix'))
        if (length(nodes) > 1) {
            rownames(g) <- colnames(g) <- nodes
        }
    } else {
        g <- switch(algorithm,
            regular = { igraph::sample_k_regular(n, exp.degree, directed=TRUE, multiple=FALSE) },
            watts = { igraph::sample_smallworld(1, n, 1, exp.degree/(n-1), loops=FALSE, multiple=FALSE) },
            er = { igraph::erdos.renyi.game(n, exp.degree*n, type='gnm', directed=TRUE, loops=FALSE, ...) },
            power = { igraph::sample_fitness_pl(n, exp.degree*n, 2, exponent.in=2, loops=FALSE, multiple=FALSE, finite.size.correction=TRUE) },
            bipartite = { igraph::make_full_bipartite_graph(round(n/2), n-round(n/2), directed=TRUE, mode='all', ...) },
            barabasi = { igraph::sample_pa(n, 1, exp.degree, directed=TRUE, ...) },
            geometric = { igraph::sample_grg(n, exp.degree/(n-1), ...) }
        )
        g <- as.data.frame(as.matrix(igraph::as_adjacency_matrix(g, type='both')))
        if (length(nodes) > 1) {
            rownames(g) <- colnames(g) <- nodes
        }
    }
    if (plot) {
        graph.plot(g, isolated.genes=FALSE)
    }
    g <- convert.format(g, from='adjacency', to=to)
    return(g)
}

#' Make an edgelist from some genes to anothers.
#'
#' This function allows you to make an edgelist from some genes to anothers.
#' @param genes Geneset with features.
#' @param from.genes User-selected 'from' genes.
#' @param to.genes User-selected 'to' genes.
#' @param from.features The edges will go from genes with some of these features.
#' @param to.features The edges will go to genes with some of these features.
#' @keywords whitelist blacklist genes
#' @export
#' @examples
#' blacklist <- make.edgelist(genes, from.features='target', to.features='tf')

make.edgelist <- function(genes, from.genes=NULL, to.genes=NULL, from.features=NULL, to.features=NULL) {
    if (is.null(from.genes)) {
        from.genes <- genes$name
    }
    if (is.null(to.genes)) {
        to.genes <- genes$name
    }
    from <- c()
    to <- c()
    if (!is.null(from.features)) {
      for (i in 1:length(from.features)) {
          feature <- from.features[i]
          from <- c(from, genes[genes[genes$name %in% from.genes,feature],]$name)
      }
    } else {
      from <- c(from, genes[genes[genes$name %in% from.genes,],]$name)
    }
    if (!is.null(to.features)) {
      for (i in 1:length(to.features)) {
          feature <- to.features[i]
          to <- c(to, genes[genes[genes$name %in% to.genes,feature],]$name)
      }
    } else {
      to <- c(to, genes[genes[genes$name %in% to.genes,],]$name)
    }
    from <- unique(from)
    to <- unique(to)
    edge.list <- expand.grid(from, to)
    colnames(edge.list) <- c('from', 'to')
    edge.list$from <- as.character(edge.list$from)
    edge.list$to <- as.character(edge.list$to)
    edge.list <- edge.list[edge.list$from != edge.list$to,]
    rownames(edge.list) <- NULL
    return(edge.list)
}

#' Graph Marginalization
#'
#' This function allows you to marginalize a graph over observed genes.
#' @param g Graph object.
#' @param obs.genes Vector of observed genes.
#' @param max.steps Maximum number of steps in the path. Default: Inf
#' @param to Output format (optional): 'adjacency', 'edges', 'graph', 'igraph', or 'bnlearn'. Default: 'igraph'
#' @keywords graph genes marginalization
#' @export
#' @examples
#' g <- graph.marginalization(g, obs.genes)

graph.marginalization <- function(g, obs.genes, max.steps=Inf, to=c('igraph', 'adjacency', 'edges', 'graph', 'bnlearn')) {
    to <- match.arg(to)
    t <- as.igraph(as.adjacency(g))
    igraph::E(t)$weight <- abs(igraph::E(t)$weight)
    R <- length(obs.genes)
    new.edges <- as.data.frame(matrix(0, nrow=R, ncol=R))
    rownames(new.edges) <- colnames(new.edges) <- obs.genes
    for (gene.1 in obs.genes) {
        for (gene.2 in obs.genes) {
            if (gene.1 != gene.2) {
                default.warn <- getOption("warn")
                options(warn=-1)
                sh.paths <- igraph::all_shortest_paths(t, from=gene.1, to=gene.2, mode='out')$res
                options(warn = default.warn)
                keep <- TRUE
                if (length(sh.paths) > 0) {
                    for (sh.path in sh.paths) {
                        sh.path <- names(sh.path)
                        if (length(sh.path) > 2 & (length(sh.path)-2) <= max.steps) {
                            sh.path <- sh.path[2:(length(sh.path)-1)]
                            for (gene.3 in sh.path) {
                                if (gene.3 %in% obs.genes) {
                                    keep <- FALSE
                                }
                            }
                        } else {
                            keep <- FALSE
                            break
                        }
                    }
                } else {
                    keep <- FALSE
                }
                if (keep) {
                    new.edges[gene.1, gene.2] <- 1
                }
            }
        }
    }
    t <- as.adjacency(t)
    t <- t[obs.genes, obs.genes]
    new.edges <- averaged.graph(list(t, new.edges))
    new.edges <- convert.format(new.edges, to=to)
    return(new.edges)
}

#' Delete Isolated Nodes
#'
#' This function allows you to delete isolated genes (nodes) in a graph.
#' @param g Graph object.
#' @param to Output format (optional): 'adjacency', 'edges', 'graph', 'igraph', or 'bnlearn'. Default: 'igraph'
#' @param from Input format (optional): 'auto', 'adjacency', 'edges', 'graph', 'igraph', or 'bnlearn'. Default: 'auto'
#' @keywords graph isolated genes
#' @export
#' @examples
#' g <- delete.isolated(g, to='igraph')

delete.isolated <- function(g, to=c('igraph', 'adjacency', 'edges', 'graph', 'bnlearn'),
                            from=c('auto', 'adjacency', 'edges', 'graph', 'igraph', 'bnlearn')) {
    to <- match.arg(to)
    from <- match.arg(from)
    if (from=='auto') {
        from <- detect.format(g)
    }
    g <- convert.format(g, from=from, to='adjacency')
    g <- drop.all.zeros(g, square.matrix='all')
    g <- convert.format(g, from='adjacency', to=to)
    return(g)
}
