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
#' @param layout igraph plot layout (optional): 'grid', 'star', 'circle', 'tree', or 'nicely'. It will be ignored if interactive=TRUE. Default: 'grid'
#' @param interactive Interactive plot (optional). Default: FALSE
#' @keywords graph plot
#' @export
#' @examples
#' graph.plot(obj, interactive=FALSE)
#' graph.plot(obj, interactive=TRUE)

graph.plot <- function(x, from=NULL, layout=c('grid','star','circle','tree','nicely'),
                       interactive=FALSE) {
    layout <- match.arg(layout)

    if (is.null(from)) {
        from=detect.format(x)
    }

    g <- as.igraph(x, from=from)

    layout <- switch(layout,
        grid = igraph::layout_on_grid(g),
        star = igraph::layout_as_star(g),
        circle = igraph::layout_in_circle(g),
        tree = igraph::layout_as_tree(g),
        nicely = igraph::layout_nicely(g)
    )

    igraph::V(g)$color <- rgb(0.9,0.9,0.9,0.7)

    if (interactive) {
        threejs::graphjs(g,
            edge.color=rgb(0.2,0.2,0.2,0.9)
        )
    } else {
        igraph::plot.igraph(g,
            vertex.label.font=2,
            vertex.label.color='black',
            vertex.label.family='Helvetica',
            vertex.frame.color='black',
            vertex.shape='circle',
            vertex.size=30,
            edge.lty='solid',
            edge.width=2,
            edge.arrow.width=1,
            edge.color=rgb(0.2,0.2,0.2,0.9),
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
#' @param feature.color Color of the nodes with the feature (optional). Default: rgb(0.7,0.9,0.9,0.7)
#' @param layout igraph plot layout (optional): 'grid', 'star', 'circle', 'tree', or 'nicely'. It will be ignored if interactive=TRUE. Default: 'grid'
#' @param interactive Interactive plot (optional). Default: FALSE
#' @keywords graph feature plot
#' @export
#' @examples
#' feature.plot(obj, genes, 'tf', interactive=FALSE)
#' feature.plot(obj, genes, 'tumor.suppressor', interactive=TRUE)

feature.plot <- function(x, genes, feature, from=NULL, feature.color=rgb(0.7,0.9,0.9,0.7),
                         layout=c('grid','star','circle','tree','nicely'), interactive=FALSE) {
    layout <- match.arg(layout)

    if (is.null(from)) {
        from=detect.format(x)
    }

    x <- as.edges(x, from=from)
    f_genes <- genes[genes[feature]==TRUE,]$name
    x <- x[x[,1] %in% f_genes | x[,2] %in% f_genes, ]
    g <- as.igraph(x, from='edges')
    igraph::V(g)$color <- ifelse(names(igraph::V(g)) %in% f_genes, feature.color, rgb(0.9,0.9,0.9,0.7))

    layout <- switch(layout,
        grid = igraph::layout_on_grid(g),
        star = igraph::layout_as_star(g),
        circle = igraph::layout_in_circle(g),
        tree = igraph::layout_as_tree(g),
        nicely = igraph::layout_nicely(g)
    )

    if (interactive) {
        threejs::graphjs(g,
            edge.color=rgb(0.2,0.2,0.2,0.9)
        )
    } else {
        igraph::plot.igraph(g,
            vertex.label.font=2,
            vertex.label.color='black',
            vertex.label.family='Helvetica',
            vertex.frame.color='black',
            vertex.shape='circle',
            vertex.size=30,
            edge.lty='solid',
            edge.width=2,
            edge.arrow.width=1,
            edge.color=rgb(0.2,0.2,0.2,0.9),
            layout=layout)
    }
}

#' Graph Comparison
#'
#' This function allows you to compare two graphs, regardless of the format (adjacency matrix, list of edges, igraph, or bn).
#' @param learned Learned graph or graph 1.
#' @param true Ground truth graph or graph 2 (reference).
#' @param arcs Whether or not to list the arcs. Default: FALSE.
#' @param plot Whether or not to plot the differences between the two graphs. Default: TRUE
#' @keywords graph comparison
#' @export
#' @examples
#' comparison <- compare.graphs(obj1, obj2, plot=TRUE)
#' comparison <- compare.graphs(obj1, obj2, plot=FALSE)

compare.graphs <- function(learned, true, arcs=FALSE, plot=TRUE) {
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

    if (plot) {
        learned.x <- igraph::difference(learned, tp)
        true.x <- igraph::difference(true, tp)
        igraph::V(tp)$color <- rgb(0.9,0.9,0.9,0.7)
        igraph::V(learned.x)$color <- rgb(0.9,0.9,0.9,0.7)
        igraph::V(true.x)$color <- rgb(0.9,0.9,0.9,0.7)

        igraph::E(tp)$color <- rgb(0.2,0.2,0.2,0.9)
        igraph::E(tp)$lty <- 'solid'

        igraph::E(learned.x)$color <- rgb(0.7,0.0,0.0,0.9)
        igraph::E(learned.x)$lty <- 'dashed'

        igraph::E(true.x)$color <- rgb(0.0,0.7,0.0,0.9)
        igraph::E(true.x)$lty <- 'dashed'

        igraph::plot.igraph(tp,
            main='True Positive Arches',
            vertex.label.font=2,
            vertex.label.color='black',
            vertex.label.family='Helvetica',
            vertex.frame.color='black',
            vertex.shape='circle',
            vertex.size=30,
            edge.width=2,
            edge.arrow.width=1,
            layout=igraph::layout_on_grid(learned))

        igraph::plot.igraph(learned.x,
            main='False Positive Arches',
            vertex.label.font=2,
            vertex.label.color='black',
            vertex.label.family='Helvetica',
            vertex.frame.color='black',
            vertex.shape='circle',
            vertex.size=30,
            edge.width=2,
            edge.arrow.width=1,
            layout=igraph::layout_on_grid(true))

        igraph::plot.igraph(true.x,
            main='False Negative Arches',
            vertex.label.font=2,
            vertex.label.color='black',
            vertex.label.family='Helvetica',
            vertex.frame.color='black',
            vertex.shape='circle',
            vertex.size=30,
            edge.width=2,
            edge.arrow.width=1,
            layout=igraph::layout_on_grid(true))
    }

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
#' @param graphs List of graphs.
#' @param threshold Minimum strength required for a coefficient to be included in the averaged graph (optional). Default: 0.5
#' @param to Output format ('adjacency', 'edges', 'igraph', or 'bn') (optional).
#' @keywords average graph
#' @export
#' @examples
#' graph <- averaged.graph(df)

averaged.graph <- function(graphs, threshold=0.5, to='igraph') {
    R <- length(graphs)
    A <- array(data=NA, dim=c(dim(graphs[[1]]), R))
    for (i in 1:R) {
        A[,,i] <- graphs[[i]]
    }
    g1 <- sign(abs(graphs[[1]]))
    for (i in 2:R) {
        g1 <- as.adjacency(g1)
        g1 <- g1[order(colnames(g1)), order(colnames(g1))]
        g2 <- as.adjacency(sign(abs(graphs[[i]])))
        g2 <- g2[order(colnames(g2)), order(colnames(g2))]
        names1 <- colnames(g1)
        names2 <- colnames(g2)
        names.i <- intersect(names1, names2)
        names.xg1 <- setdiff(names1, names.i)
        names.xg2 <- setdiff(names2, names.i)
        g1.i <- g1[names.i, names.i]
        g2.i <- g2[names.i, names.i]
        g1.x <- g1[names.xg1, names.xg1]
        g2.x <- g2[names.xg2, names.xg2]
        g1 <- (g1.i + g2.i) / 2
        g1 <- gtools::smartbind(g1, g1.x)
        rownames(g1) <- colnames(g1)
        g1 <- gtools::smartbind(g1, g2.x)
        rownames(g1) <- colnames(g1)
        g1[is.na(g1)] <- 0
    }
    A <- g1[order(colnames(g1)), order(colnames(g1))]
    A[A < threshold] <- 0
    g <- convert.format(A, to=to)
    return(g)
}

#' Rename Vertices Of A List Of Graphs
#'
#' This function allows you to rename nodes of a list of graphs.
#' @param graphs List of graphs.
#' @param names Vector with the new vertex names (in order).
#' @param to Output format ('adjacency', 'edges', 'igraph', or 'bn') (optional).
#' @keywords rename graph
#' @export
#' @examples
#' graph <- rename.graphs(graphs, names)

rename.graphs <- function(graphs, names, to='igraph') {
  R <- length(graphs)
  for (i in 1:R) {
      graphs[[i]] <- convert.format(graphs[[i]], to='adjacency')
      rownames(graphs[[i]]) <- names
      colnames(graphs[[i]]) <- names
      graphs[[i]] <- convert.format(graphs[[i]], to=to)
  }
  return(graphs)
}

#' Graph Communities
#'
#' This function allows you to detect how many communities are in the graph and to which community each node and edge belongs.
#' @param x Graph object.
#' @param algorithm Algorithm for finding communities: 'louvain', 'edge.betweenness', 'fast.greedy', 'label.prop', 'leading.eigen', 'optimal', 'spinglass', or 'walktrap'. Default: 'louvain'
#' @param network Whether or not to plot the network. Default: TRUE
#' @param network.layout igraph network layout (optional): 'grid', 'star', 'circle', 'tree', or 'nicely'. It will be ignored if interactive=TRUE. Default: 'grid'
#' @param interactive.network Interactive network (optional). Default: FALSE
#' @param dendrogram Whether or not to plot a dendrogram (when possible). Default: FALSE
#' @param dendrogram.type Type of phylogeny to be drawn: 'fan', 'phylogram', 'cladogram', 'unrooted', or 'radial'. Default: 'fan'
#' @param from Input format (optional).
#' @keywords graph community plot
#' @export
#' @examples
#' communities <- graph.communities(g)
#' communities <- graph.communities(g, algorithm='louvain', network=TRUE, dendrogram=FALSE)
#' communities <- graph.communities(g, algorithm='walktrap', network=FALSE, dendrogram=TRUE, dendrogram.type='cladogram')

graph.communities <- function(x, algorithm=c('louvain','edge.betweenness','fast.greedy','label.prop','leading.eigen','optimal','spinglass','walktrap'),
                              network=TRUE, network.layout=c('grid','star','circle','tree','nicely'), interactive.network=FALSE,
                              dendrogram=FALSE, dendrogram.type=c('fan','phylogram','cladogram','unrooted','radial'),
                              from=NULL) {
    algorithm <- match.arg(algorithm)
    network.layout <- match.arg(network.layout)
    dendrogram.type <- match.arg(dendrogram.type)

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

    if (is.null(from)) {
        from=detect.format(x)
    }

    library(dplyr)

    g <- as.igraph(x, from=from)
    c <- algorithm(igraph::as.undirected(g))
    igraph::V(g)$community <- igraph::membership(c)

    palette <- rainbow(length(unique(igraph::V(g)$community)))

    node.community <- igraph::get.data.frame(g, what='vertices')

    edge.community <- igraph::get.data.frame(g, what='edges') %>%
        inner_join(node.community %>% select(name, community), by=c('from'='name')) %>%
        inner_join(node.community %>% select(name, community), by=c('to'='name')) %>%
        mutate(community = ifelse(community.x == community.y, community.x, NA) %>% factor())
    colnames(edge.community) <- c('from', 'to', 'weight', 'from.community', 'to.community', 'community')

    if (network & interactive.network) {

        threejs::graphjs(g,
            vertex.color = palette[as.numeric(as.factor(igraph::vertex_attr(g, 'community')))],
            edge.color=rgb(0.2,0.2,0.2,0.9)
        )

    } else if (network & !interactive.network) {

        network.layout <- switch(network.layout,
            grid = igraph::layout_on_grid(g),
            star = igraph::layout_as_star(g),
            circle = igraph::layout_in_circle(g),
            tree = igraph::layout_as_tree(g),
            nicely = igraph::layout_nicely(g)
        )

        igraph::V(g)$color <- rgb(0.9,0.9,0.9,0.7)

        igraph::plot.igraph(g,
            vertex.color = palette[as.numeric(as.factor(igraph::vertex_attr(g, 'community')))],
            vertex.label.font=2,
            vertex.label.color='black',
            vertex.label.family='Helvetica',
            vertex.frame.color='black',
            vertex.shape='circle',
            vertex.size=30,
            edge.lty='solid',
            edge.width=2,
            edge.arrow.width=1,
            edge.color=rgb(0.2,0.2,0.2,0.9),
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
