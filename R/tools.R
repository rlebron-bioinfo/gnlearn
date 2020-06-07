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
            g <- from.bnlearn(g)
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
            g <- from.bnlearn(g)
            g <- as.edges(g, from='adjacency')
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
            g <- from.bnlearn(g)
            g <- as.graph(g, from='adjacency')
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
            g <- from.bnlearn(g)
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

from.bnlearn <- function(g) {
    fmt <- class(g)[1]
    if (fmt=='bn') {
        return(as.matrix(bnlearn::amat(g)))
    } else if (fmt=='bn.fit') {
        g <- bnlearn::amat(g)
        for (node in g.fit) {
            gene <- node$node
            coeff <- as.list(node$coefficients)
            coeff['(Intercept)'] <- NULL
            for (parent in names(coeff)) {
                w <- as.numeric(coeff[parent])
                g[gene, parent] <- w
            }
        }
        return(as.matrix(g))
    }
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
#' @param learned Learned graph or graph 1 (obj$average).
#' @param true Ground truth graph or graph 2 (reference).
#' @param learned.replicates List of learned replicates (obj$replicates) (optional). If provided, PR AUC will be calculated.
#' @param skeleton Whether to compare graph skeletons instead of the graphs themself. Default: FALSE
#' @param arcs Whether or not to list the arcs. Default: FALSE.
#' @param plot Whether or not to plot the differences between the two graphs. Default: TRUE
#' @param vertical.plot Whether to draw the comparison plots horizontally. Otherwise, they will be drawn horizontally. Default: TRUE
#' @param split.plot Whether to split comparison plots. Otherwise, they will be drawn together. Default: TRUE
#' @keywords graph comparison
#' @export
#' @examples
#' comparison <- compare.graphs(obj1, obj2, plot=TRUE)
#' comparison <- compare.graphs(obj1, obj2, plot=FALSE)

compare.graphs <- function(learned, true, learned.replicates=NULL, skeleton=FALSE,
                           arcs=FALSE, plot=TRUE, vertical.plot=TRUE, split.plot=TRUE) {
    learned <- as.igraph(learned)
    true <- as.igraph(true)

    if (skeleton) {
        learned <- igraph::as.undirected(learned, mode='collapse', edge.attr.comb=igraph::igraph_opt('edge.attr.comb'))
        true <- igraph::as.undirected(true, mode='collapse', edge.attr.comb=igraph::igraph_opt('edge.attr.comb'))
    }

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
    miss <- miss.rate(igraph::ecount(fn), igraph::ecount(tp))
    fdr <- fd.rate(igraph::ecount(fp), igraph::ecount(tp))

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
        tp <- igraph::ecount(tp)
        fp <- igraph::ecount(fp)
        fn <- igraph::ecount(fn)
        tp.out <- igraph::as_edgelist(tp)
        fp.out <- igraph::as_edgelist(fp)
        fn.out <- igraph::as_edgelist(fn)
    } else {
        tp.out <- tp <- igraph::ecount(tp)
        fp.out <- fp <- igraph::ecount(fp)
        fn.out <- fn <- igraph::ecount(fn)
    }

    N <- igraph::gorder(u)
    tn <- ifelse(skeleton, N*(N-1)/2, N*(N-1))
    s <- selectivity(tn, fp)
    npv <- np.value(tn, fn)
    f <- fall.out(fp, tn)
    fo.r <- fo.rate(fn, tn)
    acc <- accuracy(tp, tn, fp, fn)
    b.acc <- balanced.accuracy(tp, tn, fp, fn)

    if (!is.null(learned.replicates)) {
        R <- length(learned.replicates)
        precision.dist <- c()
        recall.dist <- c()
        fall.out.dist <- c()
        for (i in 1:R) {
            learned <- learned.replicates[[i]]
            stats <- compare.graphs(learned, true, learned.replicates=NULL, skeleton=skeleton,
                                    arcs=FALSE, plot=FALSE, vertical.plot=FALSE, split.plot=FALSE)
            precision.dist <- c(precision.dist, stats$Precision)
            recall.dist <- c(recall.dist, stats$Recall)
            fall.out.dist <- c(fall.out.dist, stats$Fall.Out)
        }

        library(dplyr)

        data <- data.frame(precision.dist, recall.dist)
        data <- data %>%
          group_by(recall.dist) %>%
          summarise(precision.dist=mean(precision.dist))
        precision.dist <- as.numeric(data$precision.dist)
        recall.dist <- as.numeric(data$recall.dist)
        pr.auc <- DescTools::AUC(recall.dist, precision.dist)

        if (plot) {
            plot(recall.dist, precision.dist, type='l', col='blue', lwd=3,
                 main=paste(c('Precision-Recall Curve\n(AUC = ', pr.auc, ')'), collapse=''), xlab='Recall', ylab='Precision')
        }

        data <- data.frame(fall.out.dist, recall.dist)
        data <- data %>%
          group_by(recall.dist) %>%
          summarise(fall.out.dist=mean(fall.out.dist))
        fall.out.dist <- as.numeric(data$fall.out.dist)
        recall.dist <- as.numeric(data$recall.dist)
        roc.auc <- DescTools::AUC(fall.out.dist, recall.dist)

        if (plot) {
            plot(fall.out.dist, recall.dist, type='l', col='blue', lwd=3,
                 main=paste(c('Receiver Operating Characteristic (ROC) Curve\n(AUC = ', pr.auc, ')'), collapse=''), xlab='Fall-Out', ylab='Recall')
        }

        return(list(
            TP = tp.out,
            FP = fp.out,
            FN = fn.out,
            TN = tn,
            Precision = p,
            Recall = r,
            F1.Score = f1,
            PR.AUC = pr.auc,
            Miss.Rate = miss,
            FDR = fdr,
            Selectivity = s,
            NPV = npv,
            Fall.Out = f,
            ROC.AUC = roc.auc,
            FOR = fo.r,
            Accuracy = acc,
            Balanced.Accuracy = b.acc,
            SHD = shd,
            Hamming = hamming
        ))
    } else {
        return(list(
            TP = tp.out,
            FP = fp.out,
            FN = fn.out,
            TN = tn,
            Precision = p,
            Recall = r,
            F1.Score = f1,
            Miss.Rate = miss,
            FDR = fdr,
            Selectivity = s,
            NPV = npv,
            Fall.Out = f,
            FOR = fo.r,
            Accuracy = acc,
            Balanced.Accuracy = b.acc,
            SHD = shd,
            Hamming = hamming
        ))
    }
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

miss.rate <- function(fn, tp) {
    return(fn/(fn+tp))
}

fd.rate <- function(fp, tp) {
    return(fp/(fp+tp))
}

selectivity <- function(tn, fp) {
    return(tn/(tn+fp))
}

np.value <- function(tn, fn) {
    return(tn/(tn+fn))
}

fall.out <- function(fp, tn) {
    return(fp/(fp+tn))
}

fo.rate <- function(fn, tn) {
    return(fn/(fn+tn))
}

accuracy <- function(tp, tn, fp, fn) {
    return((tp+tn)/(tp+tn+fp+fn))
}

balanced.accuracy <- function(tp, tn, fp, fn) {
    r <- recall(tp, fn)
    s <- selectivity(tn, fp)
    return((r+s)/2)
}

#' Degree Per Gene
#'
#' This function allows you to calculate the in-degree and out-degree of each (selected) gene.
#' @param x Graph object.
#' @param vertices Vertices you want to analyze. Default: all vertices.
#' @param in.degree Whether or not to analyze in-degree (optional). Default: TRUE.
#' @param out.degree Whether or not to analyze out-degree (optional). Default: TRUE.
#' @param loops Whether or not to consider loops (optional). Default: FALSE.
#' @param normalized Whether or not to normalize degrees (optional). Default: FALSE.
#' @keywords graph vertex degree
#' @export
#' @examples
#' mtx <- gene.degree(x)

gene.degree <- function(x, vertices=NULL, in.degree=TRUE, out.degree=TRUE, loops=FALSE, normalized=FALSE) {
    x <- as.adjacency(x)
    if (!loops) {
        diag(x) <- 0
    }
    k <- sum(c(in.degree, out.degree))
    if (k==0) {
        in.degree <- TRUE
        out.degree <- TRUE
        k <- 2
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
    mtx = matrix(0, n.vertices, k)
    rownames(mtx) <- vertices
    if (in.degree & out.degree) {
        colnames(mtx) <- c('in.degree', 'out.degree')
    } else if (in.degree) {
        colnames(mtx) <- c('in.degree')
    } else if (out.degree) {
        colnames(mtx) <- c('out.degree')
    }
    for (v in vertices) {
        if (in.degree) {
            mtx[v, 'in.degree'] <- igraph::degree(as.igraph(x), v, mode='in', normalized=normalized)
        }
        if (out.degree) {
            mtx[v, 'out.degree'] <- igraph::degree(as.igraph(x), v, mode='out', normalized=normalized)
        }
    }
    return(mtx)
}

#' Feature Degree Per Gene
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
        k <- 2
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

#' Calculate The Average Graph
#'
#' This function allows you to calculate the average graph from a list of graphs.
#' @param graphs List of graphs.
#' @param threshold Minimum strength required for a coefficient to be included in the average graph (optional). Default: 0.5
#' @param to Output format ('adjacency', 'edges', 'graph', 'igraph', or 'bnlearn') (optional).
#' @keywords average graph
#' @export
#' @examples
#' graph <- average.graph(graphs)

average.graph <- function(graphs, threshold=0.5, to='igraph') {

    R <- length(graphs)
    all.cols <- c()
    for (i in 1:R) {
        g <- as.data.frame(as.adjacency(graphs[[i]]))
        rownames(g) <- colnames(g)
        g <- g[sort(rownames(g)), sort(colnames(g))]
        cols <- colnames(g)
        all.cols <- c(all.cols, cols)
    }

    all.cols <- sort(unique( all.cols))
    all.occurr <- as.data.frame(matrix(0, nrow=length(all.cols), ncol=length(all.cols)))
    rownames(all.occurr) <- colnames(all.occurr) <- all.cols
    all.coeff <- as.data.frame(matrix(0, nrow=length(all.cols), ncol=length(all.cols)))
    rownames(all.coeff) <- colnames(all.coeff) <- all.cols
    all.adj <- as.data.frame(matrix(0, nrow=length(all.cols), ncol=length(all.cols)))
    rownames(all.adj) <- colnames(all.adj) <- all.cols

    for (i in 1:R) {
        coeff <- graphs[[i]]
        adj <- sign(abs(coeff))
        cols <- colnames(coeff)
        occurr <- as.data.frame(matrix(1, nrow=length(cols), ncol=length(cols)))
        rownames(occurr) <- colnames(occurr) <- cols
        all.occurr[cols, cols] <- all.occurr[cols, cols] + occurr
        all.coeff[cols, cols] <- all.coeff[cols, cols] + coeff
        all.adj[cols, cols] <- all.adj[cols, cols] + adj
    }

    all.occurr[all.occurr == 0] <- 1
    all.coeff <- all.coeff / all.occurr
    all.adj <- all.adj / all.occurr
    all.coeff[all.adj < threshold] <- 0

    g <- convert.format(all.coeff, to=to)
    return(g)

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

#' Drug-Gene Interactions Plotting
#'
#' This function allows you to visualize the interaction of a selected gene with drugs and other genes.
#' @param x Graph object.
#' @param drugs Geneset with drug-gene interactions.
#' @param gene Gene whose drug interactions you want to explore.
#' @param neighbors Whether or not to draw neighboring genes. Default: TRUE
#' @param from Input format (optional).
#' @param interactive Interactive plot (optional). Default: FALSE
#' @keywords genes drugs graph plot
#' @export
#' @examples
#' drugs.plot(g, 3, 'Bax')

drugs.plot <- function(x, drugs, gene, neighbors=TRUE, from=c('auto', 'adjacency', 'edges', 'graph', 'igraph', 'bnlearn'), interactive=FALSE) {
    from <- match.arg(from)

    if (class(drugs)=='numeric') {
        genesets <- list.genesets()
        type <- genesets[genesets$download.code==drugs,]$dataset
        if (grepl('Drug-Gene Interactions', type, ignore.case=TRUE)) {
            drugs <- download.geneset(drugs)
        } else {
            message(paste('Wrong type of geneset?', '->', type))
            drugs <- download.geneset(drugs)
        }
    } else {
        fmt <- detect.format(drugs)
        drugs <- as.edges(drugs, from=fmt)
    }
    if (from=='auto') {
        from=detect.format(x)
    }

    drugs$from <- gsub(' ', '\n', drugs$from)
    drugs.df <- drugs[drugs$to==gene,]
    drugs <- as.adjacency(drugs.df, from='edges')

    if (sum(drugs)==0) {

        if (neighbors) {
            genes <- as.edges(x, from=from)
            genes <- as.adjacency(genes[genes$from == gene | genes$to == gene,], from='edges')
        } else {
            genes <- as.data.frame(matrix(0, nrow=1, ncol=1))
            rownames(genes) <- colnames(genes) <- c(gene)
        }

        g <- as.igraph(genes, from='adjacency')
        igraph::V(g)$color <- rgb(0.0,0.5,0.5,0.1)
        igraph::V(g)$shape <- 'circle'
        igraph::V(g)$frame.color <- rgb(0.0,0.3,0.3)
        igraph::V(g)$label.color <- rgb(0.0,0.3,0.3)
        igraph::V(g)$label.cex <- 1

    } else {

        if (neighbors) {
            genes <- as.edges(x, from=from)
            genes <- as.adjacency(genes[genes$from == gene | genes$to == gene,], from='edges')
            genes <- genes[sort(rownames(genes)), sort(colnames(genes))]
        } else {
            genes <- as.data.frame(matrix(0, nrow=1, ncol=1))
            rownames(genes) <- colnames(genes) <- c(gene)
        }

        g <- as.adjacency(g)
        g <- average.graph(list(drugs, genes), threshold=0)
        positive <- c('activator', 'agonist', 'cofactor', 'inducer', 'partial_agonist', 'positive_allosteric_modulator', 'stimulator')
        negative <- c('channel_blocker', 'antagonist', 'antibody', 'antisense', 'antisense_oligonucleotide', 'blocker', 'gating_inhibitor', 'inhibitor', 'inhibitory_allosteric_modulator', 'inverse_agonist', 'negative_modulator', 'vaccine')
        undefined <- c('allosteric_modulator', 'binder', 'modulator')
        for (i in 1:nrow(drugs.df)) {
            drug <- drugs.df[i,]$from
            if ('type' %in% colnames(drugs.df)) {
                drug.type <- drugs.df[i,]$type
                p <- sum(grepl(drug.type, positive, ignore.case=TRUE))
                n <- sum(grepl(drug.type, negative, ignore.case=TRUE))
                u <- sum(grepl(drug.type, undefined, ignore.case=TRUE))
                if (p & !n) {
                    g[drug, gene] <- 1
                } else if (n & !p) {
                    g[drug, gene] <- -1
                } else {
                    g[drug, gene] <- 1e-100
                }
            } else {
                g[drug, gene] <- 1e-100
            }
        }

        g <- as.igraph(g)
        drugs <- as.igraph(drugs)
        drugs <- igraph::delete_vertices(drugs, gene)
        igraph::V(g)$color <- ifelse(names(igraph::V(g)) %in% names(igraph::V(drugs)), rgb(0.5,0.0,0.5,0.1), rgb(0.0,0.5,0.5,0.1))
        igraph::V(g)$shape <- ifelse(names(igraph::V(g)) %in% names(igraph::V(drugs)), 'square', 'circle')
        igraph::V(g)$frame.color <- ifelse(names(igraph::V(g)) %in% names(igraph::V(drugs)), rgb(0.3,0.0,0.3), rgb(0.0,0.3,0.3))
        igraph::V(g)$label.color <- ifelse(names(igraph::V(g)) %in% names(igraph::V(drugs)), rgb(0.3,0.0,0.3), rgb(0.0,0.3,0.3))
        igraph::V(g)$label.cex <- ifelse(names(igraph::V(g)) %in% names(igraph::V(drugs)), 0.75, 1)

    }

    if ('weight' %in% igraph::list.edge.attributes(g)) {
        igraph::E(g)$color <- ifelse(igraph::E(g)$weight == 1e-100, rgb(0.0,0.0,0.7,0.9),
                                     ifelse(igraph::E(g)$weight > 0, rgb(0.0,0.7,0.0,0.9), rgb(0.7,0.0,0.0,0.9)))
        igraph::E(g)$width <- sapply(igraph::E(g)$weight, function(x) ceiling(abs(x))+1)
        t <- as.igraph(as.adjacency(g))
        igraph::E(t)$weight <- abs(igraph::E(t)$weight)
    } else {
        igraph::E(g)$color <- rgb(0.3,0.3,0.3,0.9)
        igraph::E(g)$width <- 2
        t <- as.igraph(as.adjacency(g))
    }

    layout <- igraph::layout_as_star(g, center = gene)

    if (interactive) {
        layout <- third.axis(layout)
        threejs::graphjs(g,
            layout=layout)
    } else {
        igraph::plot.igraph(g,
            vertex.label.font=2,
            vertex.label.family='Helvetica',
            vertex.size=50,
            edge.lty='solid',
            edge.arrow.width=1,
            layout=layout,
            rescale=TRUE)
    }
}

#' Ortholog Genes Graph
#'
#' This function allows you to convert graph between different formats: adjacency matrix, list of edges, igraph and bnlearn (bnlearn).
#' @param g Graph object.
#' @param genes Geneset with features.
#' @param column Name of column with ortholog genes.
#' @param to Output format (optional): 'adjacency', 'edges', 'graph', 'igraph', or 'bnlearn'. Default: 'igraph'
#' @param from Input format (optional): 'auto', 'adjacency', 'edges', 'graph', 'igraph', or 'bnlearn'. Default: 'auto'
#' @keywords graph ortholog genes
#' @export
#' @examples
#' g <- ortholog.graph(g, genes, column='human.ortholog')

ortholog.graph <- function(g, genes, column, to=c('igraph', 'adjacency', 'edges', 'graph', 'bnlearn'),
                           from=c('auto', 'adjacency', 'edges', 'graph', 'igraph', 'bnlearn')) {
    to <- match.arg(to)
    from <- match.arg(from)

    if (from=='auto') {
        from <- detect.format(g)
    }

    g <- as.igraph(g, from=from)

    nodes <- names(igraph::V(g))
    ortholog.genes <- sapply(nodes, function(name) {
        new.name <- genes[genes$name==name, ]
        new.name <- new.name[1, column]
        if (length(new.name)==0 | is.null(new.name)) {
            new.name <- paste(c('NA', name), colapse=':')
        }
        new.name
    })
    igraph::V(g)$name <- ortholog.genes
    igraph::V(g)$label <- ortholog.genes

    g <- convert.format(g, from='igraph', to=to)
    return(g)
}

#' Estimate The Coefficients Of An Adjacency Matrix
#'
#' This function allows you to estimate the coefficients of the adjacency matrix of a previously learned graph.
#' @param g Graph object.
#' @param df Dataset.
#' @param to Output format (optional): 'adjacency', 'edges', 'graph', 'igraph', or 'bnlearn'. Default: 'igraph'
#' @param from Input format (optional): 'auto', 'adjacency', 'edges', 'graph', 'igraph', or 'bnlearn'. Default: 'auto'
#' @param cluster A cluster object from package parallel or the number of cores to be used (optional). Default: 4
#' @keywords graph fit coefficients
#' @export
#' @examples
#' g <- fit.coefficients(g, df)

fit.coefficients <- function(g, df, to=c('igraph', 'adjacency', 'edges', 'graph', 'bnlearn'),
                             from=c('auto', 'adjacency', 'edges', 'graph', 'igraph', 'bnlearn'), cluster=4) {
    to <- match.arg(to)
    from <- match.arg(from)
    if (from=='auto') {
        from <- detect.format(g)
    }

    df <- subset(df, select=colnames(as.adjacency(g, from=from)))
    g <- as.bnlearn(g, from=from)
    if (bnlearn::directed(g) & bnlearn::acyclic(g)) {
        library(parallel)
        cluster = makeCluster(cluster)
        g.fit <- bnlearn::bn.fit(g, df, method='mle', keep.fitted=TRUE, cluster=cluster)
        g <- bnlearn::amat(g)
        for (node in g.fit) {
            gene <- node$node
            coeff <- as.list(node$coefficients)
            coeff['(Intercept)'] <- NULL
            for (parent in names(coeff)) {
                w <- as.numeric(coeff[parent])
                g[gene, parent] <- w
            }
        }
        stopCluster(cluster)
        g <- convert.format(g, from='adjacency', to=to)
        return(g)
    } else {
        message('The graph contains undirected edges, cycles or loops.')
        return(NULL)
    }
}

#' Compute The Score Of A Graph
#'
#' This function allows you to compute the score of a previously learned graph.
#' @param g Graph object.
#' @param test.df Test dataset.
#' @param from Input format (optional): 'auto', 'adjacency', 'edges', 'graph', 'igraph', or 'bnlearn'. Default: 'auto'
#' @keywords graph score
#' @export
#' @examples
#' score.graph(g, test.df)

score.graph <- function(g, test.df, from=c('auto', 'adjacency', 'edges', 'graph', 'igraph', 'bnlearn')) {
    from <- match.arg(from)
    if (from=='auto') {
        from <- detect.format(g)
    }
    test.df <- subset(test.df, select=colnames(as.adjacency(g, from=from)))
    g <- as.bnlearn(g, from=from)
    if (bnlearn::directed(g) & bnlearn::acyclic(g)) {
        scores <- list()
        scores$loglik <- bnlearn::score(g, test.df, type='loglik-g')
        scores$aic <- bnlearn::score(g, test.df, type='aic-g')
        scores$bic <- bnlearn::score(g, test.df, type='bic-g')
        scores$bge <- bnlearn::score(g, test.df, type='bge')
        return(scores)
    } else {
        message('The graph contains undirected edges, cycles or loops.')
        return(NULL)
    }
}

#' Return Undirected Edges
#'
#' This function returns all undirected edges in a graph.
#' @param g Graph object.
#' @param to Output format (optional): 'adjacency', 'edges', 'graph', 'igraph', or 'bnlearn'. Default: 'igraph'
#' @param from Input format (optional): 'auto', 'adjacency', 'edges', 'graph', 'igraph', or 'bnlearn'. Default: 'auto'
#' @keywords graph undirected edges
#' @export
#' @examples
#' g <- undirected.edges(g)

undirected.edges <- function(g, to=c('igraph', 'adjacency', 'edges', 'graph', 'bnlearn'),
                             from=c('auto', 'adjacency', 'edges', 'graph', 'igraph', 'bnlearn')) {
    to <- match.arg(to)
    from <- match.arg(from)

    if (from=='auto') {
        from <- detect.format(g)
    }

    g <- as.bnlearn(g, from=from)
    g <- bnlearn::undirected.arcs(g)
    g <- convert.format(g, from='edges', to=to)
    return(g)
}

#' Return Directed Edges
#'
#' This function returns all directed edges in a graph.
#' @param g Graph object.
#' @param to Output format (optional): 'adjacency', 'edges', 'graph', 'igraph', or 'bnlearn'. Default: 'igraph'
#' @param from Input format (optional): 'auto', 'adjacency', 'edges', 'graph', 'igraph', or 'bnlearn'. Default: 'auto'
#' @keywords graph directed edges
#' @export
#' @examples
#' g <- directed.edges(g)

directed.edges <- function(g, to=c('igraph', 'adjacency', 'edges', 'graph', 'bnlearn'),
                           from=c('auto', 'adjacency', 'edges', 'graph', 'igraph', 'bnlearn')) {
    to <- match.arg(to)
    from <- match.arg(from)

    if (from=='auto') {
        from <- detect.format(g)
    }

    g <- as.bnlearn(g, from=from)
    g <- bnlearn::directed.arcs(g)
    g <- convert.format(g, from='edges', to=to)
    return(g)
}

#' Add Genes To A Graph
#'
#' This function adds genes to a previously learned graph.
#' @param g Graph object.
#' @param new.genes New genes to be added (vector).
#' @param to Output format (optional): 'adjacency', 'edges', 'graph', 'igraph', or 'bnlearn'. Default: 'igraph'
#' @param from Input format (optional): 'auto', 'adjacency', 'edges', 'graph', 'igraph', or 'bnlearn'. Default: 'auto'
#' @keywords graph genes
#' @export
#' @examples
#' g <- add.genes(g, c('Cd74', 'Ldhc', 'Tlx3'))

add.genes <- function(g, new.genes, to=c('igraph', 'adjacency', 'edges', 'graph', 'bnlearn'),
                      from=c('auto', 'adjacency', 'edges', 'graph', 'igraph', 'bnlearn')) {
    to <- match.arg(to)
    from <- match.arg(from)

    if (from=='auto') {
        from <- detect.format(g)
    }

    g <- as.igraph(g, from=from)
    new.genes <- new.genes[!new.genes %in% names(igraph::V(g))]
    if (length(new.genes) > 0) {
        g <- igraph::add_vertices(g, length(new.genes), attr=list(name=new.genes))
    }
    g <- convert.format(g, from='igraph', to=to)
    return(g)
}

#' Rename Genes To A Graph
#'
#' This function renames genes of a previously learned graph.
#' @param g Graph object.
#' @param genes Genes to be renamed (vector).
#' @param new.names New names (vector, in the same order).
#' @param to Output format (optional): 'adjacency', 'edges', 'graph', 'igraph', or 'bnlearn'. Default: 'igraph'
#' @param from Input format (optional): 'auto', 'adjacency', 'edges', 'graph', 'igraph', or 'bnlearn'. Default: 'auto'
#' @keywords graph genes
#' @export
#' @examples
#' g <- rename.genes(g, c('A', 'B', 'C'), c('Cd74', 'Ldhc', 'Tlx3'))

rename.genes <- function(g, genes, new.names, to=c('igraph', 'adjacency', 'edges', 'graph', 'bnlearn'),
                         from=c('auto', 'adjacency', 'edges', 'graph', 'igraph', 'bnlearn')) {
    to <- match.arg(to)
    from <- match.arg(from)

    if (from=='auto') {
        from <- detect.format(g)
    }

    g <- as.igraph(g, from=from)
    genes <- genes[genes %in% names(igraph::V(g))]
    if (length(genes) > 0) {
        g <- igraph::set_vertex_attr(g, 'name', index=genes, new.names)
    }
    g <- convert.format(g, from='igraph', to=to)
    return(g)
}

#' Delete Genes From A Graph
#'
#' This function deletes genes from a previously learned graph.
#' @param g Graph object.
#' @param rm.genes Genes to be deleted (vector).
#' @param to Output format (optional): 'adjacency', 'edges', 'graph', 'igraph', or 'bnlearn'. Default: 'igraph'
#' @param from Input format (optional): 'auto', 'adjacency', 'edges', 'graph', 'igraph', or 'bnlearn'. Default: 'auto'
#' @keywords graph genes
#' @export
#' @examples
#' g <- delete.genes(g, c('Cd74', 'Ldhc', 'Tlx3'))

delete.genes <- function(g, rm.genes, to=c('igraph', 'adjacency', 'edges', 'graph', 'bnlearn'),
                         from=c('auto', 'adjacency', 'edges', 'graph', 'igraph', 'bnlearn')) {
    to <- match.arg(to)
    from <- match.arg(from)

    if (from=='auto') {
        from <- detect.format(g)
    }

    g <- as.igraph(g, from=from)
    rm.genes <- rm.genes[rm.genes %in% names(igraph::V(g))]
    if (length(rm.genes) > 0) {
        g <- igraph::delete_vertices(g, rm.genes)
    }
    g <- convert.format(g, from='igraph', to=to)
    return(g)
}

#' Add Edges To A Graph
#'
#' This function adds edges to a previously learned graph.
#' @param g Graph object.
#' @param new.edges New edges to be added (dataframe with at least two columns: 'from' and 'to').
#' @param new.genes New genes (nodes) will also be included. Otherwise, only edges of nodes already present in the network will be taken into account. Default: TRUE
#' @param to Output format (optional): 'adjacency', 'edges', 'graph', 'igraph', or 'bnlearn'. Default: 'igraph'
#' @param from Input format (optional): 'auto', 'adjacency', 'edges', 'graph', 'igraph', or 'bnlearn'. Default: 'auto'
#' @keywords graph genes edges
#' @export
#' @examples
#' g <- add.edges(g, new.edges)

add.edges <- function(g, new.edges, new.genes=TRUE, to=c('igraph', 'adjacency', 'edges', 'graph', 'bnlearn'),
                      from=c('auto', 'adjacency', 'edges', 'graph', 'igraph', 'bnlearn')) {
    to <- match.arg(to)
    from <- match.arg(from)

    if (from=='auto') {
        from <- detect.format(g)
    }

    g <- as.adjacency(g, from=from)
    new.edges <- as.adjacency(new.edges, from='edges')

    if (new.genes) {
        g <- add.genes(g, colnames(new.edges), from='adjacency', to='adjacency')
    }

    g <- g[sort(colnames(g)), sort(colnames(g))]
    cols.g <- colnames(g)
    new.edges <- new.edges[sort(colnames(new.edges)), sort(colnames(new.edges))]
    cols.new <- colnames(new.edges)
    cols.new <- intersect(cols.g, cols.new)
    new.edges <- as.edges(new.edges[cols.new, cols.new], from='adjacency')
    for (i in 1:nrow(new.edges)) {
        e.from <- new.edges[i,]$from
        e.to <- new.edges[i,]$to
        e.weight <- new.edges[i,]$weight
        g[e.from, e.to] <- e.weight
    }

    g <- convert.format(g, from='adjacency', to=to)
    return(g)
}

#' Delete Edges To A Graph
#'
#' This function deletes edges to a previously learned graph.
#' @param g Graph object.
#' @param rm.edges New edges to be deleted (dataframe with at least two columns: 'from' and 'to').
#' @param to Output format (optional): 'adjacency', 'edges', 'graph', 'igraph', or 'bnlearn'. Default: 'igraph'
#' @param from Input format (optional): 'auto', 'adjacency', 'edges', 'graph', 'igraph', or 'bnlearn'. Default: 'auto'
#' @keywords graph genes edges
#' @export
#' @examples
#' g <- delete.edges(g, new.edges)

delete.edges <- function(g, rm.edges, to=c('igraph', 'adjacency', 'edges', 'graph', 'bnlearn'),
                         from=c('auto', 'adjacency', 'edges', 'graph', 'igraph', 'bnlearn')) {
    to <- match.arg(to)
    from <- match.arg(from)

    if (from=='auto') {
        from <- detect.format(g)
    }

    g <- as.adjacency(g, from=from)
    new.edges <- as.adjacency(new.edges, from='edges')
    g <- g[sort(colnames(g)), sort(colnames(g))]
    cols.g <- colnames(g)
    new.edges <- new.edges[sort(colnames(new.edges)), sort(colnames(new.edges))]
    cols.new <- colnames(new.edges)
    cols.new <- intersect(cols.g, cols.new)
    new.edges <- as.edges(new.edges[cols.new, cols.new], from='adjacency')
    for (i in 1:nrow(new.edges)) {
        e.from <- new.edges[i,]$from
        e.to <- new.edges[i,]$to
        e.weight <- 0
        g[e.from, e.to] <- e.weight
    }

    g <- convert.format(g, from='adjacency', to=to)
    return(g)
}
