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
