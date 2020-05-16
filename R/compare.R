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
