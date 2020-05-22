#' Run GLASSO Algorithm
#'
#' This function allows you to learn an undirected graph from a dataset using the GLASSO algorithm.
#' @param df Dataset.
#' @param rho Non-negative regularization parameter for GLASSO.
#' @param to Output format ('adjacency', 'edges', 'igraph', or 'bn') (optional).
#' @param from Input format (optional).
#' @param upper Whether or not to ignore the upper triangular adjacency matrix (optional).
#' @param lower Whether or not to ignore the lower triangular adjacency matrix (optional).
#' @param loops Whether or not to ignore the diagonal of the adjacency matrix (optional).
#' @param non.zero.coeff If other than NULL, non-zero adjacency coefficients shall be replaced by the value provided (optional). Default: NULL
#' @param unconnected.nodes Include unconnected nodes (optional). Default: FALSE
#' @keywords learning graph
#' @export
#' @examples
#' graph <- run.glasso(df)

run.glasso <- function(df, rho=0.5, to='igraph', upper=FALSE, lower=TRUE, loops=FALSE, non.zero.coeff=NULL, unconnected.nodes=FALSE) {
    k <- sum(c(upper, lower))
    if (k==0) {
        upper <- TRUE
        lower <- TRUE
    }
    S <- cov(as.matrix(df))
    gl <- glasso::glasso(S, rho=rho)
    A <- gl$wi
    rownames(A) <- rownames(S)
    colnames(A) <- colnames(S)
    if (!loops) {
        diag(A) <- 0
    }
    if (!upper) {
        A[upper.tri(A)] <- 0
    }
    if (!lower) {
        A[lower.tri(A)] <- 0
    }
    if (!is.null(non.zero.coeff)) {
        A <- ifelse(A!=0, non.zero.coeff, 0)
    }
    if (!unconnected.nodes) {
        A <- A[apply(A, 1, function(x) !all(x==0)),]
        A <- A[,apply(A, 2, function(x) !all(x==0))]
    }
    g <- convert_format(A, to=to)
    return(g)
}
