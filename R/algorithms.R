#' Run GLASSO Algorithm
#'
#' This function allows you to learn an undirected graph from a dataset using the GLASSO algorithm.
#' @param df Dataset.
#' @param rho Non-negative regularization parameter for GLASSO.
#' @param R Number of bootstrap replicates (optional). Default: 200
#' @param m Size of each bootstrap replicate (optional). Default: nrow(df)/2
#' @param threshold Minimum strength required for a coefficient to be included in the averaged adjacency matrix (optional). Default: 0.5
#' @param upper Whether or not to ignore the upper triangular adjacency matrix (optional).
#' @param lower Whether or not to ignore the lower triangular adjacency matrix (optional).
#' @param loops Whether or not to ignore the diagonal of the adjacency matrix (optional).
#' @param unconnected.nodes Include unconnected nodes (optional). Default: FALSE
#' @param to Output format ('adjacency', 'edges', 'igraph', or 'bn') (optional).
#' @param cluster A cluster object from package parallel or the number of cores to be used (optional). Default: 4
#' @keywords learning graph
#' @export
#' @examples
#' graph <- run.glasso(df, rho=0.1)

run.glasso <- function(df, rho=0.1, R=200, m=NULL, threshold=0.5, upper=FALSE, lower=TRUE, loops=FALSE, unconnected.nodes=FALSE, to='igraph', cluster=4) {
    k <- sum(c(upper, lower))
    if (k==0) {
        upper <- TRUE
        lower <- TRUE
    }

    library(foreach)
    library(doParallel)

    df <- drop.all.zeros(df)

    registerDoParallel(cluster)

    graphs <- foreach(rep=1:R) %dopar% {
        splitted.df <- dataframe.split(df, m=m)
        S <- cov(as.matrix(splitted.df$train))
        g <- glasso::glasso(S, rho=rho)
        g$wi
    }

    stopImplicitCluster()

    A <- averaged.graph(graphs, colnames(df), threshold=threshold, to='adjacency')
    if (!loops) {
        diag(A) <- 0
    }
    if (!upper) {
        A[upper.tri(A)] <- 0
    }
    if (!lower) {
        A[lower.tri(A)] <- 0
    }
    if (!unconnected.nodes) {
        A <- drop.all.zeros(A, square.matrix='all')
    }
    g <- convert.format(A, to=to)
    return(g)
}

#' Run GCLM Algorithm
#'
#' This function allows you to learn a directed graph from a dataset using the GCLM algorithm.
#' @param df Dataset.
#' @param R Number of bootstrap replicates (optional). Default: 200
#' @param m Size of each bootstrap replicate (optional). Default: nrow(df)/2
#' @param threshold Minimum strength required for a coefficient to be included in the averaged adjacency matrix (optional). Default: 0.5
#' @param loops Whether or not to ignore the diagonal of the adjacency matrix (optional). Default: FALSE
#' @param unconnected.nodes Include unconnected nodes (optional). Default: FALSE
#' @param to Output format ('adjacency', 'edges', 'igraph', or 'bn') (optional).
#' @param cluster A cluster object from package parallel or the number of cores to be used (optional). Default: 4
#' @keywords learning graph
#' @export
#' @examples
#' graph <- run.gclm(df)

run.gclm <- function(df, R=200, m=NULL, threshold=0.5, loops=FALSE, unconnected.nodes=FALSE, to='igraph', cluster=4) {

    library(foreach)
    library(doParallel)

    df <- drop.all.zeros(df)
    df <- as.matrix(df)

    registerDoParallel(cluster)

    graphs <- foreach(rep=1:R) %dopar% {

        splitted.df <- dataframe.split(df, m=m)
        S.train <- cov(splitted.df$train)
        S.test <-  cov(splitted.df$test)
        dd <- diag(1/sqrt(diag(S.train)))
        S.train.cor <- cov2cor(S.train)

        # estimate path
        results.path <- gclm::gclm.path(S.train.cor,
                                        B = -0.5*solve(S.train.cor),
                                        lambdac = 0.01,
                                        lambdas = 6*10^seq(-4,0, length=100),
                                        eps = 1e-6, job=0, maxIter=1000)

        # fit MLE to all path
        results.path <- lapply(results.path, function(res) {
            gclm::gclm(S.train.cor,
                       B = res$B,
                       C = res$C,
                       lambda = 0,
                       lambdac = -1,
                       eps = 1e-10,
                       job = 10)
        })

        # compute minus loglik
        tmp <- sapply(results.path, function(res) {
            mll(solve(res$Sigma), dd %*% S.test %*% dd)
        })
        bidx <- which.min(tmp)
        t(results.path[[bidx]]$B)

    }

    stopImplicitCluster()

    A <- averaged.graph(graphs, colnames(df), threshold=threshold, to='adjacency')
    if (!loops) {
        diag(A) <- 0
    }
    if (!unconnected.nodes) {
        A <- drop.all.zeros(A, square.matrix='all')
    }
    g <- convert.format(A, to=to)
    return(g)
}

mll <- function(P, S) {
    -determinant(P, logarithm=TRUE)$modulus + sum(S*P)
}

#' Run ARACNE Algorithm
#'
#' This function allows you to learn a undirected graph from a dataset using the ARACNE algorithm.
#' @param df Dataset.
#' @param whitelist A data frame with two columns, containing a set of arcs to be included in the graph.
#' @param blacklist A data frame with two columns, containing a set of arcs not to be included in the graph.
#' @param mi The estimator used for the pairwise mutual information coefficients: 'mi' (discrete mutual information) or 'mi-g' (Gaussian mutual information). Default: 'mi-g'
#' @param R Number of bootstrap replicates (optional). Default: 200
#' @param m Size of each bootstrap replicate (optional). Default: nrow(df)/2
#' @param threshold Minimum strength required for a coefficient to be included in the averaged adjacency matrix (optional). Default: 0.5
#' @param to Output format ('adjacency', 'edges', 'igraph', or 'bn') (optional).
#' @param cluster A cluster object from package parallel or the number of cores to be used (optional). Default: 4
#' @keywords learning graph
#' @export
#' @examples
#' graph <- run.aracne(df)

run.aracne <- function(df, whitelist=NULL, blacklist=NULL, mi=c('mi-g','mi'), R=200, m=NULL, threshold=0.5, to='igraph', cluster=4) {
    mi <- match.arg(mi)

    library(foreach)
    library(doParallel)

    df <- drop.all.zeros(df)

    registerDoParallel(cluster)

    graphs <- foreach(rep=1:R) %dopar% {
        splitted.df <- dataframe.split(df, m=m)
        g <- bnlearn::aracne(splitted.df$train, whitelist=whitelist, blacklist=blacklist, mi=mi)
        convert.format(g, to='adjacency')
    }

    stopImplicitCluster()

    g <- averaged.graph(graphs, colnames(df), threshold=threshold, to=to)
    return(g)
}

#' Run Chow-Liu Algorithm
#'
#' This function allows you to learn a undirected graph from a dataset using the Chow-Liu algorithm.
#' @param df Dataset.
#' @param whitelist A data frame with two columns, containing a set of arcs to be included in the graph.
#' @param blacklist A data frame with two columns, containing a set of arcs not to be included in the graph.
#' @param mi The estimator used for the pairwise mutual information coefficients: 'mi' (discrete mutual information) or 'mi-g' (Gaussian mutual information). Default: 'mi-g'
#' @param R Number of bootstrap replicates (optional). Default: 200
#' @param m Size of each bootstrap replicate (optional). Default: nrow(df)/2
#' @param threshold Minimum strength required for a coefficient to be included in the averaged adjacency matrix (optional). Default: 0.5
#' @param to Output format ('adjacency', 'edges', 'igraph', or 'bn') (optional).
#' @param cluster A cluster object from package parallel or the number of cores to be used (optional). Default: 4
#' @keywords learning graph
#' @export
#' @examples
#' graph <- run.chowliu(df)

run.chowliu <- function(df, whitelist=NULL, blacklist=NULL, mi=c('mi-g','mi'), R=200, m=NULL, threshold=0.5, to='igraph', cluster=4) {
    mi <- match.arg(mi)

    library(foreach)
    library(doParallel)

    df <- drop.all.zeros(df)

    registerDoParallel(cluster)

    graphs <- foreach(rep=1:R) %dopar% {
        splitted.df <- dataframe.split(df, m=m)
        g <- bnlearn::chow.liu(splitted.df$train, whitelist=whitelist, blacklist=blacklist, mi=mi)
        convert.format(g, to='adjacency')
    }

    stopImplicitCluster()

    g <- averaged.graph(graphs, colnames(df), threshold=threshold, to=to)
    return(g)
}

#' Run LINGAM Algorithm
#'
#' This function allows you to learn a directed graph from a dataset using the LINGAM algorithm.
#' @param df Dataset.
#' @param R Number of bootstrap replicates (optional). Default: 200
#' @param m Size of each bootstrap replicate (optional). Default: nrow(df)/2
#' @param threshold Minimum strength required for a coefficient to be included in the averaged adjacency matrix (optional). Default: 0.5
#' @param to Output format ('adjacency', 'edges', 'igraph', or 'bn') (optional).
#' @param cluster A cluster object from package parallel or the number of cores to be used (optional). Default: 4
#' @keywords learning graph
#' @export
#' @examples
#' graph <- run.lingam(df)

run.lingam <- function(df, R=200, m=NULL, threshold=0.5, to='igraph', cluster=4) {

    library(foreach)
    library(doParallel)

    df <- drop.all.zeros(df)

    registerDoParallel(cluster)

    graphs <- foreach(rep=1:R) %dopar% {
        splitted.df <- dataframe.split(df, m=m)
        g <- pcalg::lingam(splitted.df$train)
        g$Bpruned
    }

    stopImplicitCluster()

    g <- averaged.graph(graphs, colnames(df), threshold=threshold, to=to)
    return(g)
}
