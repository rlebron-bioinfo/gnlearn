#' Run GLASSO Algorithm
#'
#' This function allows you to learn an undirected graph from a dataset using the GLASSO algorithm.
#' @param df Dataset.
#' @param rho Non-negative regularization parameter for GLASSO.
#' @param to Output format ('adjacency', 'edges', 'igraph', or 'bn') (optional).
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
        j <- apply(A, 1, function(x) !all(x==0)) | apply(A, 2, function(x) !all(x==0))
        A <- A[j,j]
    }
    g <- convert_format(A, to=to)
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

    df <- df[apply(df, 1, function(x) !all(x==0)),]
    df <- df[,apply(df, 2, function(x) !all(x==0))]
    df <- as.matrix(df)
    n.genes <- ncol(df)
    if (is.null(m)) {
        m <- nrow(df)/2
    }

    registerDoParallel(cluster)

    results <- foreach(rep=1:R) %dopar% {
        repeat {
            ixs <- sample(1:nrow(df), size=m, replace=FALSE)
            S.train <- cov(df[ixs,])
            S.test <-  cov(df[-ixs,])
            j <- apply(S.train, 1, function(x) !all(x==0)) | apply(S.train, 2, function(x) !all(x==0))
            S.train <- S.train[j,j]
            j <- apply(S.test, 1, function(x) !all(x==0)) | apply(S.test, 2, function(x) !all(x==0))
            S.test <- S.test[j,j]
            if (dim(S.train)[2]==n.genes & dim(S.test)[2]==n.genes) {
                break
            }
        }

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
        results.path[[bidx]]$B

    }

    stopImplicitCluster()

    B <- array(data=NA, dim=c(n.genes, n.genes, R))
    for (i in 1:R) {
        B[, , i] <- results[[i]]
    }
    B <- apply(sign(abs(B)), c(1,2), mean)
    A <- sign(abs(t(B > threshold)))
    if (!loops) {
        diag(A) <- 0
    }
    if (!unconnected.nodes) {
        j <- apply(A, 1, function(x) !all(x==0)) | apply(A, 2, function(x) !all(x==0))
        A <- A[j,j]
    }
    rownames(A) <- colnames(df)
    colnames(A) <- colnames(df)
    g <- convert_format(A, to=to)
    return(g)
}

mll <- function(P, S) {
    -determinant(P, logarithm=TRUE)$modulus + sum(S*P)
}
