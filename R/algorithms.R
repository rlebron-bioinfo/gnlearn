ci.tests <- c('cor', 'mc-cor', 'smc-cor', 'zf', 'mc-zf', 'smc-zf', 'mi-g', 'mc-mi-g', 'smc-mi-g', 'mi-g-sh')
scores <- c('pred-loglik-g', 'loglik-g', 'aic-g', 'bic-g', 'bge')

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
#' @param R Number of bootstrap replicates (optional). Default: 200
#' @param m Size of each bootstrap replicate (optional). Default: nrow(df)/2
#' @param threshold Minimum strength required for a coefficient to be included in the averaged adjacency matrix (optional). Default: 0.5
#' @param to Output format ('adjacency', 'edges', 'igraph', or 'bn') (optional).
#' @param cluster A cluster object from package parallel or the number of cores to be used (optional). Default: 4
#' @keywords learning graph
#' @export
#' @examples
#' graph <- run.aracne(df)

run.aracne <- function(df, whitelist=NULL, blacklist=NULL, R=200, m=NULL, threshold=0.5, to='igraph', cluster=4) {
    mi <- match.arg(mi)

    library(foreach)
    library(doParallel)

    df <- drop.all.zeros(df)

    registerDoParallel(cluster)

    graphs <- foreach(rep=1:R) %dopar% {
        splitted.df <- dataframe.split(df, m=m)
        g <- bnlearn::aracne(splitted.df$train, whitelist=whitelist, blacklist=blacklist, mi='mi-g')
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
#' @param R Number of bootstrap replicates (optional). Default: 200
#' @param m Size of each bootstrap replicate (optional). Default: nrow(df)/2
#' @param threshold Minimum strength required for a coefficient to be included in the averaged adjacency matrix (optional). Default: 0.5
#' @param to Output format ('adjacency', 'edges', 'igraph', or 'bn') (optional).
#' @param cluster A cluster object from package parallel or the number of cores to be used (optional). Default: 4
#' @keywords learning graph
#' @export
#' @examples
#' graph <- run.chowliu(df)

run.chowliu <- function(df, whitelist=NULL, blacklist=NULL, R=200, m=NULL, threshold=0.5, to='igraph', cluster=4) {

    library(foreach)
    library(doParallel)

    df <- drop.all.zeros(df)

    registerDoParallel(cluster)

    graphs <- foreach(rep=1:R) %dopar% {
        splitted.df <- dataframe.split(df, m=m)
        g <- bnlearn::chow.liu(splitted.df$train, whitelist=whitelist, blacklist=blacklist, mi='mi-g')
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

#' Run Hill-Climbing Algorithm (HC)
#'
#' This function allows you to learn a directed graph from a dataset using the Hill-Climbing algorithm.
#' @param df Dataset.
#' @param start Preseeded directed acyclic graph used to initialize the algorithm (optional).
#' @param whitelist A data frame with two columns, containing a set of arcs to be included in the graph (optional).
#' @param blacklist A data frame with two columns, containing a set of arcs not to be included in the graph (optional).
#' @param score Score to be used: 'pred-loglik-g', 'loglik-g', 'aic-g', 'bic-g', or 'bge'. Default: 'pred-loglik-g'
#' @param restart Number of random restarts. Default: 0
#' @param perturb Number of attempts to randomly insert/remove/reverse an arc on every random restart. Default: 1
#' @param max.iter Maximum number of iterations. Default: Inf
#' @param maxp Maximum number of parents for a node. Default: Inf
#' @param R Number of bootstrap replicates (optional). Default: 200
#' @param m Size of each bootstrap replicate (optional). Default: nrow(df)/2
#' @param threshold Minimum strength required for a coefficient to be included in the averaged adjacency matrix (optional). Default: 0.5
#' @param to Output format ('adjacency', 'edges', 'igraph', or 'bn') (optional).
#' @param cluster A cluster object from package parallel or the number of cores to be used (optional). Default: 4
#' @keywords learning graph
#' @export
#' @examples
#' graph <- run.hc(df)

run.hc <- function(df, start=NULL, whitelist=NULL, blacklist=NULL, score=scores, restart=0, perturb=1, max.iter=Inf, maxp=Inf,
                   R=200, m=NULL, threshold=0.5, to='igraph', cluster=4) {
    start <- convert.format(start, to='bn')
    score <- match.arg(score)

    library(foreach)
    library(doParallel)

    df <- drop.all.zeros(df)

    registerDoParallel(cluster)

    graphs <- foreach(rep=1:R) %dopar% {
        splitted.df <- dataframe.split(df, m=m)
        g <- bnlearn::hc(splitted.df$train, newdata=splitted.df$test, start=start, whitelist=whitelist, blacklist=blacklist, score=score,
                         restart=restart, perturb=perturb, max.iter=max.iter, maxp=maxp, optimized=TRUE)
        convert.format(g, to='adjacency')
    }

    stopImplicitCluster()

    g <- averaged.graph(graphs, colnames(df), threshold=threshold, to=to)
    return(g)
}

#' Run Tabu Search Algorithm (TABU)
#'
#' This function allows you to learn a directed graph from a dataset using the Tabu Search algorithm.
#' @param df Dataset.
#' @param start Preseeded directed acyclic graph used to initialize the algorithm (optional).
#' @param whitelist A data frame with two columns, containing a set of arcs to be included in the graph (optional).
#' @param blacklist A data frame with two columns, containing a set of arcs not to be included in the graph (optional).
#' @param score Score to be used: 'pred-loglik-g', 'loglik-g', 'aic-g', 'bic-g', or 'bge'. Default: 'pred-loglik-g'
#' @param tabu Length of the tabu list. Default: 10
#' @param max.tabu Iterations tabu search can perform without improving the best score. Default: tabu (10)
#' @param max.iter Maximum number of iterations. Default: Inf
#' @param maxp Maximum number of parents for a node. Default: Inf
#' @param R Number of bootstrap replicates (optional). Default: 200
#' @param m Size of each bootstrap replicate (optional). Default: nrow(df)/2
#' @param threshold Minimum strength required for a coefficient to be included in the averaged adjacency matrix (optional). Default: 0.5
#' @param to Output format ('adjacency', 'edges', 'igraph', or 'bn') (optional).
#' @param cluster A cluster object from package parallel or the number of cores to be used (optional). Default: 4
#' @keywords learning graph
#' @export
#' @examples
#' graph <- run.tabu(df)

run.tabu <- function(df, start=NULL, whitelist=NULL, blacklist=NULL, score=scores, tabu=10, max.tabu=NULL, max.iter=Inf, maxp=Inf,
                     R=200, m=NULL, threshold=0.5, to='igraph', cluster=4) {
    start <- convert.format(start, to='bn')
    score <- match.arg(score)
    if (is.null(max.tabu)) {
        max.tabu <- tabu
    }

    library(foreach)
    library(doParallel)

    df <- drop.all.zeros(df)

    registerDoParallel(cluster)

    graphs <- foreach(rep=1:R) %dopar% {
        splitted.df <- dataframe.split(df, m=m)
        g <- bnlearn::tabu(splitted.df$train, newdata=splitted.df$test, start=start, whitelist=whitelist, blacklist=blacklist, score=score,
                           tabu=tabu, max.tabu=max.tabu, max.iter=max.iter, maxp=maxp, optimized=TRUE)
        convert.format(g, to='adjacency')
    }

    stopImplicitCluster()

    g <- averaged.graph(graphs, colnames(df), threshold=threshold, to=to)
    return(g)
}

#' Run Grow-Shrink Algorithm (GS)
#'
#' This function allows you to learn a directed graph from a dataset using the Grow-Shrink algorithm.
#' @param df Dataset.
#' @param whitelist A data frame with two columns, containing a set of arcs to be included in the graph (optional).
#' @param blacklist A data frame with two columns, containing a set of arcs not to be included in the graph (optional).
#' @param test Conditional independence test to be used: 'cor', 'mc-cor', 'smc-cor', 'zf', 'mc-zf', 'smc-zf', 'mi-g', 'mc-mi-g', 'smc-mi-g', or 'mi-g-sh'. Default: 'cor'
#' @param alpha Target nominal type I error rate. Default: 0.01
#' @param B Number of permutations considered for each permutation test.
#' @param max.sx Maximum allowed size of the conditioning sets.
#' @param R Number of bootstrap replicates (optional). Default: 200
#' @param m Size of each bootstrap replicate (optional). Default: nrow(df)/2
#' @param threshold Minimum strength required for a coefficient to be included in the averaged adjacency matrix (optional). Default: 0.5
#' @param to Output format ('adjacency', 'edges', 'igraph', or 'bn') (optional).
#' @param cluster A cluster object from package parallel or the number of cores to be used (optional). Default: 4
#' @keywords learning graph
#' @export
#' @examples
#' graph <- run.gs(df)

run.gs <- function(df, whitelist=NULL, blacklist=NULL, test=ci.tests, alpha=0.01, B=NULL, max.sx=NULL,
                   R=200, m=NULL, threshold=0.5, to='igraph', cluster=4) {
    test <- match.arg(test)

    library(foreach)
    library(doParallel)

    df <- drop.all.zeros(df)

    registerDoParallel(cluster)

    graphs <- foreach(rep=1:R) %dopar% {
        splitted.df <- dataframe.split(df, m=m)
        g <- bnlearn::gs(splitted.df$train, whitelist=whitelist, blacklist=blacklist, test=test, alpha=alpha,
                         B=B, max.sx=max.sx, undirected=FALSE)
        convert.format(g, to='adjacency')
    }

    stopImplicitCluster()

    g <- averaged.graph(graphs, colnames(df), threshold=threshold, to=to)
    return(g)
}

#' Run Incremental Association Algorithm (IAMB)
#'
#' This function allows you to learn a directed graph from a dataset using the Incremental Association algorithm.
#' @param df Dataset.
#' @param whitelist A data frame with two columns, containing a set of arcs to be included in the graph (optional).
#' @param blacklist A data frame with two columns, containing a set of arcs not to be included in the graph (optional).
#' @param test Conditional independence test to be used: 'cor', 'mc-cor', 'smc-cor', 'zf', 'mc-zf', 'smc-zf', 'mi-g', 'mc-mi-g', 'smc-mi-g', or 'mi-g-sh'. Default: 'cor'
#' @param alpha Target nominal type I error rate. Default: 0.01
#' @param B Number of permutations considered for each permutation test.
#' @param max.sx Maximum allowed size of the conditioning sets.
#' @param version Algorithm version: 'iamb', 'fast.iamb', 'inter.iamb', or 'iamb.fdr'. Default: 'iamb'
#' @param R Number of bootstrap replicates (optional). Default: 200
#' @param m Size of each bootstrap replicate (optional). Default: nrow(df)/2
#' @param threshold Minimum strength required for a coefficient to be included in the averaged adjacency matrix (optional). Default: 0.5
#' @param to Output format ('adjacency', 'edges', 'igraph', or 'bn') (optional).
#' @param cluster A cluster object from package parallel or the number of cores to be used (optional). Default: 4
#' @keywords learning graph
#' @export
#' @examples
#' graph <- run.iamb(df)

run.iamb <- function(df, whitelist=NULL, blacklist=NULL, test=ci.tests, alpha=0.01, B=NULL, max.sx=NULL, version=c('iamb','fast.iamb','inter.iamb','iamb.fdr'),
                     R=200, m=NULL, threshold=0.5, to='igraph', cluster=4) {
    test <- match.arg(test)
    version <- match.arg(version)

    algorithm <- switch(version,
        iamb = bnlearn::iamb,
        fast.iamb = bnlearn::fast.iamb,
        inter.iamb = bnlearn::inter.iamb,
        iamb.fdr = bnlearn::iamb.fdr
    )

    library(foreach)
    library(doParallel)

    df <- drop.all.zeros(df)

    registerDoParallel(cluster)

    graphs <- foreach(rep=1:R) %dopar% {
        splitted.df <- dataframe.split(df, m=m)
        g <- algorithm(splitted.df$train, whitelist=whitelist, blacklist=blacklist, test=test, alpha=alpha,
                         B=B, max.sx=max.sx, undirected=FALSE)
        convert.format(g, to='adjacency')
    }

    stopImplicitCluster()

    g <- averaged.graph(graphs, colnames(df), threshold=threshold, to=to)
    return(g)
}

#' Run Parents & Children Algorithm
#'
#' This function allows you to learn a directed graph from a dataset using Parents & Children algorithms.
#' @param df Dataset.
#' @param whitelist A data frame with two columns, containing a set of arcs to be included in the graph (optional).
#' @param blacklist A data frame with two columns, containing a set of arcs not to be included in the graph (optional).
#' @param test Conditional independence test to be used: 'cor', 'mc-cor', 'smc-cor', 'zf', 'mc-zf', 'smc-zf', 'mi-g', 'mc-mi-g', 'smc-mi-g', or 'mi-g-sh'. Default: 'cor'
#' @param alpha Target nominal type I error rate. Default: 0.01
#' @param B Number of permutations considered for each permutation test.
#' @param max.sx Maximum allowed size of the conditioning sets.
#' @param version Algorithm version: 'mmpc', 'si.hiton.pc', or 'hpc'. Default: 'mmpc'
#' @param R Number of bootstrap replicates (optional). Default: 200
#' @param m Size of each bootstrap replicate (optional). Default: nrow(df)/2
#' @param threshold Minimum strength required for a coefficient to be included in the averaged adjacency matrix (optional). Default: 0.5
#' @param to Output format ('adjacency', 'edges', 'igraph', or 'bn') (optional).
#' @param cluster A cluster object from package parallel or the number of cores to be used (optional). Default: 4
#' @keywords learning graph
#' @export
#' @examples
#' graph <- run.parents.children(df)

run.parents.children <- function(df, whitelist=NULL, blacklist=NULL, test=ci.tests, alpha=0.01, B=NULL, max.sx=NULL, version=c('mmpc','si.hiton.pc','hpc'),
                                 R=200, m=NULL, threshold=0.5, to='igraph', cluster=4) {
    test <- match.arg(test)
    version <- match.arg(version)

    algorithm <- switch(version,
        mmpc = bnlearn::mmpc,
        si.hiton.pc = bnlearn::si.hiton.pc,
        hpc = bnlearn::hpc
    )

    library(foreach)
    library(doParallel)

    df <- drop.all.zeros(df)

    registerDoParallel(cluster)

    graphs <- foreach(rep=1:R) %dopar% {
        splitted.df <- dataframe.split(df, m=m)
        g <- algorithm(splitted.df$train, whitelist=whitelist, blacklist=blacklist, test=test, alpha=alpha,
                         B=B, max.sx=max.sx, undirected=FALSE)
        convert.format(g, to='adjacency')
    }

    stopImplicitCluster()

    g <- averaged.graph(graphs, colnames(df), threshold=threshold, to=to)
    return(g)
}

#' Run General 2-Phase Restricted Maximization Algorithm (rsmax2)
#'
#' This function allows you to learn a directed graph from a dataset using the General 2-Phase Restricted Maximization algorithm.
#' @param df Dataset.
#' @param whitelist A data frame with two columns, containing a set of arcs to be included in the graph (optional).
#' @param blacklist A data frame with two columns, containing a set of arcs not to be included in the graph (optional).
#' @param restrict Constraint-based or local search algorithm to be used in the restrict phase: 'pc.stable', 'gs', 'iamb', 'fast.iamb', 'inter.iamb', 'iamb.fdr', 'mmpc', 'si.hiton.pc', or 'hpc'. Default: 'pc.stable'
#' @param maximize Score-based algorithm to be used in the maximize phase: 'hc' or 'tabu'. Default: 'hc'
#' @param restrict.args List of arguments to be passed to the algorithm specified by restrict.
#' @param maximize.args List of arguments to be passed to the algorithm specified by maximize.
#' @param version Algorithm version: 'rsmax2','mmhc', or 'h2pc'. Default: 'rsmax2'
#' @param R Number of bootstrap replicates (optional). Default: 200
#' @param m Size of each bootstrap replicate (optional). Default: nrow(df)/2
#' @param threshold Minimum strength required for a coefficient to be included in the averaged adjacency matrix (optional). Default: 0.5
#' @param to Output format ('adjacency', 'edges', 'igraph', or 'bn') (optional).
#' @param cluster A cluster object from package parallel or the number of cores to be used (optional). Default: 4
#' @keywords learning graph
#' @export
#' @examples
#' graph <- run.rsmax2(df)

run.rsmax2 <- function(df, whitelist=NULL, blacklist=NULL, restrict=c('pc.stable','gs','iamb','fast.iamb','inter.iamb','iamb.fdr','mmpc','si.hiton.pc','hpc'),
                       maximize=c('hc','tabu'), restrict.args=list(), maximize.args=list(), version=c('rsmax2','mmhc','h2pc'),
                       R=200, m=NULL, threshold=0.5, to='igraph', cluster=4) {
    restrict <- match.arg(restrict)
    maximize <- match.arg(maximize)
    version <- match.arg(version)

    restrict <- switch(version,
        rsmax2 = restrict,
        mmhc = 'mmpc',
        h2pc = 'hpc'
    )

    maximize <- switch(version,
        rsmax2 = maximize,
        mmhc = 'hc',
        h2pc = 'hc'
    )

    library(foreach)
    library(doParallel)

    df <- drop.all.zeros(df)

    registerDoParallel(cluster)

    graphs <- foreach(rep=1:R) %dopar% {
        splitted.df <- dataframe.split(df, m=m)
        g <- bnlearn::rsmax2(splitted.df$train, whitelist=whitelist, blacklist=blacklist, restrict=restrict, maximize=maximize,
                             restrict.args=restrict.args, maximize.args=maximize.args)
        convert.format(g, to='adjacency')
    }

    stopImplicitCluster()

    g <- averaged.graph(graphs, colnames(df), threshold=threshold, to=to)
    return(g)
}

#' Run Peter & Clark Algorithm (PC)
#'
#' This function allows you to learn a directed graph from a dataset using the Peter & Clark algorithm (stable version).
#' @param df Dataset.
#' @param whitelist A data frame with two columns, containing a set of arcs to be included in the graph (optional).
#' @param blacklist A data frame with two columns, containing a set of arcs not to be included in the graph (optional).
#' @param alpha Target nominal type I error rate. Default: 0.01
#' @param max.sx Maximum allowed size of the conditioning sets.
#' @param R Number of bootstrap replicates (optional). Default: 200
#' @param m Size of each bootstrap replicate (optional). Default: nrow(df)/2
#' @param threshold Minimum strength required for a coefficient to be included in the averaged adjacency matrix (optional). Default: 0.5
#' @param to Output format ('adjacency', 'edges', 'igraph', or 'bn') (optional).
#' @param cluster A cluster object from package parallel or the number of cores to be used (optional). Default: 4
#' @param implementation Peter & Clark algorithm implementation: 'pcalg' or 'bnlearn'. Default: 'pcalg'
#' @param pcalg.indep.test Conditional independence test to be used (pcalg implementation). Default: pcalg::gaussCItest
#' @param pcalg.u2pd Method for dealing with conflicting information when trying to orient edges (pcalg implementation). Default: 'relaxed'
#' @param pcalg.conservative Whether or not the conservative PC is used (pcalg implementation). Default: FALSE
#' @param pcalg.maj.rule Whether or not the triples shall be checked for ambiguity using a majority rule idea, which is less strict than the conservative PC algorithm (pcalg implementation). Default: FALSE
#' @param pcalg.solve.confl If TRUE, the orientation of the v-structures and the orientation rules work with lists for candidate sets and allow bi-directed edges to resolve conflicting edge orientations (pcalg implementation). Default: FALSE
#' @param bnlearn.test Conditional independence test to be used (bnlearn implementation): 'cor', 'mc-cor', 'smc-cor', 'zf', 'mc-zf', 'smc-zf', 'mi-g', 'mc-mi-g', 'smc-mi-g', or 'mi-g-sh'. Default: 'cor'
#' @param bnlearn.B Number of permutations considered for each permutation test (bnlearn implementation).
#' @keywords learning graph
#' @export
#' @examples
#' graph <- run.pc(df, implementation='pcalg')
#' graph <- run.pc(df, implementation='bnlearn')

run.pc <- function(df, whitelist=NULL, blacklist=NULL, alpha=0.01, max.sx=Inf, R=200, m=NULL, threshold=0.5,
                   to='igraph', cluster=4, implementation=c('pcalg','bnlearn'),
                   pcalg.indep.test=pcalg::gaussCItest, pcalg.u2pd=c('relaxed','rand','retry'),
                   pcalg.conservative=FALSE, pcalg.maj.rule=FALSE, pcalg.solve.confl=FALSE,
                   bnlearn.test=ci.tests, bnlearn.B=NULL) {
    implementation <- match.arg(implementation)
    pcalg.u2pd <- match.arg(pcalg.u2pd)
    bnlearn.test <- match.arg(bnlearn.test)

    if (pcalg.conservative | pcalg.solve.confl) {
        pcalg.u2pd <- 'relaxed'
    }

    if (!is.null(whitelist) & implementation=='pcalg') {
        whitelist <- convert.format(whitelist, 'adjacency')
        whitelist <- whitelist > 0
    }

    if (!is.null(blacklist) & implementation=='pcalg') {
        blacklist <- convert.format(blacklist, 'adjacency')
        blacklist <- blacklist > 0
    }

    library(foreach)
    library(doParallel)

    df <- drop.all.zeros(df)

    registerDoParallel(cluster)

    graphs <- foreach(rep=1:R) %dopar% {
        splitted.df <- dataframe.split(df, m=m)
        if (implementation=='pcalg') {
            suffStat <- list(C=cor(splitted.df$train), n=nrow(splitted.df$train))
            varNames <- colnames(splitted.df$train)
            g <- pcalg::pc(suffStat, indepTest=pcalg.indep.test, labels=varNames, alpha=alpha, m.max=max.sx,
                           u2pd=pcalg.u2pd, conservative=pcalg.conservative, maj.rule=pcalg.maj.rule, solve.confl=pcalg.solve.confl,
                           fixedEdges=whitelist, fixedGaps=blacklist, skel.method='stable', NAdelete=TRUE)
            g <- as(g@graph, 'matrix')
        } else if (implementation=='bnlearn') {
            g <- bnlearn::pc.stable(splitted.df$train, whitelist=whitelist, blacklist=blacklist, test=bnlearn.test, alpha=alpha,
                                    B=bnlearn.B, max.sx=max.sx, undirected=FALSE)
            convert.format(g, to='adjacency')
        }
    }

    stopImplicitCluster()

    g <- averaged.graph(graphs, colnames(df), threshold=threshold, to=to)
    return(g)
}

#' Run Peter & Clark Skeleton Algorithm
#'
#' This function allows you to learn a directed graph from a dataset using the Peter & Clark skeleton algorithm (stable version).
#' @param df Dataset.
#' @param whitelist A data frame with two columns, containing a set of arcs to be included in the graph (optional).
#' @param blacklist A data frame with two columns, containing a set of arcs not to be included in the graph (optional).
#' @param alpha Target nominal type I error rate. Default: 0.01
#' @param max.sx Maximum allowed size of the conditioning sets.
#' @param R Number of bootstrap replicates (optional). Default: 200
#' @param m Size of each bootstrap replicate (optional). Default: nrow(df)/2
#' @param threshold Minimum strength required for a coefficient to be included in the averaged adjacency matrix (optional). Default: 0.5
#' @param to Output format ('adjacency', 'edges', 'igraph', or 'bn') (optional).
#' @param cluster A cluster object from package parallel or the number of cores to be used (optional). Default: 4
#' @param implementation Peter & Clark algorithm implementation: 'pcalg' or 'bnlearn'. Default: 'pcalg'
#' @param pcalg.indep.test Conditional independence test to be used (pcalg implementation). Default: pcalg::gaussCItest
#' @param bnlearn.test Conditional independence test to be used (bnlearn implementation): 'cor', 'mc-cor', 'smc-cor', 'zf', 'mc-zf', 'smc-zf', 'mi-g', 'mc-mi-g', 'smc-mi-g', or 'mi-g-sh'. Default: 'cor'
#' @param bnlearn.B Number of permutations considered for each permutation test (bnlearn implementation).
#' @keywords learning graph
#' @export
#' @examples
#' graph <- run.skeleton(df, implementation='pcalg')
#' graph <- run.skeleton(df, implementation='bnlearn')

run.skeleton <- function(df, whitelist=NULL, blacklist=NULL, alpha=0.01, max.sx=Inf, R=200, m=NULL, threshold=0.5,
                   to='igraph', cluster=4, implementation=c('pcalg','bnlearn'),
                   pcalg.indep.test=pcalg::gaussCItest, pcalg.u2pd=c('relaxed','rand','retry'),
                   pcalg.conservative=FALSE, pcalg.maj.rule=FALSE, pcalg.solve.confl=FALSE,
                   bnlearn.test=ci.tests, bnlearn.B=NULL) {
    implementation <- match.arg(implementation)
    pcalg.u2pd <- match.arg(pcalg.u2pd)
    bnlearn.test <- match.arg(bnlearn.test)

    if (pcalg.conservative | pcalg.solve.confl) {
        pcalg.u2pd <- 'relaxed'
    }

    if (!is.null(whitelist) & implementation=='pcalg') {
        whitelist <- convert.format(whitelist, 'adjacency')
        whitelist <- whitelist > 0
    }

    if (!is.null(blacklist) & implementation=='pcalg') {
        blacklist <- convert.format(blacklist, 'adjacency')
        blacklist <- blacklist > 0
    }

    library(foreach)
    library(doParallel)

    df <- drop.all.zeros(df)

    registerDoParallel(cluster)

    graphs <- foreach(rep=1:R) %dopar% {
        splitted.df <- dataframe.split(df, m=m)
        if (implementation=='pcalg') {
            suffStat <- list(C=cor(splitted.df$train), n=nrow(splitted.df$train))
            varNames <- colnames(splitted.df$train)
            g <- pcalg::skeleton(suffStat, indepTest=pcalg.indep.test, labels=varNames, alpha=alpha, m.max=max.sx,
                                 fixedEdges=whitelist, fixedGaps=blacklist, method='stable', NAdelete=TRUE)
            g <- as(g@graph, 'matrix')
        } else if (implementation=='bnlearn') {
            g <- bnlearn::pc.stable(splitted.df$train, whitelist=whitelist, blacklist=blacklist, test=bnlearn.test, alpha=alpha,
                             B=bnlearn.B, max.sx=max.sx, undirected=TRUE)
            convert.format(g, to='adjacency')
        }
    }

    stopImplicitCluster()

    g <- averaged.graph(graphs, colnames(df), threshold=threshold, to=to)
    return(g)
}

#' Run Fast Causal Inference Algorithm (FCI)
#'
#' This function allows you to learn a directed graph from a dataset using the Fast Causal Inference algorithm (stable version).
#' @param df Dataset.
#' @param whitelist A data frame with two columns, containing a set of arcs to be included in the graph (optional).
#' @param blacklist A data frame with two columns, containing a set of arcs not to be included in the graph (optional).
#' @param indep.test Conditional independence test to be used (pcalg implementation). Default: pcalg::gaussCItest
#' @param alpha Target nominal type I error rate. Default: 0.01
#' @param max.sx Maximum allowed size of the conditioning sets.
#' @param pdsep.max Maximum size of Possible-D-SEP for which subsets are considered as conditioning sets in the conditional independence tests.
#' @param conservative Whether or not the conservative PC is used (pcalg implementation). Default: FALSE
#' @param maj.rule Whether or not the triples shall be checked for ambiguity using a majority rule idea, which is less strict than the conservative PC algorithm (pcalg implementation). Default: FALSE
#' @param version Version of FCI algorithm to be used: 'fci', 'rfci', or 'fci.plus'. Default: 'fci'
#' @param type Type of FCI algorithm to be used: 'normal', 'anytime', or 'adaptive'. Default: 'normal'
#' @param rules Logical vector of length 10 indicating which rules should be used when directing edges. Default: rep(TRUE,10)
#' @param doPdsep If FALSE, Possible-D-SEP is not computed, so that the algorithm simplifies to the Modified PC algorithm of Spirtes, Glymour and Scheines (2000, p.84). Default: TRUE
#' @param biCC If TRUE, only nodes on paths between nodes x and y are considered to be in Possible-D-SEP(x) when testing independence between x and y. Default: TRUE
#' @param R Number of bootstrap replicates (optional). Default: 200
#' @param m Size of each bootstrap replicate (optional). Default: nrow(df)/2
#' @param threshold Minimum strength required for a coefficient to be included in the averaged adjacency matrix (optional). Default: 0.5
#' @param to Output format ('adjacency', 'edges', 'igraph', or 'bn') (optional).
#' @param cluster A cluster object from package parallel or the number of cores to be used (optional). Default: 4
#' @keywords learning graph
#' @export
#' @examples
#' graph <- run.fci(df)

run.fci <- function(df, whitelist=NULL, blacklist=NULL, indep.test=pcalg::gaussCItest, alpha=0.01, max.sx=Inf, pdsep.max=Inf,
                    conservative=FALSE, maj.rule=FALSE, version=c('fci','rfci','fci.plus'), type=c('normal','anytime','adaptive'),
                    rules=rep(TRUE,10), doPdsep=TRUE, biCC=FALSE,
                    R=200, m=NULL, threshold=0.5, to='igraph', cluster=4) {
    version <- match.arg(version)
    type <- match.arg(type)

    if (!is.null(whitelist)) {
        whitelist <- convert.format(whitelist, 'adjacency')
        whitelist <- whitelist > 0
    }

    if (!is.null(blacklist)) {
        blacklist <- convert.format(blacklist, 'adjacency')
        blacklist <- blacklist > 0
    }

    library(foreach)
    library(doParallel)

    df <- drop.all.zeros(df)

    registerDoParallel(cluster)

    graphs <- foreach(rep=1:R) %dopar% {
        splitted.df <- dataframe.split(df, m=m)
        suffStat <- list(C=cor(splitted.df$train), n=nrow(splitted.df$train))
        varNames <- colnames(splitted.df$train)
        g <- switch(version,
            fci = { pcalg::fci(suffStat, indepTest=indep.test, labels=varNames, alpha=alpha, m.max=max.sx, pdsep.max=pdsep.max,
                               conservative=conservative, maj.rule=maj.rule, type=type, rules=rules, doPdsep=doPdsep, biCC=biCC,
                               fixedEdges=whitelist, fixedGaps=blacklist, skel.method='stable', NAdelete=TRUE) },
            rfci = { pcalg::rfci(suffStat, indepTest=indep.test, labels=varNames, alpha=alpha, m.max=max.sx,
                                 conservative=conservative, maj.rule=maj.rule, rules=rules,
                                 fixedEdges=whitelist, fixedGaps=blacklist, skel.method='stable', NAdelete=TRUE) },
            fci.plus = { pcalg::fciPlus(suffStat, indepTest=indep.test, labels=varNames, alpha=alpha) }
        )
        g <- as(g, 'matrix')
    }

    stopImplicitCluster()

    g <- averaged.graph(graphs, colnames(df), threshold=threshold, to=to)
    return(g)
}

#' Run Greedy Equivalence Search Algorithm (GES)
#'
#' This function allows you to learn a directed graph from a dataset using the Greedy Equivalence Search (GES) algorithm of Chickering (2002).
#' @param df Dataset.
#' @param blacklist A data frame with two columns, containing a set of arcs not to be included in the graph (optional).
#' @param adaptive Whether constraints should be adapted to newly detected v-structures or unshielded triples: 'none', 'vstructures', or 'triples'. Default: 'none'
#' @param maxDegree Parameter used to limit the vertex degree of the estimated graph. Default: integer(0)
#' @param R Number of bootstrap replicates (optional). Default: 200
#' @param m Size of each bootstrap replicate (optional). Default: nrow(df)/2
#' @param threshold Minimum strength required for a coefficient to be included in the averaged adjacency matrix (optional). Default: 0.5
#' @param to Output format ('adjacency', 'edges', 'igraph', or 'bn') (optional).
#' @param cluster A cluster object from package parallel or the number of cores to be used (optional). Default: 4
#' @keywords learning graph
#' @export
#' @examples
#' graph <- run.ges(df)

run.ges <- function(df, blacklist=NULL, adaptive=c('none','vstructures','triples'), maxDegree=integer(0),
                    R=200, m=NULL, threshold=0.5, to='igraph', cluster=4) {

    adaptive <- match.arg(adaptive)

    if (!is.null(blacklist)) {
        blacklist <- convert.format(blacklist, 'adjacency')
        blacklist <- blacklist > 0
    }

    library(foreach)
    library(doParallel)

    df <- drop.all.zeros(df)

    registerDoParallel(cluster)

    graphs <- foreach(rep=1:R) %dopar% {
        splitted.df <- dataframe.split(df, m=m)
        score <- new('GaussL0penObsScore', data=splitted.df$train)
        g <- pcalg::ges(score, labels=score$getNodes(), fixedGaps=blacklist, maxDegree=maxDegree,
                        adaptive=adaptive, phase=c('forward','backward'), iterate=TRUE)
        g <- g$repr$weight.mat()
    }

    stopImplicitCluster()

    g <- averaged.graph(graphs, colnames(df), threshold=threshold, to=to)
    return(g)
}

#' Run Adaptively Restricted Greedy Equivalence Search Algorithm (ARGES)
#'
#' This function allows you to learn a directed graph from a dataset using the Adaptively Restricted Greedy Equivalence Search (ARGES) algorithm.
#' @param df Dataset.
#' @param whitelist A data frame with two columns, containing a set of arcs to be included in the graph (optional).
#' @param blacklist A data frame with two columns, containing a set of arcs not to be included in the graph (optional).
#' @param indep.test Conditional independence test to be used (pcalg implementation). Default: pcalg::gaussCItest
#' @param alpha Target nominal type I error rate. Default: 0.01
#' @param max.sx Maximum allowed size of the conditioning sets.
#' @param adaptive Whether constraints should be adapted to newly detected v-structures or unshielded triples: 'none', 'vstructures', or 'triples'. Default: 'none'
#' @param maxDegree Parameter used to limit the vertex degree of the estimated graph. Default: integer(0)
#' @param R Number of bootstrap replicates (optional). Default: 200
#' @param m Size of each bootstrap replicate (optional). Default: nrow(df)/2
#' @param threshold Minimum strength required for a coefficient to be included in the averaged adjacency matrix (optional). Default: 0.5
#' @param to Output format ('adjacency', 'edges', 'igraph', or 'bn') (optional).
#' @param cluster A cluster object from package parallel or the number of cores to be used (optional). Default: 4
#' @keywords learning graph
#' @export
#' @examples
#' graph <- run.arges(df)

run.arges <- function(df, whitelist=NULL, blacklist=NULL, indep.test=pcalg::gaussCItest, alpha=0.01, max.sx=Inf,
                      adaptive=c('none','vstructures','triples'), maxDegree=integer(0),
                      R=200, m=NULL, threshold=0.5, to='igraph', cluster=4) {

    adaptive <- match.arg(adaptive)

    if (!is.null(whitelist)) {
        whitelist <- convert.format(whitelist, 'adjacency')
        whitelist <- whitelist > 0
    }

    if (!is.null(blacklist)) {
        blacklist <- convert.format(blacklist, 'adjacency')
        blacklist <- blacklist > 0
    }

    library(foreach)
    library(doParallel)

    df <- drop.all.zeros(df)

    registerDoParallel(cluster)

    graphs <- foreach(rep=1:R) %dopar% {
        splitted.df <- dataframe.split(df, m=m)
        suffStat <- list(C=cor(splitted.df$train), n=nrow(splitted.df$train))
        varNames <- colnames(splitted.df$train)
        skel <- pcalg::skeleton(suffStat, indepTest=indep.test, labels=varNames, alpha=alpha, m.max=max.sx,
                                fixedEdges=whitelist, fixedGaps=blacklist, method='stable', NAdelete=TRUE)
        skel <- as(skel@graph, 'matrix')
        score <- new('GaussL0penObsScore', data=splitted.df$train)
        g <- pcalg::ges(score, labels=score$getNodes(), fixedGaps=!skel, maxDegree=maxDegree,
                        adaptive=adaptive, phase=c('forward','backward'), iterate=TRUE)
        g <- g$repr$weight.mat()
    }

    stopImplicitCluster()

    g <- averaged.graph(graphs, colnames(df), threshold=threshold, to=to)
    return(g)
}

# NO-TEARS Linear Algorithm (REIMPLEMENTED)

notears <- function(df, lambda1, loss.type=c('l2','logistic','poisson'),
                        max.iter=100, h.tol=1e-8, rho.max=1e+16, w.threshold=0.3) {
    loss.type <- match.arg(loss.type)
    X <- df <- as.matrix(df)

    loss.func <- function(W) {
        M <- X %*% W
        if (loss.type=='l2') {
            R <- X - M
            loss <- 0.5 / dim(X)[1] * sum(R ** 2)
        } else if (loss.type=='logistic') {
            loss <- 1.0 / dim(X)[1] * sum(log(sum(exp(M)+1)) - X * M)
        } else if (loss.type=='poisson') {
            S <- exp(M)
            loss <- 1.0 / dim(X)[1] * sum(S - X * M)
        }
        return(loss)
    }

    G.loss.func <- function(W) {
        M <- X %*% W
        if (loss.type=='l2') {
            R <- X - M
            G.loss <- -1.0 / dim(X)[1] * t(X) %*% R
        } else if (loss.type=='logistic') {
            G.loss <- 1.0 / dim(X)[1] * t(X) %*% (1.0 / (1 + exp(-1 * M)) - X)
        } else if (loss.type=='poisson') {
            S <- exp(M)
            G.loss <- 1.0 / dim(X)[1] * t(X) %*% (S - X)
        }
        return(G.loss)
    }

    h.func <- function(W) {
        M <- diag(1, d, d) + W * W / d
        E <- matrixcalc::matrix.power(M, d-1)
        h.new <- sum(t(E) * M) - d
        return(h.new)
    }

    G.h.func <- function(W) {
        M <- diag(1, d, d) + W * W / d
        E <- matrixcalc::matrix.power(M, d-1)
        G.h <- t(E) * W * 2
        return(G.h)
    }

    adj <- function(w) {
        w <- as.matrix(w)
        w.pos <- w[1:(length(w)/2),]
        w.neg <- w[((length(w)/2)+1):(length(w)),]
        dim(w.pos) <- c(d,d)
        dim(w.neg) <- c(d,d)
        W <- w.pos - w.neg
        return(W)
    }

    fn <- function(w) {
        W <- adj(w)
        loss <- loss.func(W)
        h.new <- h.func(W)
        obj <- loss + 0.5 * rho * h.new * h.new + alpha * h.new + lambda1 * sum(w)
        return(obj)
    }

    gr <- function(w) {
        W <- adj(w)
        G.loss <- G.loss.func(W)
        G.h <- G.h.func(W)
        G.smooth <- G.loss + (rho * h + alpha) * G.h
        G.obj <- c(array(G.smooth + lambda1), array(-1 * G.smooth + lambda1))
        return(G.obj)
    }

    n <- nrow(df)
    d <- ncol(df)
    w.est <- replicate(2*d*d, 0)
    rho <- 1
    alpha <- 0
    h <- Inf

    for (i in 1:max.iter) {
        w.new <- NULL
        h.new <- NULL
        while (rho < rho.max) {
            #w.new <- optim(w.est, fn, gr=gr, method='L-BFGS-B', lower=0, upper=Inf)$par
            w.new <- optim(w.est, fn, method='L-BFGS-B', lower=0, upper=Inf)$par
            h.new <- h.func(adj(w.new))
            if (h.new > (0.25*h)) {
                message(h.new)
                message(h)
                rho = rho*10
            } else {
                break
            }
        }
        w.est <- w.new
        h <- h.new
        alpha <- alpha + (rho*h)
        if (h <= h.tol | rho >= rho.max) {
            break
        }
    }
    W.est <- adj(w.est)
    W.est[abs(W.est) < w.threshold] <- 0
    return(W.est)
}
