ci.tests <- c('cor', 'mc-cor', 'smc-cor', 'zf', 'mc-zf', 'smc-zf', 'mi-g', 'mc-mi-g', 'smc-mi-g', 'mi-g-sh')
scores <- c('pred-loglik-g', 'loglik-g', 'aic-g', 'bic-g', 'bge')

# Structure Learning Algorithms

## Constraint-Based

#' Peter & Clark Skeleton Algorithm With Bootstrapping
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
#' @param to Output format ('adjacency', 'edges', 'igraph', or 'bnlearn') (optional).
#' @param cluster A cluster object from package parallel or the number of cores to be used (optional). Default: 4
#' @param implementation Peter & Clark algorithm implementation: 'pcalg' or 'bnlearn'. Default: 'pcalg'
#' @param pcalg.indep.test Conditional independence test to be used (pcalg implementation). Default: pcalg::gaussCItest
#' @param bnlearn.test Conditional independence test to be used (bnlearn implementation): 'cor', 'mc-cor', 'smc-cor', 'zf', 'mc-zf', 'smc-zf', 'mi-g', 'mc-mi-g', 'smc-mi-g', or 'mi-g-sh'. Default: 'cor'
#' @param bnlearn.B Number of permutations considered for each permutation test (bnlearn implementation).
#' @keywords learning graph
#' @export
#' @examples
#' graph <- boot.skeleton(df, implementation='pcalg')
#' graph <- boot.skeleton(df, implementation='bnlearn')

boot.skeleton <- function(df, whitelist=NULL, blacklist=NULL, alpha=0.01, max.sx=Inf, R=200, m=NULL, threshold=0.5,
                   to=c('igraph', 'adjacency', 'edges', 'graph', 'bnlearn'),
                   cluster=4, implementation=c('pcalg','bnlearn'),
                   pcalg.indep.test=pcalg::gaussCItest, pcalg.u2pd=c('relaxed','rand','retry'),
                   pcalg.conservative=FALSE, pcalg.maj.rule=FALSE, pcalg.solve.confl=FALSE,
                   bnlearn.test=ci.tests, bnlearn.B=NULL) {
    to <- match.arg(to)
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

    graphs <- rename.graphs(graphs, colnames(df), to='adjacency')
    g <- averaged.graph(graphs, threshold=threshold, to=to)
    return(g)
}

#' Peter & Clark Algorithm (PC) With Bootstrapping
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
#' @param to Output format ('adjacency', 'edges', 'igraph', or 'bnlearn') (optional).
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
#' graph <- boot.pc(df, implementation='pcalg')
#' graph <- boot.pc(df, implementation='bnlearn')

boot.pc <- function(df, whitelist=NULL, blacklist=NULL, alpha=0.01, max.sx=Inf, R=200, m=NULL, threshold=0.5,
                   to=c('igraph', 'adjacency', 'edges', 'graph', 'bnlearn'),
                   cluster=4, implementation=c('pcalg','bnlearn'),
                   pcalg.indep.test=pcalg::gaussCItest, pcalg.u2pd=c('relaxed','rand','retry'),
                   pcalg.conservative=FALSE, pcalg.maj.rule=FALSE, pcalg.solve.confl=FALSE,
                   bnlearn.test=ci.tests, bnlearn.B=NULL) {
    to <- match.arg(to)
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

    if (!is.null(whitelist) & implementation=='bnlearn') {
        whitelist <- convert.format(whitelist, 'edges')
    }

    if (!is.null(blacklist) & implementation=='bnlearn') {
        blacklist <- convert.format(blacklist, 'edges')
    }

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

    graphs <- rename.graphs(graphs, colnames(df), to='adjacency')
    g <- averaged.graph(graphs, threshold=threshold, to=to)
    return(g)
}

#' Fast Causal Inference Algorithm (FCI) With Bootstrapping
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
#' @param to Output format ('adjacency', 'edges', 'igraph', or 'bnlearn') (optional).
#' @param cluster A cluster object from package parallel or the number of cores to be used (optional). Default: 4
#' @keywords learning graph
#' @export
#' @examples
#' graph <- boot.fci(df)

boot.fci <- function(df, whitelist=NULL, blacklist=NULL, indep.test=pcalg::gaussCItest, alpha=0.01, max.sx=Inf, pdsep.max=Inf,
                    conservative=FALSE, maj.rule=FALSE, version=c('fci','rfci','fci.plus'), type=c('normal','anytime','adaptive'),
                    rules=rep(TRUE,10), doPdsep=TRUE, biCC=FALSE,
                    R=200, m=NULL, threshold=0.5, to=c('igraph', 'adjacency', 'edges', 'graph', 'bnlearn'), cluster=4) {
    version <- match.arg(version)
    type <- match.arg(type)
    to <- match.arg(to)

    if (!is.null(whitelist)) {
        whitelist <- convert.format(whitelist, 'adjacency')
        whitelist <- whitelist > 0
    }

    if (!is.null(blacklist)) {
        blacklist <- convert.format(blacklist, 'adjacency')
        blacklist <- blacklist > 0
    }

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

    graphs <- rename.graphs(graphs, colnames(df), to='adjacency')
    g <- averaged.graph(graphs, threshold=threshold, to=to)
    return(g)
}

#' Grow-Shrink Algorithm (GS) With Bootstrapping
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
#' @param to Output format ('adjacency', 'edges', 'igraph', or 'bnlearn') (optional).
#' @param cluster A cluster object from package parallel or the number of cores to be used (optional). Default: 4
#' @keywords learning graph
#' @export
#' @examples
#' graph <- boot.gs(df)

boot.gs <- function(df, whitelist=NULL, blacklist=NULL, test=ci.tests, alpha=0.01, B=NULL, max.sx=NULL,
                   R=200, m=NULL, threshold=0.5, to=c('igraph', 'adjacency', 'edges', 'graph', 'bnlearn'), cluster=4) {
    test <- match.arg(test)
    to <- match.arg(to)

    if (!is.null(whitelist)) {
        whitelist <- convert.format(whitelist, 'edges')
    }

    if (!is.null(blacklist)) {
        blacklist <- convert.format(blacklist, 'edges')
    }

    df <- drop.all.zeros(df)

    registerDoParallel(cluster)

    graphs <- foreach(rep=1:R) %dopar% {
        splitted.df <- dataframe.split(df, m=m)
        g <- bnlearn::gs(splitted.df$train, whitelist=whitelist, blacklist=blacklist, test=test, alpha=alpha,
                         B=B, max.sx=max.sx, undirected=FALSE)
        convert.format(g, to='adjacency')
    }

    stopImplicitCluster()

    graphs <- rename.graphs(graphs, colnames(df), to='adjacency')
    g <- averaged.graph(graphs, threshold=threshold, to=to)
    return(g)
}

#' Incremental Association Algorithm (IAMB) With Bootstrapping
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
#' @param to Output format ('adjacency', 'edges', 'igraph', or 'bnlearn') (optional).
#' @param cluster A cluster object from package parallel or the number of cores to be used (optional). Default: 4
#' @keywords learning graph
#' @export
#' @examples
#' graph <- boot.iamb(df)

boot.iamb <- function(df, whitelist=NULL, blacklist=NULL, test=ci.tests, alpha=0.01, B=NULL, max.sx=NULL, version=c('iamb','fast.iamb','inter.iamb','iamb.fdr'),
                     R=200, m=NULL, threshold=0.5, to=c('igraph', 'adjacency', 'edges', 'graph', 'bnlearn'), cluster=4) {
    test <- match.arg(test)
    version <- match.arg(version)
    to <- match.arg(to)

    algorithm <- switch(version,
        iamb = bnlearn::iamb,
        fast.iamb = bnlearn::fast.iamb,
        inter.iamb = bnlearn::inter.iamb,
        iamb.fdr = bnlearn::iamb.fdr
    )

    if (!is.null(whitelist)) {
        whitelist <- convert.format(whitelist, 'edges')
    }

    if (!is.null(blacklist)) {
        blacklist <- convert.format(blacklist, 'edges')
    }

    df <- drop.all.zeros(df)

    registerDoParallel(cluster)

    graphs <- foreach(rep=1:R) %dopar% {
        splitted.df <- dataframe.split(df, m=m)
        g <- algorithm(splitted.df$train, whitelist=whitelist, blacklist=blacklist, test=test, alpha=alpha,
                         B=B, max.sx=max.sx, undirected=FALSE)
        convert.format(g, to='adjacency')
    }

    stopImplicitCluster()

    graphs <- rename.graphs(graphs, colnames(df), to='adjacency')
    g <- averaged.graph(graphs, threshold=threshold, to=to)
    return(g)
}

#' Parents & Children Algorithm With Bootstrapping
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
#' @param to Output format ('adjacency', 'edges', 'igraph', or 'bnlearn') (optional).
#' @param cluster A cluster object from package parallel or the number of cores to be used (optional). Default: 4
#' @keywords learning graph
#' @export
#' @examples
#' graph <- boot.parents.children(df)

boot.parents.children <- function(df, whitelist=NULL, blacklist=NULL, test=ci.tests, alpha=0.01, B=NULL, max.sx=NULL, version=c('mmpc','si.hiton.pc','hpc'),
                                 R=200, m=NULL, threshold=0.5, to=c('igraph', 'adjacency', 'edges', 'graph', 'bnlearn'), cluster=4) {
    test <- match.arg(test)
    version <- match.arg(version)
    to <- match.arg(to)

    algorithm <- switch(version,
        mmpc = bnlearn::mmpc,
        si.hiton.pc = bnlearn::si.hiton.pc,
        hpc = bnlearn::hpc
    )

    if (!is.null(whitelist)) {
        whitelist <- convert.format(whitelist, 'edges')
    }

    if (!is.null(blacklist)) {
        blacklist <- convert.format(blacklist, 'edges')
    }

    df <- drop.all.zeros(df)

    registerDoParallel(cluster)

    graphs <- foreach(rep=1:R) %dopar% {
        splitted.df <- dataframe.split(df, m=m)
        g <- algorithm(splitted.df$train, whitelist=whitelist, blacklist=blacklist, test=test, alpha=alpha,
                         B=B, max.sx=max.sx, undirected=FALSE)
        convert.format(g, to='adjacency')
    }

    stopImplicitCluster()

    graphs <- rename.graphs(graphs, colnames(df), to='adjacency')
    g <- averaged.graph(graphs, threshold=threshold, to=to)
    return(g)
}

#' Chow-Liu Algorithm With Bootstrapping
#'
#' This function allows you to learn a undirected graph from a dataset using the Chow-Liu algorithm.
#' @param df Dataset.
#' @param whitelist A data frame with two columns, containing a set of arcs to be included in the graph.
#' @param blacklist A data frame with two columns, containing a set of arcs not to be included in the graph.
#' @param R Number of bootstrap replicates (optional). Default: 200
#' @param m Size of each bootstrap replicate (optional). Default: nrow(df)/2
#' @param threshold Minimum strength required for a coefficient to be included in the averaged adjacency matrix (optional). Default: 0.5
#' @param to Output format ('adjacency', 'edges', 'igraph', or 'bnlearn') (optional).
#' @param cluster A cluster object from package parallel or the number of cores to be used (optional). Default: 4
#' @keywords learning graph
#' @export
#' @examples
#' graph <- boot.chowliu(df)

boot.chowliu <- function(df, whitelist=NULL, blacklist=NULL, R=200, m=NULL, threshold=0.5, to=c('igraph', 'adjacency', 'edges', 'graph', 'bnlearn'), cluster=4) {
    to <- match.arg(to)

    if (!is.null(whitelist)) {
        whitelist <- convert.format(whitelist, 'edges')
    }

    if (!is.null(blacklist)) {
        blacklist <- convert.format(blacklist, 'edges')
    }

    df <- drop.all.zeros(df)

    registerDoParallel(cluster)

    graphs <- foreach(rep=1:R) %dopar% {
        splitted.df <- dataframe.split(df, m=m)
        g <- bnlearn::chow.liu(splitted.df$train, whitelist=whitelist, blacklist=blacklist, mi='mi-g')
        convert.format(g, to='adjacency')
    }

    stopImplicitCluster()

    graphs <- rename.graphs(graphs, colnames(df), to='adjacency')
    g <- averaged.graph(graphs, threshold=threshold, to=to)
    return(g)
}

#' ARACNE Algorithm With Bootstrapping
#'
#' This function allows you to learn a undirected graph from a dataset using the ARACNE algorithm.
#' @param df Dataset.
#' @param whitelist A data frame with two columns, containing a set of arcs to be included in the graph.
#' @param blacklist A data frame with two columns, containing a set of arcs not to be included in the graph.
#' @param R Number of bootstrap replicates (optional). Default: 200
#' @param m Size of each bootstrap replicate (optional). Default: nrow(df)/2
#' @param threshold Minimum strength required for a coefficient to be included in the averaged adjacency matrix (optional). Default: 0.5
#' @param to Output format ('adjacency', 'edges', 'igraph', or 'bnlearn') (optional).
#' @param cluster A cluster object from package parallel or the number of cores to be used (optional). Default: 4
#' @keywords learning graph
#' @export
#' @examples
#' graph <- boot.aracne(df)

boot.aracne <- function(df, whitelist=NULL, blacklist=NULL, R=200, m=NULL, threshold=0.5, to=c('igraph', 'adjacency', 'edges', 'graph', 'bnlearn'), cluster=4) {
    to <- match.arg(to)

    if (!is.null(whitelist)) {
        whitelist <- convert.format(whitelist, 'edges')
    }

    if (!is.null(blacklist)) {
        blacklist <- convert.format(blacklist, 'edges')
    }

    df <- drop.all.zeros(df)

    registerDoParallel(cluster)

    graphs <- foreach(rep=1:R) %dopar% {
        splitted.df <- dataframe.split(df, m=m)
        g <- bnlearn::aracne(splitted.df$train, whitelist=whitelist, blacklist=blacklist, mi='mi-g')
        convert.format(g, to='adjacency')
    }

    stopImplicitCluster()

    graphs <- rename.graphs(graphs, colnames(df), to='adjacency')
    g <- averaged.graph(graphs, threshold=threshold, to=to)
    return(g)
}

## Score-Based

#' Hill-Climbing Algorithm (HC) With Bootstrapping
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
#' @param to Output format ('adjacency', 'edges', 'igraph', or 'bnlearn') (optional).
#' @param cluster A cluster object from package parallel or the number of cores to be used (optional). Default: 4
#' @keywords learning graph
#' @export
#' @examples
#' graph <- boot.hc(df)

boot.hc <- function(df, start=NULL, whitelist=NULL, blacklist=NULL, score=scores, restart=0, perturb=1, max.iter=Inf, maxp=Inf,
                   R=200, m=NULL, threshold=0.5, to=c('igraph', 'adjacency', 'edges', 'graph', 'bnlearn'), cluster=4) {
    to <- match.arg(to)

    if (!is.null(start)) {
        start <- convert.format(start, to='bnlearn')
    }
    score <- match.arg(score)

    if (!is.null(whitelist)) {
        whitelist <- convert.format(whitelist, 'edges')
    }

    if (!is.null(blacklist)) {
        blacklist <- convert.format(blacklist, 'edges')
    }

    df <- drop.all.zeros(df)

    registerDoParallel(cluster)

    graphs <- foreach(rep=1:R) %dopar% {
        splitted.df <- dataframe.split(df, m=m)
        g <- bnlearn::hc(splitted.df$train, newdata=splitted.df$test, start=start, whitelist=whitelist, blacklist=blacklist, score=score,
                         restart=restart, perturb=perturb, max.iter=max.iter, maxp=maxp, optimized=TRUE)
        convert.format(g, to='adjacency')
    }

    stopImplicitCluster()

    graphs <- rename.graphs(graphs, colnames(df), to='adjacency')
    g <- averaged.graph(graphs, threshold=threshold, to=to)
    return(g)
}

#' Tabu Search Algorithm (TABU) With Bootstrapping
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
#' @param to Output format ('adjacency', 'edges', 'igraph', or 'bnlearn') (optional).
#' @param cluster A cluster object from package parallel or the number of cores to be used (optional). Default: 4
#' @keywords learning graph
#' @export
#' @examples
#' graph <- boot.tabu(df)

boot.tabu <- function(df, start=NULL, whitelist=NULL, blacklist=NULL, score=scores, tabu=10, max.tabu=NULL, max.iter=Inf, maxp=Inf,
                     R=200, m=NULL, threshold=0.5, to=c('igraph', 'adjacency', 'edges', 'graph', 'bnlearn'), cluster=4) {
    to <- match.arg(to)

    if (!is.null(start)) {
         start <- convert.format(start, to='bnlearn')
     }
    score <- match.arg(score)
    if (is.null(max.tabu)) {
        max.tabu <- tabu
    }

    if (!is.null(whitelist)) {
        whitelist <- convert.format(whitelist, 'edges')
    }

    if (!is.null(blacklist)) {
        blacklist <- convert.format(blacklist, 'edges')
    }

    df <- drop.all.zeros(df)

    registerDoParallel(cluster)

    graphs <- foreach(rep=1:R) %dopar% {
        splitted.df <- dataframe.split(df, m=m)
        g <- bnlearn::tabu(splitted.df$train, newdata=splitted.df$test, start=start, whitelist=whitelist, blacklist=blacklist, score=score,
                           tabu=tabu, max.tabu=max.tabu, max.iter=max.iter, maxp=maxp, optimized=TRUE)
        convert.format(g, to='adjacency')
    }

    stopImplicitCluster()

    graphs <- rename.graphs(graphs, colnames(df), to='adjacency')
    g <- averaged.graph(graphs, threshold=threshold, to=to)
    return(g)
}

#' Greedy Equivalence Search Algorithm (GES) With Bootstrapping
#'
#' This function allows you to learn a directed graph from a dataset using the Greedy Equivalence Search (GES) algorithm of Chickering (2002).
#' @param df Dataset.
#' @param blacklist A data frame with two columns, containing a set of arcs not to be included in the graph (optional).
#' @param adaptive Whether constraints should be adapted to newly detected v-structures or unshielded triples: 'none', 'vstructures', or 'triples'. Default: 'none'
#' @param maxDegree Parameter used to limit the vertex degree of the estimated graph. Default: integer(0)
#' @param R Number of bootstrap replicates (optional). Default: 200
#' @param m Size of each bootstrap replicate (optional). Default: nrow(df)/2
#' @param threshold Minimum strength required for a coefficient to be included in the averaged adjacency matrix (optional). Default: 0.5
#' @param to Output format ('adjacency', 'edges', 'igraph', or 'bnlearn') (optional).
#' @param cluster A cluster object from package parallel or the number of cores to be used (optional). Default: 4
#' @keywords learning graph
#' @export
#' @examples
#' graph <- boot.ges(df)

boot.ges <- function(df, blacklist=NULL, adaptive=c('none','vstructures','triples'), maxDegree=integer(0),
                    R=200, m=NULL, threshold=0.5, to=c('igraph', 'adjacency', 'edges', 'graph', 'bnlearn'), cluster=4) {
    adaptive <- match.arg(adaptive)
    to <- match.arg(to)
    library(pcalg)

    if (!is.null(blacklist)) {
        blacklist <- convert.format(blacklist, 'adjacency')
        blacklist <- blacklist > 0
    }

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

    graphs <- rename.graphs(graphs, colnames(df), to='adjacency')
    g <- averaged.graph(graphs, threshold=threshold, to=to)
    return(g)
}

#' Linear NO-TEARS Algorithm (REIMPLEMENTED)
#'
#' This function allows you to learn an adjacency matrix from a dataset using the Linear NO-TEARS algorithm.
#' @param df Dataset.
#' @param lambda1 L1 regularization parameter. Default: 0.1
#' @param loss.type Type of loss function to be used: 'l2', 'logistic', or 'poisson'. Default: 'l2'
#' @param max.iter Maximum number of dual ascent steps. Default: 100
#' @param h.tol Minimum absolute value of h. Default: 1e-8
#' @param rho.max Maximum value of rho. Default: 1e+16
#' @param w.threshold Threshold of absolute value of weight. Default: 0.3
#' @keywords learning adjacency
#' @export
#' @examples
#' mtx <- notears(df)

notears <- function(df, lambda1=0.1, loss.type=c('l2','logistic','poisson'),
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
        h.new <- h.func(W)
        G.h <- G.h.func(W)
        G.smooth <- G.loss + (rho * h.new + alpha) * G.h
        G.obj <- c(array(G.smooth + lambda1), array(-1 * G.smooth + lambda1))
        return(G.obj)
    }

    n <- nrow(df)
    d <- ncol(df)
    nodes <- colnames(df)
    w.est <- replicate(2*d*d, 0)
    rho <- 1
    alpha <- 0
    h <- Inf

    for (i in 1:max.iter) {
        w.new <- NULL
        h.new <- NULL
        while (rho < rho.max) {
            w.new <- optim(w.est, fn, gr=gr, method='L-BFGS-B', lower=0, upper=Inf)$par
            h.new <- h.func(adj(w.new))
            if (h.new > (0.25*h)) {
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
    colnames(W.est) <- rownames(W.est) <- nodes
    return(W.est)
}

#' Linear NO-TEARS Algorithm (REIMPLEMENTED) With Bootstrapping
#'
#' This function allows you to learn a directed graph from a dataset using the Linear NO-TEARS algorithm.
#' @param df Dataset.
#' @param lambda1 L1 regularization parameter. Default: 0.1
#' @param loss.type Type of loss function to be used: 'l2', 'logistic', or 'poisson'. Default: 'l2'
#' @param max.iter Maximum number of dual ascent steps. Default: 100
#' @param h.tol Minimum absolute value of h. Default: 1e-8
#' @param rho.max Maximum value of rho. Default: 1e+16
#' @param w.threshold Threshold of absolute value of weight. Default: 0.3
#' @param R Number of bootstrap replicates (optional). Default: 200
#' @param m Size of each bootstrap replicate (optional). Default: nrow(df)/2
#' @param threshold Minimum strength required for a coefficient to be included in the averaged adjacency matrix (optional). Default: 0.5
#' @param to Output format ('adjacency', 'edges', 'igraph', or 'bnlearn') (optional).
#' @param cluster A cluster object from package parallel or the number of cores to be used (optional). Default: 4
#' @keywords learning graph
#' @export
#' @examples
#' graph <- boot.notears(df)

boot.notears <- function(df, lambda1=0.1, loss.type=c('l2','logistic','poisson'),
                         max.iter=100, h.tol=1e-8, rho.max=1e+16, w.threshold=0.3,
                         R=200, m=NULL, threshold=0.5, to=c('igraph', 'adjacency', 'edges', 'graph', 'bnlearn'), cluster=4) {
    loss.type <- match.arg(loss.type)
    to <- match.arg(to)

    df <- drop.all.zeros(df)

    registerDoParallel(cluster)

    graphs <- foreach(rep=1:R) %dopar% {
        splitted.df <- dataframe.split(df, m=m)
        g <- notears(splitted.df$train, lambda1=lambda1, loss.type=loss.type,
                     max.iter=max.iter, h.tol=h.tol, rho.max=rho.max, w.threshold=w.threshold)
        convert.format(g, to='adjacency')
    }

    stopImplicitCluster()

    graphs <- rename.graphs(graphs, colnames(df), to='adjacency')
    g <- averaged.graph(graphs, threshold=threshold, to=to)
    return(g)
}

## Hybrid (Skeleton/Constraint-Based + Score-Based)

#' General 2-Phase Restricted Maximization Algorithm (rsmax2) With Bootstrapping
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
#' @param to Output format ('adjacency', 'edges', 'igraph', or 'bnlearn') (optional).
#' @param cluster A cluster object from package parallel or the number of cores to be used (optional). Default: 4
#' @keywords learning graph
#' @export
#' @examples
#' graph <- boot.rsmax2(df)

boot.rsmax2 <- function(df, whitelist=NULL, blacklist=NULL, restrict=c('pc.stable','gs','iamb','fast.iamb','inter.iamb','iamb.fdr','mmpc','si.hiton.pc','hpc'),
                       maximize=c('hc','tabu'), restrict.args=list(), maximize.args=list(), version=c('rsmax2','mmhc','h2pc'),
                       R=200, m=NULL, threshold=0.5, to=c('igraph', 'adjacency', 'edges', 'graph', 'bnlearn'), cluster=4) {
    restrict <- match.arg(restrict)
    maximize <- match.arg(maximize)
    version <- match.arg(version)
    to <- match.arg(to)

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

    if (!is.null(whitelist)) {
        whitelist <- convert.format(whitelist, 'edges')
    }

    if (!is.null(blacklist)) {
        blacklist <- convert.format(blacklist, 'edges')
    }

    df <- drop.all.zeros(df)

    registerDoParallel(cluster)

    graphs <- foreach(rep=1:R) %dopar% {
        splitted.df <- dataframe.split(df, m=m)
        g <- bnlearn::rsmax2(splitted.df$train, whitelist=whitelist, blacklist=blacklist, restrict=restrict, maximize=maximize,
                             restrict.args=restrict.args, maximize.args=maximize.args)
        convert.format(g, to='adjacency')
    }

    stopImplicitCluster()

    graphs <- rename.graphs(graphs, colnames(df), to='adjacency')
    g <- averaged.graph(graphs, threshold=threshold, to=to)
    return(g)
}

#' Adaptively Restricted Greedy Equivalence Search Algorithm (ARGES) With Bootstrapping
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
#' @param to Output format ('adjacency', 'edges', 'igraph', or 'bnlearn') (optional).
#' @param cluster A cluster object from package parallel or the number of cores to be used (optional). Default: 4
#' @keywords learning graph
#' @export
#' @examples
#' graph <- boot.arges(df)

boot.arges <- function(df, whitelist=NULL, blacklist=NULL, indep.test=pcalg::gaussCItest, alpha=0.01, max.sx=Inf,
                      adaptive=c('none','vstructures','triples'), maxDegree=integer(0),
                      R=200, m=NULL, threshold=0.5, to=c('igraph', 'adjacency', 'edges', 'graph', 'bnlearn'), cluster=4) {

    adaptive <- match.arg(adaptive)
    to <- match.arg(to)
    library(pcalg)

    if (!is.null(whitelist)) {
        whitelist <- convert.format(whitelist, 'adjacency')
        whitelist <- whitelist > 0
    }

    if (!is.null(blacklist)) {
        blacklist <- convert.format(blacklist, 'adjacency')
        blacklist <- blacklist > 0
    }

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

    graphs <- rename.graphs(graphs, colnames(df), to='adjacency')
    g <- averaged.graph(graphs, threshold=threshold, to=to)
    return(g)
}

## Graphical Lasso

#' GLASSO Algorithm With Bootstrapping
#'
#' This function allows you to learn an undirected graph from a dataset using the GLASSO algorithm.
#' @param df Dataset.
#' @param rho Non-negative regularization parameter for GLASSO.
#' @param R Number of bootstrap replicates (optional). Default: 200
#' @param m Size of each bootstrap replicate (optional). Default: nrow(df)/2
#' @param threshold Minimum strength required for a coefficient to be included in the averaged adjacency matrix (optional). Default: 0.5
#' @param upper Whether or not to ignore the upper triangular adjacency matrix (optional).
#' @param lower Whether or not to ignore the lower triangular adjacency matrix (optional).
#' @param to Output format ('adjacency', 'edges', 'igraph', or 'bnlearn') (optional).
#' @param cluster A cluster object from package parallel or the number of cores to be used (optional). Default: 4
#' @keywords learning graph
#' @export
#' @examples
#' graph <- boot.glasso(df, rho=0.1)

boot.glasso <- function(df, rho=0.1, R=200, m=NULL, threshold=0.5, upper=FALSE, lower=TRUE, to=c('igraph', 'adjacency', 'edges', 'graph', 'bnlearn'), cluster=4) {
    to <- match.arg(to)

    k <- sum(c(upper, lower))
    if (k==0) {
        upper <- TRUE
        lower <- TRUE
    }

    df <- drop.all.zeros(df)

    registerDoParallel(cluster)

    graphs <- foreach(rep=1:R) %dopar% {
        splitted.df <- dataframe.split(df, m=m)
        S <- cov(as.matrix(splitted.df$train))
        g <- glasso::glasso(S, rho=rho)
        g$wi
    }

    stopImplicitCluster()

    graphs <- rename.graphs(graphs, colnames(df), to='adjacency')
    A <- averaged.graph(graphs, threshold=threshold, to='adjacency')
    diag(A) <- 0
    if (!upper) {
        A[upper.tri(A)] <- 0
    }
    if (!lower) {
        A[lower.tri(A)] <- 0
    }
    g <- convert.format(A, to=to)
    return(g)
}

## Restricted Structural Equation Models

#' LINGAM Algorithm With Bootstrapping
#'
#' This function allows you to learn a directed graph from a dataset using the LINGAM algorithm.
#' @param df Dataset.
#' @param R Number of bootstrap replicates (optional). Default: 200
#' @param m Size of each bootstrap replicate (optional). Default: nrow(df)/2
#' @param threshold Minimum strength required for a coefficient to be included in the averaged adjacency matrix (optional). Default: 0.5
#' @param to Output format ('adjacency', 'edges', 'igraph', or 'bnlearn') (optional).
#' @param cluster A cluster object from package parallel or the number of cores to be used (optional). Default: 4
#' @keywords learning graph
#' @export
#' @examples
#' graph <- boot.lingam(df)

boot.lingam <- function(df, R=200, m=NULL, threshold=0.5, to=c('igraph', 'adjacency', 'edges', 'graph', 'bnlearn'), cluster=4) {
    to <- match.arg(to)

    df <- drop.all.zeros(df)

    registerDoParallel(cluster)

    graphs <- foreach(rep=1:R) %dopar% {
        splitted.df <- dataframe.split(df, m=m)
        g <- pcalg::lingam(splitted.df$train)
        g$Bpruned
    }

    stopImplicitCluster()

    graphs <- rename.graphs(graphs, colnames(df), to='adjacency')
    g <- averaged.graph(graphs, threshold=threshold, to=to)
    return(g)
}

## Graphical Continuous Lyapunov Models

#' GCLM Algorithm With Bootstrapping
#'
#' This function allows you to learn a directed graph from a dataset using the GCLM algorithm.
#' @param df Dataset.
#' @param R Number of bootstrap replicates (optional). Default: 200
#' @param m Size of each bootstrap replicate (optional). Default: nrow(df)/2
#' @param threshold Minimum strength required for a coefficient to be included in the averaged adjacency matrix (optional). Default: 0.5
#' @param to Output format ('adjacency', 'edges', 'igraph', or 'bnlearn') (optional).
#' @param cluster A cluster object from package parallel or the number of cores to be used (optional). Default: 4
#' @keywords learning graph
#' @export
#' @examples
#' graph <- boot.gclm(df)

boot.gclm <- function(df, R=200, m=NULL, threshold=0.5, to=c('igraph', 'adjacency', 'edges', 'graph', 'bnlearn'), cluster=4) {
    to <- match.arg(to)

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

    graphs <- rename.graphs(graphs, colnames(df), to='adjacency')
    A <- averaged.graph(graphs, threshold=threshold, to='adjacency')
    diag(A) <- 0
    g <- convert.format(A, to=to)
    return(g)
}

mll <- function(P, S) {
    -determinant(P, logarithm=TRUE)$modulus + sum(S*P)
}

## NODAG

#' NODAG Algorithm With Bootstrapping
#'
#' This function allows you to learn a directed graph from a dataset using the NODAG algorithm.
#' @param lib.path Path of 'nodag.so' file.
#' @param df Dataset.
#' @param lambda Lambda regularization parameter. Default: 0.5
#' @param R Number of bootstrap replicates (optional). Default: 200
#' @param m Size of each bootstrap replicate (optional). Default: nrow(df)/2
#' @param threshold Minimum strength required for a coefficient to be included in the averaged adjacency matrix (optional). Default: 0.5
#' @param to Output format ('adjacency', 'edges', 'igraph', or 'bnlearn') (optional).
#' @param cluster A cluster object from package parallel or the number of cores to be used (optional). Default: 4
#' @keywords learning graph
#' @export
#' @examples
#' graph <- boot.nodag('nodag.so', df)

boot.nodag <- function(lib.path, df, lambda=0.5, R=200, m=NULL, threshold=0.5, to=c('igraph', 'adjacency', 'edges', 'graph', 'bnlearn'), cluster=4) {
    dyn.load(lib.path)
    to <- match.arg(to)

    df <- drop.all.zeros(df)
    df <- as.matrix(df)

    registerDoParallel(cluster)

    graphs <- foreach(rep=1:R) %dopar% {
        splitted.df <- dataframe.split(df, m=m)
        p <- ncol(splitted.df$train)
        out <- .Fortran('NODAG', as.integer(p),
                as.double(cor(splitted.df$train)),
                as.double(diag(p)), as.double(lambda),
                as.double(1e-5), as.double(0.5), as.integer(1e+3))
        A <- matrix(ncol=p, nrow=p, data=out[[3]])
        A <- diag(p) - diag(1/diag(A)) %*% A
        diag(A) <- 0
        A
    }

    stopImplicitCluster()

    graphs <- rename.graphs(graphs, colnames(df), to='adjacency')
    g <- averaged.graph(graphs, threshold=threshold, to=to)
    return(g)
}

#' Learn Huge Graph (With Random Gene Selection + Cells Bootstrapping)
#'
#' This function allows you to learn a directed graph from a high-dimensional dataset.
#' @param df Dataset.
#' @param algorithm Algorithm to be used (any of the gnlearn 'boot.x' algorithms, such as boot.pc or boot.hc). Default: boot.pc
#' @param n.genes Number of random genes per iteration. Default: 15
#' @param R Number of iterations. Defaults: 200
#' @param threshold Minimum strength required for a coefficient to be included in the averaged adjacency matrix (optional). Default: 0.5
#' @param iter.R Number of bootstrap replicates. Default: 200
#' @param iter.m Size of each bootstrap replicate. Default: nrow(df)/2
#' @param to Output format ('adjacency', 'edges', 'igraph', or 'bnlearn').
#' @param cluster A cluster object from package parallel or the number of cores to be used (optional). Default: 4
#' @param ... Other arguments for the specified algorithm.
#' @keywords learning huge graph
#' @export
#' @examples
#' graph <- huge.graph(df, algorithm=boot.tabu)
#' graph <- huge.graph(df, algorithm=boot.lingam)
#' graph <- huge.graph(df, algorithm=boot.iamb, n.genes=20, R=100, threshold=0.9, iter.R=10)

huge.graph <- function(df, algorithm=boot.pc, n.genes=15, R=200, threshold=0.5, iter.R=4, iter.m=NULL, to=c('igraph', 'adjacency', 'edges', 'graph', 'bnlearn'), cluster=4, ...) {
    to <- match.arg(to)

    df <- drop.all.zeros(df)
    registerDoParallel(cluster)

    graphs <- foreach(rep=1:R) %dopar% {

        sel.genes <- sample(colnames(df), n.genes, replace=FALSE)
        sel.df <- subset(df, select=sel.genes)
        algorithm(df=sel.df, R=iter.R, m=iter.m, threshold=0, to='adjacency', cluster=1, ...)

    }

    stopImplicitCluster()
    g <- averaged.graph(graphs, threshold=threshold, to=to)
    return(g)

}
