# gnlearn
Genetic Network Learning

## Package Overview

*Import & Export Files:*
* _import.dataset_: Import A Local Dataset
* _import.geneset_: Import A Local Geneset
* _import.graph_: Import A Local Graph
* _export.dataset_: Export A Dataset To A File
* _export.geneset_: Export A Geneset To A File
* _export.graph_: Export A Graph To A File

*RESTful API Client:*
* _list.datasets_: List Datasets Available Via RESTful API
* _list.genesets_: List Genesets Available Via RESTful API
* _list.graphs_: List Graphs Available Via RESTful API
* _download.dataset_: Download A Dataset Via RESTful API
* _download.geneset_: Download A Geneset Via RESTful API
* _download.graph_: Download A Graph Via RESTful API

*Dataset Manipulation:*
* _drop.all.zeros_: Drop rows and/or columns with all zeros
* _select.genes_: Select (automatically) the most appropriate gene columns of your dataset
* _dataframe.split_: Split A Dataframe Into Training And Test Datasets

*Dataset Operations:*
* _gene.histogram_: Plot expression histograms of genes in your dataset
* _gene.correlation_: Plot expression correlation of genes in your dataset
* _gene.clustering_: Gene Clustering

*Structure Learning Algorithms:*
* _huge.graph_: Learn Huge Graph (With Random Gene Selection + Cells Bootstrapping)
* _boot.skeleton_: Peter & Clark Skeleton Algorithm With Bootstrapping
* _boot.pc_: Peter & Clark Algorithm (PC) With Bootstrapping
* _boot.fci_: Fast Causal Inference Algorithm (FCI) With Bootstrapping
* _boot.gs_: Grow-Shrink Algorithm (GS) With Bootstrapping
* _boot.iamb_: Incremental Association Algorithm (IAMB) With Bootstrapping
* _boot.parents.children_: Parents & Children Algorithm With Bootstrapping
* _boot.chowliu_: Chow-Liu Algorithm With Bootstrapping
* _boot.aracne_: ARACNE Algorithm With Bootstrapping
* _boot.hc_: Hill-Climbing Algorithm (HC) With Bootstrapping
* _boot.tabu_: Tabu Search Algorithm (TABU) With Bootstrapping
* _boot.ges_: Greedy Equivalence Search Algorithm (GES) With Bootstrapping
* _notears_: Linear NO-TEARS Algorithm (Reimplemented)
* _boot.notears_: Linear NO-TEARS Algorithm (Reimplemented) With Bootstrapping
* _boot.rsmax2_: General 2-Phase Restricted Maximization Algorithm (rsmax2) With Bootstrapping
* _boot.arges_: Adaptively Restricted Greedy Equivalence Search Algorithm (ARGES) With Bootstrapping
* _boot.glasso_: Graphical Lasso (GLASSO) With Bootstrapping
* _boot.lingam_: Restricted Structural Equation Models (LINGAM) With Bootstrapping
* _boot.gclm_: Graphical Continuous Lyapunov Models (GCLM) With Bootstrapping
* _boot.nodag_: NODAG Algorithm With Bootstrapping

*Graph Format:*
* _detect.format_: Graph Format Detection
* _convert.format_: Graph Format Conversion
* _as.adjacency_: Convert Graph To Adjacency Matrix
* _as.edges_: Convert Graph To Edge List
* _as.graph_: Convert Graph To graph Format
* _as.igraph_: Convert Graph To igraph Format
* _as.bnlearn_: Convert Graph To bnlearn Format

*Graph Modifications:*
* _add.genes_: Add Genes To A Graph
* _rename.genes_: Rename Genes Of A Graph
* _delete.genes_: Delete Genes From A Graph
* _delete.isolated_: Delete Isolated Nodes
* _add.edges_: Add Edges To A Graph
* _delete.edges_: Delete Edges To A Graph
* _graph.marginalization_: Graph Marginalization
* _fit.coefficients_: Estimate The Coefficients Of An Adjacency Matrix

*Graph Operations:*
* _undirected.edges_: Return Undirected Edges
* _directed.edges_: Return Directed Edges
* _graph.plot_: Graph Plotting
* _compare.graphs_: Graph Comparison
* _feature.degree_: Feature Degree
* _feature.plot_: Feature Graph Plotting
* _graph.communities_: Graph Communities
* _ortholog.graph_: Ortholog Genes Graph
* _drugs.plot_: Drug-Gene Interactions Plotting
* _averaged.graph_: Calculate The Averaged Graph          

*Graph generation:*
* _random.graph_: Generate A Random Graph/DAG
* _ground.truth_: Create a Ground Truth Graph
* _make.edgelist_: Make an edgelist from some genes to anothers

## Install *gnlearn*

```R
if (!requireNamespace('devtools', quietly=TRUE))
    install.packages('devtools')
devtools::install_github('rlebron-bioinfo/gnlearn')
```

## Docker Container

```
docker pull rlebronbioinfo/gnlearn:latest
```

Open an R interpreter with `gnlearn` already installed:

```
docker run -ti --rm -v "$PWD":/root/wd rlebronbioinfo/gnlearn R
```

Run an R script inside this Docker container:

```
cd path/to/script/directory
docker run -ti --rm -v "$PWD":/root/wd rlebronbioinfo/gnlearn Rscript /root/wd/myscript.R
```

After running one of these commands, your working directory will be mounted in /root/wd inside the Docker container. This container will be automatically destroyed after closing the interpreter.

## Quick Start

## Documentation

## Examples
