# gnlearn
Genetic Network Learning

## Package Overview

*Import & Export Files:*
* `import.dataset()`: Import A Local Dataset
* `import.geneset()`: Import A Local Geneset
* `import.graph()`: Import A Local Graph
* `export.dataset()`: Export A Dataset To A File
* `export.geneset()`: Export A Geneset To A File
* `export.graph()`: Export A Graph To A File

*RESTful API Client:*
* `list.datasets()`: List Datasets Available Via RESTful API
* `list.genesets()`: List Genesets Available Via RESTful API
* `list.graphs()`: List Graphs Available Via RESTful API
* `download.dataset()`: Download A Dataset Via RESTful API
* `download.geneset()`: Download A Geneset Via RESTful API
* `download.graph()`: Download A Graph Via RESTful API

*Dataset Manipulation:*
* `drop.all.zeros()`: Drop rows and/or columns with all zeros
* `filter.dataset()`: Select (automatically) the most appropriate gene columns and cell rows of your dataset
* `select.genes()`: Select (automatically) the most appropriate gene columns of your dataset
* `select.cells()`: Select (automatically) the most appropriate cell rows of your dataset
* `dataframe.split()`: Split A Dataframe Into Training And Test Datasets

*Dataset Operations:*
* `gene.histogram()`: Plot expression histograms of genes in your dataset
* `gene.correlation()`: Plot expression correlation of genes in your dataset
* `gene.clustering()`: Gene Clustering

*Structure Learning Algorithms:*
* `huge.graph()`: Learn Huge Graph (With Random Gene Selection + Cells Bootstrapping)
* `boot.skeleton()`: Peter & Clark Skeleton Algorithm With Bootstrapping
* `boot.pc()`: Peter & Clark Algorithm (PC) With Bootstrapping
* `boot.fci()`: Fast Causal Inference Algorithm (FCI) With Bootstrapping
* `boot.gs()`: Grow-Shrink Algorithm (GS) With Bootstrapping
* `boot.iamb()`: Incremental Association Algorithm (IAMB) With Bootstrapping
* `boot.parents.children()`: Parents & Children Algorithm With Bootstrapping
* `boot.chowliu()`: Chow-Liu Algorithm With Bootstrapping
* `boot.aracne()`: ARACNE Algorithm With Bootstrapping
* `boot.hc()`: Hill-Climbing Algorithm (HC) With Bootstrapping
* `boot.tabu()`: Tabu Search Algorithm (TABU) With Bootstrapping
* `boot.ges()`: Greedy Equivalence Search Algorithm (GES) With Bootstrapping
* `notears()`: Linear NO-TEARS Algorithm (Reimplemented)
* `boot.notears()`: Linear NO-TEARS Algorithm (Reimplemented) With Bootstrapping
* `boot.rsmax2()`: General 2-Phase Restricted Maximization Algorithm (rsmax2) With Bootstrapping
* `boot.arges()`: Adaptively Restricted Greedy Equivalence Search Algorithm (ARGES) With Bootstrapping
* `boot.glasso()`: Graphical Lasso (GLASSO) With Bootstrapping
* `boot.lingam()`: Restricted Structural Equation Models (LINGAM) With Bootstrapping
* `boot.gclm()`: Graphical Continuous Lyapunov Models (GCLM) With Bootstrapping
* `boot.nodag()`: NODAG Algorithm With Bootstrapping

*Graph Format:*
* `detect.format()`: Graph Format Detection
* `convert.format()`: Graph Format Conversion
* `as.adjacency()`: Convert Graph To Adjacency Matrix
* `as.edges()`: Convert Graph To Edge List
* `as.graph()`: Convert Graph To graph Format
* `as.igraph()`: Convert Graph To igraph Format
* `as.bnlearn()`: Convert Graph To bnlearn Format

*Graph Modifications:*
* `add.genes()`: Add Genes To A Graph
* `rename.genes()`: Rename Genes Of A Graph
* `delete.genes()`: Delete Genes From A Graph
* `delete.isolated()`: Delete Isolated Nodes
* `add.edges()`: Add Edges To A Graph
* `delete.edges()`: Delete Edges To A Graph
* `graph.marginalization()`: Graph Marginalization
* `fit.coefficients()`: Estimate The Coefficients Of An Adjacency Matrix

*Graph Operations:*
* `undirected.edges()`: Return Undirected Edges
* `directed.edges()`: Return Directed Edges
* `graph.plot()`: Graph Plotting
* `compare.graphs()`: Graph Comparison
* `gene.degree()`:  Degree Per Gene
* `feature.degree()`: Feature Degree Per Gene
* `feature.plot()`: Feature Graph Plotting
* `graph.communities()`: Graph Communities
* `ortholog.graph()`: Ortholog Genes Graph
* `drugs.plot()`: Drug-Gene Interactions Plotting
* `average.graph()`: Calculate The Average Graph  
* `score.graph()`: Compute The Score Of A Graph        

*Graph generation:*
* `random.graph()`: Generate A Random Graph/DAG
* `ground.truth()`: Create a Ground Truth Graph
* `make.edgelist()`: Make an edgelist from some genes to anothers

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
