# gnlearn
**G**enetic **N**etwork **Learn**ing (`gnlearn`) is an R package for structural learning of huge gene regulatory networks from single-cell datasets, using random gene resampling, cell bootstrapping, and one of the available algorithms (see below).

The learned genetic networks can be obtained as graphs (in `graph` or `igraph` R packages formats), as adjacent matrices or as a lists of edges. `gnlearn` has a wide range of functions for working with both input datasets and learned networks (see *Package Overview* section).

***

*Constraint-Based Algorithms*
* Peter & Clark skeleton algorithm (`boot.skeleton()`), based on `pcalg` and `bnlearn` R packages implementations.
* Peter & Clark (PC) algorithm (`boot.pc()`), based on `pcalg` and `bnlearn` R packages implementations.
* Fast Causal Inference (FCI), Really FCI (RFCI) and FCI+ algorithms (`boot.fci()`), based on `pcalg` R package implementation.
* Grow-Shrink (GS) algorithm (`boot.gs()`), based on `bnlearn` R package implementation.
* Incremental Association (IAMB), Fast IAMB, Interleaved IAMB and IAMB with FDR Correction algorithms (`boot.iamb()`), based on `bnlearn` R package implementation.
* Max-Min Parents & Children (MMPC), Semi-Interleaved Hiton-PC (SI-HITON-PC) and Hybrid Parents & Children (HPC) algorithms (`boot.parents.children()`), based on `bnlearn` R package implementation.
* Chow-Liu algorithm (`boot.chowliu()`), based on `bnlearn` R package implementation.
* Algorithm for the Reconstruction of Accurate Cellular Networks (ARACNE) (`boot.aracne()`), based on `bnlearn` R package implementation.

*Score-Based Algorithms*
* Hill-Climbing (HC) algorithm (`boot.hc()`), based on `bnlearn` R package implementation.
* Tabu Search (Tabu) algorithm (`boot.tabu()`), based on `bnlearn` R package implementation.
* Greedy Equivalence Search (GES) (`boot.ges()`), based on `pcalg` R package implementation.
* Linear NO-TEARS algorithm (`boot.notears()`), reimplemented in R.

*Hybrid (Constraint-Based + Score-Based) Algorithms*
* General 2-Phase Restricted Maximization (RSMAX2), Max-Min Hill Climbing (MMHC) and Hybrid HPC (H2PC) algorithms (`boot.rsmax2()`), based on `bnlearn` R package implementation.
* Adaptively Restricted Greedy Equivalence Search (ARGES) algorithm (`boot.arges()`), based on `pcalg` R package implementation.

*Other Algorithms*
* Graphical Lasso (GLASSO) (`boot.glasso()`), based on `glasso` R package implementation.
* Restricted Structural Equation Models (LINGAM) (`boot.lingam()`), based on `pcalg` R package implementation.
* GEne Network Inference with Ensemble of trees (GENIE3) (`boot.genie3()`), based on `GENIE3` R package implementation.
* Graphical Continuous Lyapunov Models (GCLM), based on `gclm` R package implementation.
* NODAG algorithm, based on Gherardo Varando implementation (https://github.com/gherardovarando/nodag).

## Package Overview

<em><strong>Import & Export Files:</strong></em>
* `import.dataset()`: Import A Local Dataset
* `import.geneset()`: Import A Local Geneset
* `import.graph()`: Import A Local Graph
* `export.dataset()`: Export A Dataset To A File
* `export.geneset()`: Export A Geneset To A File
* `export.graph()`: Export A Graph To A File

<em><strong>RESTful API Client:</strong></em>
* `list.datasets()`: List Datasets Available Via RESTful API
* `list.genesets()`: List Genesets Available Via RESTful API
* `list.graphs()`: List Graphs Available Via RESTful API
* `download.dataset()`: Download A Dataset Via RESTful API
* `download.geneset()`: Download A Geneset Via RESTful API
* `download.graph()`: Download A Graph Via RESTful API

<em><strong>Dataset Manipulation:</strong></em>
* `drop.all.zeros()`: Drop rows and/or columns with all zeros
* `filter.dataset()`: Select (automatically) the most appropriate gene columns and cell rows of your dataset
* `select.genes()`: Select (automatically) the most appropriate gene columns of your dataset
* `select.cells()`: Select (automatically) the most appropriate cell rows of your dataset
* `dataset.split()`: Split A Dataset Into Training And Test Datasets

<em><strong>Dataset Operations:</strong></em>
* `gene.histogram()`: Plot expression histograms of genes in your dataset
* `gene.correlation()`: Plot expression correlation of genes in your dataset
* `gene.clustering()`: Gene Clustering

<em><strong>Structure Learning Algorithms:</strong></em>
<em>Without Bootstrapping:</em>

<em>With Bootstrapping:</em>
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
* `boot.genie3()`: GEne Network Inference with Ensemble of trees (GENIE3) With Bootstrapping
* `boot.gclm()`: Graphical Continuous Lyapunov Models (GCLM) With Bootstrapping
* `boot.nodag()`: NODAG Algorithm With Bootstrapping

<em><strong>Graph Format:</strong></em>
* `detect.format()`: Graph Format Detection
* `convert.format()`: Graph Format Conversion
* `as.adjacency()`: Convert Graph To Adjacency Matrix
* `as.edges()`: Convert Graph To Edge List
* `as.graph()`: Convert Graph To graph Format
* `as.igraph()`: Convert Graph To igraph Format
* `as.bnlearn()`: Convert Graph To bnlearn Format

<em><strong>Graph Modifications:</strong></em>
* `add.genes()`: Add Genes To A Graph
* `rename.genes()`: Rename Genes Of A Graph
* `delete.genes()`: Delete Genes From A Graph
* `delete.isolated()`: Delete Isolated Nodes
* `add.edges()`: Add Edges To A Graph
* `delete.edges()`: Delete Edges To A Graph
* `fit.coefficients()`: Estimate The Coefficients Of An Adjacency Matrix

<em><strong>Graph Operations:</strong></em>
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

<em><strong>Graph generation:</strong></em>
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

*Example #1: Breast Cancer*

*Example #2: Hepatocellular Carcinoma*

*Example #3: Erythropoiesis*

## Citation

The gnlearn article is not yet available (Ricardo Lebrón & Gherardo Varando, in preparation).

Please also use the following references depending on the algorithm used:
* PC or skeleton [[1]](#1)
* FCI [[2]](#2)
* RFCI [[3]](#3)
* FCI+ [[4]](#4)
* GS [[5]](#5)
* IAMB [[6]](#6)
* Fast IAMB [[6]](#6)
* Interleaved IAMB [[7]](#7)
* IAMB with FDR Correction [[8]](#8) [[9]](#9)
* MMPC [[10]](#10)
* SI-HITON-PC [[11]](#11)
* HPC [[12]](#12)
* Chow-Liu algorithm [[13]](#13)
* ARACNE [[14]](#14)
* HC [[15]](#15)
* Tabu [[15]](#15)
* GES [[16]](#16)
* Linear NO-TEARS [[17]](#17)
* RSMAX2 [[18]](#18)
* MMHC [[19]](#19)
* H2PC [[20]](#20)
* ARGES [[21]](#21)
* GLASSO [[22]](#22)
* LINGAM [[23]](#23)
* GENIE3 [[]](#)
* GCLM [[24]](#24)
* NODAG [[25]](#25)

If you use a pcalg-based or bnlearn-based implementation, please cite [[26]](#26) and [[27]](#27) respectively.

## References
<a id="1">[1]</a>
Colombo D, Maathuis MH (2014). "Order-Independent Constraint-Based Causal Structure Learning". Journal of Machine Learning Research, 15:3921–3962.

<a id="2">[2]</a>

<a id="3">[3]</a>

<a id="4">[4]</a>

<a id="5">[5]</a>
Margaritis D (2003). Learning Bayesian Network Model Structure from Data. Ph.D. thesis, School of Computer Science, Carnegie-Mellon University, Pittsburgh, PA.

<a id="6">[6]</a>
Tsamardinos I, Aliferis CF, Statnikov A (2003). "Algorithms for Large Scale Markov Blanket Discovery". Proceedings of the Sixteenth International Florida Artificial Intelligence Research Society Conference, 376–381.

<a id="7">[7]</a>
Yaramakala S, Margaritis D (2005). "Speculative Markov Blanket Discovery for Optimal Feature Selection". Proceedings of the Fifth IEEE International Conference on Data Mining, 809–812.

<a id="8">[8]</a>
Pena JM (2008). "Learning Gaussian Graphical Models of Gene Networks with False Discovery Rate Control". Proceedings of the Sixth European Conference on Evolutionary Computation, Machine Learning and Data Mining in Bioinformatics, 165–176.

<a id="9">[9]</a>
Gasse M, Aussem A, Elghazel H (2014). "A Hybrid Algorithm for Bayesian Network Structure Learning with Application to Multi-Label Learning". Expert Systems with Applications, 41(15):6755–6772.

<a id="10">[10]</a>


<a id="11">[11]</a>

<a id="12">[12]</a>

<a id="13">[13]</a>

<a id="14">[14]</a>

<a id="15">[15]</a>

<a id="16">[16]</a>

<a id="17">[17]</a>

<a id="18">[18]</a>

<a id="19">[19]</a>

<a id="20">[20]</a>

<a id="21">[21]</a>

<a id="22">[22]</a>

<a id="23">[23]</a>

<a id="24">[24]</a>

<a id="25">[25]</a>

<a id="26">[26]</a>

<a id="27">[27]</a>
