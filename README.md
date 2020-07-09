# gnlearn
**G**enetic **N**etwork **Learn**ing (`gnlearn`) is an R package for structural learning of transcriptional regulatory networks from single-cell datasets. Learned transcriptional regulatory networks can be obtained as graphs (in `graph` or `igraph` R packages formats), adjacent matrices or lists of edges.

***

<em><strong>Constraint-Based Algorithms</strong></em>
* Peter & Clark skeleton algorithm (`boot.skeleton()`), based on `pcalg` and `bnlearn` R packages implementations.
* Peter & Clark (PC) algorithm (`boot.pc()`), based on `pcalg` and `bnlearn` R packages implementations.
* Fast Causal Inference (FCI), Really FCI (RFCI) and FCI+ algorithms (`boot.fci()`), based on `pcalg` R package implementation.
* Grow-Shrink (GS) algorithm (`boot.gs()`), based on `bnlearn` R package implementation.
* Incremental Association (IAMB), Fast IAMB, Interleaved IAMB and IAMB with FDR Correction algorithms (`boot.iamb()`), based on `bnlearn` R package implementation.
* Max-Min Parents & Children (MMPC), Semi-Interleaved Hiton-PC (SI-HITON-PC) and Hybrid Parents & Children (HPC) algorithms (`boot.parents.children()`), based on `bnlearn` R package implementation.
* Chow-Liu algorithm (`boot.chowliu()`), based on `bnlearn` R package implementation.
* Algorithm for the Reconstruction of Accurate Cellular Networks (ARACNE) (`boot.aracne()`), based on `bnlearn` R package implementation.

<em><strong>Score-Based Algorithms</strong></em>
* Hill-Climbing (HC) algorithm (`boot.hc()`), based on `bnlearn` R package implementation.
* Tabu Search (Tabu) algorithm (`boot.tabu()`), based on `bnlearn` R package implementation.
* Greedy Equivalence Search (GES) (`boot.ges()`), based on `pcalg` R package implementation.
* Linear NO-TEARS algorithm (`boot.notears()`), reimplemented in R.

<em><strong>Hybrid (Constraint-Based + Score-Based) Algorithms</strong></em>
* General 2-Phase Restricted Maximization (RSMAX2), Max-Min Hill Climbing (MMHC) and Hybrid HPC (H2PC) algorithms (`boot.rsmax2()`), based on `bnlearn` R package implementation.
* Adaptively Restricted Greedy Equivalence Search (ARGES) algorithm (`boot.arges()`), based on `pcalg` R package implementation.

<em><strong>Other Algorithms</strong></em>
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
* `skeleton()`: Peter & Clark Skeleton Algorithm
* `pc()`: Peter & Clark Algorithm (PC)
* `fci()`: Fast Causal Inference Algorithm (FCI)
* `gs()`: Grow-Shrink Algorithm (GS)
* `iamb()`: Incremental Association Algorithm (IAMB)
* `parents.children()`: Parents & Children Algorithm
* `chowliu()`: Chow-Liu Algorithm
* `aracne()`: ARACNE Algorithm
* `hc()`: Hill-Climbing Algorithm (HC)
* `tabu()`: Tabu Search Algorithm (TABU)
* `ges()`: Greedy Equivalence Search Algorithm (GES)
* `notears()`: Linear NO-TEARS Algorithm (Reimplemented)
* `rsmax2()`: General 2-Phase Restricted Maximization Algorithm (rsmax2)
* `arges()`: Adaptively Restricted Greedy Equivalence Search Algorithm (ARGES)
* `glasso()`: Graphical Lasso (GLASSO)
* `lingam()`: Restricted Structural Equation Models (LINGAM)
* `genie3()`: GEne Network Inference with Ensemble of trees (GENIE3)
* `gclm()`: Graphical Continuous Lyapunov Models (GCLM)
* `nodag()`: NODAG Algorithm

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

## Citation

The gnlearn article is under preparation. Please also use the following references according to the algorithm used:

* PC or skeleton [[1]](#1)
* FCI [[2]](#2)
* RFCI [[3]](#3)
* FCI+ [[4]](#4)
* GS [[5]](#5)
* IAMB [[6]](#6)
* Fast-IAMB [[7]](#7)
* Interleaved-IAMB [[6]](#6)
* IAMB with FDR Correction [[8]](#8)
* MMPC [[6]](#6)
* SI-HITON-PC [[9]](#9)
* HPC [[10]](#10)
* Chow-Liu algorithm [[11]](#11)
* ARACNE [[12]](#12)
* HC [[13]](#13)
* Tabu [[13]](#13)
* GES [[14]](#14)
* Linear NO-TEARS [[15]](#15) [[16]](#16)
* RSMAX2 [[17]](#17)
* MMHC [[18]](#18)
* H2PC [[10]](#10)
* ARGES [[19]](#19)
* GLASSO [[20]](#20)
* LINGAM [[21]](#21)
* GENIE3 [[22]](#22)
* GCLM [[23]](#23)
* NODAG [[24]](#24)

If you use a pcalg-based or bnlearn-based implementation, please cite [[25]](#25) and [[26]](#26) respectively.

## References
<a id="1">[1]</a>
Colombo, D. and Maathuis, M. H. (2014). Order-independent constraint-based causal structure learning. The Journal of Machine Learning Research, 15(1):3741–3782.

<a id="2">[2]</a>
Spirtes, P., Glymour, C. N., Scheines, R., and Heckerman, D. (2000). Causation, Prediction, and Search. MIT press.

<a id="3">[3]</a>
Colombo, D., Maathuis, M. H., Kalisch, M., and Richardson, T. S. (2012). Learning high-dimensional directed acyclic graphs with latent and selection variables. The Annals of Statistics, pages 294–321.

<a id="4">[4]</a>
Claassen, T., Mooij, J., and Heskes, T. (2013). Learning sparse causal models is not np-hard. preprint arXiv:1309.6824.

<a id="5">[5]</a>
Margaritis, D. (2003). Learning bayesian network model structure from data. Technical report, Carnegie-Mellon Univ Pittsburgh Pa School of Computer Science.

<a id="6">[6]</a>
Tsamardinos, I., Aliferis, C. F., Statnikov, A. R., and Statnikov, E. (2003). Algorithms for large scale markov blanket discovery. In FLAIRS conference, volume 2, pages 376–380.

<a id="7">[7]</a>
Yaramakala, S. and Margaritis, D. (2005). Speculative markov blanket discovery for optimal feature selection. In Fifth IEEE International Conference on Data Mining, pages 1–4. IEEE.

<a id="8">[8]</a>
Peña, J. M. (2008). Learning gaussian graphical models of gene networks with false discovery rate control. In European conference on evolutionary computation, machine learning and data mining in bioinformatics, pages 165–176. Springer.

<a id="9">[9]</a>
Aliferis, F. C., Statnikov, A., Tsamardinos, I., Subramani, M. and Koutsoukos, X. D. (2010). Local Causal and Markov Blanket Induction for Causal Discovery and Feature Selection for Classification Part I: Algorithms and Empirical Evaluation. Journal of Machine Learning Research, 11:171–234.

<a id="10">[10]</a>
Gasse, M., Aussem, A., and Elghazel, H. (2014). A hybrid algorithm for bayesian network structure learning with application to multi-label learning. Expert Systems with Applications, 41(15):6755–6772.

<a id="11">[11]</a>
Chow, C. and Liu, C. (1968). Approximating discrete probability distributions with dependence trees. IEEE Transactions on Information Theory, 14(3):462–467.

<a id="12">[12]</a>
Margolin, A. A., Nemenman, I., Basso, K., Wiggins, C., Stolovitzky, G., Favera, R. D., and Califano, A. (2006). ARACNE: An Algorithm for the Reconstruction of Gene Regulatory Networks in a Mammalian Cellular Context. BMC Bioinformatics, 7(S1):S7.

<a id="13">[13]</a>
Bouckaert, R. R. (1995). Bayesian belief networks: from construction to inference. PhD thesis.

<a id="14">[14]</a>
Chickering, D. M. (2002). Learning equivalence classes of bayesian-network structures. Journal of machine learning research, 2(Feb):445–498.

<a id="15">[15]</a>
Zheng, X., Aragam, B., Ravikumar, P. K., and Xing, E. P. (2018). Dags with no tears: Continuous optimization for structure learning. In Advances in Neural Information Processing Systems, pages 9472–9483.

<a id="16">[16]</a>
Zheng, X., Dan, C., Aragam, B., Ravikumar, P., and Xing, E. P. (2019). Learning sparse nonparametric dags. preprint arXiv:1909.13189.

<a id="17">[17]</a>
Friedman, N., Nachman, I., and Pe’er, D. (2013). Learning bayesian network structure from massive datasets: The sparse candidate algorithm. preprint arXiv:1301.6696.

<a id="18">[18]</a>
Tsamardinos, I., Brown, L. E., and Aliferis, C. F. (2006). The max-min hill-climbing bayesian network structure learning algorithm. Machine learning, 65(1):31–78.

<a id="19">[19]</a>
Nandy, P., Hauser, A., and Maathuis, M. H. (2018). High-dimensional consistency in score-based and hybrid structure learning. The Annals of Statistics, 46(6A):3151–3183.

<a id="20">[20]</a>
Zhao, T., Liu, H., Roeder, K., Lafferty, J., and Wasserman, L. (2012). The huge package for high-dimensional undirected graph estimation in R. The Journal of Machine Learning Research, 13(1), 1059-1062.

<a id="21">[21]</a>
Shimizu, S., Hoyer, P. O., Hyvärinen, A., and Kerminen, A. (2006). A linear non-gaussian acyclic model for causal discovery. Journal of Machine Learning Research, 7(Oct):2003–2030.

<a id="22">[22]</a>
Huynh-Thu, V. A., Irrthum, A., Wehenkel, L., and Geurts, P. (2010). Inferring Regulatory Networks from Expression Data Using Tree-Based Methods. PLoS ONE, 5(9):e12776.

<a id="23">[23]</a>
Varando, G. and Hansen, N. R. (2020). Graphical continuous lyapunov models. arXiv preprint arXiv:2005.10483.

<a id="24">[24]</a>
Varando, G. (2020). Learning dags without imposing acyclicity. arXiv preprint arXiv:2006.03005.

<a id="25">[25]</a>
Kalisch, M., Mächler, M., Colombo, D., Maathuis, M. H., and Bühlmann, P. (2012). Causal inference using graphical models with the r package pcalg. Journal of Statistical Software, 47(11):1–26.

<a id="26">[26]</a>
Scutari, M. (2010). Learning Bayesian Networks with the bnlearn R Package. Journal of Statistical Software, 35(3).
