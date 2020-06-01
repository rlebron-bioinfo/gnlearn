# gnlearn
Genetic Network Learning

## Structure Learning Algorithms

### Constraint-Based
* Skeleton -> bnlearn & pcalg
* PC (the stable version) -> bnlearn & pcalg
* FCI -> pcalg
* RFCI -> pcalg
* FCI+ -> pcalg
* Grow-Shrink (GS) -> bnlearn
* Incremental Association Markov Blanket (IAMB) -> bnlearn
* Fast Incremental Association (Fast-IAMB) -> bnlearn
* Interleaved Incremental Association (Inter-IAMB) -> bnlearn
* Incremental Association with FDR Correction (IAMB-FDR) -> bnlearn
* Max-Min Parents & Children (MMPC) -> bnlearn
* Semi-Interleaved Hiton-PC (SI-HITON-PC) -> bnlearn
* Hybrid Parents & Children (HPC) -> bnlearn
* Chow-Liu -> bnlearn
* ARACNE -> bnlearn

### Score-Based
* Hill Climbing (HC) -> bnlearn
* Tabu Search (Tabu) -> bnlearn
* Greedy-Equivalent-Search (GES) -> pcalg
* Linear NO-TEARS -> own reimplementation in R

### Hybrid (Skeleton/Constraint-Based + Score-Based)
* General 2-Phase Restricted Maximization (RSMAX2) -> bnlearn
* Max-Min Hill Climbing (MMHC) -> bnlearn
* Hybrid HPC (H2PC) -> bnlearn
* Adaptively Restricted GES (ARGES) -> pcalg

### Graphical Lasso
* GLASSO -> glasso

### Restricted Structural Equation Models
* LINGAM -> pcalg

### Graphical Continuous Lyapunov Models
* GCLM -> gclm

### NODAG
* NODAG -> https://github.com/gherardovarando/nodag

## Install *gnlearn* R Package


```R
if (!requireNamespace('devtools', quietly=TRUE))
    install.packages('devtools')
devtools::install_github('rlebron-bioinfo/gnlearn')
```

## Quick Start


```R
library(gnlearn)
```






```R
list.datasets(sp.common='human')
```


<table>
<caption>A data.frame: 8 × 11</caption>
<thead>
	<tr><th></th><th scope=col>download.code</th><th scope=col>sp.scientific</th><th scope=col>sp.common</th><th scope=col>bio.layer</th><th scope=col>seq.protocol</th><th scope=col>cell.identity</th><th scope=col>n.genes</th><th scope=col>n.cells</th><th scope=col>ref.authors</th><th scope=col>ref.doi</th><th scope=col>raw.dataset</th></tr>
	<tr><th></th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;chr&gt;</th></tr>
</thead>
<tbody>
	<tr><th scope=row>94</th><td> 94</td><td>Homo sapiens</td><td>Human</td><td>Transcription</td><td>10x Chromium</td><td>Breast Cancer Cell Line (MCF7)</td><td>535</td><td> 4367</td><td>Ben-David et al. 2018               </td><td>10.1038/s41586-018-0409-3                            </td><td>https://github.com/rlebron-bioinfo/gnlearn-datasets/raw/master/Homo_sapiens/Transcription/Ben-David2018/breast_cancer_MCF7_rep1.tsv.gz</td></tr>
	<tr><th scope=row>95</th><td> 95</td><td>Homo sapiens</td><td>Human</td><td>Transcription</td><td>10x Chromium</td><td>Breast Cancer Cell Line (MCF7)</td><td>543</td><td> 4806</td><td>Ben-David et al. 2018               </td><td>10.1038/s41586-018-0409-3                            </td><td>https://github.com/rlebron-bioinfo/gnlearn-datasets/raw/master/Homo_sapiens/Transcription/Ben-David2018/breast_cancer_MCF7_rep2.tsv.gz</td></tr>
	<tr><th scope=row>96</th><td> 96</td><td>Homo sapiens</td><td>Human</td><td>Transcription</td><td>10x Chromium</td><td>Breast Cancer Cell Line (MCF7)</td><td>523</td><td> 7370</td><td>Ben-David et al. 2018               </td><td>10.1038/s41586-018-0409-3                            </td><td>https://github.com/rlebron-bioinfo/gnlearn-datasets/raw/master/Homo_sapiens/Transcription/Ben-David2018/breast_cancer_MCF7_rep3.tsv.gz</td></tr>
	<tr><th scope=row>97</th><td> 97</td><td>Homo sapiens</td><td>Human</td><td>Transcription</td><td>10x Chromium</td><td>Breast Cancer Cell Line (MCF7)</td><td>520</td><td> 5600</td><td>Ben-David et al. 2018               </td><td>10.1038/s41586-018-0409-3                            </td><td>https://github.com/rlebron-bioinfo/gnlearn-datasets/raw/master/Homo_sapiens/Transcription/Ben-David2018/breast_cancer_MCF7_rep4.tsv.gz</td></tr>
	<tr><th scope=row>98</th><td> 98</td><td>Homo sapiens</td><td>Human</td><td>Transcription</td><td>10x Chromium</td><td>Breast Epithelium             </td><td>545</td><td> 8458</td><td>Chen et al. 2019; Nguyen et al. 2018</td><td>10.1038/s42003-019-0554-8; 10.1038/s41467-018-04334-1</td><td>https://github.com/rlebron-bioinfo/gnlearn-datasets/raw/master/Homo_sapiens/Transcription/Nguyen2018/breast_epithelium_rep1.tsv.gz    </td></tr>
	<tr><th scope=row>99</th><td> 99</td><td>Homo sapiens</td><td>Human</td><td>Transcription</td><td>10x Chromium</td><td>Breast Epithelium             </td><td>609</td><td>11000</td><td>Chen et al. 2019; Nguyen et al. 2018</td><td>10.1038/s42003-019-0554-8; 10.1038/s41467-018-04334-1</td><td>https://github.com/rlebron-bioinfo/gnlearn-datasets/raw/master/Homo_sapiens/Transcription/Nguyen2018/breast_epithelium_rep2.tsv.gz    </td></tr>
	<tr><th scope=row>100</th><td>100</td><td>Homo sapiens</td><td>Human</td><td>Transcription</td><td>10x Chromium</td><td>Breast Epithelium             </td><td>576</td><td>10555</td><td>Chen et al. 2019; Nguyen et al. 2018</td><td>10.1038/s42003-019-0554-8; 10.1038/s41467-018-04334-1</td><td>https://github.com/rlebron-bioinfo/gnlearn-datasets/raw/master/Homo_sapiens/Transcription/Nguyen2018/breast_epithelium_rep3.tsv.gz    </td></tr>
	<tr><th scope=row>101</th><td>101</td><td>Homo sapiens</td><td>Human</td><td>Transcription</td><td>10x Chromium</td><td>Breast Epithelium             </td><td>615</td><td>10840</td><td>Chen et al. 2019; Nguyen et al. 2018</td><td>10.1038/s42003-019-0554-8; 10.1038/s41467-018-04334-1</td><td>https://github.com/rlebron-bioinfo/gnlearn-datasets/raw/master/Homo_sapiens/Transcription/Nguyen2018/breast_epithelium_rep4.tsv.gz    </td></tr>
</tbody>
</table>




```R
list.genesets(sp.common='human')
```


<table>
<caption>A data.frame: 3 × 5</caption>
<thead>
	<tr><th></th><th scope=col>download.code</th><th scope=col>sp.scientific</th><th scope=col>sp.common</th><th scope=col>dataset</th><th scope=col>url</th></tr>
	<tr><th></th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;chr&gt;</th></tr>
</thead>
<tbody>
	<tr><th scope=row>4</th><td>4</td><td>Homo sapiens</td><td>Human</td><td>Genes                 </td><td>https://raw.githubusercontent.com/rlebron-bioinfo/gnlearn-datasets/master/Homo_sapiens/Genes/human_genes.txt                 </td></tr>
	<tr><th scope=row>5</th><td>5</td><td>Homo sapiens</td><td>Human</td><td>TF-Target Interactions</td><td>https://raw.githubusercontent.com/rlebron-bioinfo/gnlearn-datasets/master/Homo_sapiens/Genes/human_tf-target_interactions.txt</td></tr>
	<tr><th scope=row>6</th><td>6</td><td>Homo sapiens</td><td>Human</td><td>Drug-Gene Interactions</td><td>https://raw.githubusercontent.com/rlebron-bioinfo/gnlearn-datasets/master/Homo_sapiens/Genes/human_drug-gene_interactions.txt</td></tr>
</tbody>
</table>




```R
list.graphs(sp.common='human')
```


<table>
<caption>A data.frame: 0 × 13</caption>
<thead>
	<tr><th scope=col>download.code</th><th scope=col>sp.scientific</th><th scope=col>sp.common</th><th scope=col>dataset</th><th scope=col>bio.layer</th><th scope=col>cell.identity</th><th scope=col>algorithm</th><th scope=col>algorithm.args</th><th scope=col>n.nodes</th><th scope=col>n.edges</th><th scope=col>n.undirected.edges</th><th scope=col>n.directed.edges</th><th scope=col>url</th></tr>
	<tr><th scope=col>&lt;int&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;lgl&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;chr&gt;</th></tr>
</thead>
<tbody>
</tbody>
</table>



## Documentation

### Genesets

### Datasets

### Graphs


```R

```
