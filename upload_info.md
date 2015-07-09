## Upload your own data here

* For reads an .RData file with a RangedData object as returned by function TEQC::get.reads() and named reads is required. Starting from a .bam or .bed file use the code below for preprocessing. 
* targets requires a RangedData object as returned by function TEQC::get.targets() and named targets. Starting from a .bed file use the code below for preprocessing. 
* Finally the coverage object required can be build from the former two objects using function TEQC::coverage.target() and should be named all_coverage.

### script for data preprocessing

```{r}
library(TEQC)
data_dir <- "path_to/your_outdir"

targets <- get.targets(targetsfile=paste(data_dir, "your.bed", sep="/"), 
                       chrcol=1, startcol=2, endcol=3, skip=0)
save(targets, file=file.path(data_dir, "targets.RData"))

# starting from a .bam
reads <- get.reads(paste(data_dir, "your.bam", sep="/"))
# starting from a .bed, adjust params as necessary, see ?TEQC::get.reads
reads <- get.reads(paste(data_dir, "your.bed", sep="/"), 
                   chrcol=1, startcol=2, endcol=3, idcol=4, zerobased=FALSE, skip=0)
# if your reads are paired
readpairs <- reads2pairs(reads)
reads <- reads[!(reads$ID%in%readpairs$singleReads$ID), , drop=TRUE]
#
save(reads, file=file.path(data_dir, "reads.RData"))

all_coverage <- coverage.target(reads, targets, perTarget=TRUE, perBase=TRUE)
save(all_coverage, file=file.path(data_dir, "all_coverage.RData"))
```
