
library(TEQC)
data_dir <- "example_data"

targets <- get.targets(targetsfile=paste(data_dir, "ExampleSet_Targets.bed", sep="/"), chrcol=1, startcol=2, endcol=3, skip=0)
save(targets, file=file.path(data_dir, "targets.RData"))

reads <- get.reads(paste(data_dir, "ExampleSet_Reads.bed", sep="/"), chrcol=1, startcol=2, endcol=3, idcol=4, zerobased=F, skip=0)
readpairs <- reads2pairs(reads)
reads <- reads[!(reads$ID%in%readpairs$singleReads$ID), , drop=TRUE]
save(reads, file=file.path(data_dir, "reads.RData"))

all_coverage <- coverage.target(reads, targets, perTarget=TRUE, perBase=TRUE)
save(all_coverage, file=file.path(data_dir, "all_coverage.RData"))
