###

library(biomaRt)
library(IRanges)
library(ggplot2)

ensembl=useMart("ensembl")  # using ensembl database data

ensembl_human=useDataset("hsapiens_gene_ensembl",mart=ensembl)

#fetch_all_genes_for_region(ensembl_human,'X',65656350,75656592)
fetch_all_genes_for_region = function(ensembl_in,chr_num,start,end) {
  results = getBM(attributes=c('ensembl_gene_id','chromosome_name','start_position','end_position', 'external_gene_name'),
                  filters = c('chromosome_name','start','end'),
                  values=list(chr_num,start,end),
                  mart = ensembl_in)
  if(dim(results)[1]>0) {
    return(results[,c('chromosome_name','start_position','end_position','external_gene_name')])
  } else {
    return(NULL)
  }
  
}

fetch_all_exons_for_region = function(ensembl_in,chr_num,start,end) {
  if(end-start>1e7) {
    stop('range too big')
  }
  results = getBM(attributes=c('ensembl_exon_id','chromosome_name','exon_chrom_start','exon_chrom_end', 'external_gene_name'),
                  filters = c('chromosome_name','start','end'),
                  values=list(chr_num,start,end),
                  mart = ensembl_in)
  return(results[,c('ensembl_exon_id', 'chromosome_name','exon_chrom_start','exon_chrom_end','external_gene_name')])

}

#fetch_coordinates_for_gene(ensembl_human,'PABPC5')
fetch_coordinates_for_gene = function(ensembl_in,gene_id, key='external_gene_name') {
  results = getBM(attributes=c('ensembl_gene_id','chromosome_name','start_position','end_position', 'external_gene_name'),
                  filters = key,
                  values=gene_id,
                  mart = ensembl_in)
  return(results[1,c('chromosome_name','start_position','end_position')])
 
}


# 
# coverageAll = ct$coverageAll
# 
# Start =  11157025  
# 
# End = 11158264
# 
# chr = 'chr1'
# plot_with_exons(coverageAll,targets,chr,Start,End, ensembl_in=ensembl_human)

plot_with_exons= function(coverageAll, targets, chr, Start, End, Offset=0, add=FALSE,
                          col.line=1, col.target="orange", col.offset="yellow", ensembl_in, ...) {
  
  covercounts = coverageAll[[chr]]
  chrom = substr(chr, 4, nchar(chr)) #chromosome number
  
  # stop if all reads lie "left" of the selected Start position
  L = length(covercounts)
  if(L < Start)
    
    stop(paste("no reads falls into the selected region on chromosome", chrom))
  
  # add 0's when 'End' is "right" of largest read position
  if(L < End)
    covercounts = c(covercounts, Rle(rep(0, End-L)))
  
  ir = IRanges(start=Start, end=End)
  covsel = covercounts[ir]  # use [ instead of deprecated seqselect
  
  # also stop if coverage is 0 for all bases in selected region
  if(all(covsel == 0))
    stop(paste("no reads falls into the selected region on chromosome", chrom))
  
  ma = max(covsel)
  #mi = .04 * ma
  #ylim = c(-mi, ma)
  
  data_to_plot = data.frame(x=Start:End, coverage=covsel)
  
  # genes = fetch_all_genes_for_region(ensembl_in = ensembl_human,chr_num = chrom, start = Start, end = End)
  
  exons = fetch_all_exons_for_region(ensembl_in = ensembl_human,chr_num = chrom, start = Start, end = End)
  
#   exons=exons[(exons$exon_chrom_start>Start &  exons$exon_chrom_start<End) |
#                 (exons$exon_chrom_end>Start &  exons$exon_chrom_end<End) ,]
  #only ones in our desired range
  
  plot = ggplot(data=data_to_plot, aes(x=x,y=covsel))+
    geom_line(size=1, colour='black',alpha=0.8)+
    scale_y_continuous('Coverage')+
    xlab(paste('position on chromososme', chr))+
    theme_bw()
  
  if(dim(exons)[1]>0) {
    
#     exons2 = exons[,3:5]
#     exons2 = as(reduce(as(exons2,'GRanges')),'data.frame')
    iexons = reduce(IRanges(start=exons$exon_chrom_start,end=exons$exon_chrom_end))
    exonsrange = range(iexons)
    iexons = intersect(ir,iexons)
    exonsrange = setdiff(intersect(ir, exonsrange),iexons)
    
    ma_exon = max(max(iexons))
    mi_exon = min(min(iexons))
    
    mid_point = (ma_exon + mi_exon)/2
    gene_name = exons$external_gene_name[1]
    #the above should be split across multiple genes (if threr are more than 1)
    
    data_names = data.frame(x=mid_point,y=8,label=gene_name)
    
  plot = plot+
      geom_rect(data=as.data.frame(exonsrange),aes(x=NULL,y=NULL,xmin=start,ymin=2,ymax=3,xmax=end),
                fill='blue', alpha = 0.3)+
      geom_rect(data=as.data.frame(iexons),aes(x=NULL,y=NULL,xmin=start,ymin=0,ymax=5,xmax=end),
                fill='blue', alpha = 0.3)+
      geom_text(data=data_names,aes(x=x,y=y,label=label),  colour='blue', alpha = 0.8)
  }
  
  tar = intersect(ir, ranges(targets)[[chr]])
  
  plot = plot +
    geom_rect(data=as.data.frame(tar),aes(x=NULL,y=NULL,xmin=start,ymin=0,ymax=ma,xmax=end),
                       fill='grey', alpha = 0.3)
    
  
  #show plot/return plot
  plot
  
}


