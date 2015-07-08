###

library(biomaRt)

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

#fetch_coordinates_for_gene(ensembl_human,'PABPC5')
fetch_coordinates_for_gene = function(ensembl_in,gene_id, key='external_gene_name') {
  results = getBM(attributes=c('ensembl_gene_id','chromosome_name','start_position','end_position', 'external_gene_name'),
                  filters = key,
                  values=gene_id,
                  mart = ensembl_in)
  
  if(dim(results)[1]>0) {
    return(results[1,c('chromosome_name','start_position','end_position')])
  } else {
    return(NULL)
  }
}